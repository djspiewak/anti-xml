/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit
package antixml

import util._

import scala.collection.generic.{CanBuildFrom, FilterMonadic}

trait Zipper[+A <: Node] extends Group[A] with ScalaCompat { self =>
  // TODO dependently-typed HList, maybe?

  protected def map: Vector[Option[ZContext]]
  protected def parent: Zipper[Node]

  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVectorCase)
  
  def unselect: Zipper[Node] = {
    val nodes2 = (map zip parent.toVectorCase).foldLeft(VectorCase[Node]()) {
      case (acc, (Some((from, to, _, _)), _: Elem)) if from == to => acc
      case (acc, (Some((from, to, rebuild, childMap)), _: Elem)) => acc :+ rebuild(self.slice(from, to), childMap)
      case (acc, (_, e)) => acc :+ e
    }

    new Group(nodes2) with Zipper[Node] {
      val map = self.parent.map
      def parent = self.parent.parent
    }
  }

  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val builder = cbf(parent.asInstanceOf[Group[A]], map)      // oddly, the type-checker isn't handling this
      builder ++= (toVectorCase map f)
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def flatMap[B, That](f: A => CompatTraversable[B])(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val result = toVectorCase.toVector map f

      val intermedMap = map map {
        case Some((from, to, rebuild, childMap)) => {
          // get the mapping from *our* indexes to source indexes
          val inverseMap = {
            val maps = for ((source, targets) <- childMap)
              yield (Map[Int, Int]() /: targets) { (acc, t) => acc + (t -> source) }
  
            (Map[Int, Int]() /: maps) { _ ++ _ }
          }
  
          val (_, aggregate, childMap2) = result.slice(from, to).zipWithIndex.foldLeft((0, Vector[B](), Map[Int, Set[Int]]())) {
            case ((start, acc, childMap2), (chunk, i)) => {
              val size = chunk.size
              val source = inverseMap(i)
  
              val contrib = Set(start until (start + size): _*)
              val set2 = childMap2.getOrElse(source, contrib) ++ contrib
  
              (start + size, acc ++ chunk, childMap2.updated(source, set2))
            }
          }
  
          val length = aggregate.length
          val delta = length - (to - from)
          Some((from, to + delta, rebuild, childMap2, aggregate, delta))
        }
        
        case None => None
      }

      val (_, map2, chunks) = intermedMap.foldLeft((0, Vector[Option[ZContext]](), Vector[Vector[B]]())) {
        case ((offset, map2, acc), Some((from, to, rebuild, childMap, aggregate, delta))) => {
          val from2 = from + offset
          val to2 = to + offset
          val offset2 = offset + delta
          (offset2, map2 :+ Some((from2, to2, rebuild, childMap)), acc :+ aggregate)
        }
        
        case ((offset, map2, acc), None) => (offset, map2 :+ None, acc)
      }

      val builder = cbf(parent.asInstanceOf[Group[A]], map2)
      chunks foreach (builder ++=)
      builder.result
    }

    case _ => super.flatMap(f)(cbf)
  }
  
  override def filter(f: A => Boolean): Zipper[A] = collect {
    case e if f(e) => e
  }
  
  override def withFilter(f: A => Boolean) = new WithFilter(List(f))
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[Group[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVectorCase) with Zipper[B] {
      val map = self.map
      val parent = self.parent
    }
  }
  
  override def toZipper = self
  
  class WithFilter(filters: List[A => Boolean]) extends FilterMonadic[A, Group[A]] {
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Group[A], B, That]) =
      self filter { a => filters forall { _(a) } } map f
    
    def flatMap[B, That](f: A => CompatTraversable[B])(implicit bf: CanBuildFrom[Group[A], B, That]) =
      self filter { a => filters forall { _(a) } } flatMap f
    
    def foreach[B](f: A => B) = self foreach f
    
    def withFilter(p: A => Boolean) = new WithFilter(p :: filters)
  }
}
