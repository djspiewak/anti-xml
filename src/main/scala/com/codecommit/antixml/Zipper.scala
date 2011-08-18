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

import scala.collection.{GenTraversable, GenTraversableOnce, IndexedSeqLike}
import scala.collection.generic.{CanBuildFrom, FilterMonadic}

trait Zipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, Zipper[A]] { self =>
  // TODO dependently-typed HList, maybe?

  protected def map: Vector[Option[ZContext]]
  protected def parent: Zipper[Node]
  protected val hasValidContext = true
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVectorCase)
  
  def unselect: Zipper[Node] = {
    def superSlice(from: Int, until: Int) = super.slice(from, until).toZipper 
    
    val nodes2 = (map zip parent.toVectorCase).foldLeft(VectorCase[Node]()) {
      case (acc, (Some((from, to, rebuild, childMap)), _: Elem)) if from == to =>
        acc :+ rebuild(Group(), childMap mapValues Function.const(0))
      
      case (acc, (Some((from, to, rebuild, childMap)), _: Elem)) =>
        acc :+ rebuild(superSlice(from, to), childMap)
      
      case (acc, (_, e)) =>
        acc :+ e
    }

    new Group(nodes2) with Zipper[Node] {
      val map = self.parent.map
      def parent = self.parent.parent
    }
  }
  
  override protected[this] def newBuilder = Zipper.newBuilder[A]
  
  override def drop(n: Int): Zipper[A] = slice(n, size)
  
  override def slice(from: Int, until: Int): Zipper[A] = {
    val zwi = Map[A, Int](zipWithIndex: _*)
    collect {
      case e if zwi(e) >= from && zwi(e) < until => e
    }    
  }
  
  override def splitAt(n: Int) = (take(n), drop(n))
  
  override def take(n: Int) = slice(0, n)

  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = cbf match {
    case cbf: CanProduceZipper[Zipper[A], B, That] => {
      implicit val cbfwz = cbf.lift
      
      val builder = cbfwz(parent.asInstanceOf[Zipper[A]], map)      // oddly, the type-checker isn't handling this
      builder ++= (toVectorCase map f)
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = cbf match {
    case cbf: CanProduceZipper[Zipper[A], B, That] => {
      implicit val cbfwz = cbf.lift
      
      if (!hasValidContext) {
        super.flatMap(f)(cbf)     // don't try to preserve
      } else {
        val result = toVectorCase.toVector map f
  
        val intermedMap = map map {
          case Some((from, to, rebuild, childMap)) => {            
            val childMapSorted = (childMap.toSeq) sortWith { _._1 < _._1 } //TODO - Maybe just make childMap a SortedMap 
            
            val (lastOffset, chunk, childMap2) = ((from, Vector[B](), Map[Int,Int]()) /: childMapSorted) {
              case ((offset, acc, childMap2),(srcIndex, 0)) =>
                (offset, acc, childMap2 + (srcIndex -> 0))
              
              case ((offset, acc, childMap2),(srcIndex,destCount)) => {
                val items = result.slice(offset, offset+destCount).flatMap(identity)
                (offset + destCount, acc ++ items, childMap2 + (srcIndex -> items.size))
              }
            }
            assert(lastOffset == to)
            
            Some(chunk, rebuild, childMap2)
          }
          
          case None => None
        }
  
        val (_, map2, chunks) = ((0, Vector[Option[ZContext]](), Vector[Vector[B]]()) /: intermedMap) {
          case ((offset, map2, acc), Some((chunk,rebuild,childMap))) => {
            (offset + chunk.size, map2 :+ Some((offset, offset+chunk.size, rebuild, childMap)), acc :+ chunk)
          }
          
          case ((offset, map2, acc), None) => (offset, map2 :+ None, acc)
        }
          
        val builder = cbfwz(parent.asInstanceOf[Zipper[A]], map2)
        chunks foreach (builder ++=)
        builder.result
      }
    }

    case _ => super.flatMap(f)(cbf)
  }
  
  override def filter(f: A => Boolean): Zipper[A] = collect {
    case e if f(e) => e
  }
  
  override def withFilter(f: A => Boolean) = new WithFilter(List(f))
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVectorCase) with Zipper[B] {
      val map = self.map
      val parent = self.parent
    }
  }
  
  override def toZipper = self
  
  class WithFilter(filters: List[A => Boolean]) extends FilterMonadic[A, Zipper[A]] {
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self filter { a => filters forall { _(a) } } map f
    
    def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self filter { a => filters forall { _(a) } } flatMap f
    
    def foreach[B](f: A => B) = self foreach f
    
    def withFilter(p: A => Boolean) = new WithFilter(p :: filters)
  }
}

object Zipper {
  implicit def canBuildFrom[A <: Node]: CanBuildFrom[Zipper[_], A, Zipper[A]] = new CanBuildFrom[Zipper[_], A, Zipper[A]] with CanProduceZipper[Zipper[_], A, Zipper[A]] {
    def apply(from: Zipper[_]) = apply()    // TODO
    def apply() = newBuilder[A]
    
    def lift = Group.canBuildFromWithZipper
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A] mapResult { vec =>
    new Group(vec) with Zipper[A] {
      val map = Vector()
      def parent = error("No zipper context available")
      override val hasValidContext = false
    }
  }
}
