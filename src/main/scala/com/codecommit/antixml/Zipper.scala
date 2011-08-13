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

import scala.collection.IndexedSeqLike
import scala.collection.generic.{CanBuildFrom, FilterMonadic}

trait Zipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, Zipper[A]] with ScalaCompat { self =>
  // TODO dependently-typed HList, maybe?
  
  protected def source: Zipper[Node]
  protected def contexts: List[ZContext]
  protected val hasValidContext = true
   
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVectorCase)
    
  /**
   * Calculates the 'unselect' replacement for a given Node in the source tree.
   *
   * @param sourceNode a node from the source tree.
   * @param path the path to `sourceNode`
   * @param unmatchedContexts the list of ZContexts remaining to be matched (must be sorted by path, lexicographically)
   * @param offset the offset into this group to copy from when `path` matches the next path in `unmatchedContexts`.
   * @return If `sourceNode` should be replaced, the return value is `Some(result, newContexts, newOffset)`
   *         where `result` is the sequence of replacement nodes, and `newOffset` and `newContexts` reflect the
   *         advancement of `offset` and `unmatchedContexts` past the replacement nodes.  Otherwise, the return value
   *         is `None`.
   */
  private def unselectNode(sourceNode: Node, path: IndexedSeq[Int], unmatchedContexts: List[ZContext], offset: Int): Option[(IndexedSeq[Node], List[ZContext], Int)] = {
    import Zipper.PrefixOrder
    
    unmatchedContexts match {
      case ZContext(contextPath, contextCount) :: contextsTail => {
        //TODO - could make the path comparison more efficient by keeping track of the length of the match between the 2 paths.
        PrefixOrder.tryCompare(path, contextPath) match {
          case Some(0) => {           //paths are the same --> replace sourceNode
            Some((toVectorCase.slice(offset, offset+contextCount), contextsTail, offset+contextCount))
          }
          case Some(n) if n < 0 => {  //path is a prefix of contextPath --> recusrively replace sourceNode's.children
            sourceNode match {
              case e@Elem(_,_,_,_,children) => {
                val (children2,umContexts2,offset2) = ((Vector0:VectorCase[Node], unmatchedContexts, offset) /: (children.zipWithIndex)) {
                  case ((acc, ctx, off),(nd,i)) => {
                    unselectNode(nd, path :+ i, ctx, off) match {
                      case None => (acc :+ nd, ctx, off)
                      case Some((nds, ctx2, off2)) => (acc ++ nds, ctx2, off2)
                    }
                  }
                }
                Some((new Vector1(e.copy(children=new Group(children2))), umContexts2, offset2))
              }
              case _ => error("Context path descends through a non-element sourceNode")
            }
          }
          case None => None  //Non-matching node
          case Some(_) => error("Node path extends context path")
        }
      }
      case Nil => None  
    }
  }
  
  /**
   * Calculates the 'unselect' replacement for each top level node in the source tree.
   */
  private def unselectTopLevelNodes: IndexedSeq[IndexedSeq[Node]] = {
    val (unsels, _, _) = ((Vector.empty[IndexedSeq[Node]], contexts, 0) /: source.zipWithIndex) {
      case ((acc, ctxs, off), (sourceNode, index)) => {
        unselectNode(sourceNode, Vector1(index), ctxs, off) match {
          case None => (acc :+ Vector1(sourceNode), ctxs, off)
          case Some((nds, ctxs2, off2)) => (acc :+ nds, ctxs2, off2)
        }
      }
    }
    unsels
  } 
  
  def unselect: Zipper[Node] = {
    val newNodeSeqs = unselectTopLevelNodes 
    val newNodes = newNodeSeqs.flatMap(identity)(VectorCase.canBuildFrom)
    
    if (!source.hasValidContext) {
      new Group(newNodes) with Zipper[Node]{
        override def source = error("Cannot unselect past original source")
        override def contexts = List()
        override val hasValidContext = false
      }
    } else {
      val (sourceChunks, _) = ((Vector0:VectorCase[Range],0) /: source.contexts) {
        case ((acc,off),ZContext(_, count)) => (acc :+ Range(off,off+count),off+count)
      }
      val newContexts = source.contexts zip sourceChunks map {
        case (zctx, chunk) => {
          val newCount = (0 /: chunk) {
            case (sz, indx) => sz + newNodeSeqs(indx).size
          }
          zctx.copy(count=newCount)
        }
      }
      new Group(newNodes) with Zipper[Node]{
        override val source = self.source.source
        override val contexts = newContexts
      }
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
      
      val builder = cbfwz(source.asInstanceOf[Zipper[A]], contexts)      // oddly, the type-checker isn't handling this
      builder ++= (toVectorCase map f)
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def flatMap[B, That](f: A => CompatTraversable[B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = cbf match {
    case cbf: CanProduceZipper[Zipper[A], B, That] => {
      implicit val cbfwz = cbf.lift
      
      if (!hasValidContext) {
        super.flatMap(f)(cbf)     // don't try to preserve
      } else {
        val result = toVectorCase.toVector map f
        
        val (newChunks, newContexts, _) = ((Vector.empty[Vector[B]], Vector.empty[ZContext], 0) /: contexts) {
          case((chunkAcc, ctxAcc, offset), z @ ZContext(_,0)) => 
            (chunkAcc :+ Vector.empty[B], ctxAcc :+ z, offset)
          case((chunkAcc, ctxAcc, offset), z @ ZContext(_,chunkSize)) => {
            val fmNodes = result.slice(offset,offset+chunkSize).flatMap(identity)
            val fmChunkSize = fmNodes.size
            (chunkAcc :+ fmNodes, ctxAcc :+ z.copy(count=fmChunkSize), offset + chunkSize)
          }
        }
            
        val builder = cbfwz(source.asInstanceOf[Zipper[A]], newContexts.toList)
        newChunks foreach (builder ++=)
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
      override val source = self.source
      override val contexts = self.contexts
    }
  }
  
  override def toZipper = self
  
  class WithFilter(filters: List[A => Boolean]) extends FilterMonadic[A, Zipper[A]] {
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self filter { a => filters forall { _(a) } } map f
    
    def flatMap[B, That](f: A => CompatTraversable[B])(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
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
      val contexts = List.empty
      def source = error("No zipper context available")
      override val hasValidContext = false
    }
  }
  
  /**
   * Partial order representing the 'startsWith' relationship for sequences.
   * 
   * 'lteq(x, y)' if and only if y.startsWith(x)
   */
  private object PrefixOrder extends scala.math.PartialOrdering[Seq[Int]] {
    override def lteq(x: Seq[Int], y:Seq[Int]): Boolean = tryCompare(x,y) match {
      case Some(n) if n <= 0 => true
      case _ => false
    }
    
    override def tryCompare(x: Seq[Int], y:Seq[Int]): Option[Int] = {
      val xi = x.iterator
      val yi = y.iterator
      while (xi.hasNext && yi.hasNext)
        if (xi.next != yi.next)
          return None
      
      if (xi.hasNext)
        Some(1)
      else if (yi.hasNext)
        Some(-1)
      else
        Some(0)
    }
  }
  
}
