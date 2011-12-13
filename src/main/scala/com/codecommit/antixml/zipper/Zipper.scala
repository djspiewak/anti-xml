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

package com.codecommit.antixml

import com.codecommit.antixml.util.VectorCase
import scala.annotation.tailrec
import scala.collection.{immutable, mutable, IndexedSeqLike, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.immutable.{SortedMap, IndexedSeq, SortedSet}
import scala.collection.mutable.Builder
import Zipper._
import CanBuildFromWithZipper._

/** 
 * Provides an `unselect` operation which copies this Group's nodes back to the XML tree from which
 * it was derived.See the [[http://anti-xml.org/zippers.html Anti-XML Overview]] for a 
 * high-level description of this functionality.  
 *
 * The `Zipper` trait augments a [[com.codecommit.antixml.Group]] with additional immutable state used 
 * to support the `unselect` method.  This state is known as the "zipper context" and is defined by:
 *  - A reference to another `Group`, known as the ''parent'' of the Zipper.
 *  - A set of (possibly deep) locations within the parent, known  
 *  as the ''holes'' of the Zipper.
 *  - A mapping from the top-level indices of the Zipper to its holes, known as
 *  the Zipper's ''replacement map'' 
 * 
 * Loosely speaking, the `unselect` method produces an updated version of the 
 * parent by replacing its holes with the nodes from the Zipper, as determined by the replacement map.
 * A formal definition of `unselect` can be found below.
 * 
 * Certain "modify" operations on a `Zipper` will propagate the zipper context to the result.
 * The new Zipper's `unselect` method can then be viewed as applying
 * these modifications back to the parent tree.  Currently, the following methods 
 * support this propagation of the zipper context:
 *   - `updated`, `map`, `flatMap`, `filter`, `collect`, `slice`, `drop`, `take`, `splitAt`, and
 * `unselect` (the latter viewed as a modification of the parent `Zipper`). 
 *
 * These operations all provide a natural identification of indices in the new Zipper with
 * the indices they were derived from in the original.  This identification is used to lift
 * the replacement map to the new Zipper.  The parent and holes of the new Zipper are always the same
 * as those of the original.
 *
 * Of course, propagation is only possible if the result can legally be a `Zipper`.  Replacing a `Node`
 * with a `String`, for example, will result in an undecorated `IndexedSeq` because the result violates
 * Zipper's type bounds.
 *
 * ==== Node Multiplication and Elision ====
 *
 * A Zipper's replacement map need neither be injective nor surjective.  
 * Injectivity can fail due to the action of `flatMap`, which replaces a node with a sequence of nodes, 
 * all of which are associated with the original node's hole. In such cases, `unselect` will replace the hole with the 
 * entire sequence of nodes mapping to it.  Surjectivity can fail due to any operation that "removes" items 
 * from the `Zipper`.  If a hole is not associated with any Zipper nodes, then `unselect` will remove that position
 * from the resulting tree.
 *
 * ==== Conflicting Holes ====
 *
 * For a given Zipper, a hole, H, is said to be ''conflicted'' if the Zipper contains another hole,
 * H,,c,, , contained in the subtree at H.  In this case, the Zipper is said to be
 * ''conflicted at'' H.  A Zipper that does not contain conflicted holes is said to be ''conflict free''. 
 * Conflicted holes arise when a selection operator yields both a node and one or more of its descendants.
 * They are of concern because there is no canonical way to specify the behavior of `unselect` at a 
 * conflicted hole.  Instead, a [[com.codecommit.antixml.ZipperMergeStrategy]], implicitly provided
 * to `unselect`, is used to resolve the conflict.
 *
 * A default ZipperMergeStrategy has been provided that should suffice for the most common use cases involving
 * conflicted holes.  In particular, if modifications to a conflicted element are limited to its top-level properties 
 * (`name`, `attributes`, etc.), then the default strategy will apply those changes while preserving any modifications 
 * made to those descendant nodes also present in the Zipper.  However, if the `children` property of a conflicted element
 * is directly modified, then the default strategy's behavior is formally unspecified.  Currently it uses a heuristic 
 * algorithm to resolve conflicts, but its details may change in a future release.
 *
 * Of the [[com.codecommit.antixml.Selectable]] operators, only `\\` is capable of producing conflicts.  
 * The `select`, `\`, and `\\!` operators always produce conflict-free Zippers.
 *
 * ====Unselection Algorithm====
 * 
 * Let G be a group, and Z be a zipper with G as its parent.  For each location, L, in G (top-level or otherwise), we make
 * the following definitions:
 * 
 *  - G(L) is the node in G at location L.
 *  - `children`(L) is the sequence of locations that are immediately below L in G. 
 *  - L is a ''hole'' if it is in Z's "holes" set.
 *  - L is ''above a hole'' if some descendant of L is a hole.
 *  - If L is a hole, the ''direct updates'' for L is the sequence of nodes given by the inverse of Z's replacement map,
 *  in the order defined by Z.
 *  - For any L, The ''indirect update'' for L is just G(L) with its children replaced by `children`(L)`.flatMap(pullback)`.
 *  - For any L, `pullback`(L) is the sequence of nodes given by the following recursive definition:
 *    - If L is not a hole, then `pullback`(L) is the singleton sequence consisting of the indirect update for L
 *    - If L is a hole, but is not above a hole, then `pullback`(L) is the direct updates for L.
 *    - Otherwise, L is conflicted and `pullback`(L) is the result of merging its direct updates and its
 *    indirect update according to the [[com.codecommit.antixml.ZipperMergeStrategy]] provided to `unselect`.
 *
 * Let T be the sequence of top-level locations in G.  Then `Z.unselect` is defined as `T.flatMap(pullback)`.
 *
 */
trait Zipper[+A <: Node] extends Group[A] 
		with IndexedSeqLike[A, Zipper[A]] 
		with ZipperGroupOverrides[A]
	    with ZipperUnselection
	    with ZipperHoleShifting
	    with ZipperAxes { self =>

  /** 
   * Returns the original group that was selected upon when the Zipper was created.  A value of `None` indicates that
   * the Zipper has no parent and `unselect` cannot be called.  This possibility is an unfortunate consequence of the
   * fact that some operations must return the static type of Zipper even though they yield no usable context.  
   * In practice, this situation is usually caused by one of the following operations:
   * 
   *   - A non-Zipper group is selected upon and then 'unselect' is used to generate an updated group. 
   *   - A method such as `++`, is used to "add" nodes to a zipper without replacing existing nodes. 
   *   
   **/
  def parent: Option[Zipper[Node]] = context map {_.parent}
  
  /** The zipper context or None if this is a broken zipper. */
  private[antixml] val context: Option[Context]
  
  /**
   * Returns a `Group` containing the same nodes as this Zipper, but without any Zipper context, and in particular,
   * without any implict references to the zipper's parent.
   */
  def stripZipper = new Group(toVectorCase)
  
  /** Applies the node updates to the parent and returns the result. */
  def unselect(implicit zms: ZipperMergeStrategy): Zipper[Node] = { 
    val ctx = context.getOrElse(sys.error("Zipper does not have a valid context"))
    unselect(ctx, zms)
  }
}

object Zipper {
    
  /**
   * Defines the zipper context
   *
   * @param parent the zipper's parent
   * @param lastUpdate A value that is greater than the update time of any node or path in the zipper. 
   * Subsequent updates must be tagged with a larger time.
   * @param metas the path and time associated with each node in the Zipper.  Each element in this structure
   * corresponds with the node at the same index in the Zipper.
   * @param additionalHoles assertions that indicate the specified path is associated with the zipper
   * and has at least the specified time.  If there are paths in this structure that are not in `metas`,
   * then the corresponding hole will be replaced with an empty sequence during `unselect`.  
   * @param hiddenNodes Nodes that are contained in the Zipper but are not accessible through indexing.
   * These elements will participate in the unselection process just as the other ones. TODO any special assumptions on these elements?
   */
  private[antixml] case class Context(
      parent: Zipper[Node], 
      lastUpdate: Time, 
      metas: VectorCase[(ZipperPath, Time)], 
      additionalHoles: immutable.Seq[(ZipperPath, Time)],
      hiddenNodes: immutable.Seq[ElemsWithContextHidden])
  
  /** The units in which time is measured in the zipper. Assumed non negative. */
  private[antixml] type Time = Int
  
  implicit def canBuildFromWithZipper[A <: Node] = {
    new CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] {      
      override def apply(parent: Option[Zipper[Node]]) = newZipperContextBuilder(parent)
    }
  }
  
  implicit def canBuildFromDeep[A <: Node]: CanBuildFrom[Group[_], A, Zipper[A]] = {
    new CanBuildFrom[Group[_], A, Zipper[A]] with CanProduceZipper[Group[_], A, Zipper[A]] {
      def apply(from: Group[_]): Builder[A, Zipper[A]] = apply()
      def apply(): Builder[A, Zipper[A]] = Zipper.newBuilder[A]

      def lift = canBuildFromWithZipper
    }
  }
  
  /** Returns a builder that produces a zipper without a parent */
  def newBuilder[A <: Node] = VectorCase.newBuilder[A].mapResult(brokenZipper(_))

  /** Returns a builder that produces a zipper with a full zipper context */
  private[antixml] def newZipperContextBuilder[A <: Node](parent: Option[Zipper[Node]]) = parent match {
    case Some(p) => new WithZipperBuilder[A](p)
    case None => brokenZipperBuilder[A]
  }
  
  /** Returns a "broken" zipper which contains the specified nodes but cannot be unselected */
  private[antixml] def brokenZipper[A <: Node](nodes: VectorCase[A]): Zipper[A] = {
    new Group[A](nodes) with Zipper[A] {
      val context = None    
    }
  }
  
  /** Ignores context and builds a "broken" zipper */
  private def brokenZipperBuilder[A <: Node]: Builder[ElemsWithContext[A],Zipper[A]] = 
    CanBuildFromWithZipper.identityCanBuildFrom(VectorCase.canBuildFrom[A])(None) mapResult(brokenZipper(_))

  /**
   * The primary builder class used to construct Zippers. 
   */
  private class WithZipperBuilder[A <: Node](parent: Zipper[Node]) extends Builder[ElemsWithContext[A],Zipper[A]] { self =>
    
    import scala.collection.mutable.HashMap
    
    private val itemsBuilder = VectorCase.newBuilder[A]
    private val metasBuilder = VectorCase.newBuilder[(ZipperPath,Time)]
    private val additionalHolesBuilder = new AdditionalHolesBuilder()
    private val hiddenNodesBuilder = VectorCase.newBuilder[ElemsWithContextHidden]
    private var size = 0 //TODO is this used anywhere?
    private var maxTime = 0
    
    override def += (ewc: ElemsWithContext[A]) = {      
      // keeping track of the time in the context
      val time: Time = ewc match {
        case ElemsWithContextVisible(pseq, t, ns) => {
          val path: ZipperPath = ZipperPath.fromSeq(pseq)
          val pathTime = (path, t)

          var nsz = 0
          for (n <- ns) {
            itemsBuilder += n
            metasBuilder += pathTime
            nsz += 1
          }
          if (nsz == 0) {
            additionalHolesBuilder += pathTime
          }

          size += nsz
          t
        }
        case e @ ElemsWithContextHidden(_, t, _) => {
          hiddenNodesBuilder += e
          t
        }
      }
      
      maxTime = math.max(maxTime, time)
      this            
    }
    override def clear() {
      itemsBuilder.clear()
      metasBuilder.clear()
      additionalHolesBuilder.clear()
      hiddenNodesBuilder.clear()
      size = 0
      maxTime = 0
    }
    override def result(): Zipper[A] = {
      val ctx = Context(
          parent, maxTime, 
          metasBuilder.result(), 
          additionalHolesBuilder.result(),
          hiddenNodesBuilder.result())
      
      new Group[A](itemsBuilder.result()) with Zipper[A] {
        val context = Some(ctx)
      }
    }
  }

  /** Builder for the `additionalHoles` list.  This builder ensures that the result has at most one
   *  entry for any given ZipperPath.  Although this isn't necessary for correctness, it ensures that
   *  the `additionalHoles` list remains bounded in size by the total number of holes. 
   *
   *  NOTE - The uniqueness guarantee may not be worth it. Methods like `filter` and `slice` would be faster 
   *  if we didn't bother with it and built directly into a sequence.  Moreover, unbounded growth is
   *  probably unlikely in practice.  I think it could only occur if there was a very strange sequence
   *  of `flatMap` calls.
   */
  private[antixml] class AdditionalHolesBuilder extends Builder[(ZipperPath,Time), immutable.Seq[(ZipperPath,Time)]] {0
    private val hm = mutable.HashMap[ZipperPath,Time]()
   
    def += (elem: (ZipperPath,Time)) = {
      val (p,t) = elem
      val t2 = hm.getOrElse(p,0)
      hm.put(p,math.max(t,t2))
      this
    }
    def clear() {
      hm.clear
    }
    def result = 
      if (hm.size==0) util.Vector0 
      else (VectorCase.newBuilder[(ZipperPath,Time)] ++= hm).result
  }
  
}