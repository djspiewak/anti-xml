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
import scala.collection.immutable.{SortedMap, IndexedSeq}
import scala.collection.mutable.Builder

import Zipper._
import CanBuildFromWithZipper.ElemsWithContext

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
trait Zipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, Zipper[A]] { self =>

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
  
  override protected[this] def newBuilder = Zipper.newBuilder[A]
  
  override def updated[B >: A <: Node](index: Int, node: B): Zipper[B] = context match {
    case Some(Context(parent, lastUpdate, metas, additionalHoles)) => {
      val updatedTime = lastUpdate + 1
      val (updatedPath,_) = metas(index)
      val updatedMetas = metas.updated(index, (updatedPath, updatedTime))
      val ctx = Context(parent, updatedTime, updatedMetas, additionalHoles)
      
      new Group(nodes.updated(index, node)) with Zipper[B] {
        val context = Some(ctx)
      }
    }
    case None => brokenZipper(nodes.updated(index,node))
  }

  override def slice(from: Int, until: Int): Zipper[A] = context match {
    case Some(Context(parent, lastUpdate, metas, additionalHoles)) => {
      val lo = math.min(math.max(from, 0), nodes.length)
      val hi = math.min(math.max(until, lo), nodes.length)
      val cnt = hi - lo
      
      //Put the ZipperPaths from the elided `metas` entries into additionalHoles
      val ahs = new AdditionalHolesBuilder()
      ahs ++= additionalHoles
      for(i <- 0 until lo)
        ahs += ((metas(i)._1, lastUpdate + 1 + i))
      for(i <- hi until nodes.length)
        ahs += ((metas(i)._1, lastUpdate + 1 + i - cnt))
      
      val ctx = Context(parent, lastUpdate + length - cnt, metas.slice(from, until), ahs.result())
      
      new Group(nodes.slice(from,until)) with Zipper[A] {
        val context = Some(ctx)
      }
    }
    case None => brokenZipper(nodes.slice(from,until))
  }
   
  override def drop(n: Int) = slice(n, size)
  
  override def take(n: Int) = slice(0, n)
  
  override def splitAt(n: Int) = (take(n), drop(n))
  
  override def filter(f: A => Boolean): Zipper[A] = collect {
    case e if f(e) => e
  }
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
    
  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = {
    val liftedF = (a: A) => Seq(f(a))
    flatMap(liftedF)(cbf)
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = cbf match {
    case cpz: CanProduceZipper[Zipper[A], B, That] if context.isDefined => {
      val Context(parent, lastUpdate, metas, additionalHoles) = context.get
      val b = cpz.lift(Some(parent), this)
      for(i <- 0 until nodes.length) {
        val (path,_) = metas(i)
        b += ElemsWithContext(path, lastUpdate+i+1, f(nodes(i)))
      }
      for ((path,time) <- additionalHoles) {
        b += ElemsWithContext[B](path,time,util.Vector0)
      }
      b.result()
    }
    case _ => {
      val b = cbf(this)
      for(n <- nodes)
        b ++= f(n).seq
      b.result()
    }
  }
  
  override def withFilter(f: A => Boolean) = new WithFilter(List(f))
  
  class WithFilter(filters: List[A => Boolean]) extends FilterMonadic[A, Zipper[A]] {
    private[this] def sat(a: A) = filters forall { _(a) }
    
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self flatMap {a => if (sat(a)) Seq(f(a)) else Nil } 
    
    def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self flatMap {a => if (sat(a)) f(a) else Nil}
    
    def foreach[B](f: A => B) = self foreach {a => if (sat(a)) f(a)}
    
    def withFilter(p: A => Boolean) = new WithFilter(p :: filters)
  }

  override def toZipper = this
  
  /**
   * Returns a `Group` containing the same nodes as this Zipper, but without any Zipper context, and in particular,
   * without any implict references to the zipper's parent.
   */
  def stripZipper = new Group(toVectorCase)
  
  /**
   * Optionally replaces each node with 0 to many nodes. Used by `unselect`.  See same-named function in `Group` for more details.
   */
  private [antixml] override def conditionalFlatMapWithIndex[B >: A <: Node] (f: (A, Int) => Option[scala.collection.Seq[B]]): Zipper[B] = {
    /* See the Group implementation for information about how this function is optimized. */ 
    context match {
      case None => brokenZipper(new Group(nodes).conditionalFlatMapWithIndex(f).nodes)
      case Some(Context(parent, lastUpdate, metas, additionalHoles)) => {
        //Optimistic function that uses `update`
        @tailrec
        def update(z: Zipper[B], index: Int): Zipper[B] = {
          if (index<z.length)
            f(nodes(index), index) match {
              case None => update(z,index + 1)
              case Some(r) if r.lengthCompare(1)==0 => update(z.updated(index, r.head), index + 1)
              case Some(r) => build(z, r, index)
            }
          else
            z
        }
        
        //Fallback function that uses a builder
        def build(z: Zipper[B], currentReplacements: Seq[B], index: Int): Zipper[B] = {
          val b = newZipperContextBuilder[B](Some(parent))
          val zc = z.context.get 
          
          for(i <- 0 until index) {
            val (p,t) = zc.metas(i)
            b += ElemsWithContext(p,t,util.Vector1(z(i)))
          }
          b += ElemsWithContext(metas(index)._1, zc.lastUpdate+1,currentReplacements)
          for(i <- (index + 1) until nodes.length) {
            val n = nodes(i)
            val m = metas(i)
            f(n,i) match {
              case None => b += ElemsWithContext(m._1, m._2, util.Vector1(n))
              case Some(r) => b += ElemsWithContext(m._1, zc.lastUpdate + 1 + i - index, r)
            }
          }
          for((p,t) <- additionalHoles)
            b += ElemsWithContext(p,t,util.Vector0)
          
          b.result
        }
        
        update(this, 0)
      }
    }    
  }
  
  /** Applies the node updates to the parent and returns the result. */
  def unselect(implicit zms: ZipperMergeStrategy): Zipper[Node] = { 
    val ctx = context.getOrElse(sys.error("Zipper does not have a valid context"))
    new Unselector(ctx, zms).unselect 
  }
  
  /** Utility class to perform unselect.  */
  private[this] class Unselector(context: Context, mergeStrategy: ZipperMergeStrategy) {
    
    /** Each hole is associated with a list of node/time pairs as well as a master update time */
    type HoleInfo = ZipperHoleMap[(VectorCase[(A,Time)],Time)]
    
    private val topLevelHoleInfo: HoleInfo = {
      val Context(_,_,metas,additionalHoles) = context
      val init:(VectorCase[(A,Time)],Time) = (util.Vector0,0)
      val hm0: HoleInfo = ZipperHoleMap.empty
      val hm1 = (hm0 /: (0 until self.length)) { (hm, i) =>
        val item = self(i)
        val (path,time) = metas(i)
        val (oldItems, oldTime) = hm.getDeep(path).getOrElse(init)
        val newItems = oldItems :+ (item, time)
        val newTime = math.max(oldTime, time)
        hm.updatedDeep(path, (newItems, newTime))
      }
      (hm1 /: additionalHoles) { case (hm,(path,time)) =>
        val (oldItems, oldTime) = hm.getDeep(path).getOrElse(init)
        val newTime = math.max(oldTime, time)
        hm.updatedDeep(path, (oldItems, newTime))
      }
    }
    
    /** Applies the node updates to the parent and returns the result. */
    def unselect: Zipper[Node] = 
      pullBackGroup(context.parent, topLevelHoleInfo)._1.asInstanceOf[Zipper[Node]]
    
    /**
     * Returns the pullback of the nodes in the specified group.
     * @param nodes the group containing the nodes to pull back.
     * @param holeInfo the HoleInfo corresponding to the group.
     * @return the pullBacks of the groups children, concatenated together, along with the latest update
     * time.
     */
    private[this] def pullBackGroup(nodes: Group[Node], holeInfo: HoleInfo): (Group[Node], Time) = {
      var maxTime: Int = 0  //mutable for performance and to avoid further complicating`conditionalFlatMapWithIndex`.
      val updatedGroup = nodes.conditionalFlatMapWithIndex[Node] { (node,index) => node match {
        case elem:Elem if (holeInfo.hasChildrenAt(index)) => {
          val (newNodes, time) = pullUp(elem, index, holeInfo)
          maxTime = math.max(maxTime,time)
          Some(newNodes)
        }
        case _ if holeInfo.contains(index) => {
          val (newNodes, time) = holeInfo(index)
          maxTime = math.max(maxTime, time)
          Some(newNodes.map {_._1})
        }
        case _ => None
      }}
      (updatedGroup,maxTime)
    }
    
    /**
     * Returns the pullback of an element that is known to be above a hole (and thus has
     * child updates that need to be pulled up).
     *
     * @param elem the element
     * @param indexInParent the index of the element in its parent
     * @param holeInfo the HoleInfo corresponding to the parent group
     * @return the pulled back nodes and their combined update time
     *
     * @note assumes `holeInfo.hasChildrenAt(indexInParent) == true`
     */
    private[this] def pullUp(elem: Elem, indexInParent: Int, holeInfo: HoleInfo): (VectorCase[Node], Time) = {
      //Recursively pull back children 
      val (childGrp, childTime) = pullBackGroup(elem.children, holeInfo.children(indexInParent))
      val indirectUpdate = elem.copy(children = childGrp)
      if (holeInfo.contains(indexInParent)) {
        //This is a conflicted hole, so merge.
        mergeConflicts(elem, holeInfo(indexInParent), (indirectUpdate, childTime))
      } else {
        //No conflicts, just let the child updates bubble up
        (VectorCase(indirectUpdate), childTime)
      }
    }
 
    /**
     * Merges updates at a conflicted node in the tree.  See the unselection algorithm, above, for more information. 
     * @param node the conflicted node
     * @param directUpdates the direct updates to `node`.
     * @param indirectUpdate the indirectUpdate to `node`.
     * @return the sequence of nodes to replace `node`, along with an overall update time for `node`.
     */
    private def mergeConflicts(node: Elem, directUpdates: (IndexedSeq[(Node,Time)], Time) , indirectUpdate: (Node, Time)): (VectorCase[Node], Time) = {
      val mergeContext = ZipperMergeContext(original=node, lastDirectUpdate = directUpdates._2, directUpdate = directUpdates._1,
          indirectUpdate = indirectUpdate)
       
      val result = mergeStrategy(mergeContext)
      (VectorCase.fromSeq(result), math.max(directUpdates._2, indirectUpdate._2))
    }
  }
}

object Zipper {
    
  import CanBuildFromWithZipper.ElemsWithContext
  
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
   */
  private[antixml] case class Context(parent: Zipper[Node], lastUpdate: Time, 
          metas: VectorCase[(ZipperPath, Time)], additionalHoles: immutable.Seq[(ZipperPath, Time)])
  
  /** The units in which time is measured in the zipper. Assumed non negative. */
  private type Time = Int
  
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
  private def newZipperContextBuilder[A <: Node](parent: Option[Zipper[Node]]) = parent match {
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
    private var size = 0
    private var maxTime = 0
    
    override def += (ewc: ElemsWithContext[A]) = {      
      val ElemsWithContext(pseq, time, ns) = ewc
      val path: ZipperPath = ZipperPath.fromSeq(pseq)
      val pathTime = (path, time)
      
      var nsz = 0
      for(n <- ns) {
        itemsBuilder += n
        metasBuilder += pathTime
        nsz += 1
      }
      if (nsz==0) {
        additionalHolesBuilder += pathTime
      }
      
      size += nsz
      maxTime = math.max(maxTime, time)
      this            
    }
    override def clear() {
      itemsBuilder.clear()
      metasBuilder.clear()
      additionalHolesBuilder.clear()
      size = 0
      maxTime = 0
    }
    override def result(): Zipper[A] = {
      val ctx = Context(parent, maxTime, metasBuilder.result(), additionalHolesBuilder.result())
      
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
  private class AdditionalHolesBuilder extends Builder[(ZipperPath,Time), immutable.Seq[(ZipperPath,Time)]] {0
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