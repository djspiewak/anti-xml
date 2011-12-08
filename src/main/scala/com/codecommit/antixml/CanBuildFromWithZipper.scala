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

import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

/** A factory for [[com.codecommit.antixml.Zipper]] instances. 
 *
 * WARNING: This is a "low-level" trait that was primarily designed for internal
 * use of the antixml package.  It is tied to the `Zipper` implementation and
 * could change significantly in a future release.
 *
 * This trait is similar to 
 * [[http://www.scala-lang.org/api/current/scala/collection/generic/CanBuildFrom.html CanBuildFrom]], 
 * except that it allows a zipper context to be specified in addition to the usual sequence of items.
 * See the [[com.codecommit.antixml.Zipper]] trait for the definition of "zipper context".
 *
 * The [[http://www.scala-lang.org/api/current/scala/collection/mutable/Builder.html Builder]] produced 
 * by this class accepts objects of type
 * [[com.codecommit.antixml.CanBuildFromWithZipper.ElemsWithContext]]. These objects
 * contain the following information:
 *  - A sequence of items (nodes) to be added to the zipper.
 *  - A path specifying the hole associated with the above items.
 *  - An update counter, which indicates the relative order in which the zipper's items were last
 * updated.  This information is used by some [[com.codecommit.antixml.ZipperMergeStrategy]] 
 * implementations when resolving conflicts.
 * 
 * Note that an `ElemsWithContext` may contain an empty sequence, in which case its path (hole)
 * is added to zipper context without being associated to any items. Also note that it is legal for 
 * the same path (hole) to be added to the Builder multiple times.  The resulting Zipper 
 * will associate ''all'' of the corresponding items to that hole. 
 *
 * The parent of the zipper context is specified to the `apply` method of this trait.
 *
 * @tparam From The type of collection that is producing the zipper.
 * @tparam Elem The type of nodes to be contained in the result (if any).
 * @tparam To the type of collection being produced.  
 * @see [[com.codecommit.antixml.Zipper]]
 */
trait CanBuildFromWithZipper[-From, -Elem, To] {
  import CanBuildFromWithZipper.ElemsWithContext
  
    /** Creates a new builder.
     * 
     *  @param parent The parent of the zipper.  If `None`, the zipper will 
     *  still function as an IndexedSeq, but zipper unselection will fail.
     */
	def apply(parent: Option[Zipper[Node]]): Builder[ElemsWithContext[Elem], To]
	
    /** Creates a new builder.
     *  @param parent The parent of the zipper.  If `None`, the zipper will 
     *  still function as an IndexedSeq, but zipper unselection will fail.
     *  @param from The collection producing the zipper
     */
  def apply(parent: Option[Zipper[Node]], from: From): Builder[ElemsWithContext[Elem], To] = this(parent)

}

/** A marker interface for [[scala.collection.mutable.CanBuildFrom]] instances that can be lifted into
 * [[com.codecommit.antixml.CanBuildFromWithZipper]] instances that operate on [[com.codecommit.antixml.Node]] types. */
trait CanProduceZipper[-From, A <: Node, To] { this: CanBuildFrom[From, A, _ >: To] =>
  def lift: CanBuildFromWithZipper[From, A, To]
}

/** Different implicit implementations of [[com.codecommit.antixml.CanBuildFromWithZipper]]. */
object CanBuildFromWithZipper {
  
  /**
   * Decorates a sequence of zipper elements with a path and an update time.  This is the
   * basic unit of information used to construct zippers.  See  [[com.codecommit.antixml.CanBuildFromWithZipper]]
   * for more information.
   * 
   * There are two types of elements visible and hidden. Visible ones provide the
   * indexed values of a zipper, while the hidden ones provide unindexable values in the zipper.
   * Both types are represented as concrete subclasses of this one.
   *
   * @tparam Elem the type of node that will be contained in the zipper.
   * @param updateTime the update time associated with these elements.  One context is considered to have
   * been updated later than another if its updateTime is greater.
   * @see [[com.codecommit.antixml.CanBuildFromWithZipper]]
   */
  sealed abstract class ElemsWithContext[+Elem](updateTime: Int)
  /**
   * A visible zipper element.
   * @param path Identifies a location (known as a "hole") in the zipper's parent.  The order of the
   * path is from top to bottom (the first item specifies the index of a top-level node in the parent Group).
   * @param elements the actual elements to be added to the zipper. 
   */
	case class ElemsWithContextVisible[+Elem](path: Seq[Int], updateTime: Int, elements: GenTraversableOnce[Elem]) extends ElemsWithContext[Elem](updateTime)
  /**
   * A hidden zipper element.
   * @tparam Elem Dummy parameterization to satisfy the signature of methods like `flatMap`.
   * @param path A zipper path instance leading to the location of the hole.
   * @param elements The elements to be mapped to the path contained in the context. The type
   * of the elements is the most general as they are not accessible through the zipper's methods
   * and hence do not participate in any sort of transformations.
   */
  case class ElemsWithContextHidden(path: ZipperPath, updateTime: Int, elements: GenTraversableOnce[Node]) extends ElemsWithContext[Nothing](updateTime)
  
  /** Implicitly lifts [[scala.collection.mutable.CanBuildFrom]] instances into instances of [[com.codecommit.antixml.CanBuildFromWithZipper]]. The resulting builders simply ignore
    * the extra information in `ElemsWithContext` and produce their collections as usual.
    */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithZipper[From, Elem, To] = {
    new CanBuildFromWithZipper[From, Elem, To] {
      
      /** Creates a builder that just ignores anything [[com.codecommit.antixml.Zipper]] related. */
      override def apply(parent: Option[Zipper[Node]], from: From) = liftBuilder(cbf(from))
      
      /** Creates a builder that just ignores anything [[com.codecommit.antixml.Zipper]] related. */
      override def apply(parent: Option[Zipper[Node]]) = liftBuilder(cbf())
      
      private def liftBuilder(b: Builder[Elem,To]) = new Builder[ElemsWithContext[Elem], To]() {
        override def += (x: ElemsWithContext[Elem]) = {
          x match {
           case ElemsWithContextVisible(_, _, elements) => b ++= elements.seq
           case ElemsWithContextHidden(_, _, _) => // nothing to do with hidden nodes
          }
          this
        }
        override def clear() {
          b.clear()
        }
        override def result() = b.result()
      }
    }
  }
}