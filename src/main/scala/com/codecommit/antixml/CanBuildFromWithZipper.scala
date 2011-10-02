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
 * This trait is similar to [[scala.collection.mutable.CanBuildFrom]], however its builders accept instances
 * of `ElemsWithContext[Elem]` rather than `Elem` instances.  In addition, its `apply`
 * methods accept an optional reference to the zipper's parent.
 *
 * @tparam From The type of collection that is producing the zipper.
 * @tparam Elem The type of nodes to be contained in the result (if any).
 * @tparam To the type of collection being produced.  
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
   * Decorates a sequence of zipper elements with a zipper context and an update time.  This is the
   * basic unit of information used to construct zippers.  
   *
   * @tparam Elem the type of node that will be contained in the zipper.
   * @param path Identifies a location in the zipper's parent.  The path order is from top to bottom
   * (the first item specifies the index of a top-level node within the parent).  When building a zipper,
   * it is legal for multiple ElemsWithContexts to specify the same path;  In such cases, all of the
   * corresponding Elems will be added to the zipper and they will all be associated with that path.
   * @param updateTime the update time associated with these elements.  One context is considered to have
   * been updated later than another if its updateTime is greater.
   * @param elements the actual elements to be added to the zipper.  Note that this sequence may be
   * empty.  This would happen, for example, if `flatMap` operation removed all items for a given path. 
   */
  case class ElemsWithContext[+Elem](path: Seq[Int], updateTime: Int, elements: GenTraversableOnce[Elem])
  
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
          b ++= x.elements.seq
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