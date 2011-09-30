package com.codecommit.antixml

import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

/** A factory for [[com.codecommit.antixml.DeepZipper]] instances.
 * This trait is similar to [[scala.collection.mutable.CanBuildFrom]], however its builders accept instances
 * of `ElemsWithContext[Elem]` rather than `Elem` instances.  In addition, its `apply`
 * methods accept an optional reference to the zipper's parent.
 *
 * @tparam From The type of collection that is producing the zipper.
 * @tparam Elem The type of nodes to be contained in the result (if any).
 * @tparam To the type of collection being produced.  
 */
trait CanBuildFromWithDeepZipper[-From, -Elem, To] {
  import CanBuildFromWithDeepZipper.ElemsWithContext
  
    /** Creates a new builder.
     * 
     *  @param parent The parent of the zipper.  If `None`, the zipper will 
     *  still function as an IndexedSeq, but zipper unselection will fail.
     */
	def apply(parent: Option[DeepZipper[Node]]): Builder[ElemsWithContext[Elem], To]
	
    /** Creates a new builder.
     *  @param parent The parent of the zipper.  If `None`, the zipper will 
     *  still function as an IndexedSeq, but zipper unselection will fail.
     *  @param from The collection producing the zipper
     */
  def apply(parent: Option[DeepZipper[Node]], from: From): Builder[ElemsWithContext[Elem], To] = this(parent)

}

/** A marker interface for [[scala.collection.mutable.CanBuildFrom]] instances that can be lifted into
 * [[com.codecommit.antixml.CanBuildFromWithDeepZipper]] instances that operate on [[com.codecommit.antixml.Node]] types. */
trait CanProduceDeepZipper[-From, A <: Node, To] { this: CanBuildFrom[From, A, _ >: To] =>
  def lift: CanBuildFromWithDeepZipper[From, A, To]
}

/** Different implicit implementations of [[com.codecommit.antixml.CanBuildFromWithDeepZipper]]. */
object CanBuildFromWithDeepZipper {
  
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
  
  /** Implicitly lifts [[scala.collection.mutable.CanBuildFrom]] instances into instances of [[com.codecommit.antixml.CanBuildFromWithDeepZipper]]. The resulting builders simply ignore
    * the extra information in `ElemsWithContext` and produce their collections as usual.
    */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithDeepZipper[From, Elem, To] = {
    new CanBuildFromWithDeepZipper[From, Elem, To] {
      
      /** Creates a builder that just ignores anything [[com.codecommit.antixml.DeepZipper]] related. */
      override def apply(parent: Option[DeepZipper[Node]], from: From) = liftBuilder(cbf(from))
      
      /** Creates a builder that just ignores anything [[com.codecommit.antixml.DeepZipper]] related. */
      override def apply(parent: Option[DeepZipper[Node]]) = liftBuilder(cbf())
      
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