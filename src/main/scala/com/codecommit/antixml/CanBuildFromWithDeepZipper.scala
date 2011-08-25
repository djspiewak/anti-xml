package com.codecommit.antixml

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import DeepZipper._

/** A factory for [[DeepZipper]] instances. 
 * @tparam N The type of nodes to be contained in the [[DeepZipper]] (if any).
 */
trait CanBuildFromWithDeepZipper[-From, -Elem, To] {
    /** Creates a new builder.
     * 
     *  @param parent The parent of the zipper
     *  @param contexts The contexts from which the zipper should be composed.
     *  The contexts will be merged to the builder's input to produce a zipper.
     *  @parent emptiesSet A set of empty locations in the zipper. */
	def apply(parent: Option[From], contexts: Vector[LocationContext], emptiesSet: EmptiesSet): Builder[Elem, To]
}

/** A marker interface for [[CanBuildFrom]] instances that can be lifted into
 * [[CanBuildFromWithDeepZipper]] instances which operate on [[Node]] types. */
trait CanProduceDeepZipper[-From, A <: Node, To] { this: CanBuildFrom[From, A, _ >: To] =>
  def lift: CanBuildFromWithDeepZipper[From, A, To]
}

/** Different implicit implementations of [[CanBuildFromWithDeepZipper]]. */
object CanBuildFromWithDeepZipper {
  
  //TODO is this used anywhere?
  
  /** Implicitly lifts [[CanBuildFrom]] instances into instances of [[CanBuildFromWithDeepZipper]]. */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]) = {
    new CanBuildFromWithDeepZipper[From, Elem, To] {
      /** Creates a builder that just ignores anything [[DeepZipper]] related. */
      def apply(parent: Option[From], contexts: Vector[LocationContext], emptiesSet: EmptiesSet) = cbf()
    }
  }
}