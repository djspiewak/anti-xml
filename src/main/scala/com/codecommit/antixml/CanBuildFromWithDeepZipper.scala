package com.codecommit.antixml

import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import DeepZipper._

/** A factory for [[DeepZipper]] instances. 
 * @tparam PathNode The type of nodes to be used on the path instances provided to the factory.
 */
trait CanBuildFromWithDeepZipper[-From, -Elem, To, PathNode <: Node] {
	def apply(parent: From, path: Path[PathNode]): Builder[Elem, To]
	def apply(path: Path[PathNode]): Builder[Elem, To]
}

/** A marker interface for [[CanBuildFrom]] instances that can be lifted into
 * [[CanBuildFromWithDeepZipper]] instances. */
trait CanProduceDeepZipper[-From, -Elem, To, PathNode <: Node] { this: CanBuildFrom[From, Elem, _ >: To] =>
  def lift: CanBuildFromWithDeepZipper[From, Elem, To, PathNode]
}

/** Different implicit implementations of [[CanBuildFromWithDeepZipper]]. */
object CanBuildFromWithDeepZipper {
  
  /** Implicitly lifts [[CanBuildFrom]] instances into instances of [[CanBuildFromWithDeepZipper]]. */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]) = {
    new CanBuildFromWithDeepZipper[From, Elem, To, Node] {
      def apply(parent: From, path: Path[Node]) = cbf()
      def apply(path: Path[Node]) = cbf()
    }
  }
}