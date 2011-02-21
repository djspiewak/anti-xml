package com.codecommit.antixml

import scala.collection.Traversable
import scala.collection.immutable.Seq

trait Selector[+A, +Coll <: Traversable[A]] extends PartialFunction[Node, A] {

  val element: Option[String] = None
}

object Selector {
  def apply[A, Coll <: Traversable[A]](pf: PartialFunction[Node, A], elem: Option[String] = None) =
    new Selector[A, Coll] {
      override val element = elem

      def apply(node: Node) = pf(node)

      def isDefinedAt(node: Node) = pf isDefinedAt node
    }
}
