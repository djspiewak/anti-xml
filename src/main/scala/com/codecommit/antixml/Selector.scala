package com.codecommit.antixml

import scala.collection.Traversable

trait Selector[+A, +Coll <: Traversable[A]] extends PartialFunction[Node, A]

object Selector {
  def apply[A, Coll <: Traversable[A]](pf: PartialFunction[Node, A]) = new Selector[A, Coll] {
    def apply(node: Node) = pf(node)
    
    def isDefinedAt(node: Node) = pf isDefinedAt node
  }
}
