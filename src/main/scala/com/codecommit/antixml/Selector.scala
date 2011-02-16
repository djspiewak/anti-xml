package com.codecommit.antixml

trait Selector[+A] extends PartialFunction[Node, A]

object Selector {
  def apply[A](pf: PartialFunction[Node, A]) = new Selector[A] {
    def apply(node: Node) = pf(node)
    
    def isDefinedAt(node: Node) = pf isDefinedAt node
  }
}
