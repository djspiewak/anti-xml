package com.codecommit

package object antixml {
  // type NodeSeq = Group[Node]        // TODO triggers a compiler bug
  
  implicit def stringToSelector(name: String): Selector[Elem, Zipper[Elem]] =
    Selector({ case e @ Elem(_, `name`, _, _) => e }, Some(name))
  
  implicit def symbolToSelector(sym: Symbol): Selector[Elem, Zipper[Elem]] = {
    val Symbol(name) = sym
    stringToSelector(name)
  }

  val `*`: Selector[Node, Zipper[Node]] = Selector({ case n: Node => n })
}
