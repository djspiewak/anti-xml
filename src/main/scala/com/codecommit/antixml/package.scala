package com.codecommit

package object antixml {
  // type NodeSeq = Group[Node]        // TODO triggers a compiler bug
  
  implicit def stringToSelector(name: String): Selector[Elem, Group[Elem]] =
    Selector({ case e @ Elem(_, `name`, _, _) => e }, Some(name))
  
  implicit def symbolToSelector(sym: Symbol): Selector[Elem, Group[Elem]] = {
    val Symbol(name) = sym
    stringToSelector(name)
  }

  val `*`: Selector[Node, Group[Node]] = Selector({ case n: Node => n })
}
