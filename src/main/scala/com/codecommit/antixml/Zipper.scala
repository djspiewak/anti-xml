package com.codecommit.antixml

trait Zipper[A <: Node] extends Group[A] {
  val rebuild: Group[Node] => Group[Node]
  val path: List[Group[Node] => Group[Node]]
  
  def up = error("")
  
  override def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That =
    search(selector, rebuild :: path)
}
