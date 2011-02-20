package com.codecommit.antixml

trait Zipper[A <: Node] extends Group[A] {
  val path: List[Zipper[A] => Group[Node]]
  
  def up = error("Not implemented yet")
}
