package com.codecommit.antixml

trait Zipper[+A <: Node] extends Group[A] { self =>
  // TODO dependently-typed HList, maybe?
  
  val map: Vector[(Int, Int, Group[Node] => Node)]
  def parent: Zipper[Node]
  
  val rebuild: Group[Node] => Group[Node] = null
  val path: List[Group[Node] => Group[Node]] = null
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVector)
  
  def up: Zipper[Node] = error("unimplemented")
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVector) with Zipper[B] {
      val map = self.map
      val parent = self.parent
    }
  }
  
  override protected def makeAsZipper = self
}
