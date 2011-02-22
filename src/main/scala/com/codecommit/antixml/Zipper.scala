package com.codecommit.antixml

trait Zipper[+A <: Node] extends Group[A] { self =>
  type Parent <: Zipper[Node]     // TODO maybe a lower bound here?
  
  val map: Vector[(Int, Int, Group[Node] => Node)]
  def parent: Parent
  
  val rebuild: Group[Node] => Group[Node] = null
  val path: List[Group[Node] => Group[Node]] = null
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVector)
  
  def up: Parent = error("unimplemented")
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVector) with Zipper[B] {
      type Parent = self.Parent
      val map = self.map
      val parent = self.parent
    }
  }
  
  override protected def makeAsZipper = self
}
