package com.codecommit.antixml

trait Zipper[+A <: Node] extends Group[A] { self =>
  // TODO dependently-typed HList, maybe?
  
  protected def map: Vector[(Int, Int, Group[Node] => Node)]
  protected def parent: Zipper[Node]
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVector)
  
  def unselect: Zipper[Node] = {
    val (_, nodes2) = map.foldLeft((0, parent.toVector)) {
      case ((i, vec), (from, to, rebuild)) => {
        val vec2 = vec(i) match {
          case _: Elem => vec.updated(i, rebuild(self.slice(from, to)))
          case _ => vec
        }
        (i + 1, vec2)
      }
    }
    
    new Group(nodes2) with Zipper[Node] {
      val map = self.parent.map
      def parent = self.parent.parent
    }
  }
  
  def mapWithContext[B <: Node](f: A => B): Zipper[B] = {
    val nodes2 = toVector map f
    new Group(nodes2) with Zipper[B] {
      val map = self.map
      def parent = self.parent
    }
  }
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVector) with Zipper[B] {
      val map = self.map
      val parent = self.parent
    }
  }
  
  override protected def makeAsZipper = self
}
