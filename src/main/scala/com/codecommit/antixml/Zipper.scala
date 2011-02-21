package com.codecommit.antixml

trait Zipper[+A <: Node] extends Group[A] { self =>
  val rebuild: Group[Node] => Group[Node]
  val path: List[Group[Node] => Group[Node]]
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVector)
  
  def up = {
    def rebuildFailure(group: Group[Node]) = error("Already at the top of the (known) tree")
    
    val (rebuildNext, pathNext) = path match {
      case rebuild :: path => (rebuild, path)
      case Nil => (rebuildFailure _, Nil)
    }
    
    new Group(rebuild(this).toVector) with Zipper[Node] {
      val rebuild = rebuildNext
      val path = pathNext
    }
  }
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVector) with Zipper[B] {
      val rebuild = self.rebuild
      val path = self.path
    }
  }
  
  override def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That =
    search(selector, rebuild :: path)
}
