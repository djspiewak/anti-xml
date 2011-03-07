package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom

trait Zipper[+A <: Node] extends Group[A] { self =>
  // TODO dependently-typed HList, maybe?
  
  val map: Vector[(Int, Int, (Group[Node], Vector[Int]) => Node)]
  val childMap: Vector[Int]
  def parent: Zipper[Node]
  
  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVector)
  
  def unselect: Zipper[Node] = {
    val nodes2 = (map zip parent.toVector).foldLeft(Vector[Node]()) {
      case (acc, ((from, to, _), _: Elem)) if from == to => acc
      case (acc, ((from, to, rebuild), _: Elem)) => acc :+ rebuild(self.slice(from, to), childMap.slice(from, to))
      case (acc, (_, e)) => acc :+ e
    }
    
    new Group(nodes2) with Zipper[Node] {
      val map = self.parent.map
      val childMap = self.parent.childMap
      def parent = self.parent.parent
    }
  }
  
  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val builder = cbf(parent.asInstanceOf[Group[A]], map, childMap)      // oddly, the type-checker isn't handling this
      builder ++= (toVector map f)
      builder.result
    }
    
    case _ => super.map(f)(cbf)
  }
  
  override def flatMap[B, That](f: A => Traversable[B])(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val result = toVector map f
      
      val map2 = for (((from, to, rebuild), i) <- map.zipWithIndex) yield {
        val chunk = result.slice(from, to).flatten
        val length = chunk.length       // nasty
        val delta = length - (to - from)
        (from, to + delta, rebuild, chunk, delta, i)
      }
      
      val childMap2 = (childMap zip result).foldLeft(Vector[Int]()) {
        case (acc, (i, chunk)) =>
          acc ++ Stream.fill(chunk.size)(i)
      }
      
      val sorted = map2 sortBy { case (from, _, _, _, _, _) => from }
      val (_, map3, chunks) = sorted.foldLeft((0, map, Vector[Traversable[B]]())) {
        case ((offset, map, chunks), (from, to, rebuild, chunk, delta, i)) =>
          (offset + delta, map.updated(i, (from + offset, to + offset, rebuild)), chunks :+ chunk)
      }
      
      val builder = cbf(parent.asInstanceOf[Group[A]], map3, childMap2)
      chunks foreach (builder ++=)
      builder.result
    }
    
    case _ => super.flatMap(f)(cbf)
  }
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVector) with Zipper[B] {
      val map = self.map
      val childMap = self.childMap
      val parent = self.parent
    }
  }
  
  override protected def makeAsZipper = self
}
