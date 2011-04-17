package com.codecommit
package antixml

import util._

import scala.collection.generic.CanBuildFrom

trait Zipper[+A <: Node] extends Group[A] { self =>
  // TODO dependently-typed HList, maybe?

  protected def map: Vector[ZContext]
  protected def parent: Zipper[Node]

  // TODO this *may* be a poor choice of words...
  def stripZipper = new Group(toVectorCase)
  
  def unselect: Zipper[Node] = {
    val nodes2 = (map zip parent.toVectorCase).foldLeft(VectorCase[Node]()) {
      case (acc, ((from, to, _, _), _: Elem)) if from == to => acc
      case (acc, ((from, to, rebuild, childMap), _: Elem)) => acc :+ rebuild(self.slice(from, to), childMap)
      case (acc, (_, e)) => acc :+ e
    }

    new Group(nodes2) with Zipper[Node] {
      val map = self.parent.map
      def parent = self.parent.parent
    }
  }

  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val builder = cbf(parent.asInstanceOf[Group[A]], map)      // oddly, the type-checker isn't handling this
      builder ++= (toVectorCase map f)
      builder.result
    }

    case _ => super.map(f)(cbf)
  }

  override def flatMap[B, That](f: A => Traversable[B])(implicit cbf: CanBuildFrom[Group[A], B, That]): That = cbf match {
    case cbf: CanBuildFromWithZipper[Group[A], B, That] => {
      val result = toVectorCase.toVector map f

      val intermedMap = for ((from, to, rebuild, childMap) <- map) yield {
        // get the mapping from *our* indexes to source indexes
        val inverseMap = {
          val maps = for ((source, targets) <- childMap)
            yield (Map[Int, Int]() /: targets) { (acc, t) => acc + (t -> source) }

          (Map[Int, Int]() /: maps) { _ ++ _ }
        }

        val (_, aggregate, childMap2) = result.slice(from, to).zipWithIndex.foldLeft((0, Vector[B](), Map[Int, Set[Int]]())) {
          case ((start, acc, childMap2), (chunk, i)) => {
            val size = chunk.size
            val source = inverseMap(i)

            val contrib = Set(start until (start + size): _*)
            val set2 = childMap2.getOrElse(source, contrib) ++ contrib

            (start + size, acc ++ chunk, childMap2.updated(source, set2))
          }
        }

        val length = aggregate.length
        val delta = length - (to - from)
        (from, to + delta, rebuild, childMap2, aggregate, delta)
      }

      val (_, map2, chunks) = intermedMap.foldLeft((0, Vector[ZContext](), Vector[Vector[B]]())) {
        case ((offset, map2, acc), (from, to, rebuild, childMap, aggregate, delta)) => {
          val from2 = from + offset
          val to2 = to + offset
          val offset2 = offset + delta
          (offset2, map2 :+ (from2, to2, rebuild, childMap), acc :+ aggregate)
        }
      }

      val builder = cbf(parent.asInstanceOf[Group[A]], map2)
      chunks foreach (builder ++=)
      builder.result
    }

    case _ => super.flatMap(f)(cbf)
  }
  
  override def filter(f: A => Boolean): Zipper[A] = collect {
    case e if f(e) => e
  }
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[Group[A], B, That]): That = flatMap {
    case e if pf isDefinedAt e => Some(pf(e))
    case _ => None
  }
  
  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVectorCase) with Zipper[B] {
      val map = self.map
      val parent = self.parent
    }
  }
  
  override protected def makeAsZipper = self
}
