package com.codecommit.antixml

import scala.annotation.unchecked.uncheckedVariance

import scala.collection.{IndexedSeqLike, TraversableLike}
import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}
import scala.collection.mutable.Builder

class Group[+A <: Node] private[antixml] (private[antixml] val nodes: Vector[A]) extends IndexedSeq[A] 
    with IndexedSeqLike[A, Group[A]] {
  
  override protected[this] def newBuilder = Group.newBuilder[A]
  
  def length = nodes.length
  
  def apply(i: Int) = nodes(i)
  
  def +:[B >: A <: Node](node: B) = new Group(node +: nodes)
  
  def :+[B >: A <: Node](node: B) = new Group(nodes :+ node)
  
  def ++[B >: A <: Node](that: Group[B]) = new Group(this.nodes ++ that.nodes)
  
  override def drop(n: Int) = new Group(nodes drop n)
  
  override def dropRight(n: Int) = new Group(nodes dropRight n)
  
  override def head = nodes.head
  
  override def init = new Group(nodes.init)
  
  override def iterator = nodes.iterator
  
  override def last = nodes.last
  
  override def lengthCompare(len: Int) = nodes lengthCompare len
  
  override def reverseIterator = nodes.reverseIterator
  
  override def slice(from: Int, until: Int) = new Group(nodes.slice(from, until))
  
  override def splitAt(n: Int) = {
    val (left, right) = nodes splitAt n
    (new Group(left), new Group(right))
  }
  
  override def tail = new Group(nodes.tail)
  
  override def take(n: Int) = new Group(nodes take n)
  
  override def takeRight(n: Int) = new Group(nodes takeRight n)
  
  def updated[B >: A <: Node](index: Int, node: B) = new Group(nodes.updated(index, node))
  
  def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: CanBuildFromWithZipper[Zipper[A], B, That]): That = {
    if (matches(selector)) {
      val results = nodes map {
        case e @ Elem(_, _, _, children) => {
          val selectedWithIndexes = children.zipWithIndex flatMap {
            case (n, i) if selector isDefinedAt n => Some(selector(n) -> i)
            case _ => None
          }
          
          val indexes = selectedWithIndexes map { case (_, i) => i }
          val selected = selectedWithIndexes map { case (e, _) => e }
          
          def rebuild(children2: Group[Node]) = {
            val revisedChildren = (indexes zip children2).foldLeft(children) {
              case (vec, (i, e)) => vec.updated(i, e)
            }
            e.copy(children=revisedChildren)
          }
          
          Some((selected, rebuild _))
        }
        
        case _ => None
      }
      
      val (_, map) = results.foldLeft((0, Vector[(Int, Int, Group[Node] => Node)]())) {
        case ((i, acc), Some((res, f))) if !res.isEmpty =>
          (i + res.length, acc :+ (i, i + res.length, f))
        
        case ((i, acc), _) => (i, acc)
      }
      
      val cat = results flatMap {
        case Some((selected, _)) => selected
        case None => Vector()
      }
      
      val builder = cbf(makeAsZipper, map)
      builder ++= cat
      builder.result
    } else {
      cbf(Vector()).result
    }
  }
  
  protected def makeAsZipper: Zipper[A] = {
    new Group(nodes) with Zipper[A] {
      val map = Vector()
      def parent = error("Attempted to move up at root of the tree")
    }
  }
  
  def \\[B, That <: IndexedSeq[B]](selector: Selector[B, That])(implicit cbf: CanBuildFromWithZipper[Traversable[_], B, That]): That = {
    val recursive = this flatMap {
      case Elem(_, _, _, children) if matches(selector) => children \\ selector
      case _ => cbf().result
    }
    
    (this \ selector) ++ recursive
  }
  
  def toVector = nodes
  
  override def toString = nodes.mkString

  private lazy val bloomFilter: BloomFilter = {
    val names =
      nodes collect {
        case Elem(_, name, _, _) => name
      }
    val subFilters =
      nodes collect {
        case Elem(_, _, _, children) => children.bloomFilter
      }
    (BloomFilter(names)(1024) /: subFilters) { _ ++ _ }
  }

  private def matches(selector: Selector[_, _]) =
    selector.elementName map bloomFilter.contains getOrElse true
}

object Group {
  implicit def canBuildFromWithZipper[A <: Node]: CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] = {
    new CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] {
      def apply(from: Traversable[_], baseMap: Vector[(Int, Int, Group[Node] => Node)]): Builder[A, Zipper[A]] = {
        new VectorBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            val map = baseMap
            
            def parent = from match {
              case group: Group[Node] => group.makeAsZipper
              case _ => error("No zipper context available")
            }
          }
        }
      }
      
      def apply(baseMap: Vector[(Int, Int, Group[Node] => Node)]): Builder[A, Zipper[A]] = {
        new VectorBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            val map = baseMap
            def parent = error("No zipper context available")
          }
        }
      }
    }
  }
  
  def newBuilder[A <: Node] = new VectorBuilder[A] mapResult { new Group(_) }
  
  def empty[A <: Node] = new Group[A](Vector.empty)
  
  def apply[A <: Node](nodes: A*) = fromSeq(nodes)
  
  def fromSeq[A <: Node](seq: Seq[A]) = seq match {
    case vec: Vector[A] => new Group(vec)
    case _ => new Group(Vector(seq: _*))
  }
}
