package com.codecommit.antixml

import scala.annotation.unchecked.uncheckedVariance

import scala.collection.{IndexedSeqLike, TraversableLike}
import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}

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
  
  // TODO optimize
  def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: CanBuildFrom[Group[A], B, That]): That = {
    this flatMap {
      case Elem(_, _, _, children) => children collect selector
      case _ => new Group(Vector())
    }
  }
  
  // TODO optimize
  def \\[B, That <: IndexedSeq[B]](selector: Selector[B, That])(implicit cbf: CanBuildFrom[Traversable[_], B, That]): That = {
    val recursive = this flatMap {
      case Elem(_, _, _, children) => children \\ selector
      case _ => cbf().result
    }
    
    (this \ selector) ++ recursive
  }
  
  override def toString = nodes.mkString
}

object Group {
  implicit def canBuildFrom[A <: Node]: CanBuildFrom[Traversable[_], A, Group[A]] = new CanBuildFrom[Traversable[_], A, Group[A]] {
    def apply(coll: Traversable[_]) = newBuilder[A]
    def apply() = newBuilder[A]
  }
  
  def newBuilder[A <: Node] = new VectorBuilder[A] mapResult { new Group(_) }
  
  def empty[A <: Node] = new Group[A](Vector.empty)
  
  def apply[A <: Node](nodes: A*) = fromSeq(nodes)
  
  def fromSeq[A <: Node](seq: Seq[A]) = seq match {
    case vec: Vector[A] => new Group(vec)
    case _ => new Group(Vector(seq: _*))
  }
}
