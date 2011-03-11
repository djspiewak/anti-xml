package com.codecommit.antixml

import scala.annotation.unchecked.uncheckedVariance

import scala.collection.{IndexedSeqLike, TraversableLike}
import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}
import scala.collection.mutable.{ArrayBuffer, Builder}

class Group[+A <: Node] private[antixml] (private[antixml] val nodes: VectorCase[A]) extends IndexedSeq[A] 
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
      // note: this is mutable and horrible for performance reasons (>2x boost doing it this way) 
      
      val catBuilder = new VectorBuilder[B]
      val chunkBuilder = new VectorBuilder[Int]
      val rebuildBuilder = new VectorBuilder[Group[Node] => Node]
      
      for (node <- nodes) {
        node match {
          case e @ Elem(_, _, _, children) if children.matches(selector) => {
            val indexBuffer = new ArrayBuffer[Int](children.length)
            var currentChunk = 0
            
            var i = 0
            for (child <- children) {
              if (selector isDefinedAt child) {
                catBuilder += selector(child)
                currentChunk += 1
                indexBuffer += i
              }
              i += 1
            }
            
            chunkBuilder += currentChunk
            
            lazy val indexes = Vector(indexBuffer: _*)
            def rebuild(children2: Group[Node]) = {
              val revisedChildren = (indexes zip children2).foldLeft(children) {
                case (vec, (i, e)) => vec.updated(i, e)
              }
              e.copy(children=revisedChildren)
            }
            
            rebuildBuilder += (rebuild _)
          }
          
          case _ =>
        }
      }
      
      val cat = catBuilder.result
      
      lazy val (_, map) = {
        (chunkBuilder.result zip rebuildBuilder.result).foldLeft((0, Vector[(Int, Int, Group[Node] => Node)]())) {
          case ((i, acc), (length, f)) if length != 0 =>
            (i + length, acc :+ (i, i + length, f))
          
          case ((i, acc), _) => (i, acc)
        }
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
    if (matches(selector)) {
      val recursive = this flatMap {
        case Elem(_, _, _, children) => children \\ selector
        case _ => cbf().result
      }
      
      (this \ selector) ++ recursive
    } else {
      cbf().result
    }
  }
  
  def toVector = nodes.toVector
  
  private[antixml] def toVectorCase: VectorCase[A] = nodes
  
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
      def apply(from: Traversable[_], baseMap: =>Vector[(Int, Int, Group[Node] => Node)]): Builder[A, Zipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            lazy val map = baseMap
            
            lazy val parent = from match {
              case group: Group[Node] => group.makeAsZipper
              case _ => error("No zipper context available")
            }
          }
        }
      }
      
      def apply(baseMap: =>Vector[(Int, Int, Group[Node] => Node)]): Builder[A, Zipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            lazy val map = baseMap
            def parent = error("No zipper context available")
          }
        }
      }
    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A] mapResult { new Group(_) }
  
  def empty[A <: Node] = new Group[A](VectorCase.empty)
  
  def apply[A <: Node](nodes: A*) = fromSeq(nodes)
  
  def fromSeq[A <: Node](seq: Seq[A]) = new Group(VectorCase(seq: _*))
}
