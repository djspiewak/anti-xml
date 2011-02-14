package com.codecommit.antixml

import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}

class NodeSeq private (private val nodes: Vector[Node]) extends IndexedSeq[Node] 
    with IndexedSeqLike[Node, NodeSeq] {
  
  override val newBuilder = NodeSeq.newBuilder
  
  def length = nodes.length
  
  def apply(i: Int) = nodes(i)
  
  def +:(node: Node) = new NodeSeq(node +: nodes)
  
  def :+(node: Node) = new NodeSeq(nodes :+ node)
  
  def ++(that: NodeSeq) = new NodeSeq(this.nodes ++ that.nodes)
  
  override def drop(n: Int) = new NodeSeq(nodes drop n)
  
  override def dropRight(n: Int) = new NodeSeq(nodes dropRight n)
  
  override def head = nodes.head
  
  override def init = new NodeSeq(nodes.init)
  
  override def iterator = nodes.iterator
  
  override def last = nodes.last
  
  override def lengthCompare(len: Int) = nodes lengthCompare len
  
  override def reverseIterator = nodes.reverseIterator
  
  override def slice(from: Int, until: Int) = new NodeSeq(nodes.slice(from, until))
  
  override def splitAt(n: Int) = {
    val (left, right) = nodes splitAt n
    (new NodeSeq(left), new NodeSeq(right))
  }
  
  override def tail = new NodeSeq(nodes.tail)
  
  override def take(n: Int) = new NodeSeq(nodes take n)
  
  override def takeRight(n: Int) = new NodeSeq(nodes takeRight n)
  
  def updated(index: Int, node: Node) = new NodeSeq(nodes.updated(index, node))
  
  // TODO optimize
  def \(name: String): NodeSeq = {
    val trimmed = name.trim
    
    if (trimmed == "_")
      this
    else {
      this filter {
        case Elem(_, `trimmed`, _, _) => true
        case _ => false
      }
    }
  }
  
  // TODO optimize
  def \\(name: String): NodeSeq = {
    val trimmed = name.trim
    
    if (trimmed == "_")
      this
    else {
      this flatMap {
        case n @ Elem(_, `trimmed`, _, children) => {
          val ns = children \\ trimmed
          n +: ns
        }
        
        case Elem(_, _, _, children) => children \\ trimmed
        
        case _ => new NodeSeq(Vector())
      }
    }
  }
  
  override def toString = nodes.mkString
}

object NodeSeq extends ((Node*) => NodeSeq) {
  implicit def canBuildFrom: CanBuildFrom[NodeSeq, Node, NodeSeq] = new CanBuildFrom[NodeSeq, Node, NodeSeq] {
    def apply(coll: NodeSeq) = newBuilder
    def apply() = newBuilder
  }
  
  def newBuilder = new VectorBuilder[Node] mapResult { new NodeSeq(_) }

  def empty = new NodeSeq(Vector.empty)
  
  def apply(nodes: Node*) = fromSeq(nodes)
  
  def fromSeq(seq: Seq[Node]) = seq match {
    case vec: Vector[Node] => new NodeSeq(vec)
    case _ => new NodeSeq(Vector(seq: _*))
  }
}
