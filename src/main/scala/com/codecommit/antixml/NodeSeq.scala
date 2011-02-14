package com.codecommit.antixml

import scala.collection.IndexedSeqLike
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}

class NodeSeq private (private val nodes: Vector[Node]) extends IndexedSeq[Node] 
    with IndexedSeqLike[Node, NodeSeq] {
  
  override val newBuilder = new VectorBuilder[Node] mapResult { new NodeSeq(_) }
  
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
  
  def \(name: String): NodeSeq = this   // TODO
  
  def \\(name: String): NodeSeq = this    // TODO
}

object NodeSeq extends ((Node*) => NodeSeq) {
  def apply(nodes: Node*) = fromSeq(nodes)
  
  def fromSeq(seq: Seq[Node]) = seq match {
    case vec: Vector[Node] => new NodeSeq(vec)
    case _ => new NodeSeq(Vector(seq: _*))
  }
}
