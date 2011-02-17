package com.codecommit.antixml

object NodeSeq {
  def empty = Group.empty[Node]
  
  def apply(nodes: Node*) = Group(nodes: _*)
  
  def fromSeq(seq: Seq[Node]) = Group fromSeq seq
}
