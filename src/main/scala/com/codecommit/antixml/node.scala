package com.codecommit.antixml

sealed trait Node

case class ProcInstr(target: String, data: String) extends Node

case class Elem(namespace: String, name: String, attrs: Map[String, String], children: NodeSeq) extends Node

case class Text(text: String) extends Node {
  override def toString = text
}

case class Whitespace(text: String) extends Node {
  override def toString = text
}
