package com.codecommit.antixml

sealed trait Node

case class ProcInstr(target: String, data: String) extends Node

case class Elem(ns: Option[String], name: String, attrs: Map[String, String], children: Group[Node]) extends Node {
  override def toString = {
    val prefix = ns map { _ + ':' } getOrElse ""
    val qName = prefix + name
    
    val attrStr = if (attrs.isEmpty) 
      ""
    else
      " " + (attrs map { case (key, value) => key + "=\"" + value + '"' } mkString " ")
    
    val partial = "<" + qName + attrStr
    if (children.isEmpty)
      partial + "/>"
    else
      partial + '>' + children.toString + "</" + qName + '>'
  }
}

case class Text(text: String) extends Node {
  override def toString = text
}

case class EntityRef(entity: String) extends Node {
  override def toString = "&" + entity + ";"
}
