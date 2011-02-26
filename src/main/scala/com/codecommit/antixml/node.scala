package com.codecommit.antixml

sealed trait Node

case class ProcInstr(target: String, data: String) extends Node

object Elem {
  def apply(ns: Option[String],
            name: String,
            attrs: Map[String, String],
            children: Group[Node]): Elem =
    new Elem(ns, name, attrs, children.nodes)
  def unapply(elem: Elem): Option[(Option[String], String, Map[String, String], Group[Node])] = Some(elem.ns, elem.name, elem.attrs, elem.children)
}
class Elem private(val ns: Option[String], val name: String, val attrs: Map[String, String], private val _children: Vector[Node]) extends Node {
  /**
   * Returns the Group of Node children of this Elem.
   * @return the Group of Node children of this Elem.
   */
  def children: Group[Node] = new Group(_children)

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

  override def equals(rhs: Any): Boolean = rhs match {
    case that: Elem => this.ns == that.ns && this.name == that.name &&
      this.attrs == that.attrs && this._children == that._children
    case _ => false
  }
    
}

case class Text(text: String) extends Node {
  override def toString = text
}

case class Whitespace(text: String) extends Node {
  override def toString = text
}
