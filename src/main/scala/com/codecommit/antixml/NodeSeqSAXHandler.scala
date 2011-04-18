package com.codecommit
package antixml

import util._

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

class NodeSeqSAXHandler extends DefaultHandler {
  private var elems = List[Group[Node] => Elem]()
  private val text = new StringBuilder
  
  private var builders = VectorCase.newBuilder[Node] :: Nil
  
  override def characters(ch: Array[Char], start: Int, length: Int) {
    text.appendAll(ch, start, length)
  }
  
  override def startElement(uri: String, localName: String, qName: String, attrs: Attributes) {
    clearText()
    
    builders ::= VectorCase.newBuilder
    elems ::= { children =>
      val ns = if (uri == "") None else Some(uri)
      val map = (0 until attrs.getLength).foldLeft(Map[String, String]()) { (map, i) =>
        map + (attrs.getQName(i) -> attrs.getValue(i))    // TODO namespacing
      }
      
      Elem(ns, localName, map, children)
    }
  }
  
  override def endElement(uri: String, localName: String, qName: String) {
    clearText()
    
    val (build :: elems2) = elems
    elems = elems2
    
    val result = build(pop())
    builders.head += result
  }
  
  override def skippedEntity(entity: String) {
    clearText()
    builders.head += EntityRef(entity)
  }
  
  def result() = pop().asInstanceOf[Group[Elem]]       // nasty, but it shouldn't be a problem
  
  private def pop() = {
    val (back :: builders2) = builders
    builders = builders2
    Group fromSeq back.result
  }
  
  private def clearText() {
    if (!text.isEmpty) {
      builders.head += Text(text.toString)
      text.clear()
    }
  }
} 
