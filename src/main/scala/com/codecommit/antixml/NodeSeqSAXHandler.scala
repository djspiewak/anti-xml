package com.codecommit
package antixml

import util._

import org.xml.sax.Attributes
import org.xml.sax.ext.DefaultHandler2

class NodeSeqSAXHandler extends DefaultHandler2 {
  private var elems = List[Group[Node] => Elem]()
  private val text = new StringBuilder
  private var isCDATA = false
  
  private var builders = VectorCase.newBuilder[Node] :: Nil
  
  override def startCDATA() {
    clearText()
    isCDATA = true
  }
  
  override def endCDATA() {
    clearText()
    isCDATA = false
  }
  
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
    val construct = if (isCDATA) CDATA else Text
    
    if (!text.isEmpty) {
      builders.head += construct(text.toString)
      text.clear()
    }
  }
} 
