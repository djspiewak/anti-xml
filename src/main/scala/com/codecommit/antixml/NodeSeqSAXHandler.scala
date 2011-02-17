package com.codecommit.antixml

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

private[antixml] class NodeSeqSAXHandler extends DefaultHandler {
  var elems = List[Group[Node] => Elem]()
  val text = new StringBuilder
  val whitespace = new StringBuilder
  
  var builders = Vector.newBuilder[Node] :: Nil
  
  override def characters(ch: Array[Char], start: Int, length: Int) {
    clearWhitespace()
    text.appendAll(ch, start, length)
  }
  
  override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int) {
    clearText()
    whitespace.appendAll(ch, start, length)
  }
  
  override def startElement(uri: String, localName: String, qName: String, attrs: Attributes) {
    clearWhitespace()
    clearText()
    
    builders ::= Vector.newBuilder
    elems ::= { children =>
      val ns = if (uri == "") None else Some(uri)
      val map = (0 until attrs.getLength).foldLeft(Map[String, String]()) { (map, i) =>
        map + (attrs.getQName(i) -> attrs.getValue(i))    // TODO namespacing
      }
      
      Elem(ns, localName, map, children)
    }
  }
  
  override def endElement(uri: String, localName: String, qName: String) {
    clearWhitespace()
    clearText()
    
    val (build :: elems2) = elems
    elems = elems2
    
    val result = build(pop())
    builders.head += result
  }
  
  def result = pop().asInstanceOf[Group[Elem]]       // nasty, but it shouldn't be a problem
  
  private def pop() = {
    val (back :: builders2) = builders
    builders = builders2
    Group fromSeq back.result
  }
  
  private def clearWhitespace() {
    if (!whitespace.isEmpty) {
      builders.head += Whitespace(text.toString)
      whitespace.clear()
    }
  }
  
  private def clearText() {
    if (!text.isEmpty) {
      builders.head += Text(text.toString)
      text.clear()
    }
  }
} 
