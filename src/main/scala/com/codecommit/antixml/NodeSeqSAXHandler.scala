/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit
package antixml

import util._

import org.xml.sax.{Attributes => SAXAttributes}
import org.xml.sax.ext.DefaultHandler2

/**
 * Defines a SAX2 handler which produces an instance
 * of [[com.codecommit.antixml.Group]]`[`[[com.codecommit.antixml.Elem]]`]` as
 * a result.  This is the handler which is used internally by [[com.codecommit.antixml.SAXParser]].
 * It is provided as part of the public API to allow Anti-XML to be used with
 * alternative SAX2 event sources (such as HTML parsers like TagSoup).  The
 * resulting [[com.codecommit.antixml.Group]] is obtained (at the conclusion of
 * the parse) from the `result()` method.
 *
 * @see [[com.codecommit.antixml.SAXParser]]
 */
class NodeSeqSAXHandler extends DefaultHandler2 {
  private var elems = List[Group[Node] => Elem]()
  private val text = new StringBuilder
  private var isCDATA = false
  private var scopes = Map[String, String]() :: Nil
  
  private var builders = VectorCase.newBuilder[Node] :: Nil
  
  override def startCDATA() {
    clearText()
    isCDATA = true
  }

  override def startPrefixMapping(prefix: String, namespace: String) {
    scopes ::= (scopes.headOption map { _ + (prefix -> namespace) } getOrElse Map())
  }

  override def endPrefixMapping(prefix: String) {
    scopes = scopes.tail
  }

  override def endCDATA() {
    clearText()
    isCDATA = false
  }
  
  override def characters(ch: Array[Char], start: Int, length: Int) {
    text.appendAll(ch, start, length)
  }
  
  override def startElement(uri: String, localName: String, qName: String, attrs: SAXAttributes) {
    clearText()
    
    // need to do this early since SAXAttributes objects may be reused
    val map = (0 until attrs.getLength).foldLeft(Attributes()) { (map, i) =>
      val ns = {
        val back = attrs.getURI(i)
        if (back == "") None else Some(back)
      }
      
      val localName = attrs.getLocalName(i)

      val prefix = {
        val back = attrs.getQName(i)
        if (back == localName) None else Some(back.substring(0, back.length - localName.length -1))
      }
    
      map + (QName(ns, prefix, localName) -> attrs.getValue(i))
    }

    builders ::= VectorCase.newBuilder
    elems ::= { children =>
      val prefix = if (qName == localName)
        None
      else
        Some(qName.substring(0, qName.length - localName.length - 1))
      
      val ns = if (uri == "") None else Some(uri)
      
      Elem(QName(ns, prefix, localName), map, scopes.headOption getOrElse Map(), children)
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
  
  /**
   * Returns the [[com.codecommit.antixml.Group]] instance resulting from the
   * SAX2 event stream.  This method is ''not'' thread-safe and should only be
   * called once (in fact, calling it multiple times ''will'' throw an exception).
   * Additionally, this method should only be called after the SAX2 stream has
   * fully completed.  The result of this method is undefined if invoked prematurely.
   */
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
