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

import java.io.Writer
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
 
class XMLSerializer(encoding: String, outputDeclaration: Boolean, indent: String = "") {
  // Only pretty-print if we have a non-empty indent string
  private val prettyPrint = (indent != "")
  private val newline = sys.props("line.separator")

  /** Serializes an XML document, whose root is given.
   *
   * Outputs the XML declaration if this XMLSerializer was created with
   * outputDeclaration = true.
   *
   * Note: it is up to the caller of this function to ensure that the
   * character encoding used by the Writer (if any) matches the encoding of this
   * XMLSerializer
   */
  def serializeDocument(elem: Elem, w: Writer) {
    if (outputDeclaration) {
      w.append("<?xml version=\"1.0\" encoding=\"")
      w.append(encoding)
      w.append("\" standalone=\"yes\"?>")
      newline(w)
    }
    serialize(elem, w)
  }

  /** Serializes an XML document, whose root is given.
   *
   * Outputs the XML declaration if this XMLSerializer was created with
   * outputDeclaration = true. 
   *
   * Uses the character encoding of this XMLSerializer (default is UTF-8).
   */
  def serializeDocument(elem: Elem, o: OutputStream) {
    val writer = new OutputStreamWriter(o, encoding)
    serializeDocument(elem, writer)
    writer.flush()
  }

  def serializeDocument(elem: Elem, outputFile: File) {
    val fos = new FileOutputStream(outputFile)
    try {
      serializeDocument(elem, fos)
    } finally {
      fos.close()
    }
  }

  /** Writes indentation, if appropriate (i.e. we have an indentation string).
   *
   */
  private def indent(level: Int, w: Writer) {
    if (prettyPrint) {
      w append(indent * level)
    }
  }

  /** Writes a newline, if appropriate (i.e. we have an indentation string).
   *
   */
  private def newline(w: Writer) {
    if (prettyPrint) {
      w append(newline)
    }
  }
  
  def serialize(elem: Elem, w: Writer) {
    var scopes: List[Map[String, String]] = Nil

    def doSerialize(node: Node, w: Writer) {
      node match {
        case Elem(prefix, name, attrs, scope, children) => {
          val parentScope = scopes.headOption getOrElse Map()
          scopes = scope :: scopes
          val attrStr = if (attrs.isEmpty) {
            ""
          } else {
            val delta = attrs map {
              case (key, value) => key.toString + "=" + Node.quoteAttribute(value)
            } mkString " "
            
            " " + delta
          }
            
          val scopeChange = scope filter { case (key, value) => parentScope.get(key) != Some(value) }
          val prefixesStr = if (scopeChange.isEmpty) { 
            ""
          } else {
            val delta = scopeChange map {
              case (key, value) =>
                (if (key == "") "xmlns" else "xmlns:" + key) + "=" + Node.quoteAttribute(value)
            } mkString " "
            
            " " + delta
          }
        
          val qname = (prefix map { _ + ":" } getOrElse "") + name
          val partial = "<" + qname + attrStr + prefixesStr

          // indent is scope - 1 because we want the first element to be at indent 0
          val indentLevel = scopes.size - 1
          indent(indentLevel, w)
          // If we have any children that are Elems, we will write a newline and indent the closing tag.
          // If we have no children, or they are all text or other special nodes, we won't.
          // Is there a tidier way to do this?
          val indentForChildren = prettyPrint && ! (children collect {case e: Elem => e}).isEmpty
          if (children.isEmpty) {
            w.append(partial)
            w.append("/>")
            newline(w)
          } else {
            w.append(partial)
            w.append('>')
            if (indentForChildren) {
              newline(w)
            }
            children foreach { doSerialize(_, w) }
            if (indentForChildren) {
              indent(indentLevel, w)
            }
            w append("</")
            w append(qname)
            w append('>')
            newline(w)
          }
          
          scopes = scopes.tail
        }
        
        case node => w.append(node.toString)
      }
    }
    doSerialize(elem, w)
  }
}

object XMLSerializer {
  def apply(encoding: String = "UTF-8", outputDeclaration: Boolean = false, indent: String = ""): XMLSerializer = {
    new XMLSerializer(encoding, outputDeclaration, indent);
  }
}
