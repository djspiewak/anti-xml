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
 * - Neither the name of the <ORGANIZATION> nor the names of its contributors may
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

package com.codecommit.antixml

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

sealed trait NodeView {
  private[antixml] def force(): Unit
}
object NodeView {
  def apply(xmlReader: XMLStreamReader): ElemView = {
    xmlReader.next
    new ElemView(xmlReader)
  }
}

class ElemView private[antixml](xmlReader: XMLStreamReader) extends NodeView {
  lazy val (ns: Option[String],
            name: String,
            namespaces: Map[String, String],
            attrs: Map[String, String],
            children: GroupNodeView) = {
    val uri = xmlReader.getNamespaceURI
    (if (uri == null || uri == "") None else Some(uri),
     xmlReader.getLocalName,
     Map((0 until xmlReader.getNamespaceCount) map { i =>
       xmlReader.getNamespacePrefix(i) -> xmlReader.getNamespaceURI(i)
     }: _*),
     Map.empty[String, String],
     new GroupNodeView(xmlReader))
  }

  private[antixml] def force() {
    ns
    assert(xmlReader.next == XMLStreamConstants.END_ELEMENT)
  }
  
  lazy val qName = ns map (_ + ":" + name) getOrElse name
  override lazy val toString: String = {
    "<" + qName + ("" /: namespaces) { (result, namespace) =>
        result + " xmlns:" + namespace._1 + "='" + namespace._2 + "'"
      } + (children.length match {
        case 0 =>  " />"
        case _ =>
          ">" + (children.mkString("")) + "</" + qName + ">"
      })
  }
}

case class TextView private[antixml](private val xmlReader: XMLStreamReader) extends NodeView {
  lazy val text: String =
    new String(xmlReader.getTextCharacters, xmlReader.getTextStart, xmlReader.getTextLength)
  private[antixml] def force() {
    text
  }
  override lazy val toString: String = text
}
