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

package com.codecommit.antixml

import util.VectorCase
import org.xml.sax.{Attributes, ContentHandler}
import org.xml.sax.helpers.AttributesImpl
import java.io.{InputStream, StringReader, Reader}
import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.{XMLInputFactory, XMLStreamException}
import javax.xml.stream.XMLStreamConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.XMLConstants.NULL_NS_URI

/**
 * An XML provider build on top of StAXIterator.
 */
class StAXParser extends XMLParser {
  import StAXEvents._
  
  override def fromInputStream(inputStream: InputStream): Elem =
    fromStreamSource(new StreamSource(inputStream))
  
  override def fromReader(reader: Reader): Elem =
    fromStreamSource(new StreamSource(reader))
  
  override def fromString(xml: String): Elem =
    fromReader(new StringReader(xml))
  
  private case class ElemBuilder(ns: Option[String], name: String, attrs: Map[String, String])

  private def fromStreamSource(source: StreamSource): Elem = {
    import XMLStreamConstants.{CDATA => CDATAFlag, CHARACTERS, COMMENT, DTD, END_ELEMENT, END_DOCUMENT, PROCESSING_INSTRUCTION, START_ELEMENT, ENTITY_REFERENCE}

    val xmlReader = XMLInputFactory.newInstance().createXMLStreamReader(source)
    var elems: List[ElemBuilder] = Nil
    var results = VectorCase.newBuilder[Node] :: Nil
    val text = new StringBuilder
    while(xmlReader.hasNext) {
      xmlReader.next match {
        case `CHARACTERS` =>
          text.appendAll(xmlReader.getTextCharacters, xmlReader.getTextStart, xmlReader.getTextLength)
        case `END_ELEMENT` => {
          val elem = elems.head
          val parents = elems.tail
          val children = results.head
          val ancestors = results.tail
          if (text.size > 0) {
            children += Text(text.result)
            text.clear()
          }
          ancestors.head += Elem(elem.ns, elem.name, elem.attrs, Group fromSeq children.result)
          elems = parents
          results = ancestors
        }
        case `START_ELEMENT` => {
          if (text.size > 0) {
            results.head += Text(text.result)
            text.clear()
          }
          var i = 0
          var attrs = Map.empty[String, String]
          while (i < xmlReader.getAttributeCount) {          
            attrs = attrs + (xmlReader.getAttributeLocalName(i) -> xmlReader.getAttributeValue(i))
            i = i + 1
          }
          val uri = xmlReader.getNamespaceURI
          elems ::= ElemBuilder(if (uri == null || uri == "") None else Some(uri), xmlReader.getLocalName, attrs)
           results ::= VectorCase.newBuilder[Node]           
        }
        case _ =>
      }
    }
    results.head.result.head.asInstanceOf[Elem]
  }
}

object StAXIterator {
  def fromString(xml: String): Iterator[StAXEvents.StAXEvent] =
    new StAXIterator(new StreamSource(new StringReader(xml)))
}
/**
 * An Iterator over StAXEvents. This implementation uses the
 * <a href="http://download.oracle.com/javase/6/docs/api/javax/xml/stream/package-summary.html">
 * Java Streaming API for XML</a>.
 * @see java.xml.stream
 */
private[antixml] class StAXIterator(source: StreamSource) extends Iterator[StAXEvents.StAXEvent] {
  import StAXEvents._
  import XMLStreamConstants.{CDATA => CDATAFlag, CHARACTERS, COMMENT, DTD,
    END_ELEMENT, END_DOCUMENT, PROCESSING_INSTRUCTION, START_ELEMENT, ENTITY_REFERENCE}
  
  private val xmlReader =
    XMLInputFactory.newInstance().createXMLStreamReader(source)
  override def next: StAXEvent = xmlReader.next match {
    case `CDATAFlag` => CDATA(xmlReader.getText)
    case `CHARACTERS` => Characters(xmlReader.getText)
    case `COMMENT` => Comment(xmlReader.getText)
    case `DTD` => DocumentTypeDefinition(xmlReader.getText)
    case `END_ELEMENT` =>
      ElemEnd(stringToOption(xmlReader.getPrefix),
                          xmlReader.getLocalName,
                          stringToOption(xmlReader.getNamespaceURI))
    case `END_DOCUMENT` => DocumentEnd
    case `PROCESSING_INSTRUCTION` =>
      ProcessingInstruction(xmlReader.getPITarget, xmlReader.getPIData)
    case `START_ELEMENT` => {
      val attrs =
        (Map.empty[QName, String] /: (0 until xmlReader.getAttributeCount)) { (attrs, i) =>
          attrs + (xmlReader.getAttributeName(i) -> xmlReader.getAttributeValue(i))
        }
      ElemStart(stringToOption(xmlReader.getPrefix),
                xmlReader.getLocalName,
                attrs,
                stringToOption(xmlReader.getNamespaceURI))
    }
    case `ENTITY_REFERENCE` =>
      EntityRef(xmlReader.getLocalName, xmlReader.getText)
    case _ =>
      throw new XMLStreamException("Unexpected StAX event of type " +
                                   xmlReader.getEventType)
  }
  override def hasNext: Boolean = xmlReader.hasNext

  /**
   * Returns Some string if string is not null or empty.
   * @return Some string if string is not null or empty.
   */
  @inline private def stringToOption(string: String): Option[String] =
    string match {
      case null => None
      case "" => None
      case string => Some(string)
    }
}

object StAXEvents {
  import XMLStreamConstants.{CDATA => CDATAFlag, CHARACTERS, COMMENT, DTD, END_ELEMENT, END_DOCUMENT, PROCESSING_INSTRUCTION, START_ELEMENT, ENTITY_REFERENCE}

  /**
   * A base trait for StAXParser generated events.
   * @see java.xml.stream.XMLEvent
   * */
  abstract class StAXEvent(val number: Int)
  object ElemStart {
    def apply(name: String, attrs: Map[QName, String]): ElemStart =
      ElemStart(None, name, attrs, None)
  }
  /**
   * A StAXEvent indicating the start of an Elem.
   */
  case class ElemStart(prefix: Option[String],
                       name: String,
                       attrs: Map[QName, String],
                       uri: Option[String]) extends StAXEvent(START_ELEMENT) {
    def attrs(name: String): String = attrs(new QName(NULL_NS_URI, name))
  }
  /**
   * A StAXEvent indicating the end of an Elem.
   */
  case class ElemEnd(prefix: Option[String],
                     name: String,
                     uri: Option[String]) extends StAXEvent(END_ELEMENT)
  /**
   * A StAXEvent indicating a text Node.
   */
  case class Characters(text: String) extends StAXEvent(CHARACTERS)
  /**
   * A StAXEvent indicating a CDATA Node.
   */
  case class CDATA(text: String) extends StAXEvent(CDATAFlag)
  /**
   * A StAXEvent indicating a comment Node.
   */
  case class Comment(text: String) extends StAXEvent(COMMENT)
  /**
   * A StAXEvent indicating a processing instruction Node.
   */
  case class ProcessingInstruction(target: String, data: String) extends StAXEvent(PROCESSING_INSTRUCTION)
  /**
   * A StAXEvent indicating a DocumentTypeDefinition (DTD).
   */
  case class DocumentTypeDefinition(declaration: String) extends StAXEvent(DTD)
  case class EntityRef(localName: String, text: String) extends StAXEvent(ENTITY_REFERENCE)
  /**
   * A StAXEvent indicating the end of an XML document.
   */
  object DocumentEnd extends StAXEvent(END_DOCUMENT)
}
