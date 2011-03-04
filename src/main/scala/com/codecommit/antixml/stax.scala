package com.codecommit.antixml

import org.xml.sax.{Attributes, ContentHandler}
import org.xml.sax.helpers.AttributesImpl
import java.io.{InputStream, StringReader, Reader}
import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.{XMLInputFactory, XMLStreamException}
import javax.xml.stream.XMLStreamConstants._
import javax.xml.transform.stream.StreamSource
import javax.xml.XMLConstants.NULL_NS_URI

/**
 * An XML provider build on top of StAXIterator.
 */
class StAXParser extends XML {
  import StAXEvents._
  
  override def fromInputStream(inputStream: InputStream): Group[Elem] =
    fromStreamSource(new StreamSource(inputStream))
  override def fromReader(reader: Reader): Group[Elem] =
    fromStreamSource(new StreamSource(reader))
  override def fromString(xml: String): Group[Elem] =
    fromReader(new StringReader(xml))
  def fromStreamSource(source: StreamSource): Group[Elem] =
    parse(new StAXIterator(source))

  def parse(source: Iterator[StAXEvent]): Group[Elem] = {
    val handler = new NodeSeqSAXHandler()
    source foreach {
      case ElemStart(prefix, name, attrs, uri) =>
        handler.startElement(uri getOrElse "",
                             name,
                             prefix map ((_: String) + ":" + name) getOrElse "",
                             mapToAttrs(attrs))
      case ElemEnd(prefix, name, uri) =>
        handler.endElement(prefix getOrElse "",
                           name,
                           prefix map ((_: String) + ":" + name) getOrElse "")
      case Characters(text) => handler.characters(text.toArray, 0, text.length)
      case Comment(text) => ()
      case ProcessingInstruction(target, data) => ()
      case DocumentTypeDefinition(declaration) => ()
      case DocumentEnd => ()
    }
    handler.result
  }
  
  /**
   * Returns an equivalent Attributes for a Map of QNames to Strings.
   * @return an equivalent Attributes for a Map of QNames to Strings.
   */
  private def mapToAttrs(attrs: Map[QName, String]): Attributes = {
    val result = new AttributesImpl()
    attrs foreach { case (qname, value) =>
      result.addAttribute(qname.getNamespaceURI,
                          qname.getLocalPart,
                          qname.toString,
                          "",
                          value)
    }
    result
  }
}

object StAXIterator {
  def fromString(xml: String): StAXIterator =
    new StAXIterator(new StreamSource(new StringReader(xml)))
}
/**
 * An Iterator over StAXEvents. This implementation uses the
 * <a href="http://download.oracle.com/javase/6/docs/api/javax/xml/stream/package-summary.html">
 * Java Streaming API for XML</a>.
 * @see java.xml.stream
 */
class StAXIterator(source: StreamSource) extends Iterator[StAXEvents.StAXEvent] {
  import StAXEvents._
  
  private val xmlReader =
    XMLInputFactory.newInstance().createXMLStreamReader(source)
  override def next: StAXEvent = xmlReader.next match {
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
  /**
   * A base trait for StAXParser generated events.
   * @see java.xml.stream.XMLEvent
   * */
  sealed trait StAXEvent
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
                       uri: Option[String]) extends StAXEvent {
    def attrs(name: String): String = attrs(new QName(NULL_NS_URI, name))
  }
  /**
   * A StAXEvent indicating the end of an Elem.
   */
  case class ElemEnd(prefix: Option[String],
                     name: String,
                     uri: Option[String]) extends StAXEvent
  /**
   * A StAXEvent indicating a text Node.
   */
  case class Characters(text: String) extends StAXEvent
  /**
   * A StAXEvent indicating a comment Node.
   */
  case class Comment(text: String) extends StAXEvent
  /**
   * A StAXEvent indicating a processing instruction Node.
   */
  case class ProcessingInstruction(target: String, data: String) extends StAXEvent
  /**
   * A StAXEvent indicating a DocumentTypeDefinition (DTD).
   */
  case class DocumentTypeDefinition(declaration: String) extends StAXEvent
  /**
   * A StAXEvent indicating the end of an XML document.
   */
  object DocumentEnd extends StAXEvent
}
