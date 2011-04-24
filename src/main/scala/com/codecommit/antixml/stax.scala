package com.codecommit.antixml

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
    while(source.hasNext) {
      val event = source.next
      event.number match {
        // this match is very active during parsing
        // using integer literals compiles to a single tableswitch
        // the techniques below become sequential if-elses on scala 2.8.1:
        // - referencing StAXEventNumber fields
        // - caching StAXEventNumbers as fields of StAXParser
        // - caching StAXEventNumbers as local vals
        // - matching on the class of the event
        case 1 => {
          val elemStart = event.asInstanceOf[ElemStart]
          handler.startElement(elemStart.uri getOrElse "",
                               elemStart.name,
                               elemStart.prefix map ((_: String) + ":" + elemStart.name) getOrElse "",
                               mapToAttrs(elemStart.attrs))
        }
        case 2 => {
          val elemEnd = event.asInstanceOf[ElemEnd]
          handler.endElement(elemEnd.prefix getOrElse "",
                             elemEnd.name,
                             elemEnd.prefix map ((_: String) + ":" + elemEnd.name) getOrElse "")
        }
        case 3 => {
          val characters = event.asInstanceOf[Characters]
          handler.characters(characters.text.toArray, 0, characters.text.length)
        }
        case 8 => {
          val entityRef = event.asInstanceOf[EntityRef]
          handler.skippedEntity(entityRef.text)
        }
        case 9 => {
          val cdata = event.asInstanceOf[CDATA]
          handler.startCDATA()
          handler.characters(cdata.text.toArray, 0, cdata.text.length)
          handler.endCDATA()
        }
        case _ => ()
      }
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
  object StAXEventNumber {
    val ElemStart = 1
    val ElemEnd = 2
    val Characters = 3
    val CDATA = 9
    val Comment = 4
    val ProcessingInstruction = 5
    val DocumentTypeDefinition = 6
    val DocumentEnd = 7
    val EntityRef = 8
  }

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
                       uri: Option[String]) extends StAXEvent(StAXEventNumber.ElemStart) {
    def attrs(name: String): String = attrs(new QName(NULL_NS_URI, name))
  }
  /**
   * A StAXEvent indicating the end of an Elem.
   */
  case class ElemEnd(prefix: Option[String],
                     name: String,
                     uri: Option[String]) extends StAXEvent(StAXEventNumber.ElemEnd)
  /**
   * A StAXEvent indicating a text Node.
   */
  case class Characters(text: String) extends StAXEvent(StAXEventNumber.Characters)
  /**
   * A StAXEvent indicating a CDATA Node.
   */
  case class CDATA(text: String) extends StAXEvent(StAXEventNumber.CDATA)
  /**
   * A StAXEvent indicating a comment Node.
   */
  case class Comment(text: String) extends StAXEvent(StAXEventNumber.Comment)
  /**
   * A StAXEvent indicating a processing instruction Node.
   */
  case class ProcessingInstruction(target: String, data: String) extends StAXEvent(StAXEventNumber.ProcessingInstruction)
  /**
   * A StAXEvent indicating a DocumentTypeDefinition (DTD).
   */
  case class DocumentTypeDefinition(declaration: String) extends StAXEvent(StAXEventNumber.DocumentTypeDefinition)
  case class EntityRef(localName: String, text: String) extends StAXEvent(StAXEventNumber.EntityRef)
  /**
   * A StAXEvent indicating the end of an XML document.
   */
  object DocumentEnd extends StAXEvent(StAXEventNumber.DocumentEnd)
}
