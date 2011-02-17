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
 * A Stream-based wrapper for javax.xml.stream.
 * @see java.xml.stream
 */
class StAXParser extends XML {
  override def fromInputStream(inputStream: InputStream): Group[Elem] =
    fromStreamSource(new StreamSource(inputStream))
  override def fromReader(reader: Reader): Group[Elem] =
    fromStreamSource(new StreamSource(reader))
  override def fromString(xml: String): Group[Elem] =
    fromReader(new StringReader(xml))
  def fromStreamSource(source: StreamSource): Group[Elem] =
    parse(view(source))
  
  /**
   * Returns a Stream[StAXEvent] by parsing the provided String.
   * @return a Stream[StAXEvent] by parsing the provided String.
   */
  def view(xml: String): Stream[StAXEvent] =
    view(new StreamSource(new StringReader(xml)))
  /**
   * Returns a Stream[StAXEvent] by parsing the provided StreamSource.
   * @return a Stream[StAXEvent] by parsing the provided StreamSource.
   */
  def view(source: StreamSource): Stream[StAXEvent] = {
    val xmlReader =
      XMLInputFactory.newInstance().createXMLStreamReader(source)
    def next: Stream[StAXEvent] = if (xmlReader.hasNext) {
      xmlReader.next match {
	case `CHARACTERS` => Stream.cons(Characters(xmlReader.getText), next)
	case `COMMENT` => Stream.cons(Comment(xmlReader.getText), next)
	case `DTD` => Stream.cons(DocumentTypeDefinition(xmlReader.getText), next)
	case `END_ELEMENT` =>
	  Stream.cons(ElemEnd(stringToOption(xmlReader.getPrefix),
			      xmlReader.getLocalName,
			      stringToOption(xmlReader.getNamespaceURI)), next)
	case `END_DOCUMENT` => Stream.cons(DocumentEnd, next)
	case `PROCESSING_INSTRUCTION` =>
	  Stream.cons(ProcessingInstruction(xmlReader.getPITarget, xmlReader.getPIData), next)
	case `START_ELEMENT` => {
	  val attrs =
	    (Map.empty[QName, String] /: (0 until xmlReader.getAttributeCount)) { (attrs, i) =>
	    attrs + (xmlReader.getAttributeName(i) -> xmlReader.getAttributeValue(i))
	  }
	  Stream.cons(ElemStart(stringToOption(xmlReader.getPrefix),
				xmlReader.getLocalName,
				attrs,
				stringToOption(xmlReader.getNamespaceURI)), next)
	}
	case _ => throw new XMLStreamException("Unexpected StAX event of type " + xmlReader.getEventType)
      }
    } else {
      Stream.Empty
    }
    next
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

  /**
   * Returns Some string if string is not null or blank.
   * @return Some string if string is not null or blank.
   */
  @inline private def stringToOption(string: String): Option[String] =
    string match {
      case null => None
      case "" => None
      case string => Some(string)
    }

  def parse(source: Stream[StAXEvent]): Group[Elem] = {
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
}
object StAXParser extends StAXParser

// XXX is this worthwhile or better just to use the javax.xml.streams events?
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
case class ElemStart(val prefix: Option[String],
		     val name: String,
		     val attrs: Map[QName, String],
		     val uri: Option[String]) extends StAXEvent {
  def attrs(name: String): String = attrs(new QName(NULL_NS_URI, name))
}
/**
 * A StAXEvent indicating the end of an Elem.
 */
case class ElemEnd(val prefix: Option[String],
		   val name: String,
		   val uri: Option[String]) extends StAXEvent
/**
 * A StAXEvent indicating a text Node.
 */
case class Characters(val text: String) extends StAXEvent
/**
 * A StAXEvent indicating a comment Node.
 */
case class Comment(val text: String) extends StAXEvent
/**
 * A StAXEvent indicating a processing instruction Node.
 */
case class ProcessingInstruction(val target: String, val data: String) extends StAXEvent
/**
 * A StAXEvent indicating a DocumentTypeDefinition (DTD).
 */
case class DocumentTypeDefinition(val declaration: String) extends StAXEvent
/**
 * A StAXEvent indicating the end of an XML document.
 */
object DocumentEnd extends StAXEvent
