package com.codecommit.antixml

import org.xml.sax.ContentHandler
import java.io.StringReader
import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.{XMLInputFactory, XMLStreamException}
import javax.xml.stream.XMLStreamConstants._
import javax.xml.XMLConstants.NULL_NS_URI

/**
 * A Stream-based wrapper for javax.xml.stream.
 * @see java.xml.stream
 */
object StAXParser {
  /**
   * Returns a Stream[StAXEvent] by parsing the provided String as XML.
   * @return a Stream[StAXEvent] by parsing the provided String as XML.
   */
  def fromString(xml: String): Stream[StAXEvent] = {
    val xmlReader =
      XMLInputFactory.newInstance().createXMLStreamReader(new StringReader(xml))
    def next: Stream[StAXEvent] = if (xmlReader.hasNext) {
      xmlReader.next match {
	case `CHARACTERS` => Stream.cons(Characters(xmlReader.getText), next)
	case `COMMENT` => Stream.cons(Comment(xmlReader.getText), next)
	case `DTD` => Stream.cons(DocumentTypeDefinition(xmlReader.getText), next)
	case `END_ELEMENT` => Stream.cons(ElemEnd, next)
	case `END_DOCUMENT` => Stream.cons(DocumentEnd, next)
	case `PROCESSING_INSTRUCTION` =>
	  Stream.cons(ProcessingInstruction(xmlReader.getPITarget, xmlReader.getPIData), next)
	case `START_ELEMENT` => {
	  val attrs =
	    (Map.empty[QName, String] /: (0 until xmlReader.getAttributeCount)) { (attrs, i) =>
	    attrs + (xmlReader.getAttributeName(i) -> xmlReader.getAttributeValue(i))
	  }
	  val namespaces =
	    (Map.empty[String, String] /: (0 until xmlReader.getNamespaceCount)) { (namespaces, i) =>
	    namespaces + (xmlReader.getNamespacePrefix(i) -> xmlReader.getNamespaceURI(i))
          }
	  val prefix = xmlReader.getPrefix match {
	    case null => None
	    case prefix => Some(prefix)
	  }
	  Stream.cons(ElemStart(/*prefix, */attrs, namespaces), next)
	}
	case _ => throw new XMLStreamException("Unexpected StAX event of type " + xmlReader.getEventType)
      }
    } else {
      Stream.Empty
    }
    next
  }

  // def parse(source: Stream[StAXEvent]): NodeSeq = {
  //   val handler = new NodeSeqSAXHandler()
  //   source foreach {
  //     case ElemStart(prefix, attrs, namespaces) => () //handler.startElement(prefix.map(namespaces(_)),
  //     case ElemEnd => ()
  //     case Characters(text) => ()
  //     case Comment(text) => ()
  //     case ProcessingInstruction(target, data) => ()
  //     case DocumentTypeDefinition(declaration) => ()
  //     case DocumentEnd => ()
  //   }
  //   handler.result
  // }
}

// XXX is this worthwhile or better just to use the javax.xml.streams events?
/**
 * A base trait for StAXParser generated events.
 * @see java.xml.stream.XMLEvent
 * */
sealed trait StAXEvent
object ElemStart {
  def apply(attrs: Map[QName, String]): ElemStart = ElemStart(/*None, */attrs, Map.empty)
}
/**
 * A StAXEvent indicating the start of an Elem.
 */
case class ElemStart(/*val prefix: Option[String], */val attrs: Map[QName, String], val namespaces: Map[String, String])
  extends StAXEvent {
  def attrs(name: String): String = attrs(new QName(NULL_NS_URI, name))
}
/**
 * A StAXEvent indicating the end of an Elem.
 */
object ElemEnd extends StAXEvent
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
case class DocumentTypeDefinition(val declaration: String) extends StAXEvent
object DocumentEnd extends StAXEvent
