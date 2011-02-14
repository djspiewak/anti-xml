package com.codecommit.antixml

import org.xml.sax._

import java.io.{InputStream, StringReader}
import javax.xml.parsers.SAXParserFactory

// TODO named arguments for configuration
object XML {
  def fromString(str: String): NodeSeq =
    fromInputSource(new InputSource(new StringReader(str)))
  
  def fromInputStream(is: InputStream): NodeSeq =
    fromInputSource(new InputSource(is))
  
  def fromInputSource(source: InputSource): NodeSeq = {
    val factory = SAXParserFactory.newInstance
    factory.setValidating(true)
    factory.setNamespaceAware(true)
    
    val parser = factory.newSAXParser
    val handler = new NodeSeqSAXHandler
    parser.parse(source, handler)
    
    handler.result
  }
}
