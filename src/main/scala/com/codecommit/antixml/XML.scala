package com.codecommit.antixml

import org.xml.sax._

import java.io.{InputStream, Reader, StringReader}
import javax.xml.parsers.SAXParserFactory

import scala.io.Source

/**
 * A trait for objects which construct antixml from XML sources.
 */
// TODO named arguments for configuration
trait XML {
  def fromString(str: String): Elem
  
  def fromInputStream(is: InputStream): Elem
  
  def fromReader(reader: Reader): Elem

  def fromSource(source: Source): Elem =
    fromReader(new SourceReader(source))

  private class SourceReader(source: Source) extends Reader {
    import scala.util.control.Breaks._
    
    def read(ch: Array[Char], offset: Int, length: Int) = {
      if (!source.hasNext) {
        -1
      } else {
        var i = offset
        breakable {
          while (i < offset + length) {
            if (!source.hasNext) {
              break
            }
            
            ch(i) = source.next()
            i += 1
          }
        }
        i - offset
      }
    }
    
    override def reset() {
      source.reset()
    }
    
    override def close() {
      source.close()
    }
  }
}
/**
 * An XML provider implemented on top of the platform-default SAX parser.
 * @see org.xml.sax
 */
class SAXParser extends XML {
  override def fromString(str: String): Elem =
    fromInputSource(new InputSource(new StringReader(str)))
  
  override def fromInputStream(is: InputStream): Elem =
    fromInputSource(new InputSource(is))
  
  override def fromReader(reader: Reader): Elem =
    fromInputSource(new InputSource(reader))

  def fromInputSource(source: InputSource): Elem = {
    val factory = SAXParserFactory.newInstance
    factory.setValidating(true)
    factory.setNamespaceAware(true)
    
    val parser = factory.newSAXParser
    val handler = new NodeSeqSAXHandler
    parser.parse(source, handler)
    
    handler.result().head   // safe because anything else won't validate
  }
}
object XML extends SAXParser

