package com.codecommit.antixml

import org.xml.sax._

import java.io.{InputStream, Reader, StringReader}
import javax.xml.parsers.SAXParserFactory

import scala.io.Source

// TODO named arguments for configuration
object XML {
  def fromString(str: String): Group[Node] =
    fromInputSource(new InputSource(new StringReader(str)))
  
  def fromInputStream(is: InputStream): Group[Node] =
    fromInputSource(new InputSource(is))
  
  def fromReader(reader: Reader): Group[Node] =
    fromInputSource(new InputSource(reader))
  
  def fromSource(source: Source): Group[Node] =
    fromInputSource(new InputSource(new SourceReader(source)))
  
  def fromInputSource(source: InputSource): Group[Node] = {
    val factory = SAXParserFactory.newInstance
    factory.setValidating(true)
    factory.setNamespaceAware(true)
    
    val parser = factory.newSAXParser
    val handler = new NodeSeqSAXHandler
    parser.parse(source, handler)
    
    handler.result
  }
  
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
