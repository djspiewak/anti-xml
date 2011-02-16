package com.codecommit.antixml

import org.xml.sax._

import java.io.{InputStream, Reader, StringReader}
import javax.xml.parsers.SAXParserFactory

import scala.io.Source

object XML {
  def fromString(str: String, validate: Boolean=true, namespaces: Boolean=true): NodeSeq =
    fromInputSource(new InputSource(new StringReader(str)), validate, namespaces)

  def fromInputStream(is: InputStream, validate: Boolean=true, namespaces: Boolean=true): NodeSeq =
    fromInputSource(new InputSource(is), validate, namespaces)

  def fromReader(reader: Reader, validate: Boolean=true, namespaces: Boolean=true): NodeSeq =
    fromInputSource(new InputSource(reader), validate, namespaces)

  def fromSource(source: Source, validate: Boolean=true, namespaces: Boolean=true): NodeSeq =
    fromInputSource(new InputSource(new SourceReader(source)), validate, namespaces)

  def fromInputSource(source: InputSource, validate: Boolean=true, namespaces: Boolean=true): NodeSeq = {
    val factory = SAXParserFactory.newInstance
    factory.setValidating(validate)
    factory.setNamespaceAware(namespaces)
    if (!validate)
      try {
        factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
        factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
        factory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
      } catch {
        case sex: SAXNotRecognizedException => Unit
        case ex: Exception => ex.printStackTrace
      }

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
