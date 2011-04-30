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

