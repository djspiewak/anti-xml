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

import org.xml.sax._

import java.io.{InputStream, Reader, StringReader}
import javax.xml.parsers.{SAXParserFactory, SAXParser => JSAXParser}

import scala.io.Source

/**
 * An XML parser build on top of `org.w3c.sax`.  This implements the same
 * API as [[com.codecommit.antixml.StAXParser]], but the runtime performance is
 * on the order of 13% slower.  The SAX2 event handler used under the surface is
 * part of the public API in the form of [[com.codecommit.antixml.NodeSeqSAXHandler]].
 * 
 * @param factory optional parameter of a function that return a javax.xml.parsers.SAXParser.
 *                The returned parser should be both validating and namespace aware.  Be wary of
 *                thread safety if the specified function reuses SAXParserFactory or SAXParser
 *                instances.
 */
class SAXParser(factory: () => JSAXParser = () => { 
                  val factory = SAXParserFactory.newInstance
                  factory.setValidating(true)
                  factory.setNamespaceAware(true)
		  factory.newSAXParser
	        }) extends XMLParser {
  def fromString(str: String): Elem =
    fromInputSource(new InputSource(new StringReader(str)))
  
  def fromInputStream(is: InputStream): Elem =
    fromInputSource(new InputSource(is))
  
  def fromReader(reader: Reader): Elem =
    fromInputSource(new InputSource(reader))

  def fromInputSource(source: InputSource): Elem = {
    val parser = factory()
    val handler = new NodeSeqSAXHandler
    parser.parse(source, handler)
    
    handler.result().head   // safe because anything else won't validate
  }
}
