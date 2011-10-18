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
package performance

import javax.xml.parsers.DocumentBuilderFactory

trait LoadTrial {self: Trial =>
  def xmlResource: java.net.URL
  def sizeDescription: String
  
  class Inst extends self.TrialInstance[Any] {
    override def resultDescription(a: Any) = XmlCounts(a).report
    override def testDataDescription = "file size is %d bytes".format(from(xmlResource)(LoadTrial.countBytes))
    
    object SAXParser extends SAXParser
    object StAXParser extends StAXParser

    val antiXml = implemented by "anti-xml" in {
      from(xmlResource) {SAXParser.fromInputStream(_)}
    }
    val antiXmlStAX = implemented by "anti-xml StAX" in {
      from(xmlResource) {StAXParser.fromInputStream(_)}
    }
    val scalaXml = implemented by "scala.xml" in {
      from(xmlResource) {scala.xml.XML.load(_)}
    }
    val javaXml = implemented by "javax.xml" in {
      from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    }
    
    /** Defines the impls that will be used for the size measurements */
    def sizeMeasurements: Seq[Implementation[_,_]] = Seq(antiXml, scalaXml, javaXml)
  }
  def create:Inst = new Inst
}

object LoadTrial {
  def countBytes(is: java.io.InputStream): Long = {
    var count: Long = 0
    while(is.read() >=0 ) {
      count += 1
      count += is.skip(100000000L)
    }
    count
  }
}
