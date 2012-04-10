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

import org.specs2.mutable._

class XMLSerializerSpecs extends Specification {
  import XML._
  
  "xml serialization" should {
    "serialize prefixes minimally" in {
      fromString("<my:test xmlns:my='urn:my-urn:quux'>\n<beef/>\n\t\n</my:test>").toString mustEqual "<my:test xmlns:my=\"urn:my-urn:quux\">\n<beef/>\n\t\n</my:test>"
    }
    
    "serialize unprefixed elements correctly" in {
      fromString("<test xmlns='urn:my-urn:quux'>\n<beef/>\n\t\n</test>").toString mustEqual "<test xmlns=\"urn:my-urn:quux\">\n<beef/>\n\t\n</test>"
    }

    "pretty-print when asked" in {
      val serializer = XMLSerializer(indent="--")
      val writer = new java.io.StringWriter
      serializer.serializeDocument(fromString("<test xmlns='urn:my-urn:quux'><beef><calf>1</calf><calf>2</calf></beef></test>"), writer)
      writer.toString mustEqual "<test xmlns=\"urn:my-urn:quux\">\n--<beef>\n----<calf>1</calf>\n----<calf>2</calf>\n--</beef>\n</test>\n"

    }

    "maintain existing whitespace when pretty-printing" in {
      val serializer = XMLSerializer(indent="--")
      val writer = new java.io.StringWriter
      serializer.serializeDocument(fromString("<test xmlns='urn:my-urn:quux'>\n<beef><calf>1</calf><calf>2</calf></beef>\n\t\n</test>"), writer)
      writer.toString mustEqual "<test xmlns=\"urn:my-urn:quux\">\n\n--<beef>\n----<calf>1</calf>\n----<calf>2</calf>\n--</beef>\n\n\t\n</test>\n"
    }

  }
}
