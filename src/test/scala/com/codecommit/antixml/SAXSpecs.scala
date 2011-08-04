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

class SAXSpecs extends Specification {
  object SAXParser extends SAXParser
  
  "SAXParser" should {
    "parse a simpleString and generate a single Elem" in {
      SAXParser.fromString("<a/>") mustEqual Elem(None, "a", Attributes(), Map(), Group())
    }
    
    "parse a simpleString and generate a single Elem even with namespaces" in {
      SAXParser.fromString("<pf:a xmlns:pf='urn:a'/>") mustEqual Elem(Some("pf"), "a", Attributes(), Map("pf" -> "urn:a"), Group())
    }

    "parse a simpleString with an non-prefixed namespace" in {
      SAXParser.fromString("<a xmlns='urn:a'/>") mustEqual Elem(None, "a", Attributes(), Map("" -> "urn:a"), Group())
    }

    "parse a String and generate an Elem" in {
      SAXParser.fromString("<p:a xmlns:p='ns'>hi<b attr='value' /> there</p:a>") mustEqual Elem(Some("p"), "a", Attributes(), Map("p"->"ns"), Group(Text("hi"), Elem(None, "b", Attributes("attr" -> "value"), Map("p"->"ns"), Group()), Text(" there")))
    }
    
    "parse a simpleString with both a namespace and an attribute" in {
      SAXParser.fromString("<a xmlns='urn:a' key='val' />") mustEqual Elem(None, "a", Attributes("key"->"val"), Map("" -> "urn:a"), Group())
    }

  }
}
