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

import java.io.StringReader
import javax.xml.transform.stream.StreamSource
import org.specs2.mutable._

class StAXSpecs extends Specification {
  import StAXEvents._
  
  object StAXParser extends StAXParser
  
  "StAXIterator" should {
    "generate ElemStarts" in {
      StAXIterator.fromString("<a />").next mustEqual ElemStart("a", Map.empty)
    }
    "parse attributes in ElemStarts" in {
      StAXIterator.fromString("<a attr='value' />").next must beLike {
        case elemStart: ElemStart => elemStart.attrs("attr") mustEqual "value"
      }
    }
    "parse namespace prefixes in ElemStarts" in {
      StAXIterator.fromString("<a:a xmlns:a='a' />").next must beLike {
        case elemStart: ElemStart => elemStart.prefix mustEqual Some("a")
      }
    }
    "gerenate ElemEnds" in {
      StAXIterator.fromString("<a />").drop(1).next mustEqual ElemEnd(None, "a", None)
    }
    "generate namespace URIs" in {
      StAXIterator.fromString("<a:a xmlns:a='http://example.com' />").next must beLike {
        case elemStart: ElemStart => {
          elemStart.uri mustEqual Some("http://example.com")
        }
      }
    }
    "generate Characters" in {
      StAXIterator.fromString("<a>a</a>").drop(1).next must beLike {
        case chars: Characters => chars.text mustEqual "a"
      }
    }
    "generate Comment" in {
      StAXIterator.fromString("<!--comment--><a />").next must beLike {
        case comment: Comment => comment.text mustEqual "comment"
      }
    }
    "generate ProcessingInstruction" in {
      StAXIterator.fromString("<?target data?><a />").next must beLike {
        case pi: ProcessingInstruction => (pi.target, pi.data) mustEqual ("target", "data")
      }
    }
    "generate DocumentEnd" in {
      StAXIterator.fromString("<a />").drop(2).next mustEqual DocumentEnd
    }
    // XMLStreamReader does some costly DTD validation so skipping...
    // "generate DocumentTypeDefinition" in {
    //        val doctype = "<!DOCTYPE html " +
    //   "PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" " +
    //   "\"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">"
    // StAXIterator.fromString(doctype + "<html />").next must beLike {
    //      case dtd: DocumentTypeDefinition => dtd.declaration == doctype
    // }
    //}
    }
  "StAXParser" should {
    "parse a StreamSource and generate an Elem" in {
      StAXParser.parse(new StreamSource(new StringReader("<a:a xmlns:a='a'>hi<b attr='value' /> there</a:a>"))) mustEqual Elem(Some("a"), "a", Map.empty, Group(Text("hi"), Elem(None, "b", Map("attr" -> "value"), Group()), Text(" there")))
    }
  }
}
