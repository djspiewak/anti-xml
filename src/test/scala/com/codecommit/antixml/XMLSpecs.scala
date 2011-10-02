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

import org.specs2.execute.Pending
import org.specs2.mutable._

class XMLSpecs extends Specification {
  import XML._
  
  "xml parsing" should {
    "parse an empty elem" in {
      fromString("<test/>") mustEqual elem("test")
    }
    
    "parse an elem with text" in {
      fromString("<test>This is a test</test>") mustEqual elem("test", Text("This is a test"))
    }
    
    "parse an elem with sub-elements" in {
      fromString("<test><sub1/><sub2/></test>") mustEqual elem("test", elem("sub1"), elem("sub2"))
    }
    
    "parse a deeply-nested structure" in {
      fromString("<test><sub1><subsub1><subsubsub1/><subsubsub2/></subsub1></sub1><sub2/><sub3><subsub1/></sub3></test>") mustEqual elem("test", elem("sub1", elem("subsub1", elem("subsubsub1"), elem("subsubsub2"))), elem("sub2"), elem("sub3", elem("subsub1")))
    }
    
    "parse mixed content" in {
      fromString("<test>This is a <inner-test/> of great glory!</test>") mustEqual elem("test", Text("This is a "), elem("inner-test"), Text(" of great glory!"))
    }
    
    "preserve whitespace" in {
      fromString("<test>\n  \n\t\n</test>") mustEqual elem("test", Text("\n  \n\t\n"))
    }

    "preserve prefixes" in {
      val ns = "urn:my-urn:quux";
      fromString("<my:test xmlns:my='urn:my-urn:quux'/>") mustEqual Elem(Some("my"), "test", Attributes(), Map("my" -> ns), Group[Node]())
    }

    "parse prefixes" in {
      fromString("<my:test xmlns:my='urn:my-urn:quux'></my:test>").name mustEqual "test"
    }
  }
  
  "fromSource" should {
    import scala.io.Source
    
    "match the semantics of fromString" in {
      val str = "<test><sub1><subsub1><subsubsub1/><subsubsub2/></subsub1></sub1><sub2/><sub3><subsub1/></sub3></test>"
      fromSource(Source fromString str) mustEqual fromString(str)
    }
    
    "load large files without difficulty" in {
      val is = getClass.getResourceAsStream("/discogs_20110201_labels.xml")
      fromSource(Source fromInputStream is) must not(throwA[StackOverflowError])
    }
  }
  
  def elem(name: QName, children: Node*) = Elem(name.prefix, name.name, Attributes(), Map(), Group(children: _*))
}
