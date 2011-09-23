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

class SelectorSpecs extends Specification {
  
  "the * selector" should {
    "select nothing when parent is empty" in {
      XML.fromString("<parent/>") \ * mustEqual Group()
    }
    
    "select entire contents of parent" in {
      val xml = XML.fromString("<parent><child1/>Test<child2/>text here we go \n with whitespace<child3>Inside!</child3></parent>")
      val expected = xml.children
      xml \ * mustEqual expected
    }
  }
  
  "the element selector(s)" should {
    "select nothing when parent is empty" in {
      <parent/>.convert \ "parent" mustEqual Group()
      <parent/>.convert \ 'parent mustEqual Group()
    }
    
    "select only the named element(s)" in {
      <parent><foo/><bar/>Baz<foo/></parent>.convert \ "foo" mustEqual Group(<foo/>.convert, <foo/>.convert)
      <parent><foo/><bar/>Baz<foo/></parent>.convert \ 'foo mustEqual Group(<foo/>.convert, <foo/>.convert)
    }
  }
  
    
  
  "Optimizing selectors" should {
    val bookstore = new StAXParser().fromString {
      "<bookstore>" +
        "<book>" +
          "<title>For Whom the Bell Tolls</title>" +
          "<author>Hemmingway</author>" +
        "</book>" +
        "<book>" +
          "<title>I, Robot</title>" +
          "<author>Isaac Asimov</author>" +
        "</book>" +
        "<book>" +
          "<title>Programming Scala</title>" +
          "<author>Dean Wampler</author>" +
          "<author>Alex Payne</author>" +
        "</book>" +
        "<book>" +
          "<title>An anonymous transcript</title>" +
        "</book>" +
      "</bookstore>" 
    }
    
    "work using a non-trivial canMatchIn and mapping" in {
      val titlesOfBooksWithAuthors = new OptimizingSelector[String]() {
        private val pf: PartialFunction[Node, String] = {
          case e:Elem if (e \ 'author).nonEmpty => (e \ 'title \ text).mkString
        }

        override def isDefinedAt(e:Node) = pf.isDefinedAt(e)
        override def apply(sel: Node):String = pf(sel)
        override def canMatchIn(g: Group[Node]) = {
          g.matches("author") && g.matches("title")
        }
      }
      
      val result = bookstore \\ titlesOfBooksWithAuthors
      result mustEqual Vector("For Whom the Bell Tolls","I, Robot","Programming Scala")
      
    }
  }
  
}
