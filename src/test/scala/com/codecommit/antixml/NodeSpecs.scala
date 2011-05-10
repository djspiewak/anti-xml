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

import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.matcher.DataTables

@RunWith(classOf[JUnitRunner])
class NodeSpecs extends Specification with DataTables {
    
  "elements" should {
    "serialize empty elements correctly" in {
      <br/>.anti.toString mustEqual "<br/>"
    }
    
    "escape reserved characters in attribute values" in {
      "character" || "elem.toString"         |>
      "\""        !! "<foo bar=\"&quot;\"/>" |
      "&"         !! "<foo bar=\"&amp;\"/>"  |
      "'"         !! "<foo bar=\"&apos;\"/>" |
      "<"         !! "<foo bar=\"&lt;\"/>"   |
      ">"         !! "<foo bar=\"&gt;\"/>"   | { (c, r) => Elem("foo", Attributes("bar" -> c), Map(), Group()).toString mustEqual r }
    }

    "select against self" in {
      val bookstore = <bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book><book><title>I, Robot</title><author>Isaac Asimov</author></book><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.anti
      (bookstore \ "book") mustEqual bookstore.children
      (bookstore \ "book") mustEqual bookstore.children
      (bookstore \\ "title") mustEqual (bookstore.children \\ "title")
    }
    
    "select text within self" in {
      (<parent>Text</parent>.anti \\ text mkString) mustEqual "Text"
    }
  }
  
  "text nodes" should {
    "escape reserved characters when serialized" in {
      Text("Lorem \" ipsum & dolor ' sit < amet > blargh").toString mustEqual "Lorem &quot; ipsum &amp; dolor &apos; sit &lt; amet &gt; blargh"
    }
  }
  
  "cdata nodes" should {
    "not escape reserved characters when serialized" in {
      CDATA("Lorem \" ipsum & dolor ' sit < amet > blargh").toString mustEqual "<![CDATA[Lorem \" ipsum & dolor ' sit < amet > blargh]]>"
    }
  }
}
