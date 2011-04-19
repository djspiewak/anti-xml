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

import org.specs._

object NodeSpecs extends Specification {
  detailedDiffs()
  
  "elements" should {
    "serialize empty elements correctly" in {
      (<br/>).anti.toString mustEqual "<br/>"
    }
    
    "escape reserved characters in the name" in {
      Elem(None, "\"", Map(), Group()).toString mustEqual "<&quot;/>"
      Elem(None, "&", Map(), Group()).toString mustEqual "<&amp;/>"
      Elem(None, "'", Map(), Group()).toString mustEqual "<&apos;/>"
      Elem(None, "<", Map(), Group()).toString mustEqual "<&lt;/>"
      Elem(None, ">", Map(), Group()).toString mustEqual "<&gt;/>"
    }
    
    "escape reserved characters in the namespace" in {
      Elem(Some("\""), "foo", Map(), Group()).toString mustEqual "<&quot;:foo/>"
      Elem(Some("&"), "foo", Map(), Group()).toString mustEqual "<&amp;:foo/>"
      Elem(Some("'"), "foo", Map(), Group()).toString mustEqual "<&apos;:foo/>"
      Elem(Some("<"), "foo", Map(), Group()).toString mustEqual "<&lt;:foo/>"
      Elem(Some(">"), "foo", Map(), Group()).toString mustEqual "<&gt;:foo/>"
    }
    
    "escape reserved characters in attribute keys" in {
      Elem(None, "foo", Map("\"" -> "bar"), Group()).toString mustEqual "<foo &quot;=\"bar\"/>"
      Elem(None, "foo", Map("&" -> "bar"), Group()).toString mustEqual "<foo &amp;=\"bar\"/>"
      Elem(None, "foo", Map("'" -> "bar"), Group()).toString mustEqual "<foo &apos;=\"bar\"/>"
      Elem(None, "foo", Map("<" -> "bar"), Group()).toString mustEqual "<foo &lt;=\"bar\"/>"
      Elem(None, "foo", Map(">" -> "bar"), Group()).toString mustEqual "<foo &gt;=\"bar\"/>"
    }
    
    "escape reserved characters in attribute values" in {
      Elem(None, "foo", Map("bar" -> "\""), Group()).toString mustEqual "<foo bar=\"&quot;\"/>"
      Elem(None, "foo", Map("bar" -> "&"), Group()).toString mustEqual "<foo bar=\"&amp;\"/>"
      Elem(None, "foo", Map("bar" -> "'"), Group()).toString mustEqual "<foo bar=\"&apos;\"/>"
      Elem(None, "foo", Map("bar" -> "<"), Group()).toString mustEqual "<foo bar=\"&lt;\"/>"
      Elem(None, "foo", Map("bar" -> ">"), Group()).toString mustEqual "<foo bar=\"&gt;\"/>"
    }
    
    "select against self" in {
      val bookstore = <bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book><book><title>I, Robot</title><author>Isaac Asimov</author></book><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.anti
      (bookstore \ "book") mustEqual bookstore.children
      (bookstore \\ "title") mustEqual (bookstore.children \\ "title")
    }
    
    "select text within self" in {
      ((<parent>Text</parent>).anti \\ text mkString) mustEqual "Text"
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
