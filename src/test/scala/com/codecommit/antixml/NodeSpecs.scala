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
import org.specs2.matcher.DataTables
import org.specs2.ScalaCheck
import org.scalacheck._

class NodeSpecs extends Specification with DataTables with ScalaCheck with XMLGenerators {
  import Prop._
  
  lazy val numProcessors = Runtime.getRuntime.availableProcessors()
  implicit val params = set(workers -> numProcessors, maxSize -> 15)      // doesn't need to be so large
  
  val nameStartChar = """:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD"""
  val name = "[" + nameStartChar + "][" + nameStartChar + """\-\.0-9\u00B7\u0300-\u036F\u203F-\u2040]*"""r
  
  "elements" should {
    "serialize empty elements correctly" in {
      <br/>.convert.toString mustEqual "<br/>"
    }
    
    "escape reserved characters in attribute values" in {
      "character" || "elem.toString"         |>
      "\""        !! "<foo bar=\"&quot;\"/>" |
      "&"         !! "<foo bar=\"&amp;\"/>"  |
      "'"         !! "<foo bar=\"&apos;\"/>" |
      "<"         !! "<foo bar=\"&lt;\"/>"   |
      ">"         !! "<foo bar=\"&gt;\"/>"   | { (c, r) => Elem(None, "foo", Attributes("bar" -> c), Map(), Group()).toString mustEqual r }
    }
    
    "allow legal name identifiers" in {
      "identifier" |>
      "name"       |
      "foo"        |
      "bar"        |
      "baz"        |
      "br"         | { name =>
        Elem(None, name, Attributes(), Map(), Group()) must not(throwAn[IllegalArgumentException])
      }
    }
    
    "detect illegal name identifiers" in check { str: String =>
      name unapplySeq str match {
        case Some(_) => Elem(None, str, Attributes(), Map(), Group()) must not(throwAn[IllegalArgumentException])
        case None => Elem(None, str, Attributes(), Map(), Group()) must throwAn[IllegalArgumentException]
      }
    }
    
    "allow legal prefix identifiers" in {
      "identifier" |>
      "name"       |
      "foo"        |
      "bar"        |
      "baz"        |
      "br"         | { name =>
        Elem(Some(name), "foo", Attributes(), Map(), Group()) must not(throwAn[IllegalArgumentException])
      }
    }
    
    "detect illegal prefix identifiers" in check { str: String =>
      name unapplySeq str match {
        case Some(_) => Elem(Some(str), "foo", Attributes(), Map(), Group()) must not(throwAn[IllegalArgumentException])
        case None => Elem(Some(str), "foo", Attributes(), Map(), Group()) must throwAn[IllegalArgumentException]
      }
    }
    
    "allow legal attribute prefixes" in {
      "identifier" |>
      "name"       |
      "foo"        |
      "bar"        |
      "baz"        |
      "br"         | { name =>
        Elem(None, "foo", Attributes(QName(Some(name), "bar") -> "bar"), Map(), Group()) must not(throwAn[IllegalArgumentException])
      }
    }
    
    "detect illegal attribute prefixes" in check { str: String =>
      name unapplySeq str match {
        case Some(_) => Elem(None, "foo", Attributes(QName(Some(str), "bar") -> "bar"), Map(), Group()) must not(throwAn[IllegalArgumentException])
        case None => Elem(None, "foo", Attributes(QName(Some(str), "bar") -> "bar"), Map(), Group()) must throwAn[IllegalArgumentException]
      }
    }
    
    "allow legal attribute names" in {
      "identifier" |>
      "name"       |
      "foo"        |
      "bar"        |
      "baz"        |
      "br"         | { name =>
        Elem(None, "foo", Attributes(name -> "bar"), Map(), Group()) must not(throwAn[IllegalArgumentException])
      }
    }
    
    "detect illegal attribute names" in check { str: String =>
      name unapplySeq str match {
        case Some(_) => Elem(None, "foo", Attributes(str -> "bar"), Map(), Group()) must not(throwAn[IllegalArgumentException])
        case None => Elem(None, "foo", Attributes(str -> "bar"), Map(), Group()) must throwAn[IllegalArgumentException]
      }
    }

    "select against self" in {
      val bookstore = <bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book><book><title>I, Robot</title><author>Isaac Asimov</author></book><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.convert
      (bookstore \ "book") mustEqual bookstore.children
      (bookstore \ "book") mustEqual bookstore.children
      (bookstore \\ "title") mustEqual (bookstore.children \\ "title")
    }
    
    "select text within self" in {
      (<parent>Text</parent>.convert \\ text mkString) mustEqual "Text"
    }
    
    "delegate canonicalization to Group" in check { e: Elem =>
      e.canonicalize mustEqual e.copy(children=e.children.canonicalize)
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
    "Reject the ]]> string in the constructor" in {
      CDATA("la di ]]> da") must throwAn[IllegalArgumentException]
    }
  }
}
