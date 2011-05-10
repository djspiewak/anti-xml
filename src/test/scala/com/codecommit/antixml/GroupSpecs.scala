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
import org.specs2.ScalaCheck
import org.scalacheck._

class GroupSpecs extends Specification with ScalaCheck with XMLGenerators with UtilGenerators {
  import Prop._
  import XML._
  
  lazy val numProcessors = Runtime.getRuntime.availableProcessors()
  implicit val params = set(workers -> numProcessors, maxSize -> 15)      // doesn't need to be so large

  "shallow selection on Group" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual Group(elem("parent"))
    }
    
    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual Group(elem("parent"))
      ns \ "parent" mustEqual Group(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \ "a" mustEqual result
    }
    
    // currently takes a lot of time and heap, but doesn't produce any valuable results
    /* "be fully specified by flatMap / collect" in check { (ns: Group[Node], selector: Selector[Node]) =>
      val result = ns \ selector
      val expected = ns flatMap {
        case Elem(_, _, _, children) => children collect selector
        case _ => Group()
      }
      result.toList mustEqual expected.toList
    } */
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \ text
      strs mustEqual Vector("Some text", "More text")
    }
  }
  
  "deep selection on Group" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \\ "parent" mustEqual Group(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \\ "a" mustEqual result
    }
    
    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val result = fromString("<parent><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><target>outside</target><target>sub1</target><target>top3-outer</target><target>phoney</target><target>top1</target><target>top2</target></parent>")
      ns \\ "target" mustEqual result.children
    }
    
    // currently takes a lot of time and heap, but doesn't produce any valuable results
    /* "be fully specified by recursive flatMap / collect" in check { (ns: Group[Node], selector: Selector[Node]) =>
      def loop(ns: Group[Node]): Group[Node] = {
        val recursive = ns flatMap {
          case Elem(_, _, _, children) => loop(children)
          case _ => Group()
        }
        
        val shallow = ns flatMap {
          case Elem(_, _, _, children) => children collect selector
          case _ => Group()
        }
        
        shallow ++ recursive
      }
      val result = ns \\ selector
      val expected = loop(ns)
      
      result.toList mustEqual expected.toList
    } */
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \\ text
      strs mustEqual Vector("Some text", "More text", "top", "outside", "sub1", "top3-outer", "top1", "top2", "phoney")
    }
  }
  
  "utility methods on Group" >> {
    implicit val arbInt = Arbitrary(Gen.choose(0, 10))

    "map should produce a Group (not a Zipper)" in {
      val group = <parent>child</parent>.anti.children
      validate[Group[Node]](group)
      validate[Group[Node]](group map identity)
    }
    
    "identity collect should return self" in check { (xml: Group[Node], n: Int) =>
      val func = (0 until n).foldLeft(identity: Group[Node] => Group[Node]) { (g, _) =>
        g andThen { _ collect { case e => e } }
      }
      
      func(xml) mustEqual xml
    }
  }
  
  "canonicalization" should {
    "merge two adjacent text nodes" in check { (left: String, right: String) =>
      Group(Text(left), Text(right)).canonicalize mustEqual Group(Text(left + right))
      Group(CDATA(left), CDATA(right)).canonicalize mustEqual Group(CDATA(left + right))
    }
    
    "merge two adjacent text nodes at end of Group" in check { (left: String, right: String) =>
      Group(elem("foo"), elem("bar", Text("test")), Text(left), Text(right)).canonicalize mustEqual Group(elem("foo"), elem("bar", Text("test")), Text(left + right))
      Group(elem("foo"), elem("bar", Text("test")), CDATA(left), CDATA(right)).canonicalize mustEqual Group(elem("foo"), elem("bar", Text("test")), CDATA(left + right))
    }
    
    "merge two adjacent text nodes at beginning of Group" in check { (left: String, right: String) =>
      Group(Text(left), Text(right), elem("foo"), elem("bar", Text("test"))).canonicalize mustEqual Group(Text(left + right), elem("foo"), elem("bar", Text("test")))
      Group(CDATA(left), CDATA(right), elem("foo"), elem("bar", Text("test"))).canonicalize mustEqual Group(CDATA(left + right), elem("foo"), elem("bar", Text("test")))
    }
    
    "merge two adjacent text nodes at depth" in check { (left: String, right: String) =>
      Group(elem("foo", elem("bar", Text(left), Text(right)))).canonicalize mustEqual Group(elem("foo", elem("bar", Text(left + right))))
      Group(elem("foo", elem("bar", CDATA(left), CDATA(right)))).canonicalize mustEqual Group(elem("foo", elem("bar", CDATA(left + right))))
    }
    
    "not merge adjacent text and cdata nodes" in check { (left: String, right: String) =>
      Group(CDATA(left), Text(right)).canonicalize mustEqual Group(CDATA(left), Text(right))
      Group(Text(left), CDATA(right)).canonicalize mustEqual Group(Text(left), CDATA(right))
    }
    
    "always preserve serialized equality" in check { g: Group[Node] =>
      g.canonicalize.toString mustEqual g.toString
    }
  }
  
  def validate[Expected] = new {
    def apply[A](a: A)(implicit evidence: A =:= Expected) = evidence must not beNull
  }

  def elem(name: String, children: Node*) = Elem(None, name, Attributes(), Map(), Group(children: _*))

  def elem(qname : QName, children: Node*) = Elem(qname.prefix, qname.name, Attributes(), Map(), Group(children: _*))
}
