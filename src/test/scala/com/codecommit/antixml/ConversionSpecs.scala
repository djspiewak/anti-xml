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

import scala.xml

class ConversionSpecs extends Specification with ScalaCheck {
  import Node.CharRegex
  import Prop._
  
  "scala.xml explicit conversions" should {
    "choose the most specific type" in {
      val e: xml.Elem = <test/>
      val t: xml.Atom[String] = xml.Text("text")
      val r: xml.EntityRef = <test>&hellip;</test>.child.head.asInstanceOf[xml.EntityRef]
      val n: xml.Node = e
      val ns: xml.NodeSeq = e
      
      val e2 = e.convert
      val t2 = t.convert
      val r2 = r.convert
      val n2 = n.convert
      val ns2 = ns.convert
      
      validate[Elem](e2)
      validate[Text](t2)
      validate[EntityRef](r2)
      validate[Node](n2)
      validate[Group[Node]](ns2)
    }

    "convert text nodes" in check { str: String =>
      if (!CharRegex.unapplySeq(str).isEmpty) {
        val node = xml.Text(str)
        node.convert mustEqual Text(str)
      } else {
        Text(str) must throwAn[IllegalArgumentException]
      }
    }
    
    "convert entity references" in check { str: String =>
      if (!CharRegex.unapplySeq(str).isEmpty) {
        val ref = xml.EntityRef(str)
        ref.convert mustEqual EntityRef(str)
        (ref: xml.Node).convert mustEqual EntityRef(str)
      } else {
        EntityRef(str) must throwAn[IllegalArgumentException]
      }
    }
    
    "not convert groups" in {
      val g = xml.Group(List(<foo/>, <bar/>))
      g.convert must throwA[RuntimeException]
    }
    
    "convert elem names without namespaces" in {
      val e = <test/>.convert
      e.prefix mustEqual None
      e.name mustEqual "test"
    }
    
    "convert elem names with namespaces" in {
      val e = <w:test/>.convert
      e.prefix mustEqual Some("w")
      e.name mustEqual "test"
    }
    
    "convert elem attributes" in {
      (<test/>).convert.attrs mustEqual Map()
      (<test a:c="1" b="foo" xmlns:a="a"/>).convert.attrs mustEqual Attributes(QName(Some("a"), "c") -> "1", "b" -> "foo")
    }
    
    "convert elem children" in {
      val e = <test>Text1<child/>Text2</test>.convert
      e.children must have size(3)
      e.children(0) mustEqual Text("Text1")
      e.children(1) mustEqual Elem(None, "child", Attributes(), Map(), Group())
      e.children(2) mustEqual Text("Text2")
    }
    
    "convert NodeSeq" in {
      xml.NodeSeq.fromSeq(Nil).convert mustEqual Group()
      
      val result = xml.NodeSeq.fromSeq(List(<test1/>, <test2/>, xml.Text("text"))).convert
      val expected = Group(Elem(None, "test1", Attributes(), Map(), Group()),
        Elem(None, "test2", Attributes(), Map(), Group()),
        Text("text"))
        
      result mustEqual expected
    }
  }
  
  def validate[Expected] = new {
    def apply[A](a: A)(implicit evidence: A =:= Expected) = evidence must not beNull
  }
}
