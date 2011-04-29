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
import org.scalacheck._

import scala.xml

object ConversionSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "scala.xml explicit conversions" should {
    "choose the most specific type" in {
      val e: xml.Elem = <test/>
      val t: xml.Atom[String] = xml.Text("text")
      val r: xml.EntityRef = <test>&hellip;</test>.child.head.asInstanceOf[xml.EntityRef]
      val n: xml.Node = e
      val ns: xml.NodeSeq = e
      
      val e2 = e.anti
      val t2 = t.anti
      val r2 = r.anti
      val n2 = n.anti
      val ns2 = ns.anti
      
      validate[Elem](e2)
      validate[Text](t2)
      validate[EntityRef](r2)
      validate[Node](n2)
      validate[Group[Node]](ns2)
    }
    
    "convert text nodes" verifies { str: String =>
      val node = xml.Text(str)
      node.anti mustEqual Text(str)
    }
    
    "convert entity references" verifies { str: String =>
      val ref = xml.EntityRef(str)
      ref.anti mustEqual EntityRef(str)
      (ref: xml.Node).anti mustEqual EntityRef(str)
    }
    
    "not convert groups" in {
      val g = xml.Group(List(<foo/>, <bar/>))
      g.anti must throwA[RuntimeException]
    }
    
    "convert elem names without namespaces" in {
      val e = <test/>.anti
      e.ns mustEqual None
      e.name mustEqual "test"
    }
    
    "convert elem names with namespaces" in {
      val e = <w:test/>.anti
      e.ns mustEqual Some("w")
      e.name mustEqual "test"
    }
    
    "convert elem attributes" in {
      (<test/>).anti.attrs mustEqual Map()
      (<test a="1" b="foo"/>).anti.attrs mustEqual Map("a" -> "1", "b" -> "foo")
    }
    
    "convert elem children" in {
      val e = <test>Text1<child/>Text2</test>.anti
      e.children must haveSize(3)
      e.children(0) mustEqual Text("Text1")
      e.children(1) mustEqual Elem(None, "child", Map(), Group())
      e.children(2) mustEqual Text("Text2")
    }
    
    "convert NodeSeq" in {
      xml.NodeSeq.fromSeq(Nil).anti mustEqual Group()
      
      val result = xml.NodeSeq.fromSeq(List(<test1/>, <test2/>, xml.Text("text"))).anti
      val expected = Group(Elem(None, "test1", Map(), Group()),
        Elem(None, "test2", Map(), Group()),
        Text("text"))
        
      result mustEqual expected
    }
  }
  
  def validate[Expected] = new {
    def apply[A](a: A)(implicit evidence: A =:= Expected) = evidence mustNotBe null
  }
}
