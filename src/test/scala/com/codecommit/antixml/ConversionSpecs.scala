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
      val n: xml.Node = e
      val ns: xml.NodeSeq = e
      
      val e2 = e.anti
      val t2 = t.anti
      val n2 = n.anti
      val ns2 = ns.anti
      
      validate[Elem](e2)
      validate[Text](t2)
      validate[Node](n2)
      validate[Group[Node]](ns2)
    }
    
    "convert text nodes" verifies { str: String =>
      val node = xml.Text(str)
      node.anti mustEqual Text(str)
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
