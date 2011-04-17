package com.codecommit.antixml

import org.specs._

object GroupSpecs extends Specification {
  import XML._
  
  "shallow selector" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = NodeSeq(elem("a"), elem("a"), elem("a"))
      ns \ "a" mustEqual result
    }
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \ text
      strs mustEqual Vector("Some text", "More text")
    }
  }
  
  "deep selector" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \\ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = NodeSeq(elem("a"), elem("a"), elem("a"))
      ns \\ "a" mustEqual result
    }
    
    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val result = fromString("<parent><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><target>outside</target><target>sub1</target><target>top3-outer</target><target>phoney</target><target>top1</target><target>top2</target></parent>")
      ns \\ "target" mustEqual result.head.children
    }
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \\ text
      strs mustEqual Vector("Some text", "More text", "top", "outside", "sub1", "top3-outer", "top1", "top2", "phoney")
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
