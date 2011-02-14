package com.codecommit.antixml

import org.specs._

object NodeSeqSpecs extends Specification {
  import XML._
  
  "shallow selectors" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns > "parent" mustEqual ns.head.asInstanceOf[Elem].children
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = NodeSeq(elem("a"), elem("a"), elem("a"))
      ns > "a" mustEqual result
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
