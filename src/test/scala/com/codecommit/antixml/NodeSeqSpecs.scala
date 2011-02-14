package com.codecommit.antixml

import org.specs._

object NodeSeqSpecs extends Specification {
  import XML._
  
  "shallow xpath" should {
    "find at the root" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual ns
    }
    
    // TODO resolve conflict between these tests!
    
    "traverse into results" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" \ "parent" mustEqual fromString("<parent/>")
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val child = ns.head.asInstanceOf[Elem].children
      val result = fromString("<parent><a/><a/><a/></parent>").head.asInstanceOf[Elem].children
      child \ "a" mustEqual result
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
