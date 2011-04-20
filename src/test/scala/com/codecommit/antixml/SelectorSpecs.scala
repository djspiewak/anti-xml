package com.codecommit.antixml

import org.specs._

object SelectorSpecs extends Specification {
  "the * selector should" in {
    "select nothing when parent is empty" in {
      XML.fromString("<parent/>") \ * mustEqual Group()
    }
    
    "select entire contents of parent" in {
      val xml = XML.fromString("<parent><child1/>Test<child2/>text here we go \n with whitespace<child3>Inside!</child3></parent>")
      val expected = xml.head.children
      xml \ * mustEqual expected
    }
  }
}
