package com.codecommit.antixml

import org.specs._

object SelectorSpecs extends Specification {
  "the * selector" should {
    "select nothing when parent is empty" in {
      XML.fromString("<parent/>") \ * mustEqual Group()
    }
    
    "select entire contents of parent" in {
      val xml = XML.fromString("<parent><child1/>Test<child2/>text here we go \n with whitespace<child3>Inside!</child3></parent>")
      val expected = xml.children
      xml \ * mustEqual expected
    }
  }
  
  "the element selector(s)" should {
    "select nothing when parent is empty" in {
      (<parent/>).anti \ "parent" mustEqual Group()
      (<parent/>).anti \ 'parent mustEqual Group()
    }
    
    "select only the named element(s)" in {
      (<parent><foo/><bar/>Baz<foo/></parent>).anti \ "foo" mustEqual Group(<foo/>.anti, <foo/>.anti)
      (<parent><foo/><bar/>Baz<foo/></parent>).anti \ 'foo mustEqual Group(<foo/>.anti, <foo/>.anti)
    }
  }
}
