package com.codecommit.antixml

import org.specs._

object XMLSpecs extends Specification {
  import XML._
  
  "xml parsing" should {
    "parse an empty elem" in {
      fromString("<test/>") mustEqual NodeSeq(elem("test"))
    }
    
    "parse an elem with text" in {
      fromString("<test>This is a test</test>") mustEqual NodeSeq(elem("test", Text("This is a test")))
    }
    
    "parse an elem with sub-elements" in {
      fromString("<test><sub1/><sub2/></test>") mustEqual NodeSeq(elem("test", elem("sub1"), elem("sub2")))
    }
    
    "parse a deeply-nested structure" in {
      fromString("<test><sub1><subsub1><subsubsub1/><subsubsub2/></subsub1></sub1><sub2/><sub3><subsub1/></sub3></test>") mustEqual NodeSeq(elem("test", elem("sub1", elem("subsub1", elem("subsubsub1"), elem("subsubsub2"))), elem("sub2"), elem("sub3", elem("subsub1"))))
    }
    
    "parse mixed content" in {
      fromString("<test>This is a <inner-test/> of great glory!</test>") mustEqual NodeSeq(elem("test", Text("This is a "), elem("inner-test"), Text(" of great glory!")))
    }
    
    "preserve whitespace" in {
      fromString("<test>\n  \n\t\n</test>") mustEqual NodeSeq(elem("test", Text("\n  \n\t\n")))
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
