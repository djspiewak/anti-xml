package com.codecommit.antixml

import org.specs._

object XMLSpecs extends Specification {
  import XML._
  
  "xml parsing" should {
    "parse an empty elem" in {
      fromString("<test/>") mustEqual elem("test")
    }
    
    "parse an elem with text" in {
      fromString("<test>This is a test</test>") mustEqual elem("test", Text("This is a test"))
    }
    
    "parse an elem with sub-elements" in {
      fromString("<test><sub1/><sub2/></test>") mustEqual elem("test", elem("sub1"), elem("sub2"))
    }
    
    "parse a deeply-nested structure" in {
      fromString("<test><sub1><subsub1><subsubsub1/><subsubsub2/></subsub1></sub1><sub2/><sub3><subsub1/></sub3></test>") mustEqual elem("test", elem("sub1", elem("subsub1", elem("subsubsub1"), elem("subsubsub2"))), elem("sub2"), elem("sub3", elem("subsub1")))
    }
    
    "parse mixed content" in {
      fromString("<test>This is a <inner-test/> of great glory!</test>") mustEqual elem("test", Text("This is a "), elem("inner-test"), Text(" of great glory!"))
    }
    
    "preserve whitespace" in {
      fromString("<test>\n  \n\t\n</test>") mustEqual elem("test", Text("\n  \n\t\n"))
    }
  }
  
  "fromSource" should {
    import scala.io.Source
    
    "match the semantics of fromString" in {
      val str = "<test><sub1><subsub1><subsubsub1/><subsubsub2/></subsub1></sub1><sub2/><sub3><subsub1/></sub3></test>"
      fromSource(Source fromString str) mustEqual fromString(str)
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), Group(children: _*))
}
