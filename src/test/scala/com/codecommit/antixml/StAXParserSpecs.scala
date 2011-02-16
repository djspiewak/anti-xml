package com.codecommit.antixml

import org.specs._

object StAXParserSpecs extends Specification {
  "StAXParser" should {
    "fromString should" >> {
      "generate ElemStarts" in {
	StAXParser.fromString("<a />").head must_== ElemStart(Map.empty)
      }
      "parse attributes in ElemStarts" in {
	StAXParser.fromString("<a attr='value' />").head must beLike {
	  case elemStart: ElemStart => elemStart.attrs("attr") must_== "value"
	}
      }
      // "parse namespace prefixes in ElemStarts" in {
      // 	StAXParser.fromString("<a:a xmlns:a='a' />").head must beLike {
      // 	  case elemStart: ElemStart => elemStart.prefix == "a"
      // 	}
      // }
      "gerenate ElemEnds" in {
	StAXParser.fromString("<a />").tail.head must_== ElemEnd
      }
      "generate Namespace" in {
	StAXParser.fromString("<a xmlns:a='http://example.com' />").head must beLike {
	  case elemStart: ElemStart =>
	    elemStart.namespaces("a") == "http://example.com"
	}
      }
      "generate Characters" in {
	StAXParser.fromString("<a>a</a>").tail.head must beLike {
	  case chars: Characters => chars.text == "a"
	}
      }
      "generate Comment" in {
	StAXParser.fromString("<!--comment--><a />").head must beLike {
	  case comment: Comment => comment.text == "comment"
	}
      }
      "generate ProcessingInstruction" in {
	StAXParser.fromString("<?target data?><a />").head must beLike {
	  case pi: ProcessingInstruction => pi.target == "target" && pi.data == "data"
	}
      }
      "generate DocumentTypeDefinition" in {
	val doctype = "<!DOCTYPE html " +
        "PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" " +
        "\"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">"

	// XMLStreamReader does some costly DTD validation so skipping...
	// StAXParser.fromString(doctype + "<html />").head must beLike {
	// 	case dtd: DocumentTypeDefinition => dtd.declaration == doctype
	// }
      }
      "generate DocumentEnd" in {
	StAXParser.fromString("<a />").tail.tail.head must_== DocumentEnd
      }
    }
    // "parse should generate NodeSeqs" in {
    // 	StAXParser.parse(StAXParser.fromString("<a:a xmlns:a='a'>hi<b attr='value' /></a:a>")) mustEqual NodeSeq(Elem(Some("a"), "a", Map.empty, NodeSeq(Text("hi"), Elem(None, "b", Map("attr" -> "value"), NodeSeq()))))
    // }
  }
}
