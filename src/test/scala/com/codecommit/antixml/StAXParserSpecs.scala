package com.codecommit.antixml

import org.specs._

object StAXParserSpecs extends Specification {
  "StAXParser" should {
    "view should" >> {
      "generate ElemStarts" in {
        StAXParser.view("<a />").head must_== ElemStart("a", Map.empty)
      }
      "parse attributes in ElemStarts" in {
        StAXParser.view("<a attr='value' />").head must beLike {
          case elemStart: ElemStart => elemStart.attrs("attr") must_== "value"
        }
      }
      "parse namespace prefixes in ElemStarts" in {
        StAXParser.view("<a:a xmlns:a='a' />").head must beLike {
          case elemStart: ElemStart => elemStart.prefix == Some("a")
        }
      }
      "gerenate ElemEnds" in {
        StAXParser.view("<a />").tail.head must_== ElemEnd(None, "a", None)
      }
      "generate namespace URIs" in {
        StAXParser.view("<a:a xmlns:a='http://example.com' />").head must beLike {
          case elemStart: ElemStart => {
            elemStart.uri == Some("http://example.com")
          }
        }
      }
      "generate Characters" in {
        StAXParser.view("<a>a</a>").tail.head must beLike {
          case chars: Characters => chars.text == "a"
        }
      }
      "generate Comment" in {
        StAXParser.view("<!--comment--><a />").head must beLike {
          case comment: Comment => comment.text == "comment"
        }
      }
      "generate ProcessingInstruction" in {
        StAXParser.view("<?target data?><a />").head must beLike {
          case pi: ProcessingInstruction => pi.target == "target" && pi.data == "data"
        }
      }
      // XMLStreamReader does some costly DTD validation so skipping...
      // "generate DocumentTypeDefinition" in {
      //        val doctype = "<!DOCTYPE html " +
      //   "PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" " +
      //   "\"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">"
        // StAXParser.view(doctype + "<html />").head must beLike {
        //      case dtd: DocumentTypeDefinition => dtd.declaration == doctype
        // }
      //}
      "generate DocumentEnd" in {
        StAXParser.view("<a />").tail.tail.head must_== DocumentEnd
      }
    }
    "parse should generate NodeSeqs" in {
      StAXParser.parse(StAXParser.view("<a:a xmlns:a='a'>hi<b attr='value' /></a:a>")) mustEqual NodeSeq(Elem(Some("a"), "a", Map.empty, NodeSeq(Text("hi"), Elem(None, "b", Map("attr" -> "value"), NodeSeq()))))
    }
  }
}
