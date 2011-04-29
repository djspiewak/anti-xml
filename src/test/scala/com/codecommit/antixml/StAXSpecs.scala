package com.codecommit.antixml

import java.io.StringReader
import javax.xml.transform.stream.StreamSource
import org.specs._

object StAXSpecs extends Specification {
  import StAXEvents._
  
  object StAXParser extends StAXParser
  
  "StAXIterator" should {
    "generate ElemStarts" in {
      StAXIterator.fromString("<a />").next must_== ElemStart("a", Map.empty)
    }
    "parse attributes in ElemStarts" in {
      StAXIterator.fromString("<a attr='value' />").next must beLike {
        case elemStart: ElemStart => elemStart.attrs("attr") must_== "value"
      }
    }
    "parse namespace prefixes in ElemStarts" in {
      StAXIterator.fromString("<a:a xmlns:a='a' />").next must beLike {
        case elemStart: ElemStart => elemStart.prefix == Some("a")
      }
    }
    "gerenate ElemEnds" in {
      StAXIterator.fromString("<a />").drop(1).next must_== ElemEnd(None, "a", None)
    }
    "generate namespace URIs" in {
      StAXIterator.fromString("<a:a xmlns:a='http://example.com' />").next must beLike {
        case elemStart: ElemStart => {
          elemStart.uri == Some("http://example.com")
        }
      }
    }
    "generate Characters" in {
      StAXIterator.fromString("<a>a</a>").drop(1).next must beLike {
        case chars: Characters => chars.text == "a"
      }
    }
    "generate Comment" in {
      StAXIterator.fromString("<!--comment--><a />").next must beLike {
        case comment: Comment => comment.text == "comment"
      }
    }
    "generate ProcessingInstruction" in {
      StAXIterator.fromString("<?target data?><a />").next must beLike {
        case pi: ProcessingInstruction => pi.target == "target" && pi.data == "data"
      }
    }
    "generate DocumentEnd" in {
      StAXIterator.fromString("<a />").drop(2).next must_== DocumentEnd
    }
    // XMLStreamReader does some costly DTD validation so skipping...
    // "generate DocumentTypeDefinition" in {
    //        val doctype = "<!DOCTYPE html " +
    //   "PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" " +
    //   "\"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">"
    // StAXIterator.fromString(doctype + "<html />").next must beLike {
    //      case dtd: DocumentTypeDefinition => dtd.declaration == doctype
    // }
    //}
    }
  "StAXParser" should {
    "parse a StreamSource and generate an Elem" in {
      StAXParser.parse(new StreamSource(new StringReader("<a:a xmlns:a='a'>hi<b attr='value' /> there</a:a>"))) mustEqual Elem(Some("a"), "a", Map.empty, Group(Text("hi"), Elem(None, "b", Map("attr" -> "value"), Group()), Text(" there")))
    }
  }
}
