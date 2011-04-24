package com.codecommit.antixml

import org.specs._

object NodeSpecs extends Specification {
  detailedDiffs()
  
  "elements" should {
    "serialize empty elements correctly" in {
      (<br/>).anti.toString mustEqual "<br/>"
    }
    
    "escape reserved characters in the name" in {
      Elem(None, "\"", Map(), Group()).toString mustEqual "<&quot;/>"
      Elem(None, "&", Map(), Group()).toString mustEqual "<&amp;/>"
      Elem(None, "'", Map(), Group()).toString mustEqual "<&apos;/>"
      Elem(None, "<", Map(), Group()).toString mustEqual "<&lt;/>"
      Elem(None, ">", Map(), Group()).toString mustEqual "<&gt;/>"
    }
    
    "escape reserved characters in the namespace" in {
      Elem(Some("\""), "foo", Map(), Group()).toString mustEqual "<&quot;:foo/>"
      Elem(Some("&"), "foo", Map(), Group()).toString mustEqual "<&amp;:foo/>"
      Elem(Some("'"), "foo", Map(), Group()).toString mustEqual "<&apos;:foo/>"
      Elem(Some("<"), "foo", Map(), Group()).toString mustEqual "<&lt;:foo/>"
      Elem(Some(">"), "foo", Map(), Group()).toString mustEqual "<&gt;:foo/>"
    }
    
    "escape reserved characters in attribute keys" in {
      Elem(None, "foo", Map("\"" -> "bar"), Group()).toString mustEqual "<foo &quot;=\"bar\"/>"
      Elem(None, "foo", Map("&" -> "bar"), Group()).toString mustEqual "<foo &amp;=\"bar\"/>"
      Elem(None, "foo", Map("'" -> "bar"), Group()).toString mustEqual "<foo &apos;=\"bar\"/>"
      Elem(None, "foo", Map("<" -> "bar"), Group()).toString mustEqual "<foo &lt;=\"bar\"/>"
      Elem(None, "foo", Map(">" -> "bar"), Group()).toString mustEqual "<foo &gt;=\"bar\"/>"
    }
    
    "escape reserved characters in attribute values" in {
      Elem(None, "foo", Map("bar" -> "\""), Group()).toString mustEqual "<foo bar=\"&quot;\"/>"
      Elem(None, "foo", Map("bar" -> "&"), Group()).toString mustEqual "<foo bar=\"&amp;\"/>"
      Elem(None, "foo", Map("bar" -> "'"), Group()).toString mustEqual "<foo bar=\"&apos;\"/>"
      Elem(None, "foo", Map("bar" -> "<"), Group()).toString mustEqual "<foo bar=\"&lt;\"/>"
      Elem(None, "foo", Map("bar" -> ">"), Group()).toString mustEqual "<foo bar=\"&gt;\"/>"
    }
  }
  
  "text nodes" should {
    "escape reserved characters when serialized" in {
      Text("Lorem \" ipsum & dolor ' sit < amet > blargh").toString mustEqual "Lorem &quot; ipsum &amp; dolor &apos; sit &lt; amet &gt; blargh"
    }
  }
  
  "cdata nodes" should {
    "not escape reserved characters when serialized" in {
      CDATA("Lorem \" ipsum & dolor ' sit < amet > blargh").toString mustEqual "<![CDATA[Lorem \" ipsum & dolor ' sit < amet > blargh]]>"
    }
  }
}
