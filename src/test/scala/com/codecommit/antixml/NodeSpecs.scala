package com.codecommit.antixml

import org.specs._

object NodeSpecs extends Specification {
  "elements" should {
    "serialize empty elements correctly" in {
      (<br/>).anti.toString mustEqual "<br/>"
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
