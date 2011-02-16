package com.codecommit.antixml

import org.specs._

object ValidationSpecs extends Specification {
  import XML._
  "A default (validating) XML parser" should {
    "complain about a wrong system identifier" in {
      fromString("""<!DOCTYPE test SYSTEM "urn:nothing:to:see:here"><test/>""") must throwA[java.net.MalformedURLException]
    }
    "complain about namespace welformedness issues" in {
      fromString("""<test:test/>""") must throwA[org.xml.sax.SAXParseException]
    }
  }
  "A non-validating XML parser" should {
    "not complain about a wrong system identifier" in {
      fromString("""<!DOCTYPE test SYSTEM "urn:nothing:to:see:here"><test/>""", validate=false) mustEqual NodeSeq(elem("test"))
    }
    "complain about namespace welformedness issues" in {
      fromString("""<test:test/>""", validate=false) must throwA[org.xml.sax.SAXParseException]
    }
  }
  "A non-validating, non-namesapce-aware XML parser" should {
    "not complain about a wrong system identifier" in {
      fromString("""<!DOCTYPE test SYSTEM "urn:nothing:to:see:here"><test/>""", validate=false, namespaces=false) mustEqual NodeSeq(elem("test"))
    }
    "not complain about namespace welformedness issues" in {
      fromString("""<test:test/>""", validate=false, namespaces=false) mustEqual NodeSeq(elem("test:test"))
    }
  }
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
