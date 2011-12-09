package com.codecommit.antixml

import org.specs2.mutable._
import XML._
import scala.collection.immutable.HashMap

class PathTransformerSpecs extends SpecificationWithJUnit {
  
  val x0 = fromString("<root0><a0>foo</a0><b0>baz</b0><c0/></root0>")
  val x1 = fromString("<root1><a1>foo</a1><b1>baz</b1><c1/></root1>")
  val x2 = fromString("<root2><a2>foo</a2><b2>baz</b2><c2/></root2>")

  val group = Group(x0, x1, x2)
  
  val empty = ZipperPath()
  val p1 = ZipperPath(0, 1, 0)
  val p2 = ZipperPath(1, 1)
  val p3 = ZipperPath(2)
  val p4 = ZipperPath(0)
  
  val transformer = PathTransformer(group)
  
  "shifting upwards" should {
    "ignore empty paths" in {
      transformer.shiftUp(empty) mustEqual None
    }
    
    "properly shift paths" in {
      transformer.shiftUp(p1) mustEqual Some(ZipperPath(0, 1))
      transformer.shiftUp(p2) mustEqual Some(ZipperPath(1))
      transformer.shiftUp(p3) mustEqual Some(ZipperPath())
    }
  }
  
  "shifting leftwards" should {
    "fail on empty paths" in {
      transformer.shiftLeft(empty) must throwAn[AssertionError]
    }
    
    "properly shift paths" in {
      transformer.shiftLeft(p1) mustEqual None
      transformer.shiftLeft(p2) mustEqual Some(ZipperPath(1, 0))
      transformer.shiftLeft(p3) mustEqual Some(ZipperPath(1))
      transformer.shiftLeft(p4) mustEqual None
    }
  }
  
  "shifting rightwards" should {
	  "fail on empty paths" in {
		  transformer.shiftRight(empty) must throwAn[AssertionError]
	  }
	  
	  "properly shift paths" in {
		  transformer.shiftRight(p1) mustEqual None
		  transformer.shiftRight(p2) mustEqual Some(ZipperPath(1, 2))
		  transformer.shiftRight(p3) mustEqual None
		  transformer.shiftRight(p4) mustEqual Some(ZipperPath(1))
	  }
  }
  
  def elem(name: String, text: String) = Elem(None, name, Attributes(), Map(), Group(Text(text)))
}