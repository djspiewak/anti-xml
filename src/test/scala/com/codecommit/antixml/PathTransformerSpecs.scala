package com.codecommit.antixml

import org.specs2.mutable._
import org.specs2.matcher.DataTables
import XML._

class PathTransformerSpecs extends SpecificationWithJUnit with DataTables {
  
  val x0 = fromString("<root0><a0>foo</a0><b0>baz</b0><c0/></root0>")
  val x1 = fromString("<root1><a1>foo</a1><b1>baz</b1><c1/></root1>")
  val x2 = fromString("<root2><a2>foo</a2><b2>baz</b2><c2/></root2>")
  
  val group = Group(x0, x1, x2)
  
  val empty = ZipperPath.empty
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
      "path" | "result" |
      p1     ! Some(ZipperPath(0, 1)) |
      p2     ! Some(ZipperPath(1)) |
      p3     ! Some(ZipperPath()) |> {
       (path, res) => transformer.shiftUp(path) mustEqual res
      }
    }
  }
  
  "shifting leftwards" should {
    "fail on empty paths" in {
      transformer.shiftLeft(empty) must throwAn[AssertionError]
    }
    
    "properly shift paths" in {
      "path" | "result" |
        p1   ! None.asInstanceOf[Option[ZipperPath]] |
        p2   ! Some(ZipperPath(1, 0)) |
        p3   ! Some(ZipperPath(1)) |
        p4   ! None |> {
          (path, res) => transformer.shiftLeft(path) mustEqual res
        }
    }
  }
  
  "shifting rightwards" should {
	  "fail on empty paths" in {
		  transformer.shiftRight(empty) must throwAn[AssertionError]
	  }

    "properly shift paths" in {
      "path" | "result" |
        p1   ! None.asInstanceOf[Option[ZipperPath]] |
        p2   ! Some(ZipperPath(1, 2)) |
        p3   ! None |
        p4   ! Some(ZipperPath(1)) |> {
          (path, res) => transformer.shiftRight(path) mustEqual res
        }
    }
  }
  
  def elem(name: String, text: String) = Elem(None, name, Attributes(), Map(), Group(Text(text)))
}