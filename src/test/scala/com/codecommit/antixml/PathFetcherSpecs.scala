package com.codecommit.antixml

import org.specs2.mutable._
import org.specs2.matcher.DataTables
import XML._

class PathFetcherSpecs extends SpecificationWithJUnit with DataTables {
  
  val x0 = fromString("<root0><a0>foo</a0><b0>baz</b0><c0/></root0>")
  val x1 = fromString("<root1><a1>foo</a1><b1>baz</b1><c1/></root1>")
  val x2 = fromString("<root2><a2>foo</a2><b2>baz</b2><c2/></root2>")

  val group = Group(x0, x1, x2)
  val fetch = PathFetcher.getNode(group)_
  val empty: Option[Node] = None
  
  "path fetching" should {
    "produce correct results" in {
        "path"           | "result" |
        ZipperPath.empty !  empty   |
        ZipperPath(0)    ! Some(x0) |
        ZipperPath(1)    ! Some(x1) |
        ZipperPath(2)    ! Some(x2) | 
        ZipperPath(0, 0) ! Some(fromString("<a0>foo</a0>")) |
        ZipperPath(0, 1) ! Some(fromString("<b0>baz</b0>")) |
		ZipperPath(0, 2) ! Some(fromString("<c0/>")) |
		ZipperPath(1, 0) ! Some(fromString("<a1>foo</a1>")) |
		ZipperPath(1, 1) ! Some(fromString("<b1>baz</b1>")) |
		ZipperPath(1, 2) ! Some(fromString("<c1/>")) |
		ZipperPath(2, 0) ! Some(fromString("<a2>foo</a2>")) |
		ZipperPath(2, 1) ! Some(fromString("<b2>baz</b2>")) |
		ZipperPath(2, 2) ! Some(fromString("<c2/>")) |> { 
         (path, res) => fetch(path) mustEqual res
        }
    }
  }
}