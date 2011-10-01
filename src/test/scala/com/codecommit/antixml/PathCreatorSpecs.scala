package com.codecommit.antixml

import org.specs2.mutable._
import com.codecommit.antixml.PathCreator._
import com.codecommit.antixml.DeepZipper._
import XML._
import scala.math.Ordering

class PathCreatorSpecs extends SpecificationWithJUnit {
  
  val s = *

  val x0 = fromString("<root0><a0>foo</a0><b0>baz</b0><c0/></root0>")
  val a0 = fromString("<a0>foo</a0>"); val b0 = fromString("<b0>baz</b0>"); val c0 = fromString("<c0/>")
  val foo0 = Text("foo"); val baz0 = Text("baz")

  val x1 = fromString("<root1><a1>foo</a1><b1>baz</b1><c1/></root1>")
  val a1 = fromString("<a1>foo</a1>"); val b1 = fromString("<b1>baz</b1>"); val c1 = fromString("<c1/>")
  val foo1 = Text("foo"); val baz1 = Text("baz")

  val x2 = fromString("<root2><a2>foo</a2><b2>baz</b2><c2/></root2>")
  val a2 = fromString("<a2>foo</a2>"); val b2 = fromString("<b2>baz</b2>"); val c2 = fromString("<c2/>")
  val foo2 = Text("foo"); val baz2 = Text("baz")

  val group = Group(x0, x1, x2)

  def ps(pars: (Elem, Int)*) = List(pars.map(ParentLoc.tupled): _*)
  def nl(n: Node, l: Int) = WithLoc(n, l)

  val root = List((nl(x0, 0), ps()), (nl(x1, 1), ps()), (nl(x2, 2), ps()))
  val directChild = List(
    (nl(a0, 0), ps((x0, 0))), (nl(b0, 1), ps((x0, 0))), (nl(c0, 2), ps((x0, 0))),
    (nl(a1, 0), ps((x1, 1))), (nl(b1, 1), ps((x1, 1))), (nl(c1, 2), ps((x1, 1))),
    (nl(a2, 0), ps((x2, 2))), (nl(b2, 1), ps((x2, 2))), (nl(c2, 2), ps((x2, 2)))
  )
  val rest = List(
    (nl(foo0, 0), ps((a0, 0), (x0, 0))), (nl(baz0, 0), ps((b0, 1), (x0, 0))),
    (nl(foo1, 0), ps((a1, 0), (x1, 1))), (nl(baz1, 0), ps((b1, 1), (x1, 1))),
    (nl(foo2, 0), ps((a2, 0), (x2, 2))), (nl(baz2, 0), ps((b2, 1), (x2, 2)))
  )
  
  "allMaximalChildren" should {
    "stop at the highest match" in {
      allMaximalChildren(s)(group) mustEqual directChild.sortBy(pathKeys)
    }
    
    "find deep matches" in {
      val sel = Selector({case x:Text => x})
      allMaximalChildren(sel)(group) mustEqual rest.sortBy(pathKeys)      
    }
    
    "find matches at mixed levels" in {
      val sel = Selector({
        case e:Elem if e.name == "a0" => e
        case t:Text => t
      })
      allMaximalChildren(sel)(group) mustEqual (directChild.take(1) ++ rest.drop(1)).sortBy(pathKeys)      
    }
  }
  
  "allMaximal" should {
    "stop at the highest match" in {
      allMaximal(s)(group) mustEqual root.sortBy(pathKeys)
    }
    
    "find deep matches" in {
      val sel = Selector({case x:Text => x})
      allMaximal(sel)(group) mustEqual rest.sortBy(pathKeys)      
    }
    
    "find matches at mixed levels" in {
      val sel = Selector({
        case e:Elem if e.name == "a0" => e
        case t:Text => t
      })
      allMaximal(sel)(group) mustEqual (directChild.take(1) ++ rest.drop(1)).sortBy(pathKeys)      
    }

    "find matches at mixed levels 2" in {
      val sel = Selector({
        case e:Elem if e.name == "root0" || e.name=="a0" || e.name=="a1" => e
        case t:Text => t 
      })
      allMaximal(sel)(group) mustEqual (root.take(1) ++ directChild.drop(3).take(1) ++ rest.drop(3)).sortBy(pathKeys)      
    }
  }
  

  
  "Path values" should {

    "ignore empty groups" in {
      val empty = Group()

      fromNodes(s)(empty) mustEqual Nil
      all(s)(empty) mustEqual Nil
      directChildren(s)(empty) mustEqual Nil
      allChildren(s)(empty) mustEqual Nil
    }


    "take from the root of the nodes" in {
      fromNodes(s)(group) mustEqual root.sortBy(pathKeys)
    }

    "take the children of the root nodes" in {
      directChildren(s)(group) mustEqual directChild.sortBy(pathKeys)
    }

    "take all the nodes recursively, depth first" in {
      all(s)(group) mustEqual (root ::: directChild ::: rest).sortBy(pathKeys) 
    }

    "take all children nodes recursively, depth first" in {
      allChildren(s)(group) mustEqual (directChild ::: rest).sortBy(pathKeys)
    }

    "apply selectors at the root level" in {
      val sel = Selector({ case Elem(_, "root1", _, _, _) => elem("selected") })
      fromNodes(sel)(group) mustEqual List((nl(elem("selected"), 1), ps()))
    }

    "apply selectors to the children of the root" in {
      val sel = Selector({ case Elem(_, "b2", _, _, _) => elem("selected") })
      directChildren(sel)(group) mustEqual List((nl(elem("selected"), 1), ps((x2, 2))))
    }

    val selDeep = Selector({
      case Elem(_, "root2", _, _, _) => elem("selected")
      case Elem(_, "c1", _, _, _) => elem("selected")
      case Elem(_, "b2", _, _, _) => elem("selected")
      case Text("baz") => Text("selected")
    })

    val selResRoot = List(
      (nl(elem("selected"), 2), ps()))

    val selResNoRoot = List(
      (nl(elem("selected"), 2), ps((x1, 1))),
      (nl(elem("selected"), 1), ps((x2, 2))),
      (nl(Text("selected"), 0), ps((b0, 1), (x0, 0))),
      (nl(Text("selected"), 0), ps((b1, 1), (x1, 1))),
      (nl(Text("selected"), 0), ps((b2, 1), (x2, 2)))
    )

    "apply selectors recursively" in {
      all(selDeep)(group) mustEqual (selResRoot ::: selResNoRoot).sortBy(pathKeys)
    }

    "apply selectors recursively on the children" in {
      allChildren(selDeep)(group) mustEqual selResNoRoot.sortBy(pathKeys)
    }
  }
  
  "Paths" should {
    "not contain duplicate locations" in {
      new Path(Seq((WithLoc("foo", 1), Nil), (WithLoc("bar", 1), Nil))) must throwA[IllegalArgumentException]
    }

    "properly split locations and contents" in {
      val p1 = ParentLoc(elem("a"), 1) :: Nil
      val loc1 = WithLoc("foo", 2)
      val p2 = ParentLoc(elem("b"), 1) :: ParentLoc(elem("c"), 2) :: Nil
      val loc2 = WithLoc("bar", 2)
      val path = new Path(Seq((loc1, p1), (loc2, p2)))

      path.contents mustEqual Seq("foo", "bar")
      path.locs mustEqual Seq(
        LocationContext(loc1.loc, p1, 0),
        LocationContext(loc2.loc, p2, 0))
    }
  }

  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  
  /** 
   * Returns the indices of the specified PathVal starting at the top.  Note that depth-first
   * order on PathVals is equivalent to Lexicographic order on their pathKeys.
   */
  def pathKeys(p: (WithLoc[_],Seq[ParentLoc])):Iterable[Location] = {
    val (w,s) = p
    (w.loc +: s.map(_.loc)).reverse
  }
  
}