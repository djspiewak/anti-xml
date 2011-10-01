package com.codecommit.antixml

import org.specs2.mutable._
import com.codecommit.antixml.PathCreator._
import com.codecommit.antixml.Zipper._
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

  def ps(pars: (Elem, Int)*) = List(pars.map(_._2): _*) //List(pars.map(ParentLoc.tupled): _*)
  def nl(n: Node, l: Int) = l //WithLoc(n, l)
  def pv(n: Node, path: Int*) = PathVal(n, Vector(path: _*))
  
  val root = Vector(pv(x0,0), pv(x1, 1), pv(x2, 2))
  val directChild = Vector (
    pv(a0, 0, 0), pv(b0, 0, 1), pv(c0, 0, 2),
    pv(a1, 1, 0), pv(b1, 1, 1), pv(c1, 1, 2),
    pv(a2, 2, 0), pv(b2, 2, 1), pv(c2, 2, 2)
  )
  val rest = Vector (
    pv(foo0, 0, 0, 0), pv(baz0, 0, 1, 0),
    pv(foo1, 1, 0, 0), pv(baz1, 1, 1, 0),
    pv(foo2, 2, 0, 0), pv(baz2, 2, 1, 0)
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
      all(s)(group) mustEqual (root ++ directChild ++ rest).sortBy(pathKeys) 
    }

    "take all children nodes recursively, depth first" in {
      allChildren(s)(group) mustEqual (directChild ++ rest).sortBy(pathKeys)
    }

    "apply selectors at the root level" in {
      val sel = Selector({ case Elem(_, "root1", _, _, _) => elem("selected") })
      fromNodes(sel)(group) mustEqual Vector(pv(elem("selected"), 1)) 
    }

    "apply selectors to the children of the root" in {
      val sel = Selector({ case Elem(_, "b2", _, _, _) => elem("selected") })
      directChildren(sel)(group) mustEqual Vector(pv(elem("selected"),2,1))
    }

    val selDeep = Selector({
      case Elem(_, "root2", _, _, _) => elem("selected")
      case Elem(_, "c1", _, _, _) => elem("selected")
      case Elem(_, "b2", _, _, _) => elem("selected")
      case Text("baz") => Text("selected")
    })

    val selResRoot = Vector(pv(elem("selected"),2))
                                
    val selResNoRoot = List(
      pv(elem("selected"),1,2),
      pv(elem("selected"),2,1),
      pv(Text("selected"),0,1,0),
      pv(Text("selected"),1,1,0),
      pv(Text("selected"),2,1,0)
    )

    "apply selectors recursively" in {
      all(selDeep)(group) mustEqual (selResRoot ++ selResNoRoot).sortBy(pathKeys)
    }

    "apply selectors recursively on the children" in {
      allChildren(selDeep)(group) mustEqual selResNoRoot.sortBy(pathKeys)
    }
    
  }
  
  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  
  /** 
   * Returns the indices of the specified PathVal starting at the top.  Note that depth-first
   * order on PathVals is equivalent to Lexicographic order on their pathKeys.
   */
  def pathKeys(p: PathVal[_]):Iterable[Int] = p.path
  
}