/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml

import org.specs2.mutable._
import com.codecommit.antixml.PathCreator._
import com.codecommit.antixml.Zipper._
import XML._
import scala.math.Ordering

class PathCreatorSpecs extends SpecificationWithJUnit {
  
  def vec[A](t: Traversable[A]) = Vector(t.toSeq:_*)
  
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
  def pv(n: Node, path: Int*) = PathVal(n, ZipperPath(path: _*))
  
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
      vec(allMaximalChildren(s)(group)) mustEqual directChild.sortBy(pathKeys)
    }
    
    "find deep matches" in {
      val sel = Selector({case x:Text => x})
      vec(allMaximalChildren(sel)(group)) mustEqual rest.sortBy(pathKeys)      
    }
    
    "find matches at mixed levels" in {
      val sel = Selector({
        case e:Elem if e.name == "a0" => e
        case t:Text => t
      })
      vec(allMaximalChildren(sel)(group)) mustEqual (directChild.take(1) ++ rest.drop(1)).sortBy(pathKeys)      
    }
  }
  
  "allMaximal" should {
    "stop at the highest match" in {
      vec(allMaximal(s)(group)) mustEqual root.sortBy(pathKeys)
    }
    
    "find deep matches" in {
      val sel = Selector({case x:Text => x})
      vec(allMaximal(sel)(group)) mustEqual rest.sortBy(pathKeys)      
    }
    
    "find matches at mixed levels" in {
      val sel = Selector({
        case e:Elem if e.name == "a0" => e
        case t:Text => t
      })
      vec(allMaximal(sel)(group)) mustEqual (directChild.take(1) ++ rest.drop(1)).sortBy(pathKeys)      
    }

    "find matches at mixed levels 2" in {
      val sel = Selector({
        case e:Elem if e.name == "root0" || e.name=="a0" || e.name=="a1" => e
        case t:Text => t 
      })
      vec(allMaximal(sel)(group)) mustEqual (root.take(1) ++ directChild.drop(3).take(1) ++ rest.drop(3)).sortBy(pathKeys)      
    }
  }
  

  
  "Path values" should {

    "ignore empty groups" in {
      val empty = Group()

      vec(fromNodes(s)(empty)) mustEqual Nil
      vec(all(s)(empty)) mustEqual Nil
      vec(directChildren(s)(empty)) mustEqual Nil
      vec(allChildren(s)(empty)) mustEqual Nil
    }

    "take from the root of the nodes" in {
      vec(fromNodes(s)(group)) mustEqual root.sortBy(pathKeys)
    }

    "take the children of the root nodes" in {
      vec(directChildren(s)(group)) mustEqual directChild.sortBy(pathKeys)
    }

    "take all the nodes recursively, depth first" in {
      vec(all(s)(group)) mustEqual (root ++ directChild ++ rest).sortBy(pathKeys) 
    }

    "take all children nodes recursively, depth first" in {
      vec(allChildren(s)(group)) mustEqual (directChild ++ rest).sortBy(pathKeys)
    }

    "apply selectors at the root level" in {
      val sel = Selector({ case Elem(_, "root1", _, _, _) => elem("selected") })
      vec(fromNodes(sel)(group)) mustEqual Vector(pv(elem("selected"), 1)) 
    }

    "apply selectors to the children of the root" in {
      val sel = Selector({ case Elem(_, "b2", _, _, _) => elem("selected") })
      vec(directChildren(sel)(group)) mustEqual Vector(pv(elem("selected"),2,1))
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
      vec(all(selDeep)(group)) mustEqual (selResRoot ++ selResNoRoot).sortBy(pathKeys)
    }

    "apply selectors recursively on the children" in {
      vec(allChildren(selDeep)(group)) mustEqual selResNoRoot.sortBy(pathKeys)
    }
    
  }
  
  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  
  /** 
   * Returns the indices of the specified PathVal starting at the top.  Note that depth-first
   * order on PathVals is equivalent to Lexicographic order on their pathKeys.
   */
  def pathKeys(p: PathVal[_]):Iterable[Int] = p.path
  
}