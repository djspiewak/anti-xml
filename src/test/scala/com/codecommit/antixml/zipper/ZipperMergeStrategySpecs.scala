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
package zipper

import org.specs2.mutable._
import ZipperMergeStrategy._
import XML._

class ZipperMergeStrategySpecs extends SpecificationWithJUnit {

  "The AlwaysPreferChildren strategy" should {
    "ignore updates to parent in a parent-child conflict" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val unsel = elems.updated(1,elem("Child")).updated(0,elem("Parent")).unselect(AlwaysPreferChildren)
      
      unsel.stripZipper mustEqual Group(<top><a><Child /></a></top>.convert)
    }
  }

  "The AlwaysPreferParents strategy" should {
    "ignore updates to child in a parent-child conflict" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val changed = elems.updated(0,elem("Parent")).updated(1,elem("Child"))
      
      val unsel = changed.unselect(AlwaysPreferParents)
      
      unsel.stripZipper mustEqual Group(<top><Parent /></top>.convert)
    }
  }

  "The AlwaysLocal strategy" should {
    "throw away direct changes to an elem's children in a parent-child conflict" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val changed = elems.updated(0,elem("Parent"))
      
      val unsel = changed.unselect(AlwaysLocal)
      
      unsel.stripZipper mustEqual Group(<top><Parent ><b /></Parent></top>.convert)
    }
    "be uniform" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      val changed = elems.flatMap(x => x.copy(name=x.name+"0") :: x.copy(name=x.name+"1") :: Nil)
      val unsel = changed.unselect(AlwaysLocal)
      unsel.stripZipper mustEqual Group(<top><a0><b0 /><b1 /></a0><a1><b0 /><b1 /></a1></top>.convert)
    }
  }  
  
  "The RequireLocal strategy" should {
    "accept local changes" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val changed = elems.updated(0,elems(0).copy(name="newname"))
      
      val unsel = changed.unselect(RequireLocal)
      
      unsel.stripZipper mustEqual Group(<top><newname ><b /></newname></top>.convert)
    }
    "throw an exception in the presence of non-local changes to conflicted nodes" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val changed = elems.updated(0,elem("Parent"))
      
      changed.unselect(RequireLocal) must throwA[RuntimeException]
      
    }
    "be uniform" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      val changed = elems.flatMap(x => x.copy(name=x.name+"0") :: x.copy(name=x.name+"1") :: Nil)
      val unsel = changed.unselect(RequireLocal)
      unsel.stripZipper mustEqual Group(<top><a0><b0 /><b1 /></a0><a1><b0 /><b1 /></a1></top>.convert)
    }
  }
  
  "The RequireConflictFree Strategy" should {
    "throw an exception in the presence of any conflicted nodes" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      elems.unselect(RequireConflictFree) must throwA[RuntimeException]
    }
  }

  "The PreferLatest Strategy" should {
    "Prefer direct updates to children when they are latest" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val unsel = elems.updated(1,elem("Child")).updated(0,elem("Parent")).unselect(PreferLatest)
      
      unsel.stripZipper mustEqual Group(<top><Parent /></top>.convert)
    }
    "Prefer indirect updates to children when they are latest" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val unsel = elems.updated(0,elem("Parent")).updated(1,elem("Child")).unselect(PreferLatest)
      
      unsel.stripZipper mustEqual Group(<top><Parent><Child /></Parent></top>.convert)
    }
    "Ignore ipdate order when changes are local" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      
      val unsel = elems.updated(1,elem("Child")).updated(0,elems(0).copy(name="Parent")).unselect(PreferLatest)
      
      unsel.stripZipper mustEqual Group(<top><Parent><Child /></Parent></top>.convert)
    }
    "be uniform" in {
      val orig = <top><a><b /></a></top>.convert
      val elems = orig \\ anyElem
      val changed = elems.flatMap(x => x.copy(name=x.name+"0") :: x.copy(name=x.name+"1") :: Nil)
      val unsel = changed.unselect(PreferLatest)
      unsel.stripZipper mustEqual Group(<top><a0><b0 /><b1 /></a0><a1><b0 /><b1 /></a1></top>.convert)
    }
  }

  
  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  
  def anyElem = Selector[Elem]({case x:Elem => x})
}
