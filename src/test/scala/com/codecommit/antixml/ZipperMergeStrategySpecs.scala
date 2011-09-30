package com.codecommit.antixml

import org.specs2.mutable._
import com.codecommit.antixml.ZipperMergeStrategy._
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
