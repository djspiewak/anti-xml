package com.codecommit.antixml

import org.specs2.mutable._
import XML._
import ZipperAxes._
import scala.collection.immutable.SortedSet

class ZipperAxesSpecs extends SpecificationWithJUnit {
  val bookstore = resource("bookstore.xml").toGroup
  
  "Axes" should {
    "preserve past modifications" in {
      val zipper = (bookstore \\ 'author).updated(3, Text("foo"))
      val res = zipper.unselect
      
      zipper.directParent.unselect mustEqual res
      zipper.ancestor.unselect mustEqual res
      zipper.ancestorOrSelf.unselect mustEqual res
      zipper.followingSibling.unselect mustEqual res
      zipper.followingSiblingOrSelf.unselect mustEqual res
      zipper.precedingSibling.unselect mustEqual res
      zipper.precedingSiblingOrSelf.unselect mustEqual res
    }
    
    "fail on broken zippers" in {
      bookstore.toZipper.directParent.unselect must throwA[RuntimeException]
      bookstore.toZipper.ancestor.unselect must throwA[RuntimeException]
      bookstore.toZipper.ancestorOrSelf.unselect must throwA[RuntimeException]
      bookstore.toZipper.followingSibling.unselect must throwA[RuntimeException]
      bookstore.toZipper.followingSiblingOrSelf.unselect must throwA[RuntimeException]
      bookstore.toZipper.precedingSibling.unselect must throwA[RuntimeException]
      bookstore.toZipper.precedingSiblingOrSelf.unselect must throwA[RuntimeException]
    }
  }
  
  "Direct parent axes" should {
    "be empty for root" in {
      val res = bookstore select 'bookstore directParent
      
      res mustEqual Group()
      res.unselect mustEqual bookstore
    }
    
    "return the direct parents of nodes" in {
      val res = bookstore \\ 'author directParent;
      
      res mustEqual (bookstore \\ 'book)
      res.unselect mustEqual bookstore
    }
  }
  
  "Ancestor axes" should {
    "be empty for root" in {
      val res = bookstore select 'bookstore ancestor
      
      res mustEqual Group()
      res.unselect mustEqual bookstore
    }
    
    "return all ancestors of nodes" in {
      val res = bookstore \\ 'author ancestor;
      
      res mustEqual (bookstore ++ bookstore \\ 'book)
      res.unselect mustEqual bookstore
    }
  }
  
  "Ancestor or self axes" should {
    "return self for root" in {
      val res = bookstore select 'bookstore ancestorOrSelf
      
      res mustEqual bookstore
      res.unselect mustEqual bookstore
    }
    
    "return all ancestors and self of nodes" in {
      val books = bookstore \\ 'book
      val authors = bookstore \\ 'author
      
      val res = authors ancestorOrSelf;
      
      
      res mustEqual (bookstore :+ books(0) :+ authors(0) :+ books(1) :+ authors(1) :+ books(2) :+ authors(2) :+ authors(3))
      res.unselect mustEqual bookstore
    }
  }
  
  "Following sibling axes" should {
    "return nothing on rightmost node" in {
      val res = bookstore \\ 'author followingSibling
      
      res mustEqual Group((bookstore \\ 'author).apply(3))
      res.unselect mustEqual bookstore
    }
    
    "return all following siblings of nodes" in {
      val res = bookstore \\ 'title followingSibling
      
      res mustEqual bookstore \\ 'author 
      res.unselect mustEqual bookstore
    }
  }
  
  "Following sibling or self axes" should {
    "return self for rightmost nodes" in {
      val res = bookstore \\ 'author followingSiblingOrSelf
      
      res mustEqual (bookstore \\ 'author)
      res.unselect mustEqual bookstore
    }
    
    "return all following siblings and self of nodes" in {
      val res = bookstore \\ 'title followingSiblingOrSelf
      
      res mustEqual bookstore \ 'book \ *
      res.unselect mustEqual bookstore
    }
  }
  
  "Preceding sibling axes" should {
    "return nothing on leftmost node" in {
      val res = bookstore \\ 'title precedingSibling
      
      res mustEqual Group()
      res.unselect mustEqual bookstore
    }
    
    "return all preceding siblings of nodes" in {
      val res = bookstore \\ 'author precedingSibling
      
      res mustEqual (bookstore \\ 'title) :+ (bookstore \\ 'author).apply(2)
      res.unselect mustEqual bookstore
    }
  }
  
  "Preceding sibling or self axes" should {
    "return self for leftmost nodes" in {
      val res = bookstore \\ 'title precedingSiblingOrSelf
      
      res mustEqual (bookstore \\ 'title)
      res.unselect mustEqual bookstore
    }
    
    "return all preceding siblings and self of nodes" in {
      val res = bookstore \\ 'author precedingSiblingOrSelf
      
      res mustEqual bookstore \ 'book \ *
      res.unselect mustEqual bookstore
    }
  }

  def resource(filename: String) =
    XML fromSource (scala.io.Source fromURL (getClass getResource ("/" + filename)))
}