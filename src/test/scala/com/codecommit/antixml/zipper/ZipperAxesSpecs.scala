package com.codecommit.antixml
package zipper

import org.specs2.mutable._
import XML._
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
      zipper.descendant.unselect mustEqual res
      zipper.descendantOrSelf.unselect mustEqual res
    }

    "fail on broken zippers" in {
      bookstore.toZipper.directParent.unselect must throwA[RuntimeException]
      bookstore.toZipper.ancestor.unselect must throwA[RuntimeException]
      bookstore.toZipper.ancestorOrSelf.unselect must throwA[RuntimeException]
      bookstore.toZipper.followingSibling.unselect must throwA[RuntimeException]
      bookstore.toZipper.followingSiblingOrSelf.unselect must throwA[RuntimeException]
      bookstore.toZipper.precedingSibling.unselect must throwA[RuntimeException]
      bookstore.toZipper.precedingSiblingOrSelf.unselect must throwA[RuntimeException]
      bookstore.toZipper.descendant.unselect must throwA[RuntimeException]
      bookstore.toZipper.descendantOrSelf.unselect must throwA[RuntimeException]
    }
  }
  
  "Direct parent axes" should {
    "be empty for root" in {
      val res = bookstore select 'bookstore directParent
      
      verify(res)(Group())
    }
    
    "return the direct parents of nodes" in {
      val res = bookstore \\ 'author directParent;
      
      verify(res)(bookstore \\ 'book)
    }
  }
  
  "Ancestor axes" should {
    "be empty for root" in {
      val res = bookstore select 'bookstore ancestor
      
      verify(res)(Group())
    }
    
    "return all ancestors of nodes" in {
      val res = bookstore \\ 'author ancestor;
      
      verify(res)(bookstore ++ bookstore \\ 'book)
    }
  }
  
  "Ancestor or self axes" should {
    "return self for root" in {
      val res = bookstore select 'bookstore ancestorOrSelf
      
      verify(res)(bookstore)
    }
    
    "return all ancestors and self of nodes" in {
      val books = bookstore \\ 'book
      val authors = bookstore \\ 'author
      
      val res = authors ancestorOrSelf;
      
      verify(res)(bookstore :+ books(0) :+ authors(0) :+ books(1) :+ authors(1) :+ books(2) :+ authors(2) :+ authors(3))
    }
  }
  
  "Following sibling axes" should {
    "return nothing on rightmost node" in {
      val res = bookstore \\ 'author followingSibling
      
      verify(res)(Group((bookstore \\ 'author).apply(3)))
    }
    
    "return all following siblings of nodes" in {
      val res = bookstore \\ 'title followingSibling
      
      verify(res)(bookstore \\ 'author) 
    }
  }
  
  "Following sibling or self axes" should {
    "return self for rightmost nodes" in {
      val res = bookstore \\ 'author followingSiblingOrSelf
      
      verify(res)(bookstore \\ 'author)
    }
    
    "return all following siblings and self of nodes" in {
      val res = bookstore \\ 'title followingSiblingOrSelf
      
      verify(res)(bookstore \ 'book \ *)
    }
  }
  
  "Preceding sibling axes" should {
    "return nothing on leftmost node" in {
      val res = bookstore \\ 'title precedingSibling
      
      verify(res)(Group())
    }
    
    "return all preceding siblings of nodes" in {
      val res = bookstore \\ 'author precedingSibling
      
      verify(res)((bookstore \\ 'title) :+ (bookstore \\ 'author).apply(2))
    }
  }
  
  "Preceding sibling or self axes" should {
    "return self for leftmost nodes" in {
      val res = bookstore \\ 'title precedingSiblingOrSelf
      
      verify(res)(bookstore \\ 'title)
    }
    
    "return all preceding siblings and self of nodes" in {
      val res = bookstore \\ 'author precedingSiblingOrSelf
      
      verify(res)(bookstore \ 'book \ *)
    }
  }
  
  val text = Selector({ case t: Text => t })
  
  "Descendant axes" should {
    "return empty for leaf nodes" in {
      val res = bookstore \\ text descendant
      
      verify(res)(Group())
    }
    
    "return all descendants of nodes" in {
      val res = bookstore \\ 'book descendant
      
      verify(res)(bookstore \\ 'book \\ *)
    }
  }

  "Descendant or self axes" should {
    "return self for leaf nodes" in {
      val res = bookstore \\ text descendantOrSelf

      verify(res)(bookstore \\ text)
    }

    "return all descendants and self of nodes" in {
      val res = bookstore \\ 'book descendantOrSelf

      verify(res)(bookstore \\ *)
    }
  }
  
  def verify(result: Zipper[Node])(expected: Group[Node]) = {
    result mustEqual expected
    result.unselect mustEqual bookstore
  }
  
  def resource(filename: String) =
    XML fromSource (scala.io.Source fromURL (getClass getResource ("/" + filename)))
}