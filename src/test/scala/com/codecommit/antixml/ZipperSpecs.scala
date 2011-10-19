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
import org.specs2.ScalaCheck
import org.scalacheck._
import XML._
import com.codecommit.antixml.Zipper._
import scala.io.Source

class ZipperSpecs extends SpecificationWithJUnit with ScalaCheck  with XMLGenerators {

  implicit val params = set(maxSize -> 10, minTestsOk -> 20)
  val bookstore = resource("bookstore.xml")
  val onlyBell = Group(<bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book></bookstore>.convert)
  val onlyPS = Group(<bookstore><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.convert)
  
  // TODO This is by no means exhaustive, just showing off what's possible.

  "Deep Zipper selection" should {
    // I'm lazy so the bookstore is hardcoded here.
	// From some reason parsing indented literals gives errors when comparing results, using a single line string.
    
    val bookstore = fromString {
      "<bookstore>" +
        "<book>" +
          "<title>For Whom the Bell Tolls</title>" +
          "<author>Hemmingway</author>" +
        "</book>" +
        "<book>" +
          "<title>I, Robot</title>" +
          "<author>Isaac Asimov</author>" +
        "</book>" +
        "<book>" +
          "<title>Programming Scala</title>" +
          "<author>Dean Wampler</author>" +
          "<author>Alex Payne</author>" +
        "</book>" +
      "</bookstore>" 
    }
      
    val bookGroup = Group(bookstore)

    "not modify the the content on clean unselection" in {
      (bookstore \ 'book unselect) mustEqual bookGroup
      (bookstore \\ 'book unselect) mustEqual bookGroup
      (bookstore \ 'book \\ 'title).unselect.unselect mustEqual bookGroup
    }

    "modify on any level" in {
      val authors = bookstore \\ 'author
      val newAuthor = <author>Tolkien</author>.convert
      val newBooks =
        authors.
          updated(0, newAuthor).
          updated(1, newAuthor).
          updated(2, newAuthor).
          updated(3, newAuthor).
        unselect

      val res = fromString {
        "<bookstore>" +
          "<book>" +
          "<title>For Whom the Bell Tolls</title>" +
          "<author>Tolkien</author>" +
          "</book>" +
          "<book>" +
          "<title>I, Robot</title>" +
          "<author>Tolkien</author>" +
          "</book>" +
          "<book>" +
          "<title>Programming Scala</title>" +
          "<author>Tolkien</author>" +
          "<author>Tolkien</author>" +
          "</book>" +
          "</bookstore>"
      }  
      
      newBooks mustEqual Group(res)
    }
    
    "perform consecutive selection/modification" in {
      val titles = bookstore \\ 'book \ 'title 
      val newBooks =
        titles.
          updated(0, <title>LOTR</title>.convert).
          updated(1, <title>Hitchhikers Guide</title>.convert).
          unselect.unselect
          
      val res = fromString {
        "<bookstore>" +
          "<book>" +
          "<title>LOTR</title>" +
          "<author>Hemmingway</author>" +
          "</book>" +
          "<book>" +
          "<title>Hitchhikers Guide</title>" +
          "<author>Isaac Asimov</author>" +
          "</book>" +
          "<book>" +
          "<title>Programming Scala</title>" +
          "<author>Dean Wampler</author>" +
          "<author>Alex Payne</author>" +
          "</book>" +
          "</bookstore>"
      }

      newBooks mustEqual Group(res)
      
    }
    
    val all = bookstore \\ * 
    
    "resolve merging problems with the merge strategy 1" in {
      val newBooks = all.updated(0, elem("erased")).updated(1, elem("replaced", Text("foo"))).unselect
      
      // notice how children are propagated from below  	
      val res = fromString(
        "<bookstore>" +
          "<erased>" +
            "<replaced>foo</replaced>" +
            "<author>Hemmingway</author>" +
          "</erased>" +
          "<book>" +
            "<title>I, Robot</title>" +
            "<author>Isaac Asimov</author>" +
          "</book>" +
          "<book>" +
            "<title>Programming Scala</title>" +
            "<author>Dean Wampler</author>" +
            "<author>Alex Payne</author>" +
          "</book>" +
        "</bookstore>")

      newBooks mustEqual Group(res)
    }
    
    "resolve merging problems with the merge strategy 2" in {
      val newBooks =
        all.
          updated(1, elem("replaced", Text("foo"))).
          updated(0, elem("erased")).
          unselect

      // this time the children are ignored, because the last change is at the root
      val res = fromString(
        "<bookstore>" +
          "<erased/>" +
          "<book>" +
            "<title>I, Robot</title>" +
            "<author>Isaac Asimov</author>" +
          "</book>" +
          "<book>" +
            "<title>Programming Scala</title>" +
            "<author>Dean Wampler</author>" +
            "<author>Alex Payne</author>" +
          "</book>" +
        "</bookstore>")

      newBooks mustEqual Group(res)
    	
    }

    "just randomly modifying stuff" in {
      val newBooks =
        all.
          updated(0, elem("boo")).
          updated(1, elem("boo")).
          updated(2, elem("boo")).
          updated(3, elem("boo")).
          updated(4, elem("boo")).
          updated(5, elem("boo")).
          updated(6, elem("boo")).
          updated(7, elem("boo")).
          updated(8, elem("boo")).
          updated(9, elem("boo")).
          updated(10, elem("boo")).
          unselect

      val res = fromString(
        "<bookstore>" +
           "<boo><boo><boo/></boo><boo><boo/></boo></boo>" +
           "<boo><boo><boo/></boo><boo><boo/></boo></boo>" +
           "<boo/>" +
        "</bookstore>")
        
      newBooks mustEqual Group(res)
          
    }
  }

  "Zipper updates within '\\' results" should {
    "rebuild from empty result set" in {
      val xml = Group(<parent><child/><child/></parent>.convert)
      (xml \ 'foo).unselect mustEqual xml
      (bookstore \ 'book \ 'foo).unselect mustEqual (bookstore \ 'book)
      (bookstore \ 'book \ 'foo).unselect.unselect mustEqual Group(bookstore)
    }

    "rebuild updated at one level" in {
      val books = bookstore \ "book"
      val book0 = books(0).copy(attrs = Attributes("updated" -> "yes"))
      val book2 = books(2).copy(attrs = Attributes("updated" -> "yes"))

      val bookstore2: Group[Node] = books.updated(0, book0).updated(2, book2).unselect // ensure we have NodeSeq

      // find afresh without using >
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes()
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }

    "rebuild updated at two levels" in {
      val authors = bookstore \ "book" \ "author"
      val author0 = authors(0).copy(attrs = Attributes("updated" -> "yes"))
      val author2 = authors(2).copy(attrs = Attributes("updated" -> "yes"))
      val author3 = authors(3).copy(attrs = Attributes("updated" -> "yes"))
      
      val bookstore2: Group[Node] = authors.updated(0, author0).updated(2, author2).updated(3, author3).unselect.unselect

      // find afresh without using >
      bookstore2.head.asInstanceOf[Elem].name mustEqual "bookstore"
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].name mustEqual "book"

      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children must haveSize(2)
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")

      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children must haveSize(2)
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes()

      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children must haveSize(3)
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }

    "rebuild after a map at the first level" in {
      val books = bookstore \ "book"
      val books2 = books map { _.copy(attrs=Attributes("updated" -> "yes")) }
      
      val bookstore2: Group[Node] = books2.unselect
      bookstore2.head.asInstanceOf[Elem].children must haveSize(3)
      
      // find afresh without using >                                                    
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }
    
     "rebuild after a drop at the first level" in {
      val books = bookstore \ "book"
      val books2 = books drop 2
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyPS
    }
    
    "rebuild after a slice at the first level" in {
      val books = bookstore \ "book"
      val books2 = books slice (2, 3)
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyPS
    }
    
    "rebuild after a take at the first level" in {
      val books = bookstore \ "book"
      val books2 = books take 1
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyBell
    }
    
    "rebuild after a splitAt at the first level" in {
      val books = bookstore \ "book"
      val (books2, books3) = books splitAt 1
      val books4 = books3 drop 1
      val bookstore2: Group[Node] = books2.unselect
      val bookstore4: Group[Node] = books4.unselect
      
      bookstore2 mustEqual onlyBell
      bookstore4 mustEqual onlyPS
    }
           
    "rebuild after a flatMap at the first level" in {
      val books = bookstore \ "book"
      val books2 = books flatMap { 
        case e @ Elem(_, _, _, _, children) if children.length > 2 =>
          List(e.copy(attrs=Attributes("updated" -> "yes")), e.copy(attrs=Attributes("updated" -> "yes")))
        
        case _ => Nil
      }
      
      val bookstore2: Group[Node] = books2.unselect
      bookstore2.head.asInstanceOf[Elem].children must haveSize(2)
      
      // find afresh without using >
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      
      (bookstore2 \ "book" \ "title" \ *) must beLike((_:Node) match { case Text("Programming Scala") => ok }).forall
    }
    
    // my attempt at a "real world" test case"
    "rebuild after non-trivial for-comprehension" in {
      val titledBooks = for {
        bookElem <- bookstore \ "book"
        title <- bookElem \ "title" \ text
        if !title.trim.isEmpty
        val filteredChildren = bookElem.children filter { case Elem(None, "title", _, _, _) => false case _ => true }
      } yield bookElem.copy(attrs=(bookElem.attrs + ("title" -> title)), children=filteredChildren)
      
      val bookstore2 = titledBooks.unselect
      val expected = <bookstore><book title="For Whom the Bell Tolls"><author>Hemmingway</author></book><book title="I, Robot"><author>Isaac Asimov</author></book><book title="Programming Scala"><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.convert
      bookstore2 mustEqual Group(expected)
    }
    
    "rebuild following identity map with selection miss at the top level" >> {
      "with suffix miss" in {
        val xml = Group(<parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><sub bar="test2"/></parent>.convert, <miss/>.convert)
        val xml2 = (xml \ "sub") map identity unselect
        
        xml2 mustEqual xml
      }
      
      "with prefix miss" in {
        val xml = Group(<miss/>.convert, <parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><sub bar="test2"/></parent>.convert)
        val xml2 = (xml \ "sub") map identity unselect
        
        xml2 mustEqual xml
      }
      
      "with prefix and suffix miss" in {
        val xml = Group(<miss/>.convert, <parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><sub bar="test2"/></parent>.convert, <miss/>.convert)
        val xml2 = (xml \ "sub") map identity unselect
        
        xml2 mustEqual xml
      }
    }
    
    "rebuild following identity map with selection miss at the second level" in {
      val xml = Group(<parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><miss/><sub bar="test2"/></parent>.convert)
      val xml2 = (xml \ "sub") map identity unselect
      
      xml2 mustEqual xml
    }
    
    "rebuild following filter at the first level" in {
      val books = bookstore \ 'book
      val bookstore2 = (books filter (books(1) !=)).unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
          children must haveSize(2)
          children \ 'title \ text mustEqual Vector("For Whom the Bell Tolls", "Programming Scala")
        }
      }
    }
    
    "rebuild following composed filters" in {
      val books = bookstore \ 'book
      val bookstore2 = (books filter (books(0) !=) filter (books(1) !=)).unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
          children must haveSize(1)
          children \ 'title \ text mustEqual Vector("Programming Scala")
        }
      }
    }
    
    "rebuild second level siblings following filter at the second level" in {
      val titles = bookstore \ 'book \ 'title
      val books2 = (titles filter (titles(1) ==)).unselect
      
      books2 must haveSize(3)
      books2(0) mustEqual <book><author>Hemmingway</author></book>.convert
      books2(1) mustEqual <book><title>I, Robot</title><author>Isaac Asimov</author></book>.convert
      books2(2) mustEqual <book><author>Dean Wampler</author><author>Alex Payne</author></book>.convert
    }

    "rebuild following filter at the second level" in {
      val titles = bookstore \ 'book \ 'title
      val bookstore2 = (titles filter (titles(1) !=)).unselect.unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty =>
          children must haveSize(3)
      }
      
      val titles2 = bookstore2 \ 'book \ 'title
      titles2 must haveSize(2)
      (titles2 \ text) mustEqual Vector("For Whom the Bell Tolls", "Programming Scala")
    }
    
    "rebuild following 2 filters at the first level" in {
      val books = bookstore \ 'book
      val bookstore2 = (books filter (books(1) !=) filter (books(0) !=)).unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
          children must haveSize(1)
          children \ 'title \ text mustEqual Vector("Programming Scala")
        }
      }      
    }

    "preserve flatMap order" in {
      val original = <top><a/></top>.convert
      val expanded = (original \ 'a) flatMap {
        case e: Elem => for (i <- 0 until 10) yield e.copy(name = e.name + i)
      }
      val modified = expanded.unselect

      modified must haveSize(1)
      modified(0) mustEqual <top><a0/><a1/><a2/><a3/><a4/><a5/><a6/><a7/><a8/><a9/></top>.convert
    }

    "utility methods on Zipper" >> {
      implicit val arbInt = Arbitrary(Gen.choose(0, 10))

      "identity collect should return self" in check { (xml: Group[Node], n: Int) =>
        val func = (0 until n).foldLeft(identity: Zipper[Node] => Zipper[Node]) { (g, _) =>
          g andThen { _ collect { case e => e } }
        }

        func(xml.toZipper) mustEqual xml
      }

      "identity filter should return self" in check { xml: Group[Node] =>
        val result = xml.toZipper filter Function.const(true)
        result mustEqual xml
      }

      "identity filter and unselect should return self" in check { xml: Group[Node] =>
        val sub = xml \ *
        (sub filter Function.const(true) unselect) mustEqual xml
      }
      
      "slice should work correctly in the presence of equal siblings" in {
        val xml = Group(<a><b /><c1 /><b /><c2 /></a>.convert)
        
        val sliced = (xml \ *).slice(1, 3)
        sliced mustEqual <a><c1 /><b /></a>.convert.children
        sliced.unselect.head mustEqual <a><c1 /><b /></a>.convert
      }
    }
  }

  "Shallow selection on a Zipper" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual Group(elem("parent"))
    }

    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual Group(elem("parent"))
      ns \ "parent" mustEqual Group(elem("parent"))
    }

    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \ "a" mustEqual result
    }
  }

  "Deep selection on Zipper" should {
    "return something of type Zipper on element select" in {
      val ns = <foo/>.convert.toZipper
      validate[Zipper[Elem]](ns \\ 'bar)
    }

    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>").toZipper
      ns \\ "parent" mustEqual Group(elem("parent"))
    }

    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>").toZipper
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \\ "a" mustEqual result
    }

    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>" +
        "Some text" +
        "<sub1><target>sub1</target></sub1>" +
        "<target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target>" +
        "<phoney><target>phoney</target></phoney>" + 
        "More text" +
        "<target>outside</target>" +
        "</parent>").toZipper
      
      val result = fromString("<parent>" +
        "<target>sub1</target>" +
        "<target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target>" +
        "<target>top1</target>" +
        "<target>top2</target>" +
        "<target>top3-outer</target>" +
        "<target>phoney</target>" +
        "<target>outside</target>" +
        "</parent>")
      ns \\ "target" mustEqual result.children
    }
  }
  
  "Short-circuit deep selection on Zipper" should {
  
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>").toZipper
      ns \\! "parent" mustEqual Group(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>").toZipper
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \\! "a" mustEqual result
    }
    
    "skip descendants of matching nodes in" in {
      val ns = fromString("<parent>" +
        "Some text" +
        "<sub1><target>sub1</target></sub1>" +
        "<target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target>" +
        "<phoney><target>phoney</target></phoney>" + 
        "More text" +
        "<target>outside</target>" +
        "</parent>").toZipper
  
      val result = fromString("<parent>" +
        "<target>sub1</target>" +
        "<target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target>" +
        "<target>phoney</target>" +
        "<target>outside</target>" +
        "</parent>")
      ns \\! "target" mustEqual result.children
    }
        
  }
   
  "select on a Zipper" should {

    "find a top level node" in {
      val ns = fromString("<parent><parent/></parent>").toZipper
      (ns select "parent") mustEqual ns
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<TOP><a><x1/></a><b><y1/></b><a><x2/></a><b><y2/></b></TOP>").children.toZipper
      val result = fromString("<TOP><a><x1/></a><a><x2/></a></TOP>").children
      ns select "a" mustEqual result
    }
    
    "only match the top level" in {
      val ns = fromString("<TOP><a /><b><a /></b><a><a /></a></TOP>").children.toZipper
      val result = fromString("<TOP><a /><a><a /></a></TOP>").children
      ns select "a" mustEqual result      
    }
    
  }
  
  "withFilter" should {
    val anyElem = Selector[Elem]( {case e: Elem => e} )
    
    "support forEach" in {
      val elems = (bookstore \\ anyElem).withFilter(e => e.name != "book")
      var count = 0
      elems.foreach { e =>
        count = count + 1
        e.name must beOneOf("title", "author")
      }
      count mustEqual 7
    }
    
    "support forEach with multiple filters" in {
      val elems = (bookstore \\ anyElem).withFilter(e => e.name != "book").withFilter(e => e.name != "title")
      var count = 0
      elems.foreach { e =>
        count = count + 1
        e.name mustEqual("author")
      }
      count mustEqual 4
    }
    
    "support map" in {
      val elems = (bookstore \\ anyElem).withFilter(e => e.name != "book").map({e => e.copy(name=e.name.toUpperCase)})
      elems.foreach { e =>
        e.name must beOneOf("TITLE", "AUTHOR")
      }
      elems.length mustEqual 7
    }

    "support map with multiple filters" in {
      val elems = (bookstore \\ anyElem).withFilter(e => e.name != "book")
          .withFilter(e => e.name != "title")
          .map({e => e.copy(name=e.name.toUpperCase)})
      elems.foreach { e =>
        e.name mustEqual("AUTHOR")
      }
      elems.length mustEqual 4
    }

    "support flatMap" in {
      val elems = (bookstore \\ anyElem)
          .withFilter(e => e.name != "book")
          .flatMap(e => e :: e :: Nil)
      elems.foreach { e =>
        e.name must beOneOf("title", "author")
      }
      elems.length mustEqual 14
    }
    
    "support flatMap with multiple filters" in {
      val elems = (bookstore \\ anyElem)
          .withFilter(e => e.name != "book")
          .withFilter(e => e.name != "title")
          .flatMap(e => e :: e :: Nil)
      elems.foreach { e =>
        e.name mustEqual("author")
      }
      elems.length mustEqual 8
    }

    "preserve zipper context after map" in {
      val elems = (bookstore \\ anyElem).withFilter(e => e.name != "author").map({e => e.copy(name=e.name.toUpperCase)})
      val result = elems.unselect
      (result \ 'BOOK).length mustEqual 3
      (result \ 'book).length mustEqual 0
      (result \ 'BOOK \ 'TITLE).length mustEqual 3
      (result \ 'BOOK \ 'title).length mustEqual 0
      (result \ 'BOOK \ 'AUTHOR).length mustEqual 0
      (result \ 'BOOK \ 'author).length mustEqual 0
    }
    
    "preserve zipper context after flatMap" in {
      val elems = (bookstore \ anyElem \ anyElem).withFilter(e => e.name != "author").flatMap(e => e :: e :: Nil)
      val result = elems.unselect.unselect
      (result \ 'book).length mustEqual 3
      (result \ 'book \ 'title).length mustEqual 6
      (result \ 'book \ 'author).length mustEqual 0
    }

  }
  
  "Zipper.conditionalFlatMapWithIndex" should {
    
    "work with simple replacements" in check { (xml: Group[Node]) =>
      val zipper = xml select *
      def f(n: Node, i: Int): Option[Seq[Node]] = n match {
        case n if (i & 1) == 0 => None
        case e: Elem => Some(Seq(e.copy(name=e.name.toUpperCase)))
        case n => None
      }
      val cfmwi = zipper.conditionalFlatMapWithIndex(f)
      val equiv = xml.zipWithIndex.flatMap {case (n,i) => f(n,i).getOrElse(Seq(n))}
      
      Seq(
        Vector(cfmwi:_*) mustEqual Vector(equiv:_*),
        cfmwi.length mustEqual xml.length
      )
    }
    
    "work with complex replacements" in check { (xml: Group[Node]) =>
      def f(n: Node, i: Int): Option[Seq[Node]] = n match {
        case n if (i & 1) == 0 => None
        case _ if (i & 2) == 0 => Some(Seq())
        case e: Elem => Some(Seq(e.copy(name=e.name+"MODIFIED"), e, e))
        case n => Some(Seq(n, n, n))
      }
      val zipper = xml select *
      val cfmwi = zipper.conditionalFlatMapWithIndex(f)
      val equiv = xml.zipWithIndex.flatMap {case (n,i) => f(n,i).getOrElse(Seq(n))}
      
      val expectedDels = (xml.length + 2) >>> 2
      val expectedTripples = (xml.length) >>> 2
      val expectedLength = xml.length - expectedDels + 2*expectedTripples
      
      Seq(
        Vector(cfmwi:_*) mustEqual Vector(equiv:_*),
        cfmwi.length mustEqual expectedLength
      )
    }
    
    "preserve zipper context with simple replacements" in {
      def f(t: Text, i: Int): Option[Seq[Text]] = {
        if ((i&1)==0) None else Some(Seq(Text(t.text + i)))
      }
      val texts = Group.fromSeq(for (i <- 0 until 100) yield Text(i.toString))
      val elems = for(t <- texts) yield elem("Text"+t.text,t)

      val zipper = elems \ textNode(".*".r)      
      zipper.toVectorCase mustEqual texts.toVectorCase //sanity check
      
      val z2 = zipper.conditionalFlatMapWithIndex(f)
      z2.length mustEqual 100
      z2.toVectorCase mustEqual zipper.toVectorCase.zipWithIndex.flatMap {
        case (t,i) => f(t,i).getOrElse(Seq(t))
      }
      
      val result = z2.unselect      
      result.length mustEqual 100
      result.forall(_.children.length == 1) must beTrue
      
      val rc = result flatMap {n => n.children}
      Vector(rc:_*) mustEqual Vector(z2:_*) 
    }
    
    "preserve zipper context with complex replacements" in {
      //Replaces every 4 nodes with 5 nodes
      def f(t: Text, i: Int): Option[Seq[Text]] = {
        if ((i&1)==0) None 
        else if ((i&2)==0) Some(Seq(t,t,t))
        else Some(Seq())
      }

      val texts = Group.fromSeq(for (i <- 0 until 100) yield Text(i.toString))
      val elems = for(t <- texts) yield elem("Text"+t.text,t)
    
      val zipper = elems \ textNode(".*".r)      
      zipper.toVectorCase mustEqual texts.toVectorCase //sanity check
      
      val z2 = zipper.conditionalFlatMapWithIndex(f)
      z2.length mustEqual 125
      z2.toVectorCase mustEqual zipper.toVectorCase.zipWithIndex.flatMap {
        case (t,i) => f(t,i).getOrElse(Seq(t))
      }
      
      val result = z2.unselect
      result.length mustEqual 100

      val rc = result flatMap {n => n.children}
      rc.length mustEqual 125      
      rc.toVectorCase mustEqual z2.toVectorCase
    }
    
    "preserve zipper context with complex replacements and empty holes" in {
      //Replaces every 4 nodes with 5 nodes
      def f(t: Text, i: Int): Option[Seq[Text]] = {
        if ((i&1)==0) None 
        else if ((i&2)==0) Some(Seq(t,t,t))
        else Some(Seq())
      }
      
      val texts = Group.fromSeq(for (i <- 0 until 200) yield Text(i.toString))
      val elems = for(t <- texts) yield elem("Text"+t.text,t)

      val zipper = elems \ textNode(".*".r)      
      zipper.toVectorCase mustEqual texts.toVectorCase //sanity check

      val z2 = zipper.slice(50,150).conditionalFlatMapWithIndex(f)
      z2.length mustEqual 125
      z2.toVectorCase mustEqual zipper.toVectorCase.slice(50,150).zipWithIndex.flatMap {
        case (t,i) => f(t,i).getOrElse(Seq(t))
      }
      
      val result = z2.unselect      
      result.length mustEqual 200      
      result.slice(0,50).forall(_.children.isEmpty) must beTrue
      result.slice(150,200).forall(_.children.isEmpty) must beTrue
      
      val rc = result flatMap {n => n.children}
      rc.length mustEqual 125
      rc.toVectorCase mustEqual z2.toVectorCase
    }
    
    "preserve zipper context with  half simple, half complex" in {
      def f(t: Text, i: Int): Option[Seq[Text]] = {
        if (i<25) None
        else if (i<50) Some(Seq(Text(t.text.toUpperCase)))
        else Some(Seq(t,t,t))
      }
      
      val texts = Group.fromSeq(for (i <- 0 until 100) yield Text(i.toString))
      val elems = for(t <- texts) yield elem("Text"+t.text,t)

      val zipper = elems \ textNode(".*".r)      
      zipper.toVectorCase mustEqual texts.toVectorCase //sanity check
      
      val z2 = zipper.conditionalFlatMapWithIndex(f)
      z2.length mustEqual 200
      z2.toVectorCase mustEqual zipper.toVectorCase.zipWithIndex.flatMap {
        case (t,i) => f(t,i).getOrElse(Seq(t))
      }
      
      val result = z2.unselect      
      result.length mustEqual 100
      result.slice(0,50).forall(_.children.length == 1) must beTrue
      
      val rc = result flatMap {n => n.children}
      rc.length mustEqual 200
      rc.toVectorCase mustEqual z2.toVectorCase
    }
    
    "only increase the update time of nodes being changed" in {
      val xml = <top><a><b /></a></top>.convert
      val z = xml \\ *
      
      //Conflicting updates.  Second node has latest update time.
      val z1 = z.updated(0,elem("a",elem("c")))
      val z2 = z1.updated(1,elem("b2"))  
      
      //Use CFMWI to update first node only, giving it the latest update time.
      val z3 = z2.conditionalFlatMapWithIndex {(e,i) => i match {
        case 0  => Some(Seq(e))
        case _  => None
      }}
 
      z2.unselect mustEqual <top><a><b2 /></a></top>.convert.toGroup
      z3.unselect mustEqual <top><a><c /></a></top>.convert.toGroup
    }
    
    "only increase the update time of nodes being changed 2" in {
      val xml = <top><x /><a><b /></a></top>.convert
      val z = xml \\ *
      
      //Conflicting updates.  Last node has latest update time.
      val z1 = z.updated(1,elem("a",elem("c")))
      val z2 = z1.updated(2,elem("b2"))  
      
      //Use CFMWI to delete the first node (forcing it to stop using its optimistic strategy)
      //and then update the second node to give it the latest update time.
      val z3 = z2.conditionalFlatMapWithIndex {(e,i) => i match {
        case 0  => Some(Seq())
        case 1  => Some(Seq(e))
        case _  => None
      }}

      z2.unselect mustEqual <top><x/><a><b2 /></a></top>.convert.toGroup
      z3.unselect mustEqual <top><a><c /></a></top>.convert.toGroup
    }
  }
  
  "Zipper.slice" should {
    val texts = Group.fromSeq(for (i <- 0 until 100) yield Text(i.toString))
    val elems = for(t <- texts) yield elem("Text"+t.text,t)
    val zipper = elems \ textNode(".*".r)     
    
    "match the behavior of Group.slice for ranges in bounds" in check { (from: Int, to: Int) =>
      val f = (from & Int.MaxValue) % zipper.length
      val t = (to & Int.MaxValue) % zipper.length
      zipper.slice(f,t).toVectorCase mustEqual texts.slice(f,t).toVectorCase
    }
    "match the behavior of Group.slice for any range" in check { (f: Int, t: Int) =>
      zipper.slice(f,t).toVectorCase mustEqual texts.slice(f,t).toVectorCase
    }
    
    "update the zipper context correctly for ranges in bounds" in check { (from: Int, to: Int) =>
      val f = (from & Int.MaxValue) % zipper.length
      val t = (to & Int.MaxValue) % zipper.length
      
      val unselected = zipper.slice(f,t).unselect
      unselected.length mustEqual elems.length
      
      val uc = unselected flatMap {n => n.children}      
      uc.toVectorCase mustEqual texts.slice(f,t).toVectorCase
    }
    
    "update the zipper context correctly for any range" in check { (f: Int, t: Int) =>      
      val unselected = zipper.slice(f,t).unselect
      unselected.length mustEqual elems.length
      
      val uc = unselected flatMap {n => n.children}      
      uc.toVectorCase mustEqual texts.slice(f,t).toVectorCase
    }
    
    "update the zipper context correctly for multiple slices" in check { (f1: Int, t1: Int, f2: Int, t2: Int) =>
      
      val unselected = zipper.slice(f1,t1).slice(f2,t2).unselect
      unselected.length mustEqual elems.length
      
      val uc = unselected flatMap {n => n.children}      
      uc.toVectorCase mustEqual texts.slice(f1,t1).slice(f2,t2).toVectorCase
    }
    
    "update the zipper context correctly for multiple slices in bounds" in check { (from1: Int, to1: Int, from2: Int, to2: Int) =>
      val f1 = (from1 & Int.MaxValue) % zipper.length
      val t1 = (to1 & Int.MaxValue) % zipper.length
      val slice1 = zipper.slice(f1,t1)
      
      val f2 = if (slice1.length == 0) 0 else (from2 & Int.MaxValue) % slice1.length
      val t2 = if (slice1.length == 0) 0 else (to2 & Int.MaxValue) % slice1.length
      
      val unselected = slice1.slice(f2,t2).unselect
      unselected.length mustEqual elems.length
      
      val uc = unselected flatMap {n => n.children}      
      uc.toVectorCase mustEqual texts.slice(f1,t1).slice(f2,t2).toVectorCase
    }
  }

  "Zipper.unselect" should {
    "only modify the update times of nodes that receive updates" in {
      val xml = <top><a><b /></a></top>.convert
      val z = xml \\ *
      
      //Conflicting updates.  Second node has latest update time.
      val z1 = z.updated(0,elem("a",elem("c")))
      val z2 = z1.updated(1,elem("b2"))  
      
      //Make an update to the first node only through a child zipper
      val zInner = z2 select 'a
      val zInner1 = zInner.updated(0, zInner(0).asInstanceOf[Elem].copy(name="A"))
      val z3 = zInner1.unselect
 
      z2.unselect mustEqual <top><a><b2 /></a></top>.convert.toGroup
      z3.unselect mustEqual <top><A><c /></A></top>.convert.toGroup

    }
  }
  
  def validate[Expected] = new {
    def apply[A](a: A)(implicit evidence: A =:= Expected) = evidence must not beNull
  }

  def resource(filename: String) =
    XML fromSource (Source fromURL (getClass getResource ("/" + filename)))

  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  def elem(name: String, children: Node*) = Elem(None, name, Attributes(), Map(), Group(children: _*))
  
  def textNode(s: scala.util.matching.Regex) = Selector[Text]({
    case t: Text if s.pattern.matcher(t.text).matches() => t
  })

}