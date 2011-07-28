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
import org.specs2.matcher.MustExpectable._
import org.scalacheck._

import scala.collection.immutable.Map
import scala.io.Source

class ZipperSpecs extends Specification with ScalaCheck with XMLGenerators {
  import Prop._
  
  lazy val numProcessors = Runtime.getRuntime.availableProcessors()
  implicit val params = set(workers -> numProcessors, maxSize -> 15)      // doesn't need to be that large
  
  val bookstore = resource("bookstore.xml")
  val onlyBell = Group(<bookstore><book><title>For Whom the Bell Tolls</title><author>Hemmingway</author></book></bookstore>.convert)
  val onlyPS = Group(<bookstore><book><title>Programming Scala</title><author>Dean Wampler</author><author>Alex Payne</author></book></bookstore>.convert)
  
  "Zipper#stripZipper" should {
    "effectively strip zipper context" in {
      val books = bookstore \ "book"
      books.stripZipper.isInstanceOf[Zipper[Node]] mustEqual false
    }
  }
  
  "zipper updates within '\\' results" should {
    "rebuild from empty result set" in {
      val xml = Group(<parent><child/><child/></parent>.convert)
      (xml \ 'foo).unselect mustEqual xml
      (bookstore \ 'book \ 'foo).unselect mustEqual (bookstore \ 'book)
      (bookstore \ 'book \ 'foo).unselect.unselect mustEqual Group(bookstore)
    }
    
    "rebuild updated at one level" in {
      val books = bookstore \ "book"
      val book0 = books(0).copy(attrs=Attributes("updated" -> "yes"))
      val book2 = books(2).copy(attrs=Attributes("updated" -> "yes"))
      
      val bookstore2: Group[Node] = books.updated(0, book0).updated(2, book2).unselect    // ensure we have NodeSeq
      
      // find afresh without using \
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes()
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }
    
    "rebuild updated at two levels" in {
      val authors = bookstore \ "book" \ "author"
      val author0 = authors(0).copy(attrs=Attributes("updated" -> "yes"))
      val author2 = authors(2).copy(attrs=Attributes("updated" -> "yes"))
      val author3 = authors(3).copy(attrs=Attributes("updated" -> "yes"))
      
      val bookstore2: Group[Node] = authors.updated(0, author0).updated(2, author2).updated(3, author3).unselect.unselect
      
      // find afresh without using \
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
      
      // find afresh without using \                                                    
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
      
      // find afresh without using \
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
      val original = <top><a /></top>.convert
      val expanded = original \ 'a flatMap {
        case e: Elem => for(i <- 0 until 10) yield e.copy(name=e.name+i)
      }
      val modified = expanded.unselect

      modified must haveSize(1)
      modified(0) mustEqual <top><a0 /><a1 /><a2 /><a3 /><a4 /><a5 /><a6 /><a7 /><a8 /><a9 /></top>.convert      
    }
  }

  "zipper updates within '\\^' results" should {
    val topLevel = Group(<a1 ><a1b1 /><a1b2 /></a1>.convert, <a2><a2b1 /><a2b2 /></a2>.convert)
    
    "rebuild from non-trivial selector" in {
      val second = topLevel \^ 'a2
      
      val changed = second map { e=>
        e.copy(children = e.children :+ <zzz />.convert)
      }
      
      val rebuild = changed.unselect
      
      rebuild must haveSize(2)
      rebuild(0) mustEqual <a1 ><a1b1 /><a1b2 /></a1>.convert
      rebuild(1) mustEqual <a2><a2b1 /><a2b2 /><zzz /></a2>.convert
      
    }
    
    "rebuild when source group comes from '\\' selection" in {
      val original = <top><a /></top>.convert
      val expanded = original \ 'a flatMap {
        case e: Elem => for(i <- 0 until 10) yield e.copy(name=e.name+i)
      }
    
      val filtered = expanded \^ elementWhere {e:Elem => ((e.name.substring(1).toInt) % 2) == 0}

      val modified = filtered map {e => e.copy(name="z"+e.name)}
      
      val result = modified.unselect.unselect
      
      result must haveSize(1)
      result(0) mustEqual <top><za0 /><a1 /><za2 /><a3 /><za4 /><a5 /><za6 /><a7 /><za8 /><a9 /></top>.convert      
    }
    
    "rebuild from empty result set" in {
      val xml = Group(<parent><child/><child/></parent>.convert)
      (xml \^ 'foo).unselect mustEqual xml
      (bookstore \ 'book \^ 'foo).unselect mustEqual (bookstore \ 'book)
      (bookstore \^ 'bookstore \^ 'foo).unselect mustEqual (bookstore \^ 'bookstore)
      (bookstore \ 'book \^ 'foo).unselect.unselect mustEqual Group(bookstore)
      (bookstore \^ 'bookstore \^ 'foo).unselect.unselect mustEqual Group(bookstore)
    }
    
  }

  "zipper updates within '\\!' results" should {
    
    "rebuild after deep select" in {
      val authors = bookstore \\! "author"
      val author0 = authors(0).copy(attrs=Attributes("updated" -> "yes", "timestamp"->"1"))
      val author2 = authors(2).copy(attrs=Attributes("updated" -> "yes", "timestamp"->"2"))
      val author3 = authors(3).copy(attrs=Attributes("updated" -> "yes", "timestamp"->"3"))
      
      val bookstore2: Group[Node] = authors.updated(0, author0).updated(2, author2).updated(3, author3).unselect
      
      // find afresh without using \
      bookstore2.head.asInstanceOf[Elem].name mustEqual "bookstore"
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].name mustEqual "book"
      
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children must haveSize(2)
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes", "timestamp"->"1")
      
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children must haveSize(2)
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes()
      
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children must haveSize(3)
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes", "timestamp"->"2")
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes", "timestamp"->"3")
    }

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
  }
  
  def resource(filename: String) =
    XML fromSource (Source fromURL (getClass getResource ("/" + filename)))   // oooh, lispy!
   
  def elementWhere(pred: Elem => Boolean) = 
    new Selector[Elem] {
      override val elementName = None

      def apply(n: Node) = n.asInstanceOf[Elem]

      def isDefinedAt(n: Node) = n match {
        case e:Elem => pred(e)
        case _ => false
      }
  }

}
