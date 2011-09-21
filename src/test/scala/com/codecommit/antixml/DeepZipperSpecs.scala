package com.codecommit.antixml

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import XML._
import com.codecommit.antixml.DeepZipper._
import scala.io.Source

class DeepZipperSpecs extends SpecificationWithJUnit with ScalaCheck  with XMLGenerators {

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
      (bookstore ~\ 'book unselect) mustEqual bookGroup
      (bookstore ~\\ 'book unselect) mustEqual bookGroup
      (bookstore > 'book unselect) mustEqual bookGroup
      (bookstore ~ 'book unselect) mustEqual bookGroup
      ((bookstore ~ 'book ~\ 'title ~\\ 'author > 'author).unselect.unselect.unselect.unselect) mustEqual bookGroup
    }

    "modify on any level" in {
      val authors = bookstore ~\\ 'author
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
      val titles = bookstore ~ 'book > 'title 
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
    
    val all = bookstore ~\\ * 
    
    "resolve merging problems with the merge strategy 1" in {
      val newBooks = 
        all.
	        updated(0, elem("erased")).
        	updated(3, elem("replaced", Text("foo"))).
        	unselect
        	
      // notice how children are propagated from below  	
      val res = fromString {
      "<erased>" +
        "<book>" +
          "<title>For Whom the Bell Tolls</title>" +
          "<author>Hemmingway</author>" +
        "</book>" +
        "<book>" +
          "<title>I, Robot</title>" +
          "<author>Isaac Asimov</author>" +
        "</book>" +
        "<replaced>foo</replaced>" +
      "</erased>" 
      }

      newBooks mustEqual Group(res)
        
    }
    
    "resolve merging problems with the merge strategy 2" in {
      val newBooks =
        all.
          updated(3, elem("replaced", Text("foo"))).
          updated(0, elem("erased")).
          unselect

      // this time the children are ignored, because the last change is at the root
      val res = <erased/>.convert

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

      val res = fromString {
        "<boo>" +
          "<boo>" +
          "<boo />" +
          "<boo />" +
          "</boo>" +
          "<boo>" +
          "<boo />" +
          "<boo />" +
          "</boo>" +
          "<boo>" +
          "<boo />" +
          "<boo />" +
          "<boo />" +
          "</boo>" +
          "</boo>"
      }

      newBooks mustEqual Group(res)
          
    }
  }

  /* 
   * Copied the selection tests below from GroupSpecs and ZipperSpecs, modified them slightly to work with the DeepZipper
   * Using the following rules:
   *  \ is > on zipper 
   *  \\ is ~ on zipper
   *  
   *  Text selector currently don't work on the zipper.
   */

  "DeepZipper updates within '>' results" should {
    "rebuild from empty result set" in {
      val xml = Group(<parent><child/><child/></parent>.convert)
      (xml > 'foo).unselect mustEqual xml
      (bookstore > 'book > 'foo).unselect mustEqual (bookstore > 'book)
      (bookstore > 'book > 'foo).unselect.unselect mustEqual Group(bookstore)
    }

    "rebuild updated at one level" in {
      val books = bookstore > "book"
      val book0 = books(0).copy(attrs = Attributes("updated" -> "yes"))
      val book2 = books(2).copy(attrs = Attributes("updated" -> "yes"))

      val bookstore2: Group[Node] = books.updated(0, book0).updated(2, book2).unselect // ensure we have NodeSeq

      // find afresh without using >
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes()
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }

    "rebuild updated at two levels" in {
      val authors = bookstore > "book" > "author"
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
      val books = bookstore > "book"
      val books2 = books map { _.copy(attrs=Attributes("updated" -> "yes")) }
      
      val bookstore2: Group[Node] = books2.unselect
      bookstore2.head.asInstanceOf[Elem].children must haveSize(3)
      
      // find afresh without using >                                                    
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Attributes("updated" -> "yes")
    }
    
     "rebuild after a drop at the first level" in {
      val books = bookstore > "book"
      val books2 = books drop 2
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyPS
    }
    
    "rebuild after a slice at the first level" in {
      val books = bookstore > "book"
      val books2 = books slice (2, 3)
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyPS
    }
    
    "rebuild after a take at the first level" in {
      val books = bookstore > "book"
      val books2 = books take 1
      val bookstore2: Group[Node] = books2.unselect
      
      bookstore2 mustEqual onlyBell
    }
    
    "rebuild after a splitAt at the first level" in {
      val books = bookstore > "book"
      val (books2, books3) = books splitAt 1
      val books4 = books3 drop 1
      val bookstore2: Group[Node] = books2.unselect
      val bookstore4: Group[Node] = books4.unselect
      
      bookstore2 mustEqual onlyBell
      bookstore4 mustEqual onlyPS
    }
           
    "rebuild after a flatMap at the first level" in {
      val books = bookstore > "book"
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
      
      (bookstore2 > "book" > "title" > *) must beLike((_:Node) match { case Text("Programming Scala") => ok }).forall
    }
    
    // my attempt at a "real world" test case"
    "rebuild after non-trivial for-comprehension" in {
      val titledBooks = for {
        bookElem <- bookstore > "book"
        title <- bookElem > "title" > text
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
        val xml2 = (xml > "sub") map identity unselect
        
        xml2 mustEqual xml
      }
      
      "with prefix miss" in {
        val xml = Group(<miss/>.convert, <parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><sub bar="test2"/></parent>.convert)
        val xml2 = (xml > "sub") map identity unselect
        
        xml2 mustEqual xml
      }
      
      "with prefix and suffix miss" in {
        val xml = Group(<miss/>.convert, <parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><sub bar="test2"/></parent>.convert, <miss/>.convert)
        val xml2 = (xml > "sub") map identity unselect
        
        xml2 mustEqual xml
      }
    }
    
    "rebuild following identity map with selection miss at the second level" in {
      val xml = Group(<parent><sub>Test1</sub><sub foo="test">Test2<subsub/></sub><miss/><sub bar="test2"/></parent>.convert)
      val xml2 = (xml > "sub") map identity unselect
      
      xml2 mustEqual xml
    }
    
    "rebuild following filter at the first level" in {
      val books = bookstore > 'book
      val bookstore2 = (books filter (books(1) !=)).unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
          children must haveSize(2)
          children > 'title > text mustEqual Vector("For Whom the Bell Tolls", "Programming Scala")
        }
      }
    }
    
    "rebuild following composed filters" in {
      val books = bookstore > 'book
      val bookstore2 = (books filter (books(0) !=) filter (books(1) !=)).unselect
      
      bookstore2.head must beLike {
        case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
          children must haveSize(1)
          children > 'title > text mustEqual Vector("Programming Scala")
        }
      }
    }
    
    "rebuild second level siblings following filter at the second level" in {
      val titles = bookstore > 'book > 'title
      val books2 = (titles filter (titles(1) ==)).unselect
      
      books2 must haveSize(3)
      books2(0) mustEqual <book><author>Hemmingway</author></book>.convert
      books2(1) mustEqual <book><title>I, Robot</title><author>Isaac Asimov</author></book>.convert
      books2(2) mustEqual <book><author>Dean Wampler</author><author>Alex Payne</author></book>.convert
    }

        "rebuild following filter at the second level" in {
          val titles = bookstore > 'book > 'title
          val bookstore2 = (titles filter (titles(1) !=)).unselect.unselect
          
          bookstore2.head must beLike {
            case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty =>
              children must haveSize(3)
          }
          
          val titles2 = bookstore2 > 'book > 'title
          titles2 must haveSize(2)
          (titles2 > text) mustEqual Vector("For Whom the Bell Tolls", "Programming Scala")
        }

        "rebuild following 2 filters at the first level" in {
          val books = bookstore > 'book
          val bookstore2 = (books filter (books(1) !=) filter (books(0) !=)).unselect
          
          bookstore2.head must beLike {
            case Elem(None, "bookstore", attrs, scopes, children) if attrs.isEmpty && scopes.isEmpty => {
              children must haveSize(1)
              children > 'title > text mustEqual Vector("Programming Scala")
            }
          }      
        }

    "preserve flatMap order" in {
      val original = <top><a/></top>.convert
      val expanded = original > 'a flatMap {
        case e: Elem => for (i <- 0 until 10) yield e.copy(name = e.name + i)
      }
      val modified = expanded.unselect

      modified must haveSize(1)
      modified(0) mustEqual <top><a0/><a1/><a2/><a3/><a4/><a5/><a6/><a7/><a8/><a9/></top>.convert
    }

    "utility methods on DeepZipper" >> {
      implicit val arbInt = Arbitrary(Gen.choose(0, 10))

      "identity collect should return self" in check { (xml: Group[Node], n: Int) =>
        val func = (0 until n).foldLeft(identity: DeepZipper[Node] => DeepZipper[Node]) { (g, _) =>
          g andThen { _ collect { case e => e } }
        }

        func(xml.toDeepZipper) mustEqual xml
      }

      "identity filter should return self" in check { xml: Group[Node] =>
        val result = xml.toZipper filter Function.const(true)
        result mustEqual xml
      }

      "identity filter and unselect should return self" in check { xml: Group[Node] =>
        val sub = xml > *
        (sub filter Function.const(true) unselect) mustEqual xml
      }
    }
  }

  "Shallow selection on a DeepZipper" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns > "parent" mustEqual Group(elem("parent"))
    }

    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns > "parent" mustEqual Group(elem("parent"))
      ns > "parent" mustEqual Group(elem("parent"))
    }

    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns > "a" mustEqual result
    }
  }

  "Deep selection on DeepZipper" should {
    "return something of type DeepZipper on element select" in {
      val ns = <foo/>.convert
      validate[DeepZipper[Elem]](ns ~ 'bar)
    }

    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns ~ "parent" mustEqual Group(elem("parent"))
    }

    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns ~ "a" mustEqual result
    }

    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val result = fromString("<parent><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><target>outside</target><target>sub1</target><target>top3-outer</target><target>phoney</target><target>top1</target><target>top2</target></parent>")
      ns ~ "target" mustEqual result.children
    }
  }

  def validate[Expected] = new {
    def apply[A](a: A)(implicit evidence: A =:= Expected) = evidence must not beNull
  }

  def resource(filename: String) =
    XML fromSource (Source fromURL (getClass getResource ("/" + filename)))

  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  def elem(name: String, children: Node*) = Elem(None, name, Attributes(), Map(), Group(children: _*))

}