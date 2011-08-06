package com.codecommit.antixml

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._
import org.specs2.matcher.DataTables
import XML._
import com.codecommit.antixml.DeepZipper._
import scala.io.Source

class DeepZipperSpecs extends SpecificationWithJUnit with ScalaCheck with DataTables with XMLGenerators {

  implicit val params = set(maxSize -> 10, minTestsOk -> 20)
  val bookstore = resource("bookstore.xml")

  // This is by no means exhaustive, just showing off what's possible.

  "Deep Zipper selection" should {
    // I'm lazy and I don't have monadic operations on the zipper, so I'm going primitive.
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

  "Zipper updates within '>' results" should {
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

  "A basic merge startegy" should {
    def maxTime(s: Seq[(Node, Int)]) = s.maxBy(_._2)

    "fail on empty lists" in {
      val n = Text("foo")
      BasicNodeMergeStrategy(n, List[(Node, Int)]()) must throwA[Exception]
    }

    implicit def timeNodes = Arbitrary { for { n <- nodeGenerator(1); t <- Gen.posNum[Int] } yield (n, t) }

    "select the newest node when the original is not an elem" in check { alts: List[(Node, Int)] =>
      !alts.isEmpty ==> {
        (BasicNodeMergeStrategy(_: Node, alts) mustEqual maxTime(alts)).forall {
          Seq(ProcInstr("foo", "bar"), Text("baz"), CDATA("qux"), EntityRef("somethin"))
        }
      }
    }

    "select the newest node when no elems are present in the alternatives" in check { alts: List[(Node, Int)] =>
      val noElems = alts.filterNot(_._1.isInstanceOf[Elem])
      !noElems.isEmpty ==> {
        BasicNodeMergeStrategy(elem("fo0"), noElems) mustEqual noElems.maxBy(_._2)
      }
    }

    val orig = elem("orig", Text("foo"))
    val alt1 = (orig.copy(name = "alt1"), 2)
    val alt2 = (orig.copy(name = "alt2"), 5)
    val alt3 = (orig.copy(name = "orig", children = Group(Text("bar"), Text("baz"))), 4)
    val alt4 = (orig.copy(name = "alt4", children = Group()), 3)
    val text = (Text("baz"), 3)

    "merge elems by taking the most recent children and most recent non child modification" in {
      val alts1 = Seq(alt1, alt2, text, alt3, alt4)
      BasicNodeMergeStrategy(orig, alts1) mustEqual (orig.copy(name = alt2._1.name, children = alt3._1.children), 5)
    }

    "keep the children intact if they were not modified" in {
      val alts2 = Seq(alt1, alt2, text)
      BasicNodeMergeStrategy(orig, alts2) mustEqual (orig.copy(name = alt2._1.name), 5)
    }

    "keep non children fields intact if they were not modified" in {
      val alts3 = Seq(text, alt3, alt4)
      BasicNodeMergeStrategy(orig, alts3) mustEqual (orig.copy(children = alt3._1.children), 4)
    }
  }

  "Paths" should {
    import PathCreator._

    val s = *

    "ignore empty groups" in {
      val empty = Group()

      fromNodes(s)(empty) mustEqual Nil
      all(s)(empty) mustEqual Nil
      directChildren(s)(empty) mustEqual Nil
      allChildren(s)(empty) mustEqual Nil
    }

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
    def nl(n: Node, l: Int) = NodeLoc(n, l)

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

    "take from the root of the nodes" in {
      fromNodes(s)(group) mustEqual root
    }

    "take the children of the root nodes" in {
      directChildren(s)(group) mustEqual directChild
    }

    "take all the nodes recursively, breadth first" in {
      all(s)(group) mustEqual root ::: directChild ::: rest
    }

    "take all children nodes recursively, breadth first" in {
      allChildren(s)(group) mustEqual directChild ::: rest
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
      all(selDeep)(group) mustEqual selResRoot ::: selResNoRoot
    }

    "apply selectors recursively on the children" in {
      allChildren(selDeep)(group) mustEqual selResNoRoot
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