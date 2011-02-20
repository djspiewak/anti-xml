package com.codecommit.antixml

import org.specs._
import scala.io.Source

object ZipperSpecs extends Specification {
  val bookstore = resource("bookstore.xml")
  
  "zipper updates within '\\' results" should {
    "rebuild updated at one level" in {
      val books = bookstore \ "book"
      val book0 = books(0).copy(attrs=Map("updated" -> "yes"))
      val book2 = books(2).copy(attrs=Map("updated" -> "yes"))
      
      val bookstore2: Group[Node] = books.updated(0, book0).updated(2, book2).up    // ensure we have NodeSeq
      
      // find afresh without using \
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].attrs mustEqual Map("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Map()
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Map("updated" -> "yes")
    }
    
    "rebuild updated at two levels" in {
      val authors = bookstore \ "book" \ "author"
      val author0 = authors(0).copy(attrs=Map("updated" -> "yes"))
      val author2 = authors(2).copy(attrs=Map("updated" -> "yes"))
      val author3 = authors(3).copy(attrs=Map("updated" -> "yes"))
      
      val bookstore2: Group[Node] = authors.updated(0, author0).updated(2, author2).updated(3, author3).up.up
      
      // find afresh without using \
      bookstore2.head.asInstanceOf[Elem].name mustEqual "bookstore"
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].name mustEqual "book"
      
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children.length mustBe 2
      bookstore2.head.asInstanceOf[Elem].children(0).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Map("updated" -> "yes")
      
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children.length mustBe 2
      bookstore2.head.asInstanceOf[Elem].children(1).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Map()
      
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children.length mustBe 3
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(1).asInstanceOf[Elem].attrs mustEqual Map("updated" -> "yes")
      bookstore2.head.asInstanceOf[Elem].children(2).asInstanceOf[Elem].children(2).asInstanceOf[Elem].attrs mustEqual Map("updated" -> "yes")
    }
  }
  
  def resource(filename: String) =
    XML fromSource (Source fromURL (getClass getResource ("/" + filename)))   // oooh, lispy!
}
