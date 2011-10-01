package com.codecommit.antixml.util

import scala.collection.generic.CanBuildFrom
import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.specs2.matcher.{Expectable, Parameters, Matcher}
import org.scalacheck.{Arbitrary, Prop, Gen}
import org.specs2.matcher.ScalaCheckMatchers._

trait OrderPreservingMapSpecs extends Specification with ScalaCheck {
  import Prop._
  import OrderPreservingMapGenerators._ 
  
  def buildMap[A,B](e: Entries[A,B]): OrderPreservingMap[A,B]
  def builderDescription: String
  
  textFragment(builderDescription)
    
  //implicit def defaultParams = set(minTestsOk->500)
  
  "order preserving maps should" should {
    implicit def testDomain = Arbitrary(byteKeyEntries[Byte](0,50))
  
    "preserve their build order" in check { e: Entries[Byte,Byte] =>
      buildMap(e) must haveSameOrderAs(e.entries)
    }
    "correctly report their size" in check { e: Entries[Byte,Byte] =>
      buildMap(e).size must beEqualTo(e.size)
    }
    "support empty" in check { e: Entries[Byte,Byte] =>
      val empty:OrderPreservingMap[Byte,Byte] = buildMap(e).empty
      empty.size must beEqualTo(0)
      empty must haveSameOrderAs(Nil)
    }
    "support get of key not in map"  in check { e: Entries[Byte,Byte] =>
      val map = buildMap(e)
      val missingKeys = complement(e.keys)
      val results = for(k <- missingKeys) yield map.get(k)
      results must haveSameOrderAs(missingKeys.map((Byte) => None))
    }
    "support add of key not in map via +"  in check { e: Entries[Byte,Byte] =>
      forAll(Gen.oneOf(complement(e.keys))) { k =>
        forAll(Gen.oneOf(allBytes)) { v =>
          val added = (k,v)
          val orig = buildMap(e)
          val updated = orig + added
          
          updated.size must beEqualTo(e.size+1)
          orig.get(k) must beEqualTo(None)
          updated.get(k) must beEqualTo(Some(v))
          updated must haveSameOrderAs(e.entries :+ added)
          updated.init must haveSameOrderAs(e.entries)
        }
      }
    }
    "support arbitrary sequences of operations" in check { e: Entries[Byte,Byte] =>
      forAll(Gen.listOfN(1000, genMapOp[Byte,Byte])) { ops =>
        val (map, expect) = ((buildMap(e),e.entries) /: ops) { (itm, op) =>
          val (m,e) = itm
          op match {
            case Add(k,v) => (m + ((k,v)), updateEntry(e,k,v))
            case Remove(k) => (m - k, removeEntry(e, k))
          }
        }
        map must haveSameOrderAs(expect)
      }
    }
  } 
  
  "non-empty order preserving maps" should {
    implicit def testDomain = Arbitrary(byteKeyEntries[Byte](1,50))
    
    "support removal of first element" in check { e: Entries[Byte,Byte] =>
      val map = buildMap(e) - e.keys.head
      
      map.size must beEqualTo(e.size - 1)
      safeHead(map) must beEqualTo(safeHead(e.entries.tail))
      map must haveSameOrderAs(e.entries.tail)
    }

    "support removal of last element" in check { e: Entries[Byte,Byte] =>
      val map = buildMap(e) - e.keys.last
      
      map.size must beEqualTo(e.size - 1)
      safeLast(map) must beEqualTo(safeLast(e.entries.init))
      map must haveSameOrderAs(e.entries.init)
    }
    
    "support removal of arbitrary element" in check { e: Entries[Byte,Byte] =>
      forAll(Gen.choose(0,e.size-1)) { n =>
        val map = buildMap(e) - e.keys(n)
        val expectedEntries = (e.entries take n) ++ (e.entries drop (n+1))
        
        map.size must beEqualTo(e.size - 1)
        safeHead(map) must beEqualTo(safeHead(expectedEntries))
        safeLast(map) must beEqualTo(safeLast(expectedEntries))
        map must haveSameOrderAs(expectedEntries)
      }
    }
    
    "support removal of each element" in check { e: Entries[Byte,Byte] =>
      //This test is similar to the previous one, except it test all indecies
      //rather than arbitrary ones, but gives less useful information on failure.
      val map = buildMap(e)
      for(indx <- 0 until e.size) {
        val removed = map - e.keys(indx)
        val expectedEntries = (e.entries take indx) ++ (e.entries drop (indx+1))
        
        removed.size must beEqualTo(e.size - 1)
        safeHead(removed) must beEqualTo(safeHead(expectedEntries))
        safeLast(removed) must beEqualTo(safeLast(expectedEntries))
        removed must haveSameOrderAs(expectedEntries)
      }
      endOfCheck
    }

    "support change of first value via +" in check { e: Entries[Byte,Byte] =>
      val changeTo = (e.keys.head , (e.values.head + 1).toByte)
      val map = buildMap(e) + changeTo
      
      map.size must beEqualTo(e.size)
      map.head must beEqualTo(changeTo)
      map.head must not(beEqualTo(e.entries.head))
      map.get(e.keys.head) must beEqualTo(Some(changeTo._2))
      map must haveSameOrderAs(IndexedSeq(changeTo) ++ e.entries.tail)
    }

    "support change of last value via +" in check { e: Entries[Byte,Byte] =>
      val changeTo = (e.keys.last , (e.values.last + 1).toByte)
      val map = buildMap(e) + changeTo
      
      map.size must beEqualTo(e.size)
      map.last must beEqualTo(changeTo)
      map.last must not(beEqualTo(e.entries.last))
      map.get(e.keys.last) must beEqualTo(Some(changeTo._2))
      map must haveSameOrderAs(e.entries.init :+ changeTo)
    }
    
    "support change of arbitrary value via +" in check { e: Entries[Byte,Byte] =>
      forAll(Gen.choose(0,e.size-1)) { n =>
        val changeTo = (e.keys(n), (e.values(n) + 1).toByte)
        val map = buildMap(e) + changeTo
        
        map.size must beEqualTo(e.size)
        map.toIndexedSeq(n) must beEqualTo(changeTo)
        map.toIndexedSeq(n) must not(beEqualTo(e.entries(n)))
        map.get(e.keys(n)) must beEqualTo(Some(changeTo._2))
        map must haveSameOrderAs((e.entries take n) ++ IndexedSeq(changeTo) ++ (e.entries drop (n+1) ))
      }
    }
    
    "support change of each element via + " in check { e: Entries[Byte,Byte] =>
      //This test is similar to the previous one, except it test all indecies
      //rather than arbitrary ones, but gives less useful information on failure.
      val map = buildMap(e)
      for(n <- 0 until e.size) {
        val changeTo = (e.keys(n), (e.values(n) + 1).toByte)
        val changed = map + changeTo
        val expectedEntries = e.entries.updated(n, changeTo)
        
        changed.size must beEqualTo(e.size)
        safeHead(changed) must beEqualTo(safeHead(expectedEntries))
        safeLast(changed) must beEqualTo(safeLast(expectedEntries))
        changed must haveSameOrderAs(expectedEntries)
      }
      endOfCheck
    }

    
    "support get of all keys contained in the map" in check { e: Entries[Byte,Byte] =>
      val map = buildMap(e)
      val results = for((key,value) <- e.entries) yield map.get(key)
      
      results must haveSameOrderAs(e.values map {Some(_)})
    }
    
    "support head" in check { e: Entries[Byte,Byte] =>
      buildMap(e).head must beEqualTo(e.entries.head)
    }
    "support last" in check { e: Entries[Byte,Byte] =>
      buildMap(e).last must beEqualTo(e.entries.last)
    }
    "support tail" in check { e: Entries[Byte,Byte] =>
      buildMap(e).tail must haveSameOrderAs(e.entries.tail)
    }
    "support init" in check { e: Entries[Byte,Byte] =>
      buildMap(e).init must haveSameOrderAs(e.entries.init)
    }
    
  }
  
  
  def removeEntry[A,B](entries: Vector[(A,B)], key:A): Vector[(A,B)] =
    entries filter {_._1 != key}
  def updateEntry[A,B](entries: Vector[(A,B)], key:A, value:B): Vector[(A,B)] = 
    if (entries exists {_._1 == key})
      entries map {kv => if (kv._1 == key) (kv._1, value) else kv}
    else
      entries :+ (key,value)
  
  
  /** Result to return from a check function if nothing else is convenient. */
  val endOfCheck = org.specs2.execute.Success("Dummy")
     
  def safeHead[A](t: Traversable[A]):Option[A] = if (t.isEmpty) None else Some(t.head)

  def safeLast[A](t: Traversable[A]):Option[A] = if (t.isEmpty) None else Some(t.last)
  
  def haveSameOrderAs(e: Iterable[Any]) = haveSameIterationOrderAs(e) and haveSameTraversalOrderAs(e)
  
  def haveSameTraversalOrderAs(e: Traversable[Any]) = new Matcher[Traversable[Any]] {
    def apply[S <: Traversable[Any]](traversable: Expectable[S]) = {
      def traverse[T] (trav: Traversable[T]):List[T] = {
        val builder = List.newBuilder[T]
        trav foreach {builder += _}
        builder.result
      }
      result(traverse(traversable.value)==traverse(e),
        ""+traversable.description+" is traversal-order-equivalent to "+e,
        ""+traversable.description+" is not traversal-order-equivalent to "+e,
        traversable)
    }
  }
  
  def haveSameIterationOrderAs(e: Iterable[Any]) = new Matcher[Iterable[Any]] {
    def apply[S <: Iterable[Any]](iterable: Expectable[S]) = {
      def iterate [T] (iterable: Iterable[T]):List[T] = {
        val builder = List.newBuilder[T]
        val iter = iterable.iterator
        while(iter.hasNext)
          builder += iter.next
        builder.result
      }
      result(iterate(iterable.value)==iterate(e),
        ""+iterable.description+" is iteration-order-equivalent to "+e,
        ""+iterable.description+" is not iteration-order-equivalent to "+e,
        iterable)
    }
  }
} 

class DefaultOrderPreservingBuilderSpecs extends OrderPreservingMapSpecs {
  import OrderPreservingMapGenerators.Entries

  def buildMap[A,B](e: Entries[A,B]): OrderPreservingMap[A,B] = 
    OrderPreservingMap(e.entries:_*)
  def builderDescription: String =
    "When using the default OrderPreservingMap builder,"
}

class LinkedOrderPreservingBuilderSpecs extends OrderPreservingMapSpecs {
  import OrderPreservingMapGenerators.Entries

  def buildMap[A,B](e: Entries[A,B]): LinkedOrderPreservingMap[A,B] = 
    LinkedOrderPreservingMap(e.entries:_*)
  def builderDescription: String =
    "When using the LinkedOrderPreservingMap builder,"
}

