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
import org.specs2.ScalaCheck
import org.specs2.execute.Result
import org.specs2.matcher.Parameters
import org.scalacheck.{Arbitrary, Prop, Gen, Choose}
import Prop._
import org.specs2.matcher.ScalaCheckMatchers._
import scala.math.Ordering

class ZipperHoleMapSpecs extends Specification with ScalaCheck {
  
  implicit object ZipperPathLexOrder extends Ordering[ZipperPath] {
    private val delg = Ordering.Iterable[Int]
    def compare(x: ZipperPath, y: ZipperPath) = delg.compare(x,y)
  }
  
  def p(i: Int*) = ZipperPath(i:_*)
  
  def toMap[B](zp: ZipperHoleMap[B]) = Map(zp.depthFirst.toSeq:_*)
  
  def extensionsOf(zp: ZipperPath) = new {
    def in[B](m: Map[ZipperPath,B]): Map[ZipperPath,B] = m.collect {
      case (k,v) if k.startsWith(zp) && k.length > zp.length => (k.drop(zp.length), v)        
    }
  }
  
  "ZipperHoleMap.depthFirst" should {
    "traverse lexicographically" in {
      forAll(Gen.listOf(saneEntries[Int])) {entries => 
        val df = ZipperHoleMap(entries:_*).depthFirst
        val expectedOrder = Map(entries:_*).toSeq.sortBy(_._1)
        List(df.toSeq:_*) mustEqual List(expectedOrder:_*)
      }
    }
  }
  
  "ZipperHoleMap.apply" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,2,3)->"c", p(3,0,0)->"d")
    "find leaf values" in {
      hm(2) mustEqual("b")
    }
    "find intermediate values" in {
      hm(1) mustEqual("a")
    }
    "throw on non-existent positions" in {
      hm(-1) must throwA[Throwable]
      hm(0) must throwA[Throwable]
      hm(4) must throwA[Throwable]
    }
    "throw on non-valued intermediate nodes" in {
      hm(3) must throwA[Throwable]
    }
    "work with arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = for(i <- (minSaneLoc - 5) to (maxSaneLoc + 5)) yield {
          if (m.contains(p(i))) {
            hm(i) mustEqual m(p(i))
          } else {
            hm(i) must throwA[Throwable]
          }
        }
        r
      }
    }
  }

  "ZipperHoleMap.get" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,2,3)->"c", p(3,0,0)->"d")
    "find leaf values" in {
      hm.get(2) mustEqual(Some("b"))
    }
    "find intermediate values" in {
      hm.get(1) mustEqual(Some("a"))
    }
    "return None on non-existent positions" in {
      hm.get(-1) mustEqual None
      hm.get(0) mustEqual None
      hm.get(4) mustEqual None
    }
    "return None on non-valued intermediate nodes" in {
      hm.get(3) mustEqual None
    }
    "work with arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = for(i <- (minSaneLoc - 5) to (maxSaneLoc + 5)) yield {
          hm.get(i) mustEqual m.get(p(i))
        }
        r
      }
    }
  }
  
  "ZipperHoleMap.contains" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,2,3)->"c", p(3,0,0)->"d")
    "find leaf values" in {
      hm.contains(2) must beTrue
    }
    "find intermediate values" in {
      hm.contains(1) must beTrue
    }
    "return None on non-existent positions" in {
      hm.contains(-1) must beFalse
      hm.contains(0) must beFalse
      hm.contains(4) must beFalse
    }
    "return None on non-valued intermediate nodes" in {
      hm.contains(3) must beFalse
    }
    "work with arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = for(i <- (minSaneLoc - 5) to (maxSaneLoc + 5)) yield {
          hm.contains(i) mustEqual m.contains(p(i))
        }
        r
      }
    }
  }
  
  "ZipperHoleMap.children" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,1)->"c", p(1,2,3)->"d", p(1,2,4)->"e", p(3,0,0)->"f")
    "throw on leaf values" in {
      hm.children(2) must throwA[Throwable]
    }
    "find intermediate valued nodes" in {
      val c = hm.children(1) 
      toMap(c) mustEqual Map(p(1)->"c",p(2,3)->"d",p(2,4)->"e")
    }
    "find intermediate non-valued nodes" in {
      val c = hm.children(3) 
      toMap(c) mustEqual Map(p(0,0)->"f")
    }
    "throw on non-existent positions" in {
      hm.children(-1) must throwA[Throwable]
      hm.children(0) must throwA[Throwable]
      hm.children(4) must throwA[Throwable]
    }
    "work with arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = for(i <- (minSaneLoc - 5) to (maxSaneLoc + 5)) yield {
          val expect = extensionsOf(p(i)).in(m)
          if (expect.isEmpty)
            hm.children(i) must throwA[Throwable]
          else
            toMap(hm.children(i)) mustEqual expect
        }
        r
      }
    }
  }
  
  "ZipperHoleMap.hasChildrenAt" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,1)->"c", p(1,2,3)->"d", p(1,2,4)->"e", p(3,0,0)->"f")
    "return false on leaf values" in {
      hm.hasChildrenAt(2) must beFalse
    }
    "return true on intermediate valued nodes" in {
      hm.hasChildrenAt(1) must beTrue
    }
    "return true on intermediate non-valued nodes" in {
      hm.hasChildrenAt(3) must beTrue
    }
    "return false on non-existent positions" in {
      hm.hasChildrenAt(-1) must beFalse
      hm.hasChildrenAt(0) must beFalse
      hm.hasChildrenAt(4) must beFalse
    }
    "work with arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = for(i <- (minSaneLoc - 5) to (maxSaneLoc + 5)) yield {
          val expect = extensionsOf(p(i)).in(m)
          hm.hasChildrenAt(i) mustEqual (!expect.isEmpty)
        }
        r
      }
    }
  }
  
  "ZipperHoleMap.getDeep" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,2)->"c", p(1,2,3)->"d", p(1,2,4)->"e", p(3,0,0)->"f")
    val hmMap = toMap(hm)
    "find leaf values" in Seq(
      hm.getDeep(p(2)) mustEqual Some("b"),
      hm.getDeep(p(1,2,3)) mustEqual Some("d"),
      hm.getDeep(p(1,2,4)) mustEqual Some("e"),
      hm.getDeep(p(3,0,0)) mustEqual Some("f")
    )
    
    "find intermediate valued nodes" in Seq(
      hm.getDeep(p(1)) mustEqual Some("a"),
      hm.getDeep(p(1,2)) mustEqual Some("c")
    )
    "return None on intermediate non-valued nodes" in Seq(
      hm.getDeep(p(3)) mustEqual None,
      hm.getDeep(p(3,0)) mustEqual None
    )
    "return None on non-existent positions" in Seq(
      hm.getDeep(p(-1)) mustEqual None,
      hm.getDeep(p(-1,0)) mustEqual None,
      hm.getDeep(p(0)) mustEqual None,
      hm.getDeep(p(0,1,2,3)) mustEqual None,
      hm.getDeep(p(1,2,3,4)) mustEqual None,
      hm.getDeep(p(4)) mustEqual None,
      hm.getDeep(p(4,4,4,4,4)) mustEqual None
    )
    "work with arbitrary entries and paths" in {
      forAll(Gen.listOf(saneEntries[Int]), sanePaths) { (entries,path) =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        hm.getDeep(path) mustEqual m.get(path)
      }
    }
    "find all of its arbitrary entries" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        val r:Result = if (!entries.isEmpty) { 
          for(path <- m.keys.toSeq) yield hm.getDeep(path) mustEqual Some(m(path))
        } else {
          //specs chokes on an empty Result sequence, so do something else for the empty case
          hm.getDeep(p(0)) mustEqual None
        }
        r
      }
    }
  }
  
  "ZipperHoleMap.updatedDeep" should {
    val hm = ZipperHoleMap(p(1)->"a", p(2)->"b", p(1,2)->"c", p(1,2,3)->"d", p(1,2,4)->"e", p(3,0,0)->"f")
    val hmMap = toMap(hm)
    "replace leaf values" in Seq(
      toMap(hm.updatedDeep(p(2),"XYZ")) mustEqual hmMap.updated(p(2),"XYZ"),
      toMap(hm.updatedDeep(p(1,2,3),"123")) mustEqual hmMap.updated(p(1,2,3),"123")
    )
    "set intermediate valued nodes" in Seq(
      toMap(hm.updatedDeep(p(1),"IM1")) mustEqual hmMap.updated(p(1),"IM1"),
      toMap(hm.updatedDeep(p(1,2),"IM12")) mustEqual hmMap.updated(p(1,2),"IM12")
    )
    "set intermediate non-valued nodes" in Seq(
      toMap(hm.updatedDeep(p(3),"NV3")) mustEqual hmMap.updated(p(3),"NV3"),
      toMap(hm.updatedDeep(p(3,0),"NV30")) mustEqual hmMap.updated(p(3,0),"NV30")
    )
    "set non-existent positions" in Seq(
      toMap(hm.updatedDeep(p(-1),"QQQ")) mustEqual hmMap.updated(p(-1),"QQQ"),
      toMap(hm.updatedDeep(p(-1,0),"QQQ")) mustEqual hmMap.updated(p(-1,0),"QQQ"),
      toMap(hm.updatedDeep(p(0),"QQQ")) mustEqual hmMap.updated(p(0),"QQQ"),
      toMap(hm.updatedDeep(p(0,1,2,3),"QQQ")) mustEqual hmMap.updated(p(0,1,2,3),"QQQ"),
      toMap(hm.updatedDeep(p(1,2,3,4),"QQQ")) mustEqual hmMap.updated(p(1,2,3,4),"QQQ"),
      toMap(hm.updatedDeep(p(4),"QQQ")) mustEqual hmMap.updated(p(4),"QQQ"),
      toMap(hm.updatedDeep(p(4,4,4,4),"QQQ")) mustEqual hmMap.updated(p(4,4,4,4),"QQQ"),
      toMap(hm.updatedDeep(p(2,99),"QQQ")) mustEqual hmMap.updated(p(2,99),"QQQ")
    )
    "work with arbitrary entries, paths, and values" in {
      forAll(Gen.listOf(saneEntries[Int]), sanePaths, Arbitrary.arbInt.arbitrary) { (entries,path,value) =>
        val hm = ZipperHoleMap(entries:_*)
        val m = Map(entries:_*)
        toMap(hm.updatedDeep(path,value)) mustEqual m.updated(path,value)
      }
    }
  }
  
  "ZipperHoleMap.toString" should {
    "be non-empty" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        val s = hm.toString
        s.length must beGreaterThan(0)
      }
    }
  }
  
  "ZipperHoleMap companion" should {
    "have an empty empty" in {
      val hm:ZipperHoleMap[Int] = ZipperHoleMap.empty
      toMap(hm).size mustEqual 0
    }
    "build ZipperHoleMaps using apply" in {
      forAll(Gen.listOf(saneEntries[Int])) { entries =>
        val hm = ZipperHoleMap(entries:_*)
        toMap(hm) mustEqual Map(entries:_*)
      }
    }
  }
  
  def saneEntries[B](implicit valGen: Arbitrary[B]): Gen[(ZipperPath, B)] = for {
    path <- sanePaths
    value <- valGen.arbitrary
  } yield (path,value)
  
  def sanePaths: Gen[ZipperPath] = for {
    items <- Gen.listOf(saneLocations)
    head <- saneLocations
  } yield ZipperPath((head :: items):_*)
  
  //Using a small range to ensure some overlapping prefixes
  private final val minSaneLoc = 0
  private final val maxSaneLoc = 10
  def saneLocations: Gen[Int] = Choose.chooseInt.choose(minSaneLoc,maxSaneLoc) 
}
