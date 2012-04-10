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

import scala.collection.{IndexedSeqOptimized,Seq, LinearSeq}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuilder, Builder}
import scala.collection.immutable.{IndexedSeq}

/** The `IndexedSeq[Int]` implementation used by `Zipper` to represent paths.
 *
 * This implementation is optimized specifically for Zipper.  It is backed by
 * an `Array[Int]` and is both space efficient and fast for reading.  "Modify" operations are `O(n)`, 
 * in general, but with a small constant factor and are acceptably fast for paths coming from
 * reasonable XML documents.   `Zipper` doesn't ever modify paths in any case.
 *
 * @see [[com.codecommit.antixml.Zipper]]
 */
private[antixml] final class ZipperPath private (private val arr: Array[Int]) extends IndexedSeq[Int] with IndexedSeqOptimized[Int, ZipperPath] {
  override def apply(i: Int) = arr(i)
  override def length = arr.length
  override def newBuilder = ZipperPath.newBuilder
  def valueAt(i: Int): Int = arr(i)
  
  //TODO : Needed?  
  def :+(value: Int): ZipperPath = {
    val ln = arr.length
    val a2 = new Array[Int](ln + 1)
    Array.copy(arr,0,a2,0,ln)
    a2(ln) = value
    new ZipperPath(a2)
  }
}


private[antixml] object ZipperPath {
  
  def apply(is: Int*) = fromSeq(is)
  
  def fromSeq(s: Seq[Int]): ZipperPath = s match {
    case iv: ZipperPath => iv
    case _ => new ZipperPath(toArray(s))
  }
  
  def reversed(s: Seq[Int]): ZipperPath = {
    val a = toArray(s)
    val len = a.length
    val mid = len >> 1
    for(low <- 0 until mid) {
      val high = len - low - 1
      val tmp = a(low)
      a(low) = a(high)
      a(high) = tmp
    }
    new ZipperPath(a)
  }
  
  private def toArray(s: Seq[Int]): Array[Int] = s match {
    case _: collection.IndexedSeq[_] => {
      val len = s.length
      val a = new Array[Int](len)
      s.copyToArray(a,0,len)
      a
    }
    case _ => {
      if (s.isEmpty)
        new Array[Int](0)
      else if (s.lengthCompare(1)==0) {
        val a = new Array[Int](1)
        a(0) = s.head
        a
      } else
        ((new ArrayBuilder.ofInt) ++= s).result()
    }
  }
    
  def empty = new ZipperPath(new Array[Int](0))
  def newBuilder: Builder[Int,ZipperPath] = 
    new ArrayBuilder.ofInt mapResult (a => if (a.length==0) empty else new ZipperPath(a))
  
  object BuilderFactory extends CanBuildFrom[Any,Int,ZipperPath] {
    def apply() = newBuilder
    def apply(from: Any) = newBuilder
  }
  
  implicit def canBuildFrom: CanBuildFrom[ZipperPath,Int,ZipperPath] = BuilderFactory
  
}

