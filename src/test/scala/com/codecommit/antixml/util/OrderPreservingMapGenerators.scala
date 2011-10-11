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

package com.codecommit.antixml.util

import org.scalacheck._
import scala.collection.immutable.Vector

object OrderPreservingMapGenerators {
  import Arbitrary.arbitrary
  import Gen._
  
  /**
   * The largest size for which we use special logic for OrderPreserrvingMap implementations.
   */
  private val smallMapMax = 5
  
  case class Entries[A,B](entries: Vector[(A,B)]) {
    val (keys,values) = entries.unzip
    val size = entries.size
    require(keys.distinct == keys, entries)
  }

  def byteKeyEntries[A](minSize:Int,maxSize:Int)(implicit arbA: Arbitrary[A]): Gen[Entries[Byte,A]] = {
    require(minSize<=maxSize && minSize>=0)
    //Want equal frequency between small and large maps if the range intersects both kinds
    val sizeGen = if (minSize<=smallMapMax && maxSize>smallMapMax)
      frequency((1,choose(minSize,smallMapMax)),(1,choose(smallMapMax+1,maxSize)))
    else
      choose(minSize,maxSize)
    for (
      size <- sizeGen;
      entries <- entriesFor(seqOfNDistinctBytes(size),arbA.arbitrary)
    ) yield Entries(entries)
  }
  
  def entriesFor[A,B](keyGen: => Gen[Seq[A]], valGen: => Gen[B]): Gen[Vector[(A,B)]] = for {
    keys <- keyGen;
    vals <- listOfN(keys.size, valGen)
  } yield Vector(keys.zip(vals): _*)
  
  val allBytes = Range.inclusive(Byte.MinValue,Byte.MaxValue) map {_.toByte}
  
  def complement(src: Traversable[Byte]):IndexedSeq[Byte] = allBytes.filterNot(Set(src.toSeq:_*))
  
  def seqOfNDistinctBytes(n: Int):Gen[Seq[Byte]] = pick(n,allBytes)
  
  
  sealed trait MapOp[+A,+B]
  case class Add[+A,+B] (key: A, value: B) extends MapOp[A,B]
  case class Remove[+A] (key: A) extends MapOp[A,Nothing]
  
  def genMapOp[A,B](implicit arbKey: Arbitrary[A], arbVal: Arbitrary[B]): Gen[MapOp[A,B]] = 
    oneOf(genAddOp(arbKey,arbVal),genRemoveOp(arbKey))
  
  def genAddOp[A,B](implicit arbKey: Arbitrary[A], arbVal: Arbitrary[B]): Gen[Add[A,B]] = for {
    key <- arbKey.arbitrary
    value <- arbVal.arbitrary
  } yield Add(key,value)
  
  def genRemoveOp[A](implicit arbKey: Arbitrary[A]):Gen[Remove[A]] = arbKey.arbitrary map {Remove[A](_)}
}


