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

import scala.collection.immutable.BitSet
import scala.util.Random

private[antixml] object BloomFilter {
  import math._
  
  private val ProbableDefaultMAndK = (2363, 2)

  def apply(elements: Seq[Any] = Nil)(n: Int = (elements.size + 1) * 2, p: Float = 0.33f): BloomFilter = {
    require(elements != null, "elements must not be null!")

    val (m, k) = optimalMAndK(n, p)
    val hashes = elements flatMap hash(m, k)
    new BloomFilter(BitSet(hashes: _*), n, m, k)
  }

  private def hash(m: Int, k: Int)(element: Any): Seq[Int] = {
    // TODO Is tihs approach valid and if so does it offer enough performance?
    val rnd = new Random(element.hashCode)
    (1 until k) map { _ => abs(rnd.nextInt) % m }
  }

  private def optimalMAndK(n: Int, p: Float): (Int, Int) = {
    if (n == 1024 && p == 0.33f) {    // fast-path for the hard-coded case
      ProbableDefaultMAndK
    } else {
      val m = {
        val m = round((- n * log(p) / pow(log(2), 2)).toFloat)
        if (m > 0) m else 1
      }
      val k =
        if (n == 0) {
          1
        } else {
          val k = round(log(2).toFloat * m / n)
          if (k > 0) k else 1
        }
      (m, k)
    }
  }
}

private[antixml] class BloomFilter(private val bits: BitSet, private val n: Int, private val m: Int, private val k: Int) {
  import BloomFilter._

  def contains(element: Any): Boolean =
    hash(m, k)(element) forall bits.contains

  def ++(that: BloomFilter): BloomFilter = {
    if (this.n != that.n || this.m != that.m || this.k != that.k) {
      throw new IllegalArgumentException("BloomFilter properties must match")
    }
    new BloomFilter(this.bits | that.bits, n, m, k)
  }
  
  override def equals(a: Any) = a match {
    case that: BloomFilter =>
      this.bits == that.bits && this.n == that.n && this.m == that.m && this.k == that.k
    
    case _ => false
  }
  
  override def hashCode = bits.hashCode ^ n ^ m ^ k
}
