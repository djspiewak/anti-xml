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
 * - Neither the name of the <ORGANIZATION> nor the names of its contributors may
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

private[antixml] class CatamorphicVector[S, +A] private (
    private val body: Vector[A],
    private val tail: Vector[A],
    private val state: S,
    private val f: S => Option[(S, A)]) {
  
  def apply(i: Int): (A, CatamorphicVector[S, A]) = {
    val this2 = extend(i)
    if (i < this2.body.length)
      (this2.body(i), this2)
    else
      throw new IndexOutOfBoundsException(i.toString)
  }
  
  def updated[B >: A](i: Int, b: B): CatamorphicVector[S, B] = {
    val this2 = extend(i)
    val body2 = if (i < this2.body.length)
      this2.body.updated(i, b)
    else
      throw new IndexOutOfBoundsException(i.toString)
    
    new CatamorphicVector(body2, this2.tail, this2.state, this2.f)
  }
  
  def +:[B >: A](b: B): CatamorphicVector[S, B] =
    new CatamorphicVector(b +: body, tail, state, f)
  
  def :+[B >: A](b: B): CatamorphicVector[S, B] =
    new CatamorphicVector(body, tail :+ b, state, f)
  
  def ++[B >: A](that: CatamorphicVector[S, B]): CatamorphicVector[S, B] =
    new CatamorphicVector(force ++ that.body, that.tail, that.state, that.f)
  
  def ++[B >: A](that: Vector[B]): CatamorphicVector[S, B] =
    new CatamorphicVector(body, tail ++ that, state, f)
  
  def map[B](f: A => B): CatamorphicVector[S, B] = {
    val body2 = body map f
    val tail2 = tail map f
    val f2 = this.f andThen { _ map { case (s, a) => (s, f(a)) } }
    new CatamorphicVector(body2, tail2, state, f2)
  }
  
  def force: Vector[A] = extend(Int.MaxValue - 1).body
  
  private def extend(i: Int) = {
    if (i < body.length) {
      this
    } else {
      val shifted = i - body.length + 1
      
      def gen(state: S): Stream[(S, A)] =
        f(state) map { case tuple @ (state2, _) => tuple #:: gen(state2) } getOrElse Stream.Empty
      
      val stream = gen(state) take shifted
      val body2 = body ++ (stream map { case (_, a) => a })
      lazy val state2 = stream.lastOption map { case (s, _) => s } getOrElse state
      
      if (i < body2.length)
        new CatamorphicVector(body2, tail, state2, f)
      else
        new CatamorphicVector(body2 ++ tail, Vector(), state2, f)       // we're basically done at this point
    }
  }
  
  override def toString = "CatamorphicVector(%s, %s, %s, %s)".format(body, tail, state, f)
}

private[antixml] object CatamorphicVector {
  def apply[S, A](init: S)(f: S => Option[(S, A)]): CatamorphicVector[S, A] =
    new CatamorphicVector(Vector(), Vector(), init, f)
}
