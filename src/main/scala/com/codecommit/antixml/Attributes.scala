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

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{HashMap, Map, MapLike}
import scala.collection.mutable.Builder

class Attributes(delegate: Map[QName, String]) extends Map[QName, String] with MapLike[QName, String, Attributes] {
  override def empty = new Attributes(Map())
  
  // totally shadowed by the overload; compiler won't even touch it without ascribing a supertype
  def +[B >: String](kv: (QName, B)) = delegate + kv
  
  // don't tell anyone I did something this evil...
  def +(kv: (QName, String))(implicit f: String => String) = new Attributes(delegate + kv)
  
  def -(key: QName) = new Attributes(delegate - key)
  
  def iterator = delegate.iterator
  
  def get(key: QName) = delegate get key
}

object Attributes {
  implicit val canBuildFrom: CanBuildFrom[Attributes, (QName, String), Attributes] = new CanBuildFrom[Attributes, (QName, String), Attributes] {
    def apply() = newBuilder
    def apply(from: Attributes) = apply()
  }
  
  implicit val canBuildFromString: CanBuildFrom[Attributes, (String, String), Attributes] = new CanBuildFrom[Attributes, (String, String), Attributes] {
    def apply() = {
      val delegate = newBuilder
      new Builder[(String, String), Attributes] {
        def +=(pair: (String, String)) = {
          val (key, value) = pair
          delegate += (QName(None, key) -> value)
          this
        }
        
        def clear() {
          delegate.clear()
        }
        
        def result() = delegate.result()
        
        override def sizeHint(size: Int) = delegate.sizeHint(size)
      }
    }
    def apply(from: Attributes) = apply()
  }
  
  def newBuilder = HashMap.newBuilder[QName, String] mapResult { m: Map[QName, String] => new Attributes(m) }
  
  val empty = apply()
  
  def apply(attrs: (QName, String)*) = new Attributes(Map(attrs: _*))
}
