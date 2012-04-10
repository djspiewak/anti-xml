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
import scala.collection.mutable.Builder

import zipper._

package object performance {
  
  def simpleNameOf(n: org.w3c.dom.Node) = 
    if (n.getLocalName == null) n.getNodeName else n.getLocalName
  
  
  def from[A](u: java.net.URL)(f: java.io.InputStream => A): A = {
    val is = u.openStream()
    try {
      f(is)
    } finally {
      is.close()
    }
  }

  /** Selects elements with the specified name, without taking advantage of the bloom filter.*/
  def noBloom(s: String) = Selector({case e:Elem if e.name == s => e})
  
  /** Selects any element */
  val anyElem = Selector[Elem] {case e:Elem => e}

  /** A CBFWZ that builds a plain `Group` rather than a `Zipper` */
  def withoutZipperContext[A <: Node] = CanBuildFromWithZipper.identityCanBuildFrom(new CanBuildFrom[Traversable[_], A, Group[A]] {
    def apply(from: Traversable[_]): Builder[A, Group[A]] = apply()
    def apply(): Builder[A, Group[A]] = Group.newBuilder[A]
  })
  
  
  def cleanVM() {
    System.gc()
  }
  
  def deepsize(x: => Any) = {
    com.github.dmlap.sizeof.SizeOf.deepsize(x)
  }
  

}

