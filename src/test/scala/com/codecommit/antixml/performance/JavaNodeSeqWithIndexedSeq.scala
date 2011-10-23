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
package performance

import org.w3c.dom
import scala.collection.IndexedSeqOptimized

sealed trait JavaNodeSeqWithIndexedSeq extends scala.collection.IndexedSeq[dom.Node] 
      with dom.NodeList 
      with IndexedSeqOptimized[dom.Node,scala.collection.IndexedSeq[dom.Node]] {
        
  /** Simulated one-level select */
  def \(name: String): JavaNodeSeqWithIndexedSeq = {
    val b = JavaNodeSeqWithIndexedSeq.newBuilder
    for(node <- this; node2 <- JavaNodeSeqWithIndexedSeq.wrap(node.getChildNodes)) node2 match {
      case e: dom.Element => {
        if (simpleNameOf(e) == name)
          b += e
      }
      case _ => ()
    }
    b.result
  }
}

object JavaNodeSeqWithIndexedSeq {
  def wrap(s: scala.collection.IndexedSeq[dom.Node]) = s match {
    case jnswis: JavaNodeSeqWithIndexedSeq => jnswis
    case _ =>  new JavaNodeSeqWithIndexedSeq {
      override def apply(i: Int) = s(i)
      override def length = s.length
      override def getLength = s.length
      override def item(i: Int) = s(i)
    }
  }
  def wrap(nl: dom.NodeList) = nl match {
    case jnswis: JavaNodeSeqWithIndexedSeq => jnswis
    case _=> new JavaNodeSeqWithIndexedSeq {
      override def apply(i: Int) = nl.item(i)
      override def length = nl.getLength
      override def getLength = nl.getLength
      override def item(i: Int) = nl.item(i)
    }
  }
  
  def newBuilder = Array.newBuilder[dom.Node] mapResult {wrap(_)}
  
  //Use this instead of `wrap` to force a lazy NodeList to be realized.
  def copy(nl: dom.NodeList) = {
    val b = newBuilder
    for(i <- 0 until nl.getLength)
      b += nl.item(i)
    b.result
  }
}
