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

import scala.collection.Traversable
import scala.collection.immutable.Seq

trait Selector[+A] extends PartialFunction[Node, A]

trait OptimizingSelector[+A] extends Selector[A] {
  /***
   * Provides a hint as to whether the selector can any children or their
   * descendants in the specified group.  Note that this is mearly a hint,
   * `isDefinedAt` is the authoritative determinant of whether a selector matches.
   */
  def canMatchIn(group: Group[Node]): Boolean
}

object Selector {

  /**
   * Implicitly lifts a [[scala.String]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ "name"`
   */
  implicit def stringToSelector(name: String): Selector[Elem] =
    new OptimizingSelector[Elem] {
      private val pf: PartialFunction[Node, Elem] = {
        case e @ Elem(_, `name`, _, _, _) => e
      }
      private val hash = Group.bloomFilterHash(name)

      def apply(node: Node) = pf(node)
      def isDefinedAt(node: Node) = pf isDefinedAt node
      def canMatchIn(group: Group[Node]) = group.matches(hash)
    }
  

  /**
   * Implicitly lifts a [[scala.Symbol]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ 'name`
   */
  implicit def symbolToSelector(sym: Symbol): Selector[Elem] = {
    val Symbol(name) = sym
    stringToSelector(name)
  }
  
  def apply[A](pf: PartialFunction[Node, A]) = {
    new Selector[A] {
      def apply(node: Node) = pf(node)
      def isDefinedAt(node: Node) = pf isDefinedAt node
    }
  }
}
