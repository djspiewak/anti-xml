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
import scala.collection.immutable.Vector
import scala.collection.mutable.Builder

/**
 * An implicit factory creator for use in selection in the style
 * of [[scala.collection.generic.CanBuildFrom]] with functionality required to
 * generate zippers.  In addition to providing the standard CanBuildFrom
 * functionality (producing an instanceof [[scala.collection.mutable.Builder]])
 * with respect to a provided zipper context, this typeclass also provides a
 * monoidal append method on the `To` type.   This is required for deep-select on
 * [[com.codecommit.antixml.Group]].
 *
 * A default implicit "lifting" is provided from [[scala.collection.generic.CanBuildFrom]] to
 * an instances of [[com.codecommit.antixml.CanBuildFromWithZipper]] for all target
 * types which are implicitly convertable to [[scala.collection.Traversable]].
 * (the reason for this restriction is to allow a default implementation of the
 * aforementioned monoidal append)  This implicit lifting is defined in the
 * companion object for this trait, giving it lower priority in the implicit
 * resolution, but still accessible without requiring an explicit import.
 */
trait CanBuildFromWithZipper[-From, -Elem, To] { self =>
  def apply(parent: From, map: =>Vector[Option[ZContext]]): Builder[Elem, To]
  def apply(map: =>Vector[Option[ZContext]]): Builder[Elem, To]
  
  /**
   */
  def append(left: To, right: To): To
  
  def lift[CC >: To]: CanBuildFrom[From, Elem, CC] = new CanBuildFrom[From, Elem, CC] {
    def apply(from: From) = apply()
    def apply() = self(Vector())
  }
}

/**
 * Serves as a simple container for the implicit lifting
 * of [[scala.collection.generic.CanBuildFrom]] to [[com.codecommit.antixml.CanBuildFromWithZipper]].
 */
object CanBuildFromWithZipper {
  
  /**
   * Implicitly "lifts" an existing instance of [[scala.collection.generic.CanBuildFrom]] into
   * an instance of [[com.codecommit.antixml.CanBuildFromWithZipper]], provided
   * that the result type of the builder is implicitly convertable (potentially
   * via `identity`) to [[scala.collection.Traversable]].  In practice, this
   * should be effectively all conceivable instances of [[scala.collection.generic.CanBuildFrom]],
   * so this should not be a problematic restriction.
   *
   * This implicit lifting makes it possible to define instances
   * of [[com.codecommit.antixml.Selector]] which produce
   * non-[[com.codecommit.antixml.Node]] result types.  More precisely, it allows
   * such selectors to be ''used'' with the `\` and `\\` methods
   * on [[com.codecommit.antixml.Group]].
   */
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To], coerce: To => Traversable[Elem]): CanBuildFromWithZipper[From, Elem, To] = new CanBuildFromWithZipper[From, Elem, To] {
    def apply(parent: From, map: =>Vector[Option[ZContext]]) = cbf()
    def apply(map: =>Vector[Option[ZContext]]) = cbf()
    
    def append(left: To, right: To) = {
      val builder = cbf()
      builder ++= left
      builder ++= right
      builder.result()
    }  
  }
}
