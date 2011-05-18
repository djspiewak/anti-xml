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

/**
 * Trait which signifies a special type of [[scala.collection.generic.CanBuildFrom]],
 * capable of lifting itself into an instance of [[com.codecommit.antixml.CanBuildFromWithZipper]].
 * Note that even though all CanBuildFrom instances for Traversable targets are
 * implicitly liftable into [[com.codecommit.antixml.CanBuildFromWithZipper]],
 * this lifting is generic and often different from the results of the `lift`
 * method defined by this trait.
 *
 * In practice, this trait is used as a marker trait by the utility methods
 * on [[com.codecommit.antixml.Zipper]] (particularly `map` and `flatMap`).
 * Without this trait, it would be impossible to distinguish zippable collection
 * types from non-zippable collection types while preserving the override of
 * these methods.
 */
trait CanProduceZipper[-From, -Elem, To] { this: CanBuildFrom[From, Elem, _ >: To] =>
  def lift: CanBuildFromWithZipper[From, Elem, To]
}
