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

package com.codecommit

/**
 * Base package for the Anti-XML framework.  Note that importing this package
 * brings in a number of implicit conversions.  Specifically:
 *
 * <ul>
 * <li>`String => Selector[Elem, Zipper[Elem]]` – Used to allow string-based
 * selection syntax with [[com.codecommit.antixml.Group]].  (e.g. `ns \ "name"`)</li>
 * <li>`Symbol => Selector[Elem, Zipper[Elem]]` – Used to allow symbol-based
 * selection syntax with [[com.codecommit.antixml.Group]].  (e.g. `ns \ 'name`)</li>
 * <li>∀`A` . `A => Converter[A]` – Implements ''explicit'' conversions from
 * `scala.xml` types to Anti-XML correspondents (where applicable).  This
 * technically makes the `anti` method available on all types.  However, that
 * method will only be callable on very specific types in the `scala.xml`
 * library, and thus it shouldn't cause any collsion issues.</li>
 * <li>`(String, String) => (QName, String)` – Required to get nice syntax for
 * unqualified attribute names.  Note there is an additional conversion of type
 * `String => QName`, but that conversion is defined on the companion object for
 * [[com.codecommit.antixml.QName]], which prevents it from cluttering the dispatch
 * implicit space (i.e. it only applies as a type coercion, ''not'' a pimp).</li>
 * </ul>
 */
package object antixml {
  // (from, to, rebuild, internal map)
  private[antixml] type ZContext = (Int, Int, (Group[Node], Map[Int, Set[Int]]) => Node, Map[Int, Set[Int]])

  /**
   * Implicitly lifts a [[scala.String]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ "name"`
   */
  implicit def stringToSelector(name: String): Selector[Elem] =
    Selector({ case e @ Elem(_, `name`, _, _) => e }, Some(name))

  /**
   * Implicitly lifts a [[scala.Symbol]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ 'name`
   */
  implicit def symbolToSelector(sym: Symbol): Selector[Elem] = {
    val Symbol(name) = sym
    stringToSelector(name)
  }
  
  /**
   * Pimps the `anti` method onto any object for which there exists a conversion
   * into Anti-XML.  Note that this conversion is an implicit value, statically
   * enforced and thus shouldn't be the source of any collision issues.  It should
   * actually be possible to have another implicit conversion in scope which
   * pimps the `anti` method without seeing conflicts.
   * 
   * @see [[com.codecommit.antixml.XMLConvertable]]
   */
  implicit def nodeSeqToConverter[A](a: A): Converter[A] = new Converter(a)
  
  // I feel justified in this global implicit since it doesn't pimp anything
  implicit def stringTupleToQNameTuple(pair: (String, String)): (QName, String) = {
    val (key, value) = pair
    (QName(None, key), value)
  }

  /**
   * Wildcard selector which passes ''all'' nodes unmodified.  This is analogous
   * to the `"_"` selector syntax in `scala.xml`.  For example: `ns \ * \ "name"`
   */
  val `*`: Selector[Node] = Selector({ case n: Node => n })
  
  /**
   * Non-node selector which finds exclusively [[com.codecommit.antixml.Text]]
   * nodes and pulls out their `String` content.  Unlike most selectors, the
   * result of using this selector is not a [[com.codecommit.antixml.Group]], but
   * a generic [[scala.collection.Traversable]]`[String]`.  This selector can
   * be used to emulate the `NodeSeq#text` method provided by `scala.xml`.  For
   * example: `ns \\ text mkString` (this is analogous, but not quite equivalent
   * to calling `ns.text` in `scala.xml`).
   */
  val text: Selector[String] = Selector({ case Text(str) => str })
}
