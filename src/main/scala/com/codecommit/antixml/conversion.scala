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
package antixml

/**
 * Pimp container for the explicit conversions into Anti-XML types.  Out of the
 * box, conversions are provided from `scala.xml` types.  However, this mechanism
 * is very extensible due to the use of a typeclass ([[com.codecommit.antixml.XMLConvertable]])
 * to represent the actual conversion.  Thus, it is possible to add conversions
 * by defining an implicit instance of the typeclass and having it in scope.  It
 * is even possible to override the built-in conversions for `scala.xml` types
 * simply by shadowing the conversions for types like [[scala.xml.Elem]].  The
 * built-in conversions are defined in such a way that Scala's implicit resolution
 * will give precedence to almost anything you define, as long as it is somehow
 * in scope.
 */
class Converter[A](a: A) {
  
  /**
   * Converts a target type `A` into some result type B (presumably in the Anti-XML
   * API).  Technically, this function is not just restricted to converting into
   * Anti-XML types.  However, it would probably minimize confusion if it were
   * exclusively used for this purpose.  This generality comes from the fact that
   * the `anti` function itself doesn't perform any conversion, but merely delegates
   * directly to the `apply` method on whatever instance of `XMLConvertable` it
   * happens to be passed.
   *
   * '''Note:''' If no conversion is available for the target type, then the compiler
   * will reject this method call.  Similarly, if more than one conversion is in
   * scope and neither has implicit precedence over the other, then the compiler
   * will reject this method call as ambiguous.  In such cases, it is always
   * possible to pass the conversion explicitly.
   *
   * @see [[com.codecommit.antixml.XMLConvertable]]
   * @usecase def anti: Node 
   */
  def anti[B](implicit conversion: XMLConvertable[A, B]) = conversion(a)
}


/**
 * Typeclass definition for conversions used by the [[com.codecommit.antixml.Converter]] pimp.
 * Note that this type is ''exactly'' isomorphic to [[scala.Function1]], right
 * down to the method name (`apply`).  Normally, such a class would in fact extend
 * `A => B`, rather than simply emulating its interface.  However, because most
 * instances of `XMLConvertable` will be implicit, we cannot blithely extend
 * `Function1`.  To do so would polute the scope with an unexpected proliferation
 * of ''implicit'' conversions which would be automatically injected by the Scala
 * compiler, rather than allowing us to tag them ''explicitly'' using the `anti` method.
 * 
 * @see [[com.codecommit.antixml.Converter]]
 */
trait XMLConvertable[-A, +B] {      // note: doesn't extend Function1 to avoid coercion
  
  /**
   * Convert a value of type `A` into a (hopefully equivalent) value of type `B`.
   */
  def apply(a: A): B
}

/**
 * Contains the built-in explicit conversions into Anti-XML.  Currently, these
 * conversions only cover types in `scala.xml`.  This may be expanded in future.
 *
 * All of the members in this object are implicit, and thus it is rare for a user
 * to need to access them directly.  The membership is contrived in such a way
 * that the implicit resolution will use the following precedence order:
 *
 * <ul>
 * <li>`ElemConvertable`</li>
 * <li>`TextConvertable`</li>
 * <li>`EntityRefConvertable`</li>
 * <li>`NodeConvertable`</li>
 * <li>`NodeSeqConvertable`</li>
 * </ul>
 *
 * This corresponds with the roughly-intuitive conversion precedence.  Thus, if
 * we have a value of type [[scala.xml.Elem]] and we invoke the `anti` method on
 * that value, the result will be of type [[com.codecommit.antixml.Elem]].  However,
 * if we take that same value and ascribe it the type of [[scala.xml.Node]],
 * the `anti` method will return a value of type [[com.codecommit.antixml.Node]].
 * Finally, we can take this same value and ascribe it the even less-specific type
 * of [[scala.xml.NodeSeq]] (or even [[scala.Seq]]`[`[[scala.xml.Node]]`]`, for
 * that matter).  Invoking the `anti` method on this maximally-widened type will
 * produce a value of type [[com.codecommit.antixml.Group]]`[`[[com.codecommit.antixml.Node]]`]`.
 * Thus, the most specific conversion is chosen in all cases.
 */
object XMLConvertable extends SecondPrecedenceConvertables {
  implicit object ElemConvertable extends XMLConvertable[xml.Elem, Elem] {
    def apply(e: xml.Elem) = {
      val prefix = if (e.prefix == null) None else Some(e.prefix)
      val ns = if (e.namespace == null) None else Some(e.namespace)
        
      val attrs = (Attributes() /: e.attributes) {
        case (attrs, pa: xml.PrefixedAttribute) => attrs + (QName(Some(e.scope.getURI(pa.pre)), Some(pa.pre), pa.key) -> pa.value.mkString)
        case (attrs, ua: xml.UnprefixedAttribute) => attrs + (ua.key -> ua.value.mkString)
        case (attrs, _) => attrs
      }
    
      val children = NodeSeqConvertable(xml.NodeSeq fromSeq e.child)
      Elem(QName(ns, prefix, e.label), attrs, Map(), children)
    }
  }
  
  implicit object TextConvertable extends XMLConvertable[xml.Atom[String], Text] {
    def apply(t: xml.Atom[String]) = Text(t.text)
  }
  
  implicit object EntityRefConvertable extends XMLConvertable[xml.EntityRef, EntityRef] {
    def apply(ref: xml.EntityRef) = EntityRef(ref.entityName)
  }
}

// it really amazes me that this even works
private[antixml] sealed trait SecondPrecedenceConvertables extends ThirdPrecedenceConvertables { this: XMLConvertable.type =>
  implicit object NodeConvertable extends XMLConvertable[xml.Node, Node] {
    def apply(n: xml.Node) = n match {
      case e: xml.Elem => ElemConvertable(e)
      case a: xml.Atom[String] => TextConvertable(a)
      case r: xml.EntityRef => EntityRefConvertable(r)
      case g: xml.Group => error("xml.Group should never have been a Node; there is no sane conversion")
    }
  }
}

private[antixml] sealed trait ThirdPrecedenceConvertables { this: XMLConvertable.type =>
  // written against Seq[xml.Node] rather than NodeSeq since scala.xml isn't consistent
  implicit object NodeSeqConvertable extends XMLConvertable[Seq[xml.Node], Group[Node]] {
    def apply(ns: Seq[xml.Node]) = Group(ns map NodeConvertable.apply: _*)
  }
}
