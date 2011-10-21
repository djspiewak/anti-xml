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
import scala.collection.immutable.{Map, MapLike}
import scala.collection.mutable.Builder
import com.codecommit.antixml.util.OrderPreservingMap

/**
 * A special implementation of [[scala.collection.Map]]`[`[[com.codecommit.antixml.QName]]`, String]` with
 * nice overloading and some implicit magic designed for use containing element
 * attributes in [[com.codecommit.antixml.Elem]].  The actual stored keys are of
 * type [[com.codecommit.antixml.QName]].  This is how (optional) namespace
 * information for attributes is stored in Anti-XML trees.  However, there are
 * some syntactic tricks which allow you to ignore
 * the [[com.codecommit.antixml.QName]] boiler-plate when you don't actually need
 * namespace support.  For example:
 *
 * {{{
 * val attrs = Attributes("foo" -> "bar", "baz" -> "bin")
 * attrs("foo")                  // => "bar"
 * attrs(QName(None, "foo"))     // => "bar"
 *
 * val attrs2 = attrs + ("even" -> "more")                       // => Attributes(...)
 * val attrs3 = attrs + (QName(Some("pre"), "even" -> "less")    // => Attributes(...)
 * }}}
 *
 * With very, ''very'' few exceptions, `String` and [[com.codecommit.antixml.QName]] are interchangable.
 * Of course, this is being done with implicit conversions.  However, you don't
 * need to worry about the conversion `String => QName` poluting the implicit
 * dispatch space!  The conversion is defined on the companion object
 * for [[com.codecommit.antixml.QName]], meaning that the compiler will only
 * select it when the result of an expression is ''explicitly'' of type `QName`.
 * It will not automatically inject the conversion to satisfy method dispatch on
 * `String`.  For example:
 *
 * {{{
 * val str = "fubar"
 * 
 * val qn: QName = str      // works!
 * str.ns                   // won't compile!
 * }}}
 *
 * In this example, it is important to note that `ns` is a method
 * on [[com.codecommit.antixml.QName]].  Thus, if the implicit conversion were
 * pimp-enabling, the compiler would have accepted the last line of the example.
 * However, as you can see, the implicit dispatch space has not been cluttered
 * while the convenience of `String` rather than [[com.codecommit.antixml.QName]] has
 * been preserved.
 *
 * One implicit space-cluttering that ''couldn't'' be avoided is the conversion
 * defined as ``(String, String) => (QName, String)``.  This is required to enable
 * the nice `String` syntax on things like the `+` method and the companion
 * object `apply` factory.  Unfortunately, this conversion had to be defined in
 * the [[com.codecommit.antixml]] companion object.  Fortunately, it is a conversion
 * within the same type (simply different parameters passed to `Tuple2`).  Thus,
 * it shouldn't cause any scoping problems.
 * 
 * @see [[com.codecommit.antixml.QName]]
 */
class Attributes private (delegate: OrderPreservingMap[QName, String]) extends Map[QName, String] with MapLike[QName, String, Attributes] {
  import Node.hasOnlyValidChars

  for ((name, value) <- delegate) {
    if (!hasOnlyValidChars(value))
      throw new IllegalArgumentException("Illegal character in attribute value '" + value + "'")
  }

  @deprecated("Use the factory methods on the Attributes companion instead","0.4")
  def this(entries: Map[QName, String]) = this(OrderPreservingMap(entries.toSeq:_*))
  
  override def empty = Attributes.empty
  
  // totally shadowed by the overload; compiler won't even touch it without ascribing a supertype
  def +[B >: String](kv: (QName, B)): Map[QName, B] = delegate + kv
  
  /**
   * Special overload of the `+` method to return `Attributes` rather than
   * `Map[QName, String]`.  Unfortunately, the `+` method is one of those places
   * where the magic of the 2.8 collections library really falls flat.  The good
   * news is that Scala's overload resolution will select ''this'' version of the
   * `+` method for all where the second tuple value is of type `String` (this
   * precedence is ensured by the `DummyImplicit` parameter).  Implicit conversions
   * can be applied to the parameter.  Thus, it is possible to invoke `+` on
   * `Attributes` passing a value of type `(String, String)`.  That value will
   * be implicitly converted to `(QName, String)` and ''this'' version of `+`
   * will be invoked.
   
   * In short, the only time the "other" overload of `+` is dispatched is when
   * you attempt to add a key/value pair where the value is ''not'' of type
   * `String`, or if you ascribe the super-type of `Map[QName, String]` to
   * something of type `Attributes`.  Neither of these are common scenarios, so
   * this implicit overload trick should be sufficient to practically support
   * the illusion of [[com.codecommit.antixml.QName]] / `String` interchangability.
   *
   * @usecase def +(kv: (QName, String)): Attributes
   */
  def +(kv: (QName, String))(implicit d: DummyImplicit) = new Attributes(delegate + kv)
  
  // totally shadowed by the overload; compiler won't even touch it without ascribing a supertype
  override def + [B >: String] (kv1: (QName, B), kv2: (QName, B), kvs: (QName, B) *): Map[QName, B] = delegate + (kv1, kv2, kvs:_*)
  
  /**
   * Special overload of the multiple-argument `+` method to return `Attributes` rather than
   * `Map[QName, String]`.
   *
   * See the corresponding overload of single-argument `+` for more details.
   *
   * @usecase def +(kv1: (QName, String), kv2: (QName, String), kvs: (QName, String) *): Attributes
   */
  def +(kv1: (QName, String), kv2: (QName, String), kvs: (QName, String) *)(implicit d: DummyImplicit) = new Attributes(delegate + (kv1,kv2,kvs:_*))
  
  
  // totally shadowed by the overload; compiler won't even touch it without ascribing a supertype
  override def updated [B >: String](key: QName, value: B): Map[QName, B] = this + ((key, value))

  /**
   * Special overload of the `updated` method to return `Attributes` rather than
   * `Map[QName, String]`.  
   *
   * See the corresponding overload of single-argument `+` for more details.
   *
   * @usecase def updated(key: QName, value: String): Attributes
   */
  def updated(key: QName, value: String)(implicit d: DummyImplicit) = new Attributes(delegate.updated(key,value))
  
  
  def -(key: QName) = new Attributes(delegate - key)
  
  def iterator = delegate.iterator
  
  def get(key: QName) = delegate get key
  
  //The following two overrides are recommended by MapLike for efficiency:  
  override def size: Int = delegate.size
  
  override def foreach[U] (f:((QName,String)) => U) {
    delegate.foreach(f)
  }
  
}

/**
 * Factory companion for the [[com.codecommit.antixml.Attributes]] specialized
 * `Map`.  The only method of serious interest in this object is the `apply` method,
 * which works exactly the same as the `apply` method on any `Map` companion
 * object.
 */
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
  
  def newBuilder = OrderPreservingMap.newBuilder[QName, String] mapResult { m: OrderPreservingMap[QName, String] => new Attributes(m) }
  
  val empty = new Attributes(OrderPreservingMap.empty[QName,String])
  
  def apply(attrs: (QName, String)*) = if (attrs.isEmpty) empty else new Attributes(OrderPreservingMap(attrs: _*))
}
