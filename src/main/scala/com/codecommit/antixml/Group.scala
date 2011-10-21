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

import util._

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec

import scala.collection.{IndexedSeqLike, TraversableLike, GenTraversableOnce}
import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}
import scala.collection.mutable.{ArrayBuffer, Builder, ListBuffer}

/**
 * Represents a collection of arbitrary nodes ([[com.codecommit.antixml.Node]])).
 * Note that this collection need not have a single root parent element.  Thus,
 * a valid `Group` could be as follows:
 *
 * {{{
 * Group(EntityRef("quot"), Text("Daniel is "), Elem(None, "em", Attributes(), Map(), Group(Text("delusional!"))), EntityRef("quot"))
 * }}}
 *
 * This would correspond to the following XML fragment (note: not actually well-formed
 * XML, since it is lacking a single root element):
 * 
 * {{{
 * &quot;Daniel is <em>delusional!</em>&quot;
 * }}}
 *
 * Note that unlike `scala.xml`, `Group` is ''not'' a special type of [[com.codecommit.antixml.Node]]!
 * This design decision has a very profound impact on the framework as a whole.
 * In general, the result is an API which is more consistent and more predictable
 * than it otherwise would have been.  However, it also resulted in some unfortunate
 * sacrifices: specifically, full XPath semantics.  The exact semantics of the
 * `\` and `\\` operators are defined in their respective scaladocs.
 * 
 * `Group` is parameterized based on the type of `Node` it contains.  In the
 * general case (such as the one illustrated above), this will be exactly `Node`.
 * However, there are some very common cases wherein a `Group` may have a more
 * specific type than just `Node`.  For example:
 *
 * {{{
 * val ns: Group[Node] = ...
 * val results = ns \ "name"
 * }}}
 *
 * In this example, `results` will have type `Group[Elem]`.  This is because the
 * selector employed (`"name"`) can ''only'' produce results of type `Elem`.  This
 * mechanism forms the basis for the typed selectors mechanism, which is extremely
 * powerful and serves to eliminate a great deal of boiler-plate casting when
 * traversing XML hierarchies.
 *
 * In the general case, `Group` is backed by an instance of [[scala.collection.immutable.Vector]].
 * This implementation detail is significant as it implies two things.  First,
 * the implementation of `Group` is truly immutable, meaning that there are no
 * tricky concurrency semantics to worry about.  Second, unlike `scala.xml` (which
 * backs its sequences by either `List` or `ArrayBuffer`, depending on phase of
 * the moon), it is possible to perform ''efficient'' random-access and updates
 * across the entire `Group`.  Random access is implemented by the `apply` method,
 * while random "updates" are implemented by the `updated` method.  Fast prepend
 * and append operations are also available.
 *
 * Beyond this, all standard collection operations are available on `Group` (e.g.
 * `flatMap`, `exists`, `collect`, `slice`, etc).  The appropriate incantations
 * have been spoken to allow these methods to return the correct type.  Thus, if
 * you `map` over a `Group` and your function returns something which extends
 * `Node`, the result will be a `Group`.  If your function returns something which
 * does ''not'' extend `Node` (e.g. `Int`), then the result will be something
 * else (probably a generic `IndexedSeq` backed by `Vector`).  `Group` itself
 * extends [[scala.collection.immutable.IndexedSeq]] and thus can be used in
 * situations which require this abstraction.
 *
 * @author Daniel Spiewak
 */
class Group[+A <: Node] private[antixml] (private[antixml] val nodes: VectorCase[A]) extends IndexedSeq[A] 
    with IndexedSeqLike[A, Group[A]] with Selectable[A] {
  
  override protected[this] def newBuilder = Group.newBuilder[A]
  
  def length = nodes.length
  
  def apply(i: Int) = nodes(i)
  
  /**
   * Efficient (and slightly tricky) overload of `+:` on parameters which are
   * specifically of type [[com.codecommit.antixml.Node]].
   */
  def +:[B >: A <: Node](node: B) = new Group(node +: nodes)
  
  /**
   * Efficient (and slightly tricky) overload of `:+` on parameters which are
   * specifically of type [[com.codecommit.antixml.Node]].
   */
  def :+[B >: A <: Node](node: B) = new Group(nodes :+ node)
  
  /**
   * Efficient (and slightly tricky) overload of `++` on parameters which are
   * specifically of type [[com.codecommit.antixml.Group]]`[_]`.
   */
  def ++[B >: A <: Node](that: Group[B]) = new Group(this.nodes ++ that.nodes)
  
  override def drop(n: Int) = new Group(nodes drop n)
  
  override def dropRight(n: Int) = new Group(nodes dropRight n)
  
  override def head = nodes.head
  
  override def foreach[U](f: A => U) {
    nodes.foreach(f)
  }
  
  override def init = new Group(nodes.init)
  
  override def iterator = nodes.iterator
  
  override def last = nodes.last
  
  override def lengthCompare(len: Int) = nodes lengthCompare len
  
  override def reverseIterator = nodes.reverseIterator
  
  override def slice(from: Int, until: Int) = new Group(nodes.slice(from, until))
  
  override def splitAt(n: Int) = {
    val (left, right) = nodes splitAt n
    (new Group(left), new Group(right))
  }
  
  override def tail = new Group(nodes.tail)
  
  override def take(n: Int) = new Group(nodes take n)
  
  override def takeRight(n: Int) = new Group(nodes takeRight n)

  /** Optionally replaces each node with 0 to many nodes.
   *  
   *  This is used by `Zipper.unselect` to update groups.  It is an optimized version of:
   *
   * {{{
   * group.zipWithIndex.flatMap {case (n,i) => f(n,i).getOrElse(Seq(group(i))) }
   * }}}
   *
   */
  private [antixml] def conditionalFlatMapWithIndex[B >: A <: Node] (f: (A, Int) => Option[Seq[B]]): Group[B] = {
    /*
     * The key observation is that most of the time we are only updating a few of the group's nodes.
     * So instead of rebuilding a Group from scratch, we try to just call `updated` on the nodes
     * that are changing.  However, we must fall back to a complete rebuild if we discover
     * a node being replaced by 0 or many nodes.
     */
    
    //Optimistic function that uses `update`
    @tailrec
    def update(g: VectorCase[B], index: Int): VectorCase[B] = {
      if (index<g.length) {
        val node = nodes(index)
        f(node, index) match {
          case None => update(g, index + 1)
          case Some(replacements) => {
            if (replacements.lengthCompare(1)==0) {
              val newNode = replacements.head
              if (newNode eq node) 
                update(g, index + 1) //if same instance, don't bother updating the vector
              else 
                update(g.updated(index, newNode), index + 1)
            } else {
              build(g, replacements, index)
            }
          }
        }
      } 
      else g
    }
    
    //Fallback function that uses a builder
    def build(g: VectorCase[B], currentReplacements: Seq[B], index: Int): VectorCase[B] = {
      val b = VectorCase.newBuilder[B] ++= g.view.take(index)
      b ++= currentReplacements
      for(i <- (index + 1) until g.length) {
        val n = nodes(i)
        f(n,i) match {
          case None => b += n
          case Some(r) => b ++= r
        }
      }
      b.result      
    }
    
    new Group(update(nodes, 0))
  }
  
  /**
   * Merges adjacent [[com.codecommit.antixml.Text]] as well as adjacent
   * [[com.codecommit.antixml.CDATA]] nodes to produce a `Group` which represents
   * an identical XML fragment but with a minimized structure.  Slightly more
   * formally, for any XML fragment with ''n'' characters of textual data, there
   * are ''2^n^'' possible ways of representing that fragment as a `Group`.  All
   * of these representations are semantically distinct (i.e. structurally different
   * in memory) but logically equivalent in that they will all generate the same
   * XML fragment if serialized.  Of these ''2^n^'' distinct representations,
   * there will be exactly one representation which is ''minimal'', in that the
   * smallest possible number
   * of [[com.codecommit.antixml.Text]] and [[com.codecommit.antixml.CDATA]] nodes
   * are used to represent the textual data.  This form may be considered
   * "canonical".  This method converts an arbitrary `Group` into its canonical
   * form, a ''logically'' equivalent `Group` which represents the same XML fragment
   * in its structurally minimized form.
   * 
   * This method is perhaps best explained by an example:
   *
   * {{{
   * val xml = Group(Text("Daniel "), Text("Spiewak"))
   * xml.canonicalize    // => Group(Text("Daniel Spiewak"))
   * }}}
   *
   * The `Group` resulting from the `canonicalize` invocation will produce exactly
   * the same result as would `xml` were we to invoke the `toString` method on
   * each of them.  However, the canonicalized result has only one text node for
   * the entire character block, while `xml` (the original `Group`) has two.
   *
   * This is actually a very common gotcha in `scala.xml`.  The issue comes up
   * most frequently in the area of equality.  As you can see in the example above,
   * `xml` ''clearly'' will not be equivalent (according to the `equals` method)
   * to `xml.canonicalize`.  However, it is very natural to assume that these
   * two structures are in fact equal due to their ''logical'' equivalence in
   * that they represent the same textual XML fragment.  Oftentimes, people will
   * get around this issue in `scala.xml` by converting all `NodeSeq`(s) into
   * strings prior to comparison.  In Anti-XML, all that is necessary to handle
   * potential semantic divergence in cases of logical equality is to simply
   * invoke the `canonicalize` method on each of the two equality operands.
   */
  def canonicalize: Group[A] = {
    val (back, tail) = nodes.foldLeft((Group[Node](), None: Option[Either[String, String]])) {
      // primary Text
      case ((back, None), Text(str)) => (back, Some(Left(str)))
      case ((back, Some(Left(acc))), Text(str)) => (back, Some(Left(acc + str)))
      
      // primary CDATA
      case ((back, None), CDATA(str)) => (back, Some(Right(str)))
      case ((back, Some(Right(acc))), CDATA(str)) => (back, Some(Right(acc + str)))
      
      // cross-over
      case ((back, Some(Left(acc))), CDATA(str)) => (back :+ Text(acc), Some(Right(str)))
      case ((back, Some(Right(acc))), Text(str)) => (back :+ CDATA(acc), Some(Left(str)))
      
      // terminal recursive
      case ((back, Some(Left(acc))), Elem(prefix, name, attrs, scope, children)) =>
        (back :+ Text(acc) :+ Elem(prefix, name, attrs, scope, children.canonicalize), None)
      
      case ((back, Some(Right(acc))), Elem(prefix, name, attrs, scope, children)) =>
        (back :+ CDATA(acc) :+ Elem(prefix, name, attrs, scope, children.canonicalize), None)
      
      // primary recursive
      case ((back, None), Elem(prefix, name, attrs, scope, children)) =>
        (back :+ Elem(prefix, name, attrs, scope, children.canonicalize), None)
      
      // terminal normal
      case ((back, Some(Left(acc))), n) => (back :+ Text(acc) :+ n, None)
      case ((back, Some(Right(acc))), n) => (back :+ CDATA(acc) :+ n, None)
      
      // primary normal
      case ((back, None), n) => (back :+ n, None)
    }
    
    val result = tail map {
      case Left(str) => back :+ Text(str)
      case Right(str) => back :+ CDATA(str)
    } getOrElse back
    
    result.asInstanceOf[Group[A]]       // ugly, but safe; type-checker doesn't understand catamorphism
  }
  
  /**
   * Efficient (and slightly tricky) overload of `updated` on parameters which are
   * specifically of type [[com.codecommit.antixml.Node]].
   */
  def updated[B >: A <: Node](index: Int, node: B) = new Group(nodes.updated(index, node))
  
  override def toZipper: Zipper[A] = Zipper.brokenZipper(toVectorCase) 
  
  /**
   * Produces a [[scala.collection.immutable.Vector]] which contains all of the
   * nodes in this `Group`.  This function is guaranteed to run in constant time.
   */
  def toVector = nodes.toVector
  
  def toGroup = this
  
  private[antixml] def toVectorCase: VectorCase[A] = nodes
  
  /**
   * Serializes the nodes in this `Group` into their most compact XML representation
   * and concatenates the result.  Note that the result of this method may not
   * be well-formed XML, since well-formed XML is required to have only a single
   * parent element (whereas a `Group` may contain multiple nodes at its top level).
   *
   * This is not a pretty-print.  The resulting XML will not be formatted in any
   * way.  It will be ''precisely'' the XML fragment represented by this `Group`,
   * no more and no less.
   *
   * @return The XML fragment represented by this `Group` in `String` form
   */
  override def toString = nodes.mkString

  private lazy val bloomFilter: BloomFilter = {
    // note: mutable and horrible for performance
    import Group._
    val names = new ListBuffer[String]
    var childFilter: BloomFilter = null
    
    for (node <- nodes) {
      node match {
        case Elem(_, name, _, _, children) => {
          names += name
          
          val chbf = children.bloomFilter
          if (chbf ne emptyBloomFilter) {
            childFilter = if (childFilter == null)
              chbf
            else
              childFilter ++ chbf
          }
        }
        
        case _ =>
      }
    }
    
    val ourFilter = if (names.isEmpty) emptyBloomFilter else BloomFilter(names)(bloomFilterN)
    if (childFilter == null)
      ourFilter
    else
      ourFilter ++ childFilter
  }

  /** If true this group may contain an element with the given name as one of its children (recursively). */
  def matches(elementName: String) = bloomFilter contains elementName
  
  /** Same as `matches(String)`, but works with hashes created by Group.bloomFilterHash. */
  private[antixml] def matches(hash: BloomFilter.Hash) = bloomFilter containsHash hash
}

/**
 * Factory singleton for `Group`.  This object is primarily used for creating
 * new `Group`(s) from specified nodes.
 */
object Group {
  
  /** The "N" value we always use for Group bloom filters */
  private final val bloomFilterN = 1024  
  
  private val emptyBloomFilter = BloomFilter(Nil)(bloomFilterN)
  
  private[antixml] def bloomFilterHash(elementName: String): BloomFilter.Hash =
    BloomFilter.generateHash(bloomFilterN)(elementName)
  
  /** 
   * Creates instances of [[com.codecommit.antixml.CanBuildFromWithZipper]] for [[com.codecommit.antixml.Group]] types.  
   */
  implicit def canBuildFromWithZipper[A <: Node] = Zipper.canBuildFromWithZipper[A]
  
  
  /** Provides a [[scala.collection.generic.CanBuildFrom]] instance which mixes in [[com.codecommit.antixml.CanProduceZipper]]  */
  implicit def canBuildFromDeep[A <: Node]: CanBuildFrom[Traversable[_], A, Group[A]] = {
    new CanBuildFrom[Traversable[_], A, Group[A]] with CanProduceZipper[Traversable[_], A, Zipper[A]] {
      def apply(from: Traversable[_]): Builder[A, Group[A]] = apply()
      def apply(): Builder[A, Group[A]] = newBuilder[A]

      def lift = canBuildFromWithZipper
    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A] mapResult { new Group(_) }
  
  private val theEmpty: Group[Nothing] = new Group(Vector0)
  
  /**
   * @return An empty [[com.codecommit.antixml.Group]] with the given parameter type.
   */
  def empty[A <: Node]: Group[A] = theEmpty
  
  /**
   * Builds a new group with the specified set of nodes in order.  The most specific
   * group type possible is selected (e.g. `Group[Elem]`).  This method delegates
   * to the `fromSeq` method.
   */
  def apply[A <: Node](nodes: A*) = fromSeq(nodes)
  
  /**
   * Builds a new group with the specified set of nodes in order.  The most specific
   * group type possible is selected (e.g. `Group[Elem]`).  If the given `Seq`
   * has the ''runtime'' type of [[scala.collection.immutable.Vector]], then no
   * copying will be performed and the resulting `Group` will be a simple wrapper
   * around the specified `Vector`.  However, if the `Seq` has ''any'' other
   * runtime type, its contents will be copied into a new `Vector` internal to
   * the resulting `Group`.  This design makes it impossible to accidentally
   * leak a mutable data structure into an XML tree.  If the performance
   * implications of the copy operation prove prohibitive, then you must attempt
   * to ensure that the sequences you pass to this method are always of type
   * `Vector`, since this will avoid the penalty.
   */
  def fromSeq[A <: Node](seq: Seq[A]): Group[A] = if (seq.isEmpty) theEmpty else new Group(VectorCase(seq: _*))
}

