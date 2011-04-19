package com.codecommit
package antixml

import util._

import scala.annotation.unchecked.uncheckedVariance

import scala.collection.{IndexedSeqLike, TraversableLike}
import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{IndexedSeq, Vector, VectorBuilder}
import scala.collection.mutable.{ArrayBuffer, Builder, ListBuffer}

/**
 * Represents a collection of arbitrary nodes ([[com.codecommit.antixml.Node]])).
 * Note that this collection need not have a single root parent element.  Thus,
 * a valid `Group` could be as follows:
 *
 * {{{
 * Group(EntityRef("quot"), Text("Daniel is "), Elem(None, "em", Map(), Group(Text("delusional!"))), EntityRef("quot"))
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
    with IndexedSeqLike[A, Group[A]] {
  
  override protected[this] def newBuilder = Group.newBuilder[A]
  
  def length = nodes.length
  
  def apply(i: Int) = nodes(i)
  
  def +:[B >: A <: Node](node: B) = new Group(node +: nodes)
  
  def :+[B >: A <: Node](node: B) = new Group(nodes :+ node)
  
  def ++[B >: A <: Node](that: Group[B]) = new Group(this.nodes ++ that.nodes)
  
  override def drop(n: Int) = new Group(nodes drop n)
  
  override def dropRight(n: Int) = new Group(nodes dropRight n)
  
  override def head = nodes.head
  
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
  
  def updated[B >: A <: Node](index: Int, node: B) = new Group(nodes.updated(index, node))
  
  /**
   * Performs a shallow-select on the XML tree according to the specified selector
   * function.  Shallow selection is defined according to the following expression:
   *
   * {{{
   * nodes flatMap {
   *   case Elem(_, _, _, children) => children collect selector
   *   case _ => Group()
   * }
   * }}}
   *
   * In English, this means that a shallow selection works by first selecting
   * ''only'' the [[com.codecommit.antixml.Elem]](s) at the top level and then
   * filtering their children according to the selector.  The results of these
   * filtrations are concatenated together, producing a single flattened result.
   *
   * '''Very important:''' This is ''not'' the same as the XPath `/` operator!
   * (nor does it strive to be)  XPath is inconsistent in its selection semantics,
   * varying them slightly depending on whether or not you are selecting from
   * the top level or in the middle of a compound expression.  As a result, it
   * is categorically ''impossible'' to implement XPath in a combinatorial
   * fashion.  Rather than give up on combinators, we chose to give up on XPath.
   * In practice, this "select child `Elem`(s), then filter their children" behavior
   * tends to be the most useful variant of the XPath selection.  The "missed"
   * case here is applying a filter to the top-level set of nodes.  This is
   * currently not handled by the API (perhaps in future).  For now, if you need
   * this functionality, it's pretty easy to get it using the `filter` method.
   * 
   * The results of this function will be a collection of some variety, but the
   * exact type is determined by the selector itself.  For example:
   *
   * {{{
   * val ns: Group[Node] = ...
   * ns \ "name"
   * ns \ *
   * ns \ text
   * }}}
   *
   * The three selection expressions here produce very different results.  The
   * first will produce a collection of type [[com.codecommit.antixml.Zipper]]`[`[[com.codecommit.antixml.Elem]]`]`,
   * the second will produce [[com.codecommit.antixml.Zipper]]`[`[[com.codecommit.antixml.Node]]`]`,
   * while the third will produce [[scala.collection.Traversable]]`[`[[scala.String]]`]`.
   * This reflects the fact that the selector produced (by implicit conversion)
   * from a `String` will only filter for nodes of type [[com.codecommit.antixml.Elem]].
   * However, the `*` selector will filter for ''all'' nodes (as the wildcard
   * symbol would suggest) and thus it must return a collection containing the
   * fully-generic [[com.codecommit.antixml.Node]].  Finally, the `text` selector
   * specifically pulls out the textual contents of nodes, and thus its results
   * will not be nodes at all, but raw `String`(s).
   *
   * Whenever supported by the resulting collection type, the selection operation
   * will preserve a "backtrace", or an "undo log" of sorts.  This backtrace is
   * known as a zipper context.  This context makes it possible to operate on
   * the collection resulting from performing a selection, then ''unselect'' to
   * rebuild the original collection around it (modulo the changes made).  This
   * provides a very clean and powerful way of drilling down into an XML tree,
   * making some changes and then realizing those changes within the context of
   * the full tree.  In a sense, it solves the major inconvenience associated
   * with immutable tree structures: the need to manually rebuild the entire
   * ancestry of the tree after making a change somewhere within.
   * 
   * @see [[com.codecommit.antixml.Zipper]]
   * @usecase def \(selector: Selector[Node, Zipper[Node]]): Zipper[Node]
   */
  def \[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: CanBuildFromWithZipper[Zipper[A], B, That]): That = {
    if (matches(selector)) {
      // note: this is mutable and horrible for performance reasons (>2x boost doing it this way) 
      
      val catBuilder = new VectorBuilder[B]
      val chunkBuilder = new VectorBuilder[Int]
      val rebuildBuilder = new VectorBuilder[(Group[Node], Map[Int, Set[Int]]) => Node]
      val childMapBuilder = new VectorBuilder[Map[Int, Set[Int]]]
      
      for (node <- nodes) {
        node match {
          case e @ Elem(_, _, _, children) if children.matches(selector) => {
            var childMap = Map[Int, Set[Int]]()
            var currentChunk = 0
            
            var i = 0
            for (child <- children) {
              if (selector isDefinedAt child) {
                catBuilder += selector(child)
                childMap += (i -> Set(currentChunk))
                currentChunk += 1
              }
              i += 1
            }
            
            chunkBuilder += currentChunk
            childMapBuilder += childMap
            
            def rebuild(children2: Group[Node], indexes: Map[Int, Set[Int]]) = {
              val revisedChildren = children.zipWithIndex.foldLeft(Group[Node]()) {
                case (acc, (_, i)) if indexes contains i =>
                  indexes(i).foldLeft(acc) { _ :+ children2(_) }
                
                case (acc, (e, _)) => acc :+ e
              }
              
              e.copy(children=revisedChildren)
            }
            
            rebuildBuilder += (rebuild _)
          }
          
          case _ =>
        }
      }
      
      val cat = catBuilder.result
      
      lazy val (_, map) = {
        (chunkBuilder.result zip rebuildBuilder.result zip childMapBuilder.result).foldLeft((0, Vector[ZContext]())) {
          case ((i, acc), ((length, f), childMap)) if length != 0 =>
            (i + length, acc :+ (i, i + length, f, childMap))
          
          case ((i, acc), _) => (i, acc)
        }
      }
      
      val builder = cbf(makeAsZipper, map)
      builder ++= cat
      builder.result
    } else {
      cbf(Vector()).result
    }
  }
  
  protected def makeAsZipper: Zipper[A] = {
    new Group(nodes) with Zipper[A] {
      val map = Vector()
      def parent = error("Attempted to move up at root of the tree")
    }
  }
  
  def \\[B, That <: Traversable[B]](selector: Selector[B, That])(implicit cbf: CanBuildFromWithZipper[Zipper[Node], B, That], cbf2: CanBuildFrom[Traversable[_], B, That]): That = {
    if (matches(selector)) {
      val recursive = this flatMap {
        case Elem(_, _, _, children) => children \\ selector
        case _ => cbf().result
      }
      
      (this \ selector) ++ recursive
    } else {
      cbf().result
    }
  }
  
  /**
   * Produces a [[scala.collection.immutable.Vector]] which contains all of the
   * nodes in this `Group`.  This function is guaranteed to run in constant time.
   */
  def toVector = nodes.toVector
  
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

  private val bloomFilter: BloomFilter = {
    // note: mutable and horrible for performance
    val names = new ListBuffer[String]
    var childFilter: BloomFilter = null
    
    for (node <- nodes) {
      node match {
        case Elem(_, name, _, children) => {
          names += name
          
          childFilter = if (childFilter == null)
            children.bloomFilter
          else
            childFilter ++ children.bloomFilter
        }
        
        case _ =>
      }
    }
    
    val ourFilter = BloomFilter(names)(1024)
    if (childFilter == null)
      ourFilter
    else
      ourFilter ++ childFilter
  }

  private def matches(selector: Selector[_, _]) =
    selector.elementName map bloomFilter.contains getOrElse true
}

object Group {
  implicit def canBuildFromWithZipper[A <: Node]: CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] = {
    new CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] {
      def apply(from: Traversable[_], baseMap: =>Vector[ZContext]): Builder[A, Zipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            lazy val map = baseMap
            
            lazy val parent = from match {
              case group: Group[Node] => group.makeAsZipper
              case _ => error("No zipper context available")
            }
          }
        }
      }
      
      def apply(baseMap: =>Vector[ZContext]): Builder[A, Zipper[A]] = {
        VectorCase.newBuilder[A] mapResult { vec =>
          new Group(vec) with Zipper[A] {
            lazy val map = baseMap
            def parent = error("No zipper context available")
          }
        }
      }
    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A] mapResult { new Group(_) }
  
  def empty[A <: Node] = new Group[A](VectorCase.empty)
  
  def apply[A <: Node](nodes: A*) = fromSeq(nodes)
  
  def fromSeq[A <: Node](seq: Seq[A]) = new Group(VectorCase(seq: _*))
}
