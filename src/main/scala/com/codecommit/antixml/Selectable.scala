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

import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.{Vector, VectorBuilder}

trait Selectable[+A <: Node] {
  
  /**
   * Performs a shallow-select on the XML tree according to the specified selector
   * function.  Shallow selection is defined according to the following expression:
   *
   * {{{
   * nodes flatMap {
   *   case Elem(_, _, children) => children collect selector
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
   * @usecase def \(selector: Selector[Node]): Zipper[Node]
   */
  def \[B, That](selector: Selector[B])(implicit cbf: CanBuildFromWithZipper[Group[_], B, That]): That = {
    if (matches(selector)) {
      // note: this is mutable and horrible for performance reasons (>2x boost doing it this way) 
      
      val catBuilder = new VectorBuilder[B]
      val chunkBuilder = new VectorBuilder[Int]
      val rebuildBuilder = new VectorBuilder[(Group[Node], Map[Int, Set[Int]]) => Node]
      val childMapBuilder = new VectorBuilder[Map[Int, Set[Int]]]
      
      for (node <- toGroup) {
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
          
          case _ => {
            chunkBuilder += 0
            childMapBuilder += Map()
            rebuildBuilder += { (_, _) => error("invoked rebuild for non-match") }
          }
        }
      }
      
      val cat = catBuilder.result
      
      lazy val (_, map) = {
        (chunkBuilder.result zip rebuildBuilder.result zip childMapBuilder.result).foldLeft((0, Vector[Option[ZContext]]())) {
          case ((i, acc), ((length, f), childMap)) if length != 0 =>
            (i + length, acc :+ Some((i, i + length, f, childMap)))
          
          case ((i, acc), _) => (i, acc :+ None)
        }
      }
      
      val builder = cbf(toZipper, map)
      builder ++= cat
      builder.result
    } else {
      cbf(Vector()).result
    }
  }
  
  /**
   * Performs a deep-select on the XML tree according to the specified selector
   * function.  Deep selection is defined according to the following recursion:
   *
   * {{{
   * val recursive = nodes flatMap {
   *   case Elem(_, _, _, children) => children \\ selector
   *   case _ => Group()
   * }
   * 
   * (this \ selector) ++ recursive
   * }}}
   *
   * In English, this means that deep selection is defined simply as the recursive
   * application of shallow selection (`\`), all the way from the root down to the
   * leaves.  Note that the recursion does not short circuit when a result is
   * found.  Thus, if a parent node matches the selector as well as one of its
   * children, then both the parent ''and'' the child will be returned, with the
   * parent preceeding the child in the results.
   *
   * Just as with shallow selection, the very outermost level of the group is not
   * considered in the selection.  Thus, deep selection is not ''exactly'' the
   * same as the XPath `//` operator, since `//` will consider the outermost level,
   * while Anti-XML's deep selection `\\` will not.
   *
   * '''Note:''' For certain selectors (such as an element name selector defined
   * using a `String` or `Symbol`), the result of this method will be a zipper,
   * similar to the results from the shallow-select operator (`\`).  This zipper
   * will ''not'' be valid!  Zipper context synthesis for deep selection is
   * currently unimplemented.  Thus, any attempt to use the `unselect` method on
   * the resulting zipper will likely throw an exception.  At the very least, it
   * won't return a useful result.  The implementation of this feature is ongoing.
   * 
   * @usecase def \\(selector: Selector[Node]): Zipper[Node]
   */
  def \\[B, That](selector: Selector[B])(implicit cbf: CanBuildFromWithZipper[Group[_], B, That]): That = {
    if (matches(selector)) {
      val recursive = toGroup collect {
        case Elem(_, _, _, children) => children \\ selector
        case _ => cbf().result
      }
      
      ((this \ selector) /: recursive)(cbf.append)
    } else {
      cbf().result
    }
  }
  
 def matches(selector: Selector[_]): Boolean = true
  
  def toGroup: Group[A]
  
  def toZipper: Zipper[A] = toGroup.toZipper
}
