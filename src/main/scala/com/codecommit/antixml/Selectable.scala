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

import zipper._

import scala.collection.generic.{CanBuildFrom, HasNewBuilder}
import scala.collection.immutable.VectorBuilder

import CanBuildFromWithZipper.ElemsWithContextVisible
import com.codecommit.antixml.util.VectorCase

trait Selectable[+A <: Node] {
  import PathCreator.{allChildren, directChildren, fromNodes, allMaximalChildren, PathFunction, PathVal}
  
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
  def \[B, That](selector: Selector[B])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That = {
    fromPathFunc(directChildren(selector), cbfwz)
  }
  
  /**
   * Performs a deep-select on the XML tree according to the specified selector
   * function.  Deep selection is defined according to the following recursion:
   *
   * {{{
   * def deep(g: Group[Node]):That =
   *   g flatMap {n => Group(n).collect(selector) ++ deep(n.children)}
   *
   * nodes flatMap {n => deep(n.children)}
   * }}}
   *
   * In English, this means that deep selection is defined simply as a depth-first
   * search through the tree, all the way from the root down to the leaves.  Note that 
   * the recursion does not short circuit when a result is found.  Thus, if a parent 
   * node matches the selector as well as one of its children, then both the parent ''and'' 
   * the child will be returned, with the parent preceeding the child in the results.
   *
   * Just as with shallow selection, the very outermost level of the group is not
   * considered in the selection.  Thus, deep selection is not ''exactly'' the
   * same as the XPath `//` operator, since `//` will consider the outermost level,
   * while Anti-XML's deep selection `\\` will not.
   * 
   * @usecase def \\(selector: Selector[Node]): Zipper[Node]
   */
  def \\[B, That](selector: Selector[B])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That = {
    fromPathFunc(allChildren(selector), cbfwz)
  }
  
  /**
   * Performs a short-circuiting deep-select on the XML tree according to the specified selector.
   * Short-circuit deep selection is defined according to the following recursion:
   *
   * {{{
   * def deep(g: Group[Node]):That =
   *   g flatMap {n => 
   *     if (selector.isDefinedAt(n)) Group(n).collect(selector) 
   *     else deep(n.children)
   *   }
   * 
   * nodes flatMap {n => deep(n.children)}
   * }}}
   *
   * Like `\\`, this performs a depth-first search through the tree.  However, any time 
   * the selector matches a node, it's children are skipped over rather than being searched.
   * Thus, the result is guaranteed to never contain both a node and one of its descendants.
   * In terms of the [com.codecommit.antixml.Zipper] trait, the result is guaranteed to be
   * conflict-free.
   *
   * Just as with shallow selection, the very outermost level of the group is not
   * considered in the selection.
   *
   * @usecase def \\!(selector: Selector[Node]): Zipper[Node]
   */
  def \\![B, That](selector: Selector[B])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That = {
    fromPathFunc(allMaximalChildren(selector), cbfwz)
  }
  
  /**
   * Performs a selection on the top-level nodes of this group.  The nodes returned by this method
   * are exactly the same as the nodes returned by:
   *
   * {{{
   * nodes.collect(selector)
   * }}}
   *
   * However, this method differs from `collect` in that it can return a [[com.codecommit.antixml.Zipper]]
   * with full `unselect` functionality.  Thus, it is possible to `select` a subset of a group,
   * operate on that subset, and then call `unselect` to pull those operations back to the original group.
   *
   * @usecase def select(selector: Selector[Node]): Zipper[Node]
   */
  def select[B, That](selector: Selector[B])(implicit cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That = {
    fromPathFunc(fromNodes(selector),cbfwz)
  }

  /**
   * Utility method to apply a path function to the group and bundle up the results in a Zipper.
   *
   * NOTE: If this method, or something like it, were to be made public then the issue of duplicates
   * would need to be addressed.  As it stands, Zipper always handles duplicates by concatenating the
   * corresponding nodes during `unselect`.  That's the right thing to do when duplicates arise from
   * flatMapping another Zipper, but it may not be the expected behavior from the viewpoint of selection.
   *
   * In the current implementation, this method is never called with duplicates, so the question never arises.
   */
  private def fromPathFunc[B,That](pf: PathFunction[B], cbfwz: CanBuildFromWithZipper[Group[A], B, That]): That = {
    val grp = toGroup
    val bld = cbfwz(Some(toZipper), grp)
    for( PathVal(value, path) <- pf(grp) ) {
      bld += ElemsWithContextVisible[B](path, 0, VectorCase(value))
    }
    bld.result()
  }
  
  def toGroup: Group[A]
  
  def toZipper: Zipper[A] = toGroup.toZipper
}
