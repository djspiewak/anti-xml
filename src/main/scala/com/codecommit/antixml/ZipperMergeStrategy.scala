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

import util.VectorCase
import scala.collection.immutable.IndexedSeq

/**
 * Defines the merge function used to resolve conflicting updates to a node during zipper unselection.
 * See the description of the unselection algorithm in [[com.codecommit.antixml.Zipper]] for more
 * details.
 * 
 * The companion object contains some predefined strategies, including the default implicit strategy,
 * `ZipperMergeStrategy.AlwaysLocal`.
 */
trait ZipperMergeStrategy {
  /** Returns the sequence of Nodes that should replace the original node for the specified merge context. */
  def apply(context: ZipperMergeContext): Seq[Node]
}


object ZipperMergeStrategy {
  
  /**
   * Returns a [[com.codecommit.antixml.ZipperMergeStrategy]] obtained by uniformly applying the specified function to each
   * `directUpdate` node in the merge context and concatenating the results.  The function takes the merge context,
   * a directUpdate node and its associated update time as arguments and returns a sequence of replacement nodes.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  def uniformlyApply(f: (ZipperMergeContext, Node,Int) => Seq[Node]): ZipperMergeStrategy = new ZipperMergeStrategy() {
    override def apply(context: ZipperMergeContext) = context.directUpdate.flatMap(n => f(context, n._1,n._2))
  }

  /**
   * This strategy unconditionally replaces the `original` node with its `indirectUpdate`.  Direct updates
   * are ignored.  
   * 
   * In other words, if a zipper contains both a node and one of its descendants, then updates to the node
   * are unconditionally ignored and the result of the merge will be based solely on its descendants.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  object AlwaysPreferChildren extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = VectorCase(context.indirectUpdate._1)
  }
  
  /**
   * This strategy unconditionally replaces the `original` node with its `directUpdate`.  It is essentially
   * the opposite of `AlwaysPreferChildren`.
   * 
   * This strategy is mainly listed for the sake of completeness.  In practice, it is preferable to use 
   * a selection operator such as `\\!` which prevents conflicting children from entering the zipper in
   * the first place.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  object AlwaysPreferParents extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdate.map(_._1)
  }
  
  /**
   * This strategy unconditionally replaces the "top-level" properties of a node with its direct update
   * but replaces the children of the node with the children of the its indirect update.
   * 
   * Loosely speaking, this strategy causes `unselect` to only pull back updates to an element's `name`, `prefix`, 
   * `scope`, and `attributes`.  Updates to children property of an element are simply ignored.  Instead, the 
   * node's children are either replaced by the result of lower level updates or are left unchanged if this is a non-conflicting node.
   * 
   * If a node has been multiplied via a `flatMap` operation or the like, then the strategy will be uniformly applied to
   * all of the resulting nodes.  If it has been completely elided, then it will be elided in the result as well.
   * 
   * See also the `RequireLocal` strategy, which behaves similarly except
   * that it throws an error if it detects changes to an element's `children` property.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  object AlwaysLocal extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdate map {
      case (e:Elem, _) => e.copy(children=context.indirectUpdate._1.children)
      case (n, _) => n
    }
  }
  
  /**
   * This strategy is similar to `RequireLocal` except that it throws an
   * error if it detects a change to the element's `children` property that would otherwise be ignored.
   * 
   * The price of this added safety is that the strategy depends on testing for Group equality and thus is potentially less performant
   * than `RequireLocal`.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  object RequireLocal extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = context.directUpdate map {
      case (e:Elem, _) if (e.children==context.original.children) => e.copy(children=context.indirectUpdate._1.children)
      case n => sys.error("A conflict has been detected in the following node that cannot be resolved by the RequireLocal merge strategy:\n" + n)
    }
  }

  /**
   * A strategy that simply throws an exception if it is ever invoked.  In other words, the strategy prevents unselection in Zippers
   * that contain conflicts.
   * @see [[[com.codecommit.antixml.Zipper]] 
   */
  object RequireConflictFree extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext): Nothing =
      sys.error("A node and one or more of its descendents were contained in the same zipper.\n" +
          "Possible fixes include either using a different merge strategy or using a different selection\n" +
          "operator.\n"+
          context.original)
  }

  /**
   * A strategy that prefers later updates to earlier ones.  Note that this strategy is marked as implicit and will be chosen as
   * the default strategy in the absence of any external implicits of higher priority.
   * 
   * The strategy is similar to `AlwaysLocal` in that the top-level properties of the
   * original node are unconditionally replaced by their direct updates.  The difference is in the treatment of the nodes children;
   * The children of the node are replaced with the children of either its indirect update or its direct update depending on which
   * update was more recent and on whether the children of each update differ from the children of the original.
   * 
   * The intent of the strategy is to approximate the notion of preferring the most recent update in case of conflict.  
   * 
   * If a node has been multiplied via a `flatMap` operation or the like, then the strategy will be uniformly applied to
   * all of the resulting nodes.  If it has been completely elided, then it will be elided in the result as well.
   * 
   * @see [[[com.codecommit.antixml.Zipper]]
   */
  implicit object PreferLatest extends ZipperMergeStrategy {
    override def apply(context: ZipperMergeContext) = {
      import context._
      val (indRep, indTime) = indirectUpdate
      context.directUpdate map {
          case (e:Elem, time) if ((indTime>=time) || (e.children==original.children)) => e.copy(children=context.indirectUpdate._1.children)
          case (n, _) => n
        }
    }
  }
}