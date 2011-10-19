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


 /** Defines type related to paths on a tree.
  * Also contains factory methods for [[PathFunction]]s  */
private[antixml] object PathCreator {
  
  /** The basic result type of a match.
   * @param value the selector result
   * @param path the top-down path to the selected node.
   */
  private[antixml] case class PathVal[+A](value: A, path: ZipperPath)

  private[antixml] trait PathVals[+A] extends Traversable[PathVal[A]]

  private[antixml] type PathFunction[+A] = (Group[Node]) => PathVals[A]

  /** Alias for the internal bottom-up path used during selection.  This must be reversed before
    * being returned in a [[PathVal]].
    */
  private type BottomUp = List[Int]
    
  private def reverse(rev: BottomUp) = ZipperPath.reversed(rev)
  
  /** A path function that selects on nodes in the given group. */
  def fromNodes[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachIn(nodes, selector, Nil, f)
    }
  }

  /** A path function that selects on the given nodes and recursively on the children (depth first). */
  def all[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachInDeep(nodes, selector, Nil, f)
    }
  }

  /**
   * A path function that selects on the given nodes and recursively on the children, returning those
   * matching nodes that are not themselves descendants of matching nodes. (depth first). 
   */
  def allMaximal[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachInDeepShortCircuit(nodes, selector, Nil, f)
    }
  }
  
  /** A path function that selects on the children of the given group. */
  def directChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachChildIn(nodes, selector, Nil, f)
    }
  }
  
  /** A path function that selects on the recursively on all the children of the given group (breadth first). */
  def allChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachChildInDeep(nodes, selector, Nil, f)
    }
  }

  /** A path function that returns all the matching children that are not descendants of matching children (depth first). */
  def allMaximalChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = new PathVals[A] {
    override def foreach[U] (f: (PathVal[A]) => U) {
      forEachChildInDeepShortCircuit(nodes, selector, Nil, f)
    }
  }
  
  private def forEachIn[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val n = g(i)
        if (s.isDefinedAt(n))
          f(PathVal(s(n), reverse(i :: parentPath)))
      }
    }
  }
  
  private def forEachInDeep[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val n = g(i)
        if (s.isDefinedAt(n))
          f(PathVal(s(n), reverse(i :: parentPath)))
        val ch = n.children
        if (!ch.isEmpty)
          forEachInDeep(ch, s, i :: parentPath, f)
      }
    }
  }
  
  private def forEachInDeepShortCircuit[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val n = g(i)
        if (s.isDefinedAt(n))
          f(PathVal(s(n), reverse(i :: parentPath)))
        else {
          val ch = n.children
          if (!ch.isEmpty)
            forEachInDeepShortCircuit(ch, s, i :: parentPath, f)
        }
      }
    }
  }
  
  private def forEachChildIn[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val ch = g(i).children
        if (!ch.isEmpty)
          forEachIn(ch,s, i::parentPath, f)
      }
    }
  }

  private def forEachChildInDeep[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val ch = g(i).children
        if (!ch.isEmpty)
          forEachInDeep(ch,s, i::parentPath, f)
      }
    }
  }

  private def forEachChildInDeepShortCircuit[A,U](g: Group[Node], s: Selector[A], parentPath: BottomUp, f: PathVal[A] => U) {
    if (dispatchSelector(s,g)) {
      for(i <- 0 until g.length) {
        val ch = g(i).children
        if (!ch.isEmpty)
          forEachInDeepShortCircuit(ch,s, i::parentPath, f)
      }
    }
  }
    
  /** Returns true if there is a chance that applying the given selector on the group
   * would yield some results. */
  private def dispatchSelector(s: Selector[_], g: Group[Node]) = {
    s match {
      case opt: OptimizingSelector[_] => opt.canMatchIn(g)
      case _ => true // no info about the selector, should proceed
    }
  }

}