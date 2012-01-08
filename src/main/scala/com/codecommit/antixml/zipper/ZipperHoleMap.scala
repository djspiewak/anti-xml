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
package zipper

import util.{VectorCase}
import scala.annotation.tailrec
import scala.collection.immutable.{Map, IndexedSeq}
import scala.collection.{Traversable}

/** Used by `Zipper` to associate information with its "holes".
 *
 * The `ZipperHoleMap` has a similar tree structure to `Group` and is used to maintain
 * information about selected locations within the `Group` tree.  It differs from `Group`
 * in the following respects:
 *   - It allows arbitrary element types.
 *   - It maintains its children separately from its elements
 *   - It is sparse; Only a subset of it's indices are valued.
 *
 * This structure can also be viewed as a map from `ZipperPath` to `B`, although it does
 * not actually implement the `Map` trait.
 *
 * @tparam the element type of the `ZipperHoleMap`. 
 * @see [[com.codecommit.antixml.Zipper]]
 */
private[antixml] final class ZipperHoleMap[+B] private (items: Map[Int, ZipperHoleMap.HoleMapNode[B]]) { self =>
  import ZipperHoleMap._
  
  private def find(i: Int): HoleMapNode[B] = 
    items.getOrElse(i,unusedLocation)

  /** Returns the value at the specified position or throws an exception if there is no
   *  such value (if `contains(loc) == false`).
   */
  def apply(loc: Int): B = find(loc).value.get

  /** Returns the value at the specified position or `None` if there is no
   *  such value (if `contains(loc) == false`).
   */
  def get(loc: Int): Option[B] = find(loc).value
  
  /** Tests whether the hole map contains a value at the specified position */
  def contains(loc: Int): Boolean = find(loc).value.isDefined

  /** Returns the children at the specified position or throws an exception if there are
   *  no children there (if `hasChildrenAt(loc) == false`).
   */
  def children(loc: Int):ZipperHoleMap[B] = find(loc).children.get
  
  /** Tests whether the hole map has children at the specified position */
  def hasChildrenAt(loc: Int): Boolean = find(loc).children.isDefined
  
  /** Returns the value at the deep location at the specified path, or `None` if there is
   *  no such value.
   */
  def getDeep(path: ZipperPath): Option[B] = getDeep(path, 0)
  
  @tailrec
  private def getDeep(path: ZipperPath, from: Int): Option[B] = {
    if (from==(path.length - 1))
      get(path.valueAt(from))
    else find(path(from)) match {
      case HoleMapNode(_, Some(c)) => c.getDeep(path,from+1)
      case _ => None
    }
  }    
  
  /** Creates a new `ZipperHoleMap` by updating the value at the specified deep location. */
  final def updatedDeep[B2 >: B](path: ZipperPath, value: B2): ZipperHoleMap[B2] = updatedDeep(path,0,value)
  
  private def updatedDeep[B2 >: B](path: ZipperPath, from: Int, value: B2): ZipperHoleMap[B2] = {
    val loc = path.valueAt(from)
    val HoleMapNode(v,c) = find(loc)
    if (from == (path.length - 1))
      new ZipperHoleMap(items.updated(loc, HoleMapNode(Some(value), c)))
    else {
      val updatedChildren = c.getOrElse(empty).updatedDeep(path,from+1,value)
      new ZipperHoleMap(items.updated(loc, HoleMapNode(v, Some(updatedChildren))))
    }
  }

  /** Returns a traversable represented the tree's contents in pre-order (lexicographically by path).
   * Note that this has not been optimized, as it is only currently used by toString and for testing.
   * 
   * TODO using this for zipper shifting, consider optimizing
   */
  def depthFirst: Traversable[(ZipperPath, B)] = 
    new Traversable[(ZipperPath, B)] {
      override def foreach[U] (f: ((ZipperPath, B)) => U) {
        self.traverseDepthFirst(Nil, f)
      }
    }
  
  private def traverseDepthFirst[U](parents : List[Int], f: ((ZipperPath, B)) => U) {
    val sortedItems = items.toSeq.sortBy(_._1)
    sortedItems foreach {case (loc,node) =>
      val p2 = loc :: parents
      if (node.value.isDefined)
        f((ZipperPath.reversed(p2), node.value.get))
      if (node.children.isDefined)
        node.children.get.traverseDepthFirst(p2,f)
    }
  }
  
  override def toString = {
    val els = depthFirst.view map { case (p,v) =>
      p.mkString("[",",","]") + " -> "+v
    }
    els.mkString("[",", ","]")
  }
}

private[antixml] object ZipperHoleMap {
  def apply[B](items: (ZipperPath,B)*) = {
    val e: ZipperHoleMap[B] = empty
    (e /: items) { (mp,i) => mp.updatedDeep(i._1, 0, i._2) }
  }
  
  private [antixml] case class HoleMapNode[+B](value: Option[B], children: Option[ZipperHoleMap[B]])
  
  private val unusedLocation = HoleMapNode(None,None)
  
  val empty: ZipperHoleMap[Nothing] = new ZipperHoleMap(Map.empty[Int,Nothing])
}
