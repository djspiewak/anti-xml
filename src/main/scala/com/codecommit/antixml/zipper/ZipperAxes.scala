package com.codecommit.antixml
package zipper

import scala.annotation.tailrec

/**
 * Adds some XPath like axes to [[com.codecommit.antixml.Zipper]] instances.
 * 
 * Note1: the axes are applied to each node in a zipper individually and the result
 * is a new zipper with the nodes concatenated and sorted lexicographically by
 * location (removing any duplicate locations).
 * 
 * Note2: the axes are calculated using holes in the zipper, hence for a modified
 * zipper some nodes may be multiplied or elided.
 */
private[antixml] trait ZipperAxes { self: Zipper[Node] =>
  /** Returns the direct parent of a node. */
  def directParent = {
    shiftHoles (g => (PathTransformer(g).shiftUp(_)).andThen(_.toList))
  }
  
  /** Returns the ancestors of a node. */
  def ancestor = transFuncToShift(_.shiftUp, false)

  /** Returns the ancestors of a node including itself. */
  def ancestorOrSelf = transFuncToShift(_.shiftUp, true)
  
  /** Returns the following siblings of a node. */
  def followingSibling = transFuncToShift(_.shiftRight, false)
  
  /** Returns the following siblings of a node including itself. */
  def followingSiblingOrSelf = transFuncToShift(_.shiftRight, true)

  /** Returns the preceding siblings of a node. */
  def precedingSibling = transFuncToShift(_.shiftLeft, false)
  
  /** Returns the preceding siblings of a node including itself. */
  def precedingSiblingOrSelf = transFuncToShift(_.shiftLeft, true)

  /** Takes a path transformer function and converts it to a shifting function which is applied until
   *  the transformer return `None`.
   *  @param appendSource True if the initial path should be part of the result.
   */
  private def transFuncToShift(func: PathTransformer => ZipperPath => Option[ZipperPath], withSource: Boolean) = {
    shiftHoles { g =>
      val pathToOpt = func(PathTransformer(g))

      @tailrec
      def traverse(path: ZipperPath, res: List[ZipperPath]): List[ZipperPath] = {
        val opt = pathToOpt(path)
        opt match {
          case None => res
          case Some(p) => traverse(p, p :: res)
        }
      }

      val shiftFunc = (p: ZipperPath) => {
        val init =
          if (withSource) List(p)
          else List()
        traverse(p, init)
      }
      shiftFunc
    }
  }
  
}