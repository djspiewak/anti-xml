package com.codecommit.antixml

import scala.annotation.tailrec
import PathTransformer._

/** Transforms [[com.codecommit.antixml.ZipperPath]]s with predefined functions.
 *
 *  The transformations rely on a source parent group from which all paths are
 *  calculated. Any paths passed to instances of this class are assumed to be valid
 *  paths in the source group.
 *
 *  @param source The source for the transformed paths.
 */
private[antixml] case class PathTransformer(source: Group[Node]) {
  //TODO this whole class is probably quite slow
  //TODO state monad for caching?

  /** Shifts the path one step upwards, if possible.
   * @param path The path to be shifted
   * @return An optional path which is the parent of the original path, a new path transformer
   * with an updated cache. */
  def shiftUp(path: ZipperPath): Option[ZipperPath] = if (path.isEmpty) None else Some(path.init)

  /** Shifts the path one step to the left, if possible.
   * @param path The path to be shifted
   * @return An optional path which is a sibling of the original path from the left, a new path transformer
   * with an updated cache. */
  def shiftLeft(path: ZipperPath): Option[ZipperPath] = {
    shiftSideways(path, -1)
  }

  /** Shifts the path one step to the right, if possible.
   * @param path The path to be shifted
   * @return An optional path which is a sibling of the original path from the right, a new path transformer
   * with an updated cache. */
  def shiftRight(path: ZipperPath): Option[ZipperPath] = {
    shiftSideways(path, +1)
  }

  /** @return An optional node at the end of the path and updated cache. */
  private def getNode(path: ZipperPath): Option[Node] = getNode(source, path)

  @tailrec
  private def getNode(currLevel: Group[Node], path: ZipperPath): Option[Node] = {
    if (path.isEmpty) None
    else if (path.size == 1) Some(currLevel(path.head))
    else {
      // the cast must succeed otherwise the path is invalid
      val children = currLevel(path.head).asInstanceOf[Elem].children
      getNode(children, path.tail)
    }
  }

  /** Tries to shift the path sideways by the given increment. */
  private def shiftSideways(path: ZipperPath, increment: Int): Option[ZipperPath] = {
    assert(!path.isEmpty, "Cannot shift an empty path.")

    val currLevel =
      if (path.size == 1) source
      else {
        val parent = getNode(path.init) // size > 1
        parent.get.asInstanceOf[Elem].children // must be an elem for a valid path
      }

    val end = path.size - 1

    val newLoc = path(end) + increment

      if (currLevel.indices.contains(newLoc)) Some(path.updated(end, newLoc))
      else None
  }
}

private[antixml] object PathTransformer {
  private[antixml]type PathCache = Map[ZipperPath, Option[Node]]
}