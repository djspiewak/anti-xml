package com.codecommit.antixml

import Zipper._
import ZipperHoleShifting._
import util.VectorCase
import scala.collection.immutable.SortedSet
import CanBuildFromWithZipper._

/** Responsible for zipper hole shifting support. */
private[antixml] trait ZipperHoleShifting extends ZipperHoleMapper { self: Zipper[Node] =>

  /** Shifts the focus of the zipper to another set of holes.
   *
   *  The shifting is performed using a shifting function which is applied to each
   *  path visible in the zipper and produces a new sequence of paths.
   *  These paths are sorted lexicographically and duplicates are removed.
   *
   *  A new zipper displaying the above paths is returned while internally maintaining data
   *  about any previously contained paths.
   *
   *  The values which are attached to the paths come from two sources:
   *  - If the zipper previously contained the path, the data attached to it is used.
   *  - If the path is new, the data is fetched directly from the parent of the zipper.
   *
   *  In case a hole was previously multiplied (e.g. using flatMap) it is placed
   *  as is in the resulting zipper.
   *
   *  Note: shifting is not supported for parentless (broken) zippers.
   *
   *  @param shiftFunc A function to be supplied with the parent of the zipper
   *  and applied to the indexed contents of the zipper.
   *  Assumed to produce valid paths with regard to the supplied parent.
   */
  private[antixml] def shiftHoles(shiftFunc: Group[Node] => ZipperPath => Seq[ZipperPath]): Zipper[Node] = context match {
    case Some(context @ Context(parent, lastUpdate, metas, additionalHoles, hiddenNodes)) => {
      implicit val lexicographic = ZipperPathOrdering

      val shift = shiftFunc(parent)
      val unsoretedPaths = for {
        m <- metas
        path <- shift(m._1) if path != ZipperPath.empty // ignoring empty paths
      } yield path

      // not allowing duplicates and empty paths and sorting lexicographically
      val newPaths = SortedSet(unsoretedPaths: _*)
      val holeInfo = toHoleInfo(context)

      val b = newZipperContextBuilder[Node](Some(parent))
      val pathsInit: VectorCase[ElemsWithContextVisible[Node]] = util.Vector0

      val (unusedPaths, usedPaths) = // leaving paths that were never used before
        holeInfo.depthFirst.foldLeft((newPaths, pathsInit)) {
          case ((nPaths, used), hole) =>
            val (path, (nodesTimes, masterTime)) = hole

            val holes =
              if (nodesTimes.isEmpty) Seq((path, masterTime, util.Vector0))
              else nodesTimes.map { case (n, t) => (path, t, Seq(n)) } // this contains duplicates for multiplied locations

            val visible = (ElemsWithContextVisible.apply[Node] _).tupled
            val hidden = (ElemsWithContextHidden.apply _).tupled

            if (nPaths contains path) {
              (nPaths - path, used ++ holes.map(visible))
            } else {
              b ++= holes.map(hidden)
              (nPaths, used)
            }
        }

      val initTime = 0 // these paths were never modified
      val unusedElems = unusedPaths.toList map { p =>
        ElemsWithContextVisible[Node](p, initTime, PathFetcher.getNode(parent)(p))
      }

      // this can contain duplicate locations from the previously used paths
      val visibleElems = (unusedElems ++ usedPaths).sortBy(_.path)
      b ++= visibleElems

      b.result
    }
    case None => sys.error("Cannot shift root.")
  }
}

private object ZipperHoleShifting {
  /** Lexicographic ordering for path objects. */
  private object ZipperPathOrdering extends Ordering[ZipperPath] {
    override def compare(x: ZipperPath, y: ZipperPath) =
      Ordering.Iterable[Int].compare(x,y)
  }
}