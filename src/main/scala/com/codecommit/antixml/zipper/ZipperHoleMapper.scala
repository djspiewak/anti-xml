package com.codecommit.antixml

import Zipper._
import ZipperHoleMapper._
import util.VectorCase
import scala.collection.GenTraversableOnce
import CanBuildFromWithZipper._

/** Converts a zipper into a hole map. */
private[antixml] trait ZipperHoleMapper { self: Zipper[Node] =>

  private val initHoleInfoItem: (VectorCase[(Node, Time)], Time) = (util.Vector0, 0)

  private type HoleMapGet[A] = A => (ZipperPath, Time, GenTraversableOnce[Node])

  /** Adding items to the hole info object using the given getter function to transform the items
   *  into the appropriate format.
   */
  private def addToHoleInfo[A](items: Seq[A], h: HoleInfo, get: HoleMapGet[A]) = {
    (h /: items) { (hi, item) =>
      val (path, time, nodes) = get(item)
      val (oldNodes, oldTime) = hi.getDeep(path).getOrElse(initHoleInfoItem)
      val newItems = (oldNodes /: nodes) { case (old, node) => old :+ (node, time) }
      val newTime = math.max(oldTime, time)
      hi.updatedDeep(path, (newItems, newTime))
    }
  }

  /** Using the given context to convert self to a hole map. */
  def toHoleInfo(context: Context): HoleInfo = {
    val Context(_, _, metas, additionalHoles, hiddenNodes) = context

    /* Getters for the different parts of the zipper. */

    val indicesGet = (i: Int) => {
      val (path, time) = metas(i)
      val items = util.Vector1(self(i))
      (path, time, items)
    }

    val hiddenGet = (ewc: ElemsWithContextHidden) => {
      val ElemsWithContextHidden(path, time, items) = ewc
      (path, time, items)
    }

    val additonalGet = (pt: (ZipperPath, Time)) => {
      val (path, time) = pt
      (path, time, util.Vector0)
    }

    case class ItemsGet[A](items: Seq[A], get: HoleMapGet[A])
    val itemsGetters = List(
        ItemsGet(indices, indicesGet), 
        ItemsGet(hiddenNodes, hiddenGet), 
        ItemsGet(additionalHoles, additonalGet))

    val holeInit: HoleInfo = ZipperHoleMap.empty
    (holeInit /: itemsGetters) {
      case (hi, ItemsGet(items, get)) =>
        addToHoleInfo(items, hi, get)
    }
  }
}

object ZipperHoleMapper {
  /** Each hole is associated with a list of node/time pairs as well as a master update time */
  private[antixml] type HoleInfo = ZipperHoleMap[(VectorCase[(Node,Time)],Time)]
}