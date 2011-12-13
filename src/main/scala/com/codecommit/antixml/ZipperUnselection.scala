package com.codecommit.antixml

import Zipper._
import util.VectorCase
import collection.immutable.IndexedSeq

/** Provides unselection support for zipper. */
private[antixml] trait ZipperUnselection { self: Zipper[Node] =>

  /** Applies the node updates to the context parent and returns the result. */
  private def unselect(context: Context, mergeStrategy: ZipperMergeStrategy) = {
    val topLevelHoleInfo = toHoleInfo(context)
    pullBackGroup(context.parent, topLevelHoleInfo, mergeStrategy)._1.asInstanceOf[Zipper[Node]]
  }

  /** Returns the pullback of the nodes in the specified group.
   *  @param nodes the group containing the nodes to pull back.
   *  @param holeInfo the HoleInfo corresponding to the group.
   *  @param zms The strategy to be used for conflict resolution.
   *  @return the pullBacks of the groups children, concatenated together, along with the latest update
   *  time.
   */
  private def pullBackGroup(nodes: Group[Node], holeInfo: HoleInfo, zms: ZipperMergeStrategy): (Group[Node], Time) = {
    var maxTime: Int = 0 //mutable for performance and to avoid further complicating`conditionalFlatMapWithIndex`.
    val updatedGroup = nodes.conditionalFlatMapWithIndex[Node] { (node, index) =>
      node match {
        case elem: Elem if (holeInfo.hasChildrenAt(index)) => {
          val (newNodes, time) = pullUp(elem, index, holeInfo, zms)
          maxTime = math.max(maxTime, time)
          Some(newNodes)
        }
        case _ if holeInfo.contains(index) => {
          val (newNodes, time) = holeInfo(index)
          maxTime = math.max(maxTime, time)
          Some(newNodes.map { _._1 })
        }
        case _ => None
      }
    }
    (updatedGroup, maxTime)
  }

  /** Returns the pullback of an element that is known to be above a hole (and thus has
   *  child updates that need to be pulled up).
   *
   *  @param elem the element
   *  @param indexInParent the index of the element in its parent
   *  @param holeInfo the HoleInfo corresponding to the parent group
   *  @param zms The strategy to be used for conflict resolution.
   *  @return the pulled back nodes and their combined update time
   *
   *  @note assumes `holeInfo.hasChildrenAt(indexInParent) == true`
   */
  private def pullUp(elem: Elem, indexInParent: Int, holeInfo: HoleInfo, zms: ZipperMergeStrategy): (VectorCase[Node], Time) = {
    //Recursively pull back children 
    val (childGrp, childTime) = pullBackGroup(elem.children, holeInfo.children(indexInParent), zms)
    val indirectUpdate = elem.copy(children = childGrp)
    if (holeInfo.contains(indexInParent)) {
      //This is a conflicted hole, so merge.
      mergeConflicts(elem, holeInfo(indexInParent), (indirectUpdate, childTime), zms)
    } else {
      //No conflicts, just let the child updates bubble up
      (VectorCase(indirectUpdate), childTime)
    }
  }

  /** Merges updates at a conflicted node in the tree.  See the unselection algorithm, above, for more information.
   *  @param node the conflicted node
   *  @param directUpdates the direct updates to `node`.
   *  @param indirectUpdate the indirectUpdate to `node`.
   *  @param mergeStrategy The strategy to be used for conflict resolution.
   *  @return the sequence of nodes to replace `node`, along with an overall update time for `node`.
   */
  private def mergeConflicts(node: Elem, directUpdates: (IndexedSeq[(Node, Time)], Time), indirectUpdate: (Node, Time), mergeStrategy: ZipperMergeStrategy): (VectorCase[Node], Time) = {
    val mergeContext = ZipperMergeContext(original = node, lastDirectUpdate = directUpdates._2, directUpdate = directUpdates._1,
      indirectUpdate = indirectUpdate)

    val result = mergeStrategy(mergeContext)
    (VectorCase.fromSeq(result), math.max(directUpdates._2, indirectUpdate._2))
  }
}