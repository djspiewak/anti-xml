package com.codecommit.antixml

import DeepZipper._

/** A zipper which allows deep selection.
 *
 *  Zipper instances may be created through factory methods on the companion.
 */
trait DeepZipper[+A <: Node] extends Group[A] { self =>

  private case class FullContext(
    nodeLoc: NodeLoc[Node],
    parentsList: ParentsList,
    updateTime: Time)

  /*
   * All the vectors beneath should have the same length.
   */

  /** Keeping track of internal time.
   *
   *  Should be initialized to 0 at the initial creation of the zipper.
   */
  protected def time: Time

  /** The initial time of the zipper. */
  private val initTime: Time = 0

  /** The last update time corresponding to each node in the zipper.
   *
   *  Should be initialized to 0 values at the initial creation of the zipper.
   */
  protected def updateTimes: Vector[Time]

  protected def parent: DeepZipper[Node]

  /** The location of each node in the group in its parent's list of children. */
  protected def locations: Vector[Location]

  /** Each item in the vector is the list of parents of the corresponding
   *  node in the group.
   *
   *  The order of the parents is given in reverse. i.e. the first item in the
   *  list is the direct parent of the corresponding node in the group,
   *  and the last one is at the root of the tree.
   */
  protected def parentLists: Vector[ParentsList]

  /** The full context list of the zipper. */
  private lazy val fullContext = {
    val nodesWithLocs = (self.toVector zip locations).map(Function.tupled(NodeLoc[A]))

    // constructing a vector of full context objects from the vectorized constituents
    val nodesWithContext =
      for (i <- nodesWithLocs.indices; val updateTime = updateTimes(i); if updateTime != initTime) yield {
        // only taking items that were updated 
        FullContext(nodesWithLocs(i), parentLists(i), updateTime)
      }

    nodesWithContext
  }

  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVectorCase) with DeepZipper[B] {
      val parentLists = self.parentLists
      val locations = self.locations
      def parent = self.parent
      val mergeDuplicates = self.mergeDuplicates

      // setting new time
      val time = self.time + 1
      val updateTimes = self.updateTimes.updated(index, this.time)
    }
  }

  /** Applying the node updates. */
  lazy val unselect: DeepZipper[Node] = {
    if (fullContext.isEmpty) self.parent // no updates
    else {
      // grouping the nodes by their depth in the tree
      val byDepth = fullContext.groupBy(_.parentsList.length).withDefaultValue(Vector())

      val newVals = mergeContext(byDepth)

      new Group(newVals.toVectorCase) with DeepZipper[Node] {
        val parentLists = self.parent.parentLists
        val locations = self.parent.locations
        def parent = self.parent.parent
        val mergeDuplicates = self.parent.mergeDuplicates

        // setting new time
        val time = self.parent.time + 1
        val updateTimes = self.parent.updateTimes.map(t => time)
      }
    }
  }

  /** The zipper context grouped by depth in the tree. */
  private type DepthContext = Map[Int, Seq[FullContext]]

  /** Merging all the nodes that were update in the zipper to provide the new
   *  values after unselection.
   *
   *  Assuming the context is not empty.
   */
  private def mergeContext(context: DepthContext): Group[Node] = {
    assert(!context.isEmpty, "Cannot merge an empty context")
    val (depth, deepest) = context.maxBy(_._1) // the nodes deepest in the tree

    // having only a single depth, the context is fully merged
    if (depth == 0) mergeRoot(context) 
    else {
      val newDepth = depth - 1 // merging a single level
      val newDepthSeq = (newDepth, context(newDepth) ++ mergeDepth(deepest)) // merging with entries at the new depth
      val newContext = (context - depth) + newDepthSeq // removing old depth, setting the new one
      mergeContext(newContext)
    }
  }

  /** Merging a depth context at the root level into the original parent. */
  private def mergeRoot(root: DepthContext) = {
    val flat = root.values.flatten
    val byLoc = flat.groupBy(_.nodeLoc.loc)
    val uniques = mergeRootDuplicates(byLoc)
    mergeWithOriginal(parent, uniques)
  }

  /** Applying the merging strategy to a to full contexts at duplicate locations at the root level. */
  private def mergeRootDuplicates(root: Map[Location, Iterable[FullContext]]) = {
    val uniques = // merging duplicates
      root.map { lc =>
        val (l, c) = lc
        val orig = parent(l)
        val alternatives = c.map(fc => (fc.nodeLoc.node, fc.updateTime)).toSeq
        val (merged, _) = mergeDuplicates(orig, alternatives)
        NodeLoc(merged, l)
      }
    uniques
  }

  /** Taking a sequence of context at a single depth and merging them into a list
   *  of context at depth -1 from the original.
   */
  private def mergeDepth(singleDepth: Seq[FullContext]) = {
    val byParent = singleDepth.groupBy(_.parentsList)
    byParent.map(mergeParent)
  }

  /** Taking a sequence of contexts under a single parent's list and merging them
   *  into a single context at the same depth as the direct parent of the sequence.
   */
  private def mergeParent(parentVals: (ParentsList, Seq[FullContext])) = {
    val (parents, vals) = parentVals

    // The parent list is never empty because the merging stops at the lowest depth.
    assert(!parents.isEmpty, "Cannot merge under an empty parent.")

    val directParentLoc = parents.head
    val directParent = directParentLoc.elem
    val grandParents = parents.tail

    val uniques = uniqueLocations(directParent, parents, vals)
    val oldChildren = directParent.children

    val newChildren = mergeOriginalWithContext(oldChildren, uniques)

    // the update time of the parent is the max of the children
    val newUpdateTime = uniques.maxBy(_.updateTime).updateTime

    val newParent = directParent.copy(children = newChildren)
    val newParentLoc = NodeLoc(newParent, directParentLoc.loc)

    FullContext(newParentLoc, grandParents, newUpdateTime)
  }

  /** Taking a sequence of contexts under the given direct parent and having the given
   *  parent's list and merging nodes which represent the same location under the parent.
   *  @return A list contexts which are all unique under the given parent.
   */
  private def uniqueLocations(directParent: Elem, parents: ParentsList, contexts: Seq[FullContext]) = {

    val byLocation = contexts.groupBy(_.nodeLoc.loc) // grouping by nodes by the location in the parent
    val locationUnique = byLocation.map { entry =>
      val (loc, context) = entry
      val origNode = directParent.children(loc) // the node before updates on the zipper
      val modNodes = context.map(c => (c.nodeLoc.node, c.updateTime))

      val (newNode, updateTime) = mergeDuplicates(origNode, modNodes)
      FullContext(NodeLoc(newNode, loc), parents, updateTime)
    }
    locationUnique
  }

  /** Merges an iterable of context objects into the original group representing them. */
  private def mergeOriginalWithContext(originals: Group[Node], contexts: Iterable[FullContext]) = {
    mergeWithOriginal(originals, contexts.map(_.nodeLoc))
  }

  /** Merges an iterable of node locations into the original group reprsenting them. */
  private def mergeWithOriginal[B >: A <: Node](originals: Group[Node], nodeLocs: Iterable[NodeLoc[B]]) = {
    val newNodes = nodeLocs.foldLeft(originals) { (oldNodes, nl) =>
      val NodeLoc(node, loc) = nl
      oldNodes.updated(loc, node)
    }

    newNodes
  }

  /** The merge strategy for the duplicate nodes in the zipper.
   *
   *  TODO consider passing it as an implicit to the unselect function, or maybe
   *  just simple currying
   */
  protected val mergeDuplicates: NodeMergeStrategy

}

/** A factory for [[DeepZipper]] instances.
 *  Zippers may be created directly from groups through [[DeepZipper.groupToZipper]] or
 *  through selection using a [[PathFunction]] with [[DeepZipper.fromPath]]
 *
 *  By importing the implicits in this object any [[Selectable]] can be pimped with
 *  shallow/deep selection methods, which directly take selectors as input.
 *  TODO examples
 */
object DeepZipper {

  /** The number represents the number of the node in its parent's children list.
   *  In case the node is root, the number is its position in the group to which it belongs.
   */
  private[antixml]type Location = Int

  private[antixml] sealed abstract class WithLoc[+A <: Node](node: Node, loc: Location)
  /** A location of a node within its parent. */
  private[antixml] case class NodeLoc[+A <: Node](node: A, loc: Int) extends WithLoc[A](node, loc)
  /** Parents can only be [[Elem]]s. */
  private[antixml] case class ParentLoc(elem: Elem, loc: Int) extends WithLoc[Elem](elem, loc)

  /** Represents a list of a node's parents, where the first item is the direct
   *  parent of the node, and the last is the root of the tree.
   */
  private[antixml]type ParentsList = List[ParentLoc]

  /** The units in which time is measured in the zipper. Assumed non negative. */
  private[antixml]type Time = Int

  /** A merging function which takes a node, which represents the node before any modifications
   *  and a sequence of nodes with their corresponding update times,
   *  which are versions of the same node after some modifications.
   *
   *  The function decides how to merge the nodes to produce a single node with corresponding time stamp.
   *
   *  It should be taken into account that nodes may be modified directly by the user
   *  or through mergings from deeper levels.
   */
  private[antixml]type NodeMergeStrategy = (Node, Seq[(Node, Time)]) => (Node, Time)

  private[antixml]type Path[+A <: Node] = Seq[(NodeLoc[A], ParentsList)]
  /** A function that creates paths on group, to be used when constructing zippers. */
  private[antixml]type PathFunction[+A <: Node] = Group[Node] => Path[A]

  /** Pimping selectables with [[DeepZipper]] methods. */
  implicit def groupableToSelectable[A <: Node](g: Selectable[A]) = {
    import PathCreator._
    new {
      //TODO using strange names to avoid conflicts

      private def zipper[B <: Node](path: PathFunction[B]) = {
        fromPath(g.toGroup, path)
      }

      /** Searching at the current level . */
      def ~\[B <: Node](s: Selector[B]) = {
        zipper(fromNodes(s))
      }

      /** Searching on all levels (breadth first). */
      def ~\\[B <: Node](s: Selector[B]) = {
        zipper(all(s))
      }

      /** Searching one level below. */
      def >[B <: Node](s: Selector[B]) = {
        zipper(directChildren(s))
      }

      /** Searching one level below and beyond (breadth first). */
      def ~[B <: Node](s: Selector[B]) = {
        zipper(allChildren(s))
      }

    }
  }

  /** Converts a group into a zipper. */
  def groupToZipper[A <: Node](group: Group[A]) = {
    group match {
      case zipper: DeepZipper[_] => zipper
      case _ => {
        val emptyParent: ParentsList = List[ParentLoc]()
        val locs = Vector(group.indices: _*)

        new Group[A](group.toVectorCase) with DeepZipper[A] {
          val parentLists = locs.map(_ => emptyParent)
          val locations = locs
          def parent = sys.error("Root has no parent")
          val mergeDuplicates = BasicNodeMergeStrategy // TODO this should be pluggable

          val time = 0
          val updateTimes = locs.map(_ => time)
        }
      }
    }
  }

  /** Converts the nodes gathered from applying the path function to the given group into a zipper. */
  def fromPath[A <: Node](group: Group[Node], path: PathFunction[A]) = {
    import com.codecommit.antixml.util.VectorCase

    val pathVals = path(group)
    val pathNodes = VectorCase.fromSeq(pathVals.map(_._1.node))

    new Group[A](pathNodes) with DeepZipper[A] {
      val parentLists = Vector(pathVals.map(_._2): _*)
      val locations = Vector(pathVals.map(_._1.loc): _*)
      val parent = groupToZipper(group)
      val mergeDuplicates = parent.mergeDuplicates

      // setting new time
      val time = 0
      val updateTimes = locations.map(_ => time)
    }
  }

  /** A factory for [[PathFunction]]s  */
  object PathCreator {
    //TODO generalize to arbitrary types of selectors

    /*
     * First applying the paths using the overloads without the selector,
     * then applying the selector.
     * This way the traversal is not modified during selection.
     */

    /** A path function that selects on nodes in the given group. */
    def fromNodes[A <: Node](selector: Selector[A])(nodes: Group[Node]): Path[A] = {
      applySelector(selector)(fromNodesWithParent(Nil, nodes))
    }

    /** A path function that selects on the given nodes and recursively on the children (breadth first). */
    def all[A <: Node](selector: Selector[A])(nodes: Group[Node]): Path[A] = {
      fromNodes(selector)(nodes) ++ allChildren(selector)(nodes)
    }

    /** A path function that selects on the children of the given group. */
    def directChildren[A <: Node](selector: Selector[A])(nodes: Group[Node]): Path[A] = {
      applySelector(selector)(directChildren(nodes))
    }

    /** A path function that selects on the recursively on all the children of the given group (breadth first). */
    def allChildren[A <: Node](selector: Selector[A])(nodes: Group[Node]): Path[A] = {
      applySelector(selector)(allChildren(nodes))
    }

    /** Lifting the selector so that it can operate on path entries. */
    private def liftSelector[A <: Node](s: Selector[A]): PartialFunction[(NodeLoc[Node], ParentsList), (NodeLoc[A], ParentsList)] = {
      case (NodeLoc(n, i), p) if s.isDefinedAt(n) => (NodeLoc(s(n), i), p)
    }

    /** Applies the selector to the given path. */
    private def applySelector[A <: Node](s: Selector[A])(path: Path[Node]) = {
      path.collect(liftSelector(s))
    }

    /** Converting a group of nodes to the corresponding node locations. */
    private def nodesToLocs[A <: Node](nodes: Group[Node]) = {
      nodes.zipWithIndex.map(Function.tupled(NodeLoc[Node]))
    }

    /** Creating a path from this group of nodes. */
    private def fromNodesWithParent(p: ParentsList, n: Group[Node]) = {
      nodesToLocs(n) zip Stream.continually[ParentsList](p)
    }

    private def directChildren(nodes: Group[Node]): Path[Node] = collectChild(nodes, Nil)

    private def allChildren(nodes: Group[Node]): Path[Node] = {
      allChildren(directChildren(nodes))
    }

    /** Recursively taking all the children of a given path. */
    private def allChildren(p: Path[Node]): Path[Node] = {
      if (p.isEmpty) Nil
      else {
        val children =
          p.flatMap{ nlp =>
            val (NodeLoc(n, l), p) = nlp
            collectChild(n, l, p)
          }
        p ++ allChildren(children)
      }
    }

    /** Collecting the children of a single node. */
    private def collectChild(n: Node, l: Location, p: ParentsList): Path[Node] = {
      n match {
        case e: Elem => fromNodesWithParent(ParentLoc(e, l) :: p, e.children)
        case _ => Nil
      }
    }

    /** Collecting the children of the given nodes. */
    private def collectChild(n: Group[Node], p: ParentsList): Path[Node] = {
      n.zipWithIndex flatMap { nl =>
        val (n, l) = nl
        collectChild(n, l, p)
      }
    }
  }

  /** A basic merging strategy which takes the most recent updates from the different nodes
   *  and creates a single node from them.
   */
  private[antixml] object BasicNodeMergeStrategy extends NodeMergeStrategy {
    def apply(node: Node, alternatives: Seq[(Node, Time)]) = {
      val max @ (newestNode, maxUpTime) = alternatives.maxBy(_._2) // by time

      node match {
        case e: Elem => {
          newestNode match {
            case ne: Elem =>
              // at least one elem to merge with
              mergeElem(e, alternatives.collect({ case (e: Elem, t: Time) => (e, t) })) // deconstructing because of type erasure 
            case _ => max // simple nodes, not trying to combine anything, just taking the most recent
          }
        }
        case _ => max // a simple node again
      }
    }

    /** Trying to merge an [[Elem]] with others of the same type, assuming that changes may have propagated from
     *  the children, hence separately merging the children and the rest of the node.
     */
    private def mergeElem(origElem: Elem, alternatives: Seq[(Elem, Time)]) = {
      val child = (_: Elem).children;

      // properties that don't depend on deeper levels
      val pref = (_: Elem).prefix;
      val name = (_: Elem).name;
      val attrs = (_: Elem).attrs;
      val scope = (_: Elem).scope;
      val nonChildFuncs = List(pref, name, attrs, scope)

      def maxBy(f: Elem => Any) = alternatives.maxBy { et =>
        val (e, t) = et
        if (f(e) != f(origElem)) Some(t)
        else None
      }

      // the nodes that were updated in non child parts
      val (maxNonChild, nonChildTime) = nonChildFuncs.map(maxBy(_)).maxBy(_._2) // by time
      val (maxChild, childTime) = maxBy(child)

      (maxNonChild.copy(children = maxChild.children), nonChildTime.max(childTime))
    }
  }
}