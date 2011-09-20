package com.codecommit.antixml

import DeepZipper._

 /** Defines type related to paths on a tree.
  * Also contains factory methods for [[PathFunction]]s  */
object PathCreator {

  /** The values from a path function in raw form. */
  private[antixml] type PathVals[+A] = Seq[(WithLoc[A], ParentsList)]

  /** A wrapper for path function values, cannot contain duplicate locations. */
  private[antixml] class Path[+A](vals: PathVals[A]) {
    private val contexts =
      vals.map { wp =>
        val (withLoc, parents) = wp
        (LocationContext(withLoc.loc, parents, initTime), withLoc.content)
      }
    
    /** The location contexts and the corresponding contents. */
    val (locs, contents) = contexts.unzip
    require(locs.toSet.size == locs.size, "Cannot have duplicate locations in path") // enforcing no duplicates policy 
  }
  
  /** A function that creates paths on group, to be used when constructing zippers. */
  private[antixml] type PathFunction[+A] = Group[Node] => PathVals[A]
  
  /*
     * First applying the paths using the overloads without the selector,
     * then applying the selector.
     * This way the traversal is not modified during selection.
     */

    /** A path function that selects on nodes in the given group. */
    def fromNodes[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
      applySelector(selector)(fromNodesWithParent(Nil, nodes))
    }

    /** A path function that selects on the given nodes and recursively on the children (breadth first). */
    def all[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
      fromNodes(selector)(nodes) ++ allChildren(selector)(nodes)
    }

    /** A path function that selects on the children of the given group. */
    def directChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
      applySelector(selector)(directChildren(nodes))
    }

    /** A path function that selects on the recursively on all the children of the given group (breadth first). */
    def allChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
      applySelector(selector)(allChildren(nodes))
    }

    /** Lifting the selector so that it can operate on path entries. */
    private def liftSelector[A](s: Selector[A]): PartialFunction[(WithLoc[Node], ParentsList), (WithLoc[A], ParentsList)] = {
      case (WithLoc(n, i), p) if s.isDefinedAt(n) => (WithLoc(s(n), i), p)
    }

    /** Applies the selector to the given path. */
    private def applySelector[A](s: Selector[A])(path: PathVals[Node]): PathVals[A] = {
      path.collect(liftSelector(s))
    }

    /** Converting a group of nodes to the corresponding node locations. */
    private def nodesToLocs[A <: Node](nodes: Group[Node]) = {
      nodes.zipWithIndex.map(Function.tupled(WithLoc[Node]))
    }

    /** Creating a path from this group of nodes. */
    private def fromNodesWithParent(p: ParentsList, n: Group[Node]) = {
      nodesToLocs(n) map ((_, p))
    }

    private def directChildren(nodes: Group[Node]): PathVals[Node] = collectChild(nodes, Nil)

    private def allChildren(nodes: Group[Node]): PathVals[Node] = {
      allChildren(directChildren(nodes))
    }

    /** Recursively taking all the children of a given path. */
    private def allChildren(p: PathVals[Node]): PathVals[Node] = {
      if (p.isEmpty) Nil
      else {
        val children =
          p.flatMap{ nlp =>
            val (WithLoc(n, l), p) = nlp
            collectChild(n, l, p)
          }
        p ++ allChildren(children)
      }
    }

    /** Collecting the children of a single node. */
    private def collectChild(n: Node, l: Location, p: ParentsList): PathVals[Node] = {
      n match {
        case e: Elem => fromNodesWithParent(ParentLoc(e, l) :: p, e.children)
        case _ => Nil
      }
    }

    /** Collecting the children of the given nodes. */
    private def collectChild(n: Group[Node], p: ParentsList): PathVals[Node] = {
      n.zipWithIndex flatMap { nl =>
        val (n, l) = nl
        collectChild(n, l, p)
      }
    }
}