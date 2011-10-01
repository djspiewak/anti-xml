package com.codecommit.antixml

import DeepZipper._

 /** Defines type related to paths on a tree.
  * Also contains factory methods for [[PathFunction]]s  */
private[antixml] object PathCreator {

  /** The values from a path function in raw form. */
  type PathVals[+A] = Seq[(WithLoc[A], ParentsList)]
  
  /** A function that creates paths on group, to be used when constructing zippers. */
  type PathFunction[+A] = Group[Node] => PathVals[A]
  
  /** A path function that selects on nodes in the given group. */
  def fromNodes[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroup(nodes, selector, Nil)
  }

  /** A path function that selects on the given nodes and recursively on the children (depth first). */
  def all[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroupRecursive(nodes, Nil, selector)
  }
  
  /**
   * A path function that selects on the given nodes and recursively on the children, returning those
   * matching nodes that are not themselves descendants of matching nodes. (depth first). 
   */
  def allMaximal[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectMaximalRecursive(nodes, Nil, selector)
  }
  
  /** A path function that selects on the children of the given group. */
  def directChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectChildrenOfGroup(nodes, selector)
  }
  
  /** A path function that selects on the recursively on all the children of the given group (depth first). */
  def allChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroupChildren(nodes, Nil, selector) flatMap {case (g,p) => collectGroupRecursive(g,p,selector)}
  }
  
  /** A path function that returns all the matching children that are not descendants of matching children (depth first). */
  def allMaximalChildren[A](selector: Selector[A])(nodes: Group[Node]): PathVals[A] = {
    collectGroupChildren(nodes, Nil, selector) flatMap {case (g,p) => collectMaximalRecursive(g,p,selector)}
  }
  
  /** Collects items from the given group that match the selector. */
  private def collectGroup[A](nodes: Group[Node], s: Selector[A], p: ParentsList): PathVals[A] = {
    dispatchSelector(s, nodes) {
      val ni = nodes.zipWithIndex
      for ((n, i) <- ni if s isDefinedAt n) yield (WithLoc(s(n), i), p)
    }
  }
  
  /** Applies the group selector collection function on the children of the given group. */
  private def collectChildrenOfGroupWith[A]
	  (nodes: Group[Node], s: Selector[A], p: ParentsList)
	  (toVals: (Group[Node], Selector[A], ParentsList) => PathVals[A]): PathVals[A] = {
    dispatchSelector(s, nodes) {
      val ni = nodes.zipWithIndex
      ni flatMap {
        case (e: Elem, i) => toVals(e.children, s, ParentLoc(e, i) :: p)
        case _ => Nil
      }
    }
  }
  
  /** Collects items from the children of the given group that match the selector. */
  private def collectChildrenOfGroup[A](nodes: Group[Node], s: Selector[A]): PathVals[A] = {
    collectChildrenOfGroupWith(nodes, s, Nil) (collectGroup _)
  }
  
  /** Recursively collects the specified node if matches the selector, followed by its matching descendants (depth first) */ 
  private def collectNodeRecursive[A](n: Node, l: Location, p: ParentsList,s: Selector[A]): PathVals[A] = {
    val rest = n match {
      case e:Elem => collectGroupRecursive(e.children, ParentLoc(e, l) :: p, s)
      case _ => Nil
    }
    if (s.isDefinedAt(n)) (WithLoc(s(n), l), p) +: rest
    else rest
  }
  
  /** Recursively collects items from the given group that match the selector. */
  private def collectGroupRecursive[A](group: Group[Node], parents: ParentsList, s: Selector[A]): PathVals[A] = {
    if (group.isEmpty) Nil
    else 
      dispatchSelector(s, group) {
        group.zipWithIndex flatMap {case (n,i) => collectNodeRecursive(n, i, parents, s)}
      }
  }
  
  /** 
   * Collects the specified node if matches the selector, otherwise collects its matching descendants that are 
   * not themselves descendants of matching nodes (depth first).
   */ 
  private def collectMaximalRecursive[A](n: Node, l: Location, p: ParentsList,s: Selector[A]): PathVals[A] = {
    if (s.isDefinedAt(n)) (WithLoc(s(n), l), p) :: Nil
    else n match {
      case e:Elem => collectMaximalRecursive(e.children, ParentLoc(e, l) :: p, s)
      case _ => Nil
    }
  }
  
  /** Recursively collects items from the given group that match the selector and are not descendants of matching items. */
  private def collectMaximalRecursive[A](group: Group[Node], parents: ParentsList, s: Selector[A]): PathVals[A] = {
    if (group.isEmpty) Nil
    else 
      dispatchSelector(s, group) {
        group.zipWithIndex flatMap {case (n,i) => collectMaximalRecursive(n, i, parents, s)}
      }
  }
  
  /** Gathering all the children of the group that may match the selector. */
  private def collectGroupChildren(g: Group[Node], p: ParentsList, s: Selector[_]): Seq[(Group[Node], ParentsList)] = {
    dispatchSelector[Seq[(Group[Node], ParentsList)]](s, g)(Nil) {
      val gi = g.zipWithIndex
      gi flatMap {
        case (e: Elem, i) => Some((e.children, ParentLoc(e, i) :: p))
        case _ => None
      }
    }
  }
  
  /** If dispatching on the selector yields true, executing the given code block, otherwise returning 
   * the default value.*/
  private def dispatchSelector[A](s: Selector[_], g: Group[Node])(default: A)(vals: => A): A = {
    if (dispatchSelector(s, g)) vals
    else default
  }

  /** If dispatching on the selector yields true, executing the given code block, otherwise returning
   *  an empty list.
   */
  private def dispatchSelector[A](s: Selector[A], g: Group[Node])(vals: => PathVals[A]): PathVals[A] = {
    dispatchSelector[PathVals[A]](s, g)(Nil)(vals)
  }
  
  /** Returns true if there is a chance that applying the given selector on the group
   * would yield some results. */
  private def dispatchSelector(s: Selector[_], g: Group[Node]) = {
    s match {
      case opt: OptimizingSelector[_] => opt.canMatchIn(g)
      case _ => true // no info about the selector, should proceed
    }
  }

  /** A wrapper for path function values, cannot contain duplicate locations. */
  class Path[+A](vals: PathVals[A]) {
    private val contexts =
      vals.map { wp =>
        val (withLoc, parents) = wp
        (LocationContext(withLoc.loc, parents, 0), withLoc.content)
      }
    
    /** The location contexts and the corresponding contents. */
    val (locs, contents) = contexts.unzip
    // this can only be used if [[Elem]] has efficient hashing
    require((locs).toSet.size == locs.size, "Cannot have duplicate locations in path") // enforcing no duplicates policy 
  }
}