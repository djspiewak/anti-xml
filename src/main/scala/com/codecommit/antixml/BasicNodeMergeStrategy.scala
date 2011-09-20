package com.codecommit.antixml
import DeepZipper._

/** A basic merging strategy which takes the most recent updates from the different nodes
 *  and creates a single node from them.
 *  
 *  The strategy considers changes that come from children and changes that come
 *  directly from the node separately.
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