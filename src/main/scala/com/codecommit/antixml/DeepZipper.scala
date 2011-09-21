package com.codecommit.antixml

import DeepZipper._
import scala.collection.generic.CanBuildFrom
import com.codecommit.antixml.util.VectorCase
import scala.collection.IndexedSeqLike
import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder

/** A zipper which allows deep selection.
 *
 *  Zipper instances may be created through factory methods on the companion.
 */
trait DeepZipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, DeepZipper[A]] { self =>

  /*
   * All the vectors beneath should have the same length.
   */
  
  import DeepZipper.{FullContext => FullContextParam}
  type FullContext = FullContextParam[Node]

  /** Keeping track of internal time.
   *
   *  Should be initialized to 0 at the initial creation of the zipper.
   */
  protected def time: Time

  /** The last update time corresponding to each node in the zipper.
   *
   *  Should be initialized to 0 values at the initial creation of the zipper.
   */
  protected def updateTimes: Vector[Time]

  protected def parent: Option[DeepZipper[Node]]
  
  private def getParent = parent getOrElse sys.error("Root has no parent")

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
  
  /** A set of the location that should be empty (removed) in the zipper upon unselection. */
  protected def emptiesSet: EmptiesSet

  /** The full context list of the zipper. */
  private lazy val fullContext = {
    val nodesWithLocs: Vector[NodeLoc[Node]] = (self.toVector zip locations).map(Function.tupled(NodeLoc[A]))

    // constructing a vector of full context objects from the vectorized constituents
    val nodesWithContext =
      for (i <- nodesWithLocs.indices; val updateTime = updateTimes(i)) yield {
        FullContext(nodesWithLocs(i), parentLists(i), updateTime)
      }

    nodesWithContext
  }

  override def updated[B >: A <: Node](index: Int, node: B) = {
    new Group(super.updated(index, node).toVectorCase) with DeepZipper[B] {
      val parentLists = self.parentLists
      val emptiesSet = self.emptiesSet
      val locations = self.locations
      def parent = self.parent
      val mergeDuplicates = self.mergeDuplicates

      // setting new time
      val time = self.time + 1
      val updateTimes = self.updateTimes.updated(index, this.time)
    }
  }

  override protected[this] def newBuilder = DeepZipper.newBuilder[A]

  // TODO copy coded from Zipper
  override def slice(from: Int, until: Int): DeepZipper[A] = {
    val zwi = Map[A, Int](zipWithIndex: _*)
    collect {
      case e if zwi(e) >= from && zwi(e) < until => e
    }
  }
  override def drop(n: Int) = slice(n, size)
  override def take(n: Int) = slice(0, n)
  override def splitAt(n: Int) = (take(n), drop(n))
  override def filter(f: A => Boolean): DeepZipper[A] = collect {
    case e if f(e) => e
  }
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
  // end copy code
    
  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That = {
    val liftedF = (a: A) => Seq(f(a))
    flatMap(liftedF)(cbf)
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[DeepZipper[A], B, That]): That = {
    cbf match {
      // subtypes of this are the only expected types, hence ignoring type erasure
      case cbf: CanProduceDeepZipper[DeepZipper[Node], B, That] => {
        val liftedF = (x: (A, Int)) => f(x._1)
        flatMapWithIndex(liftedF)(cbf.lift)
      }
      
      case _ => super.flatMap(f)(cbf)
    }
  }
  
  /** A specialized flatMap where the mapping function receives the index of the 
   * current element as an argument. */
  private def flatMapWithIndex[B, That](f: ((A, Int)) => GenTraversableOnce[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[DeepZipper[Node], B, That]): That = {
    val result = toVector.zipWithIndex.map(f)
    val indices = result.indices
    
    val emptyContext = Vector[LocationContext]()

    /* This will hold true for locations preserved by flatMapping and false
       for the ones that were removed. */
    val initLocMap = Map[FullLoc, Boolean]() withDefaultValue false

    val initData = (initLocMap, emptyContext, time)

    val (locMap, contexts, newTime) =
      indices.foldLeft(initData) { (x, localIndex) =>
        val (locMap, contexts, time) = x
        val res = result(localIndex)

        val parent = parentLists(localIndex)

        /* Assuming here that duplicate locations come only from flatMapping, 
	       otherwise the results of unselection will be undefined. */
        val location = locations(localIndex)

        // each flatMapped segment gets its own time, this way the merging order can be properly defined
        val newTime = time + 1
        
        // converting res into context, folding because it doesn't have a "size" method
        val (newContexts, resSize) =  
          res.foldLeft((emptyContext, 0)) { (ci, n) =>
            val (contexts, i) = ci

            val context = LocationContext(location, parent, newTime)
            (contexts :+ context, i + 1)
          }

        val loc = FullLoc(parent, location)
        val resEmpty = resSize == 0
        // if at least one non empty result is present for this loc, we get a true
        val locEmpty = locMap(loc) || resEmpty
        val newLocMap = locMap.updated(loc, locEmpty)

        (newLocMap, contexts ++ newContexts, newTime)
      }

    // assigning the empties the latest time in this flatMap action
    val newEmpties = locMap.filter(_._2).keySet.map((_, newTime)) // holding on to locations flatMapped to oblivion
    val builder = cbfwdz(self.parent, contexts, emptiesSet ++ newEmpties)
    result foreach (builder ++= _.toList)
    builder.result
  }
  
  /** Transforming a node with its update time into a sequence of nodes with an overall update time. */
  private type NodeTransform = Node => (Seq[Node], Time)
  
  /** Preparing the context of the zipper for unselection.
   * 
   *  Given that all duplicate locations in the zipper were created by applications of flatMap:
   *  * We separate a single entry from each duplicates list which will remain in the full context
   *  * The leftovers are converted into node transformation functions.
   *  * The node transforms should be applied at the location to which they were mapped to, to replace the values at these locations (appending the duplicates to the location).
   *  * In addition to the above transforms, transforms for locations that were removed from the zipper are also provided (to remove the nodes from these locations). 
   *  
   *  The unselection data is composed from the non duplicate contexts and the transformation functions. */
  private def unselectionData: (Vector[FullContext], Map[FullLoc, NodeTransform]) = {
    val (contexts, transforms) = contextsWithTransforms
    val allTransforms = transforms ++ emptyTransforms
    (contexts, allTransforms)
  }

  /** The contexts objects from the zipper after removing duplicates.
   *  The removed duplicates are returned as transforms which append the duplicates to a given node mapped to the appropriate location.*/
  private def contextsWithTransforms: (Vector[FullContext], Map[FullLoc, NodeTransform]) = {
    val byLoc = fullContext.groupBy(fc => FullLoc(fc.parentsList, fc.nodeLoc.loc))

    val initContexts = Vector[FullContext]()
    val initTransforms = Map[FullLoc, NodeTransform]()

    val (contexts, transforms) =
      byLoc.foldLeft((initContexts, initTransforms)) { (ct, le) =>
        val (cont, trans) = ct
        val (loc, entry) = le

        val (h, t) = (entry.head, entry.tail) // entries cannot be empty as they were obtained by groupBy

        val newContexts = cont :+ h
        val newTransforms =
          if (t.isEmpty) trans
          else {
            val transFunc = (n: Node) => {
              val nodes = t.map(_.nodeLoc.node)
              val times = t.map(_.updateTime)
              (n +: nodes, times.max) // appending extras
            }
            trans + ((loc, transFunc))
          }

        (newContexts, newTransforms)
      }
    
    (contexts, transforms)
  }
  
  /** The node transforms that should be applied at locations that were removed from the zipper. */
  private def emptyTransforms: Set[(FullLoc, NodeTransform)] = {
    def toEmpty(time: Time) = (_: Node) => (Seq[Node](), time) // removing the node for an empty location
    val res = emptiesSet.map { lt =>
      val (loc, time) = lt
      (loc, toEmpty(time))
    }
    res
  }
  
  /** Applying the node updates. */
  lazy val unselect: DeepZipper[Node] = {
    val (fullContext, transforms) = unselectionData
    
    if (fullContext.isEmpty && transforms.isEmpty) getParent // no updates
    else {
      // grouping the nodes by their depth in the tree
      val byDepthContexts = fullContext.groupBy(_.parentsList.length) withDefaultValue Vector()
      val byDepthTransforms = transforms.groupBy(_._1.parentsList.length) withDefaultValue Map()

      val newZipper = mergeContext(byDepthContexts, byDepthTransforms)

      newZipper
    }
  }
  
  /** Converting anything that may be empty into an optional value. */
  private def toOpt[A <: {def isEmpty: Boolean}](s: A) = if (s.isEmpty) None else Some(s) 
  
  /** The zipper context grouped by depth in the tree. */
  private type DepthContext = Map[Int, Seq[FullContext]]
  /** Node transforms grouped by depth in the tree. */
  private type DepthTransforms = Map[Int, Map[FullLoc, NodeTransform]]

  /** Merging all the nodes that were updated in the zipper to provide the new
   *  values after unselection.
   *
   *  Contexts and transforms cannot be empty simultaneously.
   */
  private def mergeContext(context: DepthContext, transforms: DepthTransforms): DeepZipper[Node] = {
    val maxDepth = getMaxDepth(context, transforms)
    
    // having only a single depth, the context is fully merged
    if (maxDepth == 0) mergeRoot(context, transforms) 
    else {
      val (deepestContexts, deepestTransforms) =  (context(maxDepth), transforms(maxDepth))
      val newDepth = maxDepth - 1 // merging a single level
      val newDepthSeq = (newDepth, context(newDepth) ++ mergeDepth(deepestContexts, deepestTransforms)) // merging with entries at the new depth
      
      // removing old depth, setting the new one
      val newContext = (context - maxDepth) + newDepthSeq 
      val newTransforms = transforms - maxDepth
      mergeContext(newContext, newTransforms)
    }
  }

  /** @return the max depth implied by the given context and transforms (cannot be both empty). */
  private def getMaxDepth(context: DepthContext, transforms: DepthTransforms) = {

    assert(!(context.isEmpty && transforms.isEmpty), "Cannot merge an empty context")

    val optContext = toOpt(context)
    val optTransforms = toOpt(transforms)

    val maxFunc = (_: Map[Int, _]).maxBy(_._1)._1 // taking the maximal key from a map
    val contDepth = optContext map maxFunc
    val transDepth = optTransforms map maxFunc
    val depths = contDepth ++ transDepth // not empty as per above assertion

    depths.max
  }

  /** Taking contexts and transforms at a single depth and merging them into a list
   *  of contexts at depth -1 from the original.
   */
  private def mergeDepth(singleDepthContexts: Seq[FullContext], singleDepthTransforms: Map[FullLoc, NodeTransform]) = {
    val contexts = singleDepthContexts.groupBy(_.parentsList) withDefaultValue Seq()
    val transforms = singleDepthTransforms.groupBy(_._1.parentsList) withDefaultValue Map()
    
    val allParents = contexts.keySet ++ transforms.keySet
    
    allParents.map(p => mergeParent(p, contexts(p), transforms(p)))
  }

  /** Taking contexts and transforms under a single parent's list and merging them
   *  into a single context at the same depth as the lowest parent in the parent's list.
   */
  private def mergeParent(parents: ParentsList, contexts: Seq[FullContext], transforms: Map[FullLoc, NodeTransform]) = {

    // The parent list is never empty because the merging stops at the lowest depth.
    assert(!parents.isEmpty, "Cannot merge under an empty parent.")

    val directParentLoc = parents.head
    val directParent = directParentLoc.elem
    val grandParents = parents.tail

    val uniques = uniqueLocations(directParent, parents, contexts)
    val oldChildren = directParent.children
    
    val mergedChildren = mergeOriginalWithContext(oldChildren, uniques)
    
    
    val defaultTransform: NodeTransform = n => (Seq(n), initTime)

    // incorporating the transform into the merged children
    val transformed =
      for {
        i <- mergedChildren.indices
        loc = FullLoc(parents, i)
        transform <- transforms.get(loc) orElse Some(defaultTransform) // keeping original node
        node = mergedChildren(i)
      } yield transform(node)
    
    val (nodes, times) = transformed.unzip
    
    // the maximal update time inferred from the context object
    val maxTimeByContext = toOpt(uniques) map (_.maxBy(_.updateTime).updateTime)
    
    // the update time of the parent is the maximal time inferred from both contexts and transforms
    val newUpdateTime = (times ++ maxTimeByContext).max // both cannot be empty otherwise we wouldn't be merging 
    val newChildren = Group.fromSeq(nodes.flatten)

    val newParent = directParent.copy(children = newChildren)
    val newParentLoc = NodeLoc(newParent, directParentLoc.loc)

    FullContext(newParentLoc, grandParents, newUpdateTime)
  }

  /** Taking a sequence of contexts under the given direct parent and the given
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

  /** Merging a depth context at the root level into the original parent. */
  private def mergeRoot(rootContext: DepthContext, rootTransforms: DepthTransforms): DeepZipper[Node] = {
    // the identity function for the different mappings
    val idTransformMap = (n: Node) => Seq(n)
    val idContextMap = (n: Node) => n

    // mapping functions obtained from contexts	
    val contextMaps = contextMapFuncs(rootContext: DepthContext) withDefaultValue idContextMap
    // mapping functions obtained from transforms
    val transformMaps = transformMapFuncs(rootTransforms) withDefaultValue idTransformMap

    // this function will merge all the zipper's changes into the parent
    val flatMapFunc = (ni: (Node, Location)) => {
      val (n, i) = ni
      val contextMap = contextMaps(i)
      val transformMap = transformMaps(i)
      val fullMap = contextMap andThen transformMap

      fullMap(n)
    }

    getParent.flatMapWithIndex(flatMapFunc)
  }
  
  /** Creating the mapping functions that should be applied at the root level implied by the transform functions. */
  private def transformMapFuncs(rootTransforms: DepthTransforms) = {
    def liftTransform(tr: NodeTransform) = (n: Node) => tr(n)._1 // removing the time stamp from the transform
    val mapFuncs = // merging all transform under the same root location and lifting transforms
      rootTransforms.foldLeft(Map[Location, Node => Seq[Node]]()) { (byLoc, e) =>
        val (_, fullLocMap) = e
        val newByLoc =
          fullLocMap.foldLeft(byLoc) { (transformMap, lt) =>
            val (FullLoc(_, loc), tr) = lt
            transformMap.updated(loc, liftTransform(tr))
          }
        newByLoc
      }
    
    mapFuncs
  }
  
  /** Creating the mapping functions that should be applied at the root level implied by the context objects. */
  private def contextMapFuncs(rootContext: DepthContext) = {
    val flat = rootContext.values.flatten
    val contextByLoc = flat.groupBy(_.nodeLoc.loc)
    val uniques = mergeRootDuplicates(contextByLoc).toSeq
    
    val mergedSeqByLoc = uniques.groupBy(_.loc)
    
    // we are at the root, each location has a single context derived entry 
    val mergedByLoc = mergedSeqByLoc.mapValues(_.head) // can't fail because of groupBy
    // mapping each location to the nodes acquired from merging
    val mergedTransforms = mergedByLoc.mapValues { n =>
      (_: Node) => n.node
    }
    
    mergedTransforms
  }

  /** Applying the merging strategy to full contexts at duplicate locations at the root level. */
  private def mergeRootDuplicates(root: Map[Location, Iterable[FullContext]]) = {
    val uniques = // merging duplicates
      root.map { lc =>
        val (l, c) = lc
        val orig = getParent(l)
        val alternatives = c.map(fc => (fc.nodeLoc.node, fc.updateTime)).toSeq
        val (merged, _) = mergeDuplicates(orig, alternatives)
        NodeLoc(merged, l)
      }
    uniques
  }

  /** Merges an iterable of context objects into the original group representing them. */
  private def mergeOriginalWithContext(originals: Group[Node], contexts: Iterable[FullContext]) = {
    mergeWithOriginal(originals, contexts.map(_.nodeLoc))
  }

  /** Merges an iterable of node locations into the original group representing them. */
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

  override def toZipper = this
  
  def stripZipper = new Group(toVectorCase)
}

/** A factory for [[DeepZipper]] instances.
 *  Zippers may be created directly from groups through [[DeepZipper.groupToZipper]] or
 *  through selection using a [[PathFunction]] with [[DeepZipper.fromPath]]/[[DeepZipper.fromPathFunc]]
 *
 *  By importing the implicits in this object any [[Selectable]] can be pimped with
 *  shallow/deep selection methods, which directly take selectors as input.
 *  TODO examples
 */
object DeepZipper {
  import PathCreator._

  /** The number represents the number of the node in its parent's children list.
   *  In case the node is root, the number is its position in the group to which it belongs.
   */
  private[antixml] type Location = Int

  /** A location of a node within its parent. */
  private[antixml] case class NodeLoc[+A <: Node](node: A, loc: Location) 
  /** Parents can only be [[Elem]]s. */
  private[antixml] case class ParentLoc(elem: Elem, loc: Location)
  /** Containing any data. */
  private[antixml] case class WithLoc[+A](content: A, loc: Location)
  
  /** A location of a node under its list of parents. */
  private[antixml] case class FullLoc(parentsList: ParentsList, loc: Location)
  
  /** A set of locations nested in parents lists that are empty locations in a zipper 
   * coupled with their last update time. */
  private[antixml] type EmptiesSet = Set[(FullLoc, Time)]
  
  /** A default empties set. */
  private val defaultEmptiesSet: EmptiesSet = Set[(FullLoc, Time)]()

  /** Represents a list of a node's parents, where the first item is the direct
   *  parent of the node, and the last is the root of the tree.
   */
  private[antixml] type ParentsList = List[ParentLoc]

  /** The units in which time is measured in the zipper. Assumed non negative. */
  private[antixml] type Time = Int
  
  /** The initial time of the zipper. */
  private[antixml] val initTime: Time = 0

  /** A wrapper for the full context of a [[DeepZipper]] location. */
  private[antixml] case class FullContext[+A <: Node](
    nodeLoc: NodeLoc[A], 
    parentsList: ParentsList,
    updateTime: Time)

  /** A [[DeepZipper]] context for a location, fully describes its surrounding without specifying the content. */
  private[antixml] case class LocationContext(
    /** The location of the context beneath its parent. */
    loc: Location,
    parentsList: ParentsList,
    updateTime: Time)
    
  /** A merging function which takes a node, which represents the node before any modifications
   *  and a sequence of nodes with their corresponding update times,
   *  which are versions of the same node after some modifications.
   *
   *  The function decides how to merge the nodes to produce a single node with corresponding time stamp.
   *
   *  It should be taken into account that nodes may be modified directly by the user
   *  or through mergings from deeper levels.
   */
  private[antixml] type NodeMergeStrategy = (Node, Seq[(Node, Time)]) => (Node, Time)
  
  implicit def canBuildFromWithDeepZipper[A <: Node] = {
    new CanBuildFromWithDeepZipper[DeepZipper[_ <: Node], A, DeepZipper[A]] {
      def builder(parent: Option[DeepZipper[_ <: Node]], contexts: Vector[LocationContext], emptiesSet: EmptiesSet) = {
        Vector.newBuilder[A] mapResult { res =>
          val nodeContexts = (contexts zip res) map { cn => 
            val (context, node) = cn
            import context._
            val nodeLoc = NodeLoc(node, loc)
            FullContext(nodeLoc, parentsList, updateTime)
          }
          fromContexts(parent, nodeContexts, emptiesSet)
        }
      }
      
      def apply(parent: Option[DeepZipper[_ <: Node]], contexts: Vector[LocationContext], emptiesSet: EmptiesSet) = {
        builder(parent, contexts, emptiesSet)
      }
    }
  }
  
  implicit def canBuildFromDeep[A <: Node]: CanBuildFrom[DeepZipper[_ <: Node], A, DeepZipper[A]] = {
    new CanBuildFrom[DeepZipper[_ <: Node], A, DeepZipper[A]] with CanProduceDeepZipper[DeepZipper[_ <: Node], A, DeepZipper[A]] {
      def apply(from: DeepZipper[_ <: Node]): Builder[A, DeepZipper[A]] = apply()
      def apply(): Builder[A, DeepZipper[A]] = DeepZipper.newBuilder[A]

      def lift = canBuildFromWithDeepZipper
    }
  }
  
  /** Pimping selectables with [[DeepZipper]] methods. */
  implicit def groupableToSelectable[A <: Node](g: Selectable[A]) = {
    import PathCreator._
    new {
      //TODO using strange names to avoid conflicts

      private def zipper[B, That](path: PathFunction[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], B, That]): That = {
        fromPathFunc(g.toGroup, path)
      }

      /** Searching at the current level. */
      
      def ~\[B, That](s: Selector[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], B, That]): That = {
        zipper(fromNodes(s))
      }

      /** Searching on all levels (breadth first). */
      def ~\\[B, That](s: Selector[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], B, That]): That = {
        zipper(all(s))
      }

      /** Searching one level below. */
      def >[B, That](s: Selector[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], B, That]): That = {
        zipper(directChildren(s))
      }

      /** Searching one level below and beyond (breadth first). */
      def ~[B, That](s: Selector[B])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], B, That]): That = {
        zipper(allChildren(s))
      }

    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A] mapResult { vec =>
    new Group(vec).toZipper
  }

  /** Converts a contexts into zipper instances.
   * @param parentGroup The parent of the newly created zipper.
   * @param contexts The contents of the zipper.
   * @param empties The set of empty locations in the zipper. */
  def fromContexts[A <: Node](parentGroup: Option[Group[Node]], contexts: Vector[FullContext[A]], empties: EmptiesSet): DeepZipper[A] = {
    val vals = VectorCase.fromSeq(contexts map (_.nodeLoc.node))
    val locs = contexts map (_.nodeLoc.loc)
    val parents = contexts map (_.parentsList)
    val newUpdateTimes = contexts map (_.updateTime)

    new Group[A](vals) with DeepZipper[A] {
      val parentLists = parents
      val emptiesSet = empties
      val locations = locs
      def parent = parentGroup map { _.toZipper }
      val mergeDuplicates = parent map (_.mergeDuplicates) getOrElse BasicNodeMergeStrategy

      val time = if (newUpdateTimes.isEmpty) initTime else newUpdateTimes.max
      val updateTimes = newUpdateTimes
    }
  }
  
  /** Converts a path with parent into a zipper.
   *  @param parentGroup The parent from which the path was created.
   *  @param path Cannot contain duplicate locations.
   */
  def fromPath[A, That](parent: Group[Node], path: Path[A])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], A, That]): That = {
	 import path._
	 
	// this is valid only if the path has no duplicate locations
    val emptiesSet = defaultEmptiesSet 
    
    val builder = cbfwdz(Some(parent), Vector(locs: _*), emptiesSet)
    builder ++= contents
    
    builder.result
  }
  
  /** Converts the nodes gathered from applying the path function to the given group into a `That`. */
  def fromPathFunc[A, That](parent: Group[Node], path: PathFunction[A])(implicit cbfwdz: CanBuildFromWithDeepZipper[Group[Node], A, That]): That = {
    fromPath(parent, new Path(path(parent)))
  }

  
}