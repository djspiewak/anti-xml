package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom
import com.codecommit.antixml.util.VectorCase
import scala.collection.immutable.{SortedMap, IndexedSeq, Seq}
import scala.collection.IndexedSeqLike
import scala.collection.GenTraversableOnce
import scala.collection.mutable.Builder

import Zipper._
import CanBuildFromWithZipper.ElemsWithContext

/** A zipper which allows deep selection.
 *
 * ==Unselection Algorithm==
 * 
 * Let G be a group, and Z be a zipper with G as its parent.  For each node, N, in G (top-level or otherwise), we make
 * the following definitions:
 * 
 *  - N ''lies in'' Z if N was matched by the original selection operation that led to the creation the zipper.
 *  - N ''lies above'' Z if a descendant of N lies in Z
 *  - For any N that lies in Z, the ''direct updates'' for N are the sequence of nodes that replaced N via update operations on the zipper.
 *  - For any N that lies above Z, the ''indirect update'' for N is just N with its children flatMapped with the pullback function (defined below).
 *  - The ''pullback'' of N consists of a sequence of nodes as defined below:
 *    - If N does not lie in Z, then the pullback of N is the singleton sequence consisting of the indirect update for N
 *    - If N lies in Z, but does not lie above Z, then the pullback of N is its direct updates. 
 *    - If N lies in Z ''and'' lies above Z, then the pullback of N is the result of merging its direct updates and its indirect update,
 * according to the [[com.codecommit.antixml.ZipperMergeStrategy]] passed as an implicit parameter to `unselect`.
 *
 * (Strictly speaking, these definitions should be made on the location of N in G rather than on N itself, but we abuse the
 * terminology for the sake of brevity)
 *
 * With the above definitions, the result of `unselect` can be defined simply as G flatMapped with the pullBack function.
 * 
 * The last line in the definition of "pullback" deserves special mention because it defines the only condition under which
 * the [[com.codecommit.antixml.ZipperMergeStrategy]] is invoked. Given zipper Z and parent G, if there exists an N in G such that N lies both in and above Z, 
 * then Z is said to be ''conflicted at N''.  A zipper without such an N then Z is said to be ''conflict free'' and will never invoke
 * a ZipperMergeStrategy during unselection. 
 *
 */
trait Zipper[+A <: Node] extends Group[A] with IndexedSeqLike[A, Zipper[A]] { self =>
  
  /** 
   * A value that is greater than the update time of any node or path in the zipper. Subsequent updates must
   * be tagged with a larger time.
   */
  protected def time: Time

  /** 
   * Returns the original group that was selected upon when the Zipper was created.  A value of `None` indicates that
   * the Zipper has no parent and `unselect` cannot be called.  This possibility is an unfortunate consequence of the
   * fact that some operations must return the static type of Zipper even though they yield no usable context.  
   * In practice, this situation is usually caused by one of the following operations:
   * 
   *   - A non-Zipper group is selected upon and then 'unselect' is used to generate an updated group. 
   *   - A method such as `++`, is used to "add" nodes to a zipper without replacing existing nodes. 
   *   
   **/
  def parent: Option[Zipper[Node]]
  
  private def parentOrError = parent getOrElse sys.error("Root has no parent")

  /** Context information corresponding to each node in the zipper. */
  protected def metas: IndexedSeq[(Path, Time)]

  /** 
   * Information corresponding to each path in the zipper. The map's values consist of the indices of the corresponding nodes, 
   * along with a master update time for the path.  An empty sequence indicates the path has been elided and should be 
   * removed upon `unselect`.  In this case, the update time indicates the time of elision.
   * 
   * The map is sorted lexicographically by the path primarily to facilitate the `isBeneathPath` method.
   */
  protected def pathIndex: SortedMap[Path,(IndexedSeq[Int], Time)]

  override protected[this] def newBuilder = Zipper.newBuilder[A]
  
  override def updated[B >: A <: Node](index: Int, node: B): Zipper[B] = {
    val updatedTime = time + 1
    val (updatedPath,_) = metas(index)
    val (updatePathIndices,_) = pathIndex(updatedPath)
    
    new Group(super.updated(index, node).toVectorCase) with Zipper[B] {
      def parent = self.parent
      val time = updatedTime      
      val metas = self.metas.updated(index, (updatedPath, updatedTime))
      val pathIndex = self.pathIndex.updated(updatedPath,(updatePathIndices, updatedTime))
    }
  }

  override def slice(from: Int, until: Int): Zipper[A] = flatMapWithIndex {
    case (e, i) if i >= from && i < until => VectorCase(e)
    case (e, _) => VectorCase()
  }
   
  override def drop(n: Int) = slice(n, size)
  
  override def take(n: Int) = slice(0, n)
  
  override def splitAt(n: Int) = (take(n), drop(n))
  
  override def filter(f: A => Boolean): Zipper[A] = collect {
    case e if f(e) => e
  }
  
  override def collect[B, That](pf: PartialFunction[A, B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That =
    flatMap(pf.lift andThen { _.toTraversable })
    
  override def map[B, That](f: A => B)(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = {
    val liftedF = (a: A) => Seq(f(a))
    flatMap(liftedF)(cbf)
  }

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = {
    cbf match {
      // subtypes of this are the only expected types, hence ignoring type erasure
      case cbf: CanProduceZipper[Zipper[A], B, That] => {
        val liftedF = (x: (A, Int)) => f(x._1)
        flatMapWithIndex(liftedF)(cbf.lift)
      }
      
      case _ => super.flatMap(f)(cbf)
    }
  }

  override def toZipper = this
  
  /**
   * Returns a `Group` containing the same nodes as this Zipper, but without any Zipper context, and in particular,
   * without any implict references to the zipper's parent.
   */
  def stripZipper = new Group(toVectorCase)
  
  /** A specialized flatMap where the mapping function receives the index of the 
   * current element as an argument. */
  private def flatMapWithIndex[B, That](f: ((A, Int)) => GenTraversableOnce[B])(implicit cbfwdz: CanBuildFromWithZipper[Zipper[A], B, That]): That = {
    val result = toVector.zipWithIndex map {x => (f(x),x._2)}
    
    val builder = cbfwdz(parent, this)
    for ( (items, index) <- result) {
      val (path, _) = metas(index)
      builder += ElemsWithContext[B](path, time+index+1, items)
    }
    //Add any paths that had been previously emptied out.  (TODO - Can optimize this)
    for ( (path,(inds,time)) <- pathIndex) {
      if (inds.isEmpty)
        builder += ElemsWithContext[B](path,time,VectorCase.empty)
    }
    builder.result
  }

  /** Returns true iff the specified path is one of the contexts maintained by the zipper. */
  private[antixml] def containsPath(p: Path) = pathIndex.contains(p)
  
  /** Returns true iff the specified path is the ancestor of one of the contexts maintained by the zipper. */
  private[antixml] def isBeneathPath(p: Path) = {
    //This would be easier if OrderedMap had a variant of the `from` method that was exclusive.
    val ifp = pathIndex.keySet.from(p)
    val result = for {
      h <- ifp.headOption
      h2 <- if (h != p) Some(h) else ifp.take(2).tail.headOption
    } yield h2.startsWith(p) && h2.length != p.length
    result.getOrElse(false)    
  }

  /** 
   * Returns the direct updates for the specified path.  The result is unspecified  if the path is not contained
   * in the zipper.  
   * @return the time of last update to the path followed by a sequence of the direct update nodes and their update times.  
   **/
  private def directUpdatesFor(p: Path): (IndexedSeq[(A,Time)], Time) = {
    val (indices, time) = pathIndex(p)
    (indices map {x => (self(x), metas(x)._2)}, time)
  }
  
  /** Applies the node updates to the parent and returns the result. */
  def unselect(implicit mergeStrategy: ZipperMergeStrategy): Zipper[Node] = {
    //TODO - Should we pull back update times as well as nodes?
    parentOrError flatMapWithIndex {
      case (node,index) => pullBack(node, VectorCase(index), mergeStrategy)._1
    }
  }
    
  /**
   * Returns the pullback of a path. 
   * @param node the node that is at the specified path in the zipper's parent
   * @param path the path
   * @return the pullback nodes along with the path's latest update time.
   */
  private def pullBack(node: Node, path: Path, mergeStrategy: ZipperMergeStrategy): (IndexedSeq[Node], Time) = node match {
    case elem: Elem if isBeneathPath(path) => {
      val childPullBacks @ (childGroup, childTime) = pullBackChildren(elem.children, path, mergeStrategy)
      val indirectUpdate = elem.copy(children = childGroup)
      if (containsPath(path)) {
        mergeConflicts(elem, directUpdatesFor(path), (indirectUpdate, childTime), mergeStrategy)
      } else {
        (VectorCase(indirectUpdate), childTime)
      }
    }
    case _ if containsPath(path) => {
      val (items, time) = directUpdatesFor(path)
      (items.map(_._1), time)
    }
    case _ => (VectorCase(node), 0)
  }

  /**
   * Returns the pullback of the children of a path in the zipper's parent tree. 
   * @param node the node that is at the specified path in the zipper's parent
   * @param path the path
   * @return the pullBacks of the path's children, concatenated together, along with the latest update
   * time of the child paths.
   */
  private def pullBackChildren(nodes: IndexedSeq[Node], path: Path, mergeStrategy: ZipperMergeStrategy): (Group[Node], Time) = {
    val childPullbacks = nodes.zipWithIndex.map {
      case (node, index) => pullBack(node, path :+ index, mergeStrategy)
    }
    (childPullbacks.flatMap[Node,Group[Node]](_._1), childPullbacks.maxBy(_._2)._2)
  }
  
  /**
   * Merges updates at a conflicted node in the tree.  See the unselection algorithm, above, for more information. 
   * @param node the conflicted node
   * @param directUpdates the direct updates to `node`.
   * @param indirectUpdate the indirectUpdate to `node`.
   * @param mergeStrategy the merge strategy
   * @return the sequence of nodes to replace `node`, along with an overall update time for `node`.
   */
  private def mergeConflicts(node: Elem, directUpdates: (IndexedSeq[(Node,Time)], Time) , indirectUpdate: (Node, Time), mergeStrategy: ZipperMergeStrategy): (IndexedSeq[Node], Time) = {
    val mergeContext = ZipperMergeContext(original=node, lastDirectUpdate = directUpdates._2, directUpdate = directUpdates._1,
        indirectUpdate = indirectUpdate)
     
    val result = mergeStrategy(mergeContext)
    (VectorCase.fromSeq(result), math.max(directUpdates._2, indirectUpdate._2))
  }
}

object Zipper {
    
  import CanBuildFromWithZipper.ElemsWithContext
  
  /** The units in which time is measured in the zipper. Assumed non negative. */
  private type Time = Int
  
  /** A top-down path used to represent a location in the Group tree.*/
  private type Path = VectorCase[Int]
  
  private implicit object PathOrdering extends Ordering[Path] {
    override def compare(x: Path, y: Path) =
      Ordering.Iterable[Int].compare(x,y)
  }
  
  implicit def canBuildFromWithZipper[A <: Node] = {
    new CanBuildFromWithZipper[Traversable[_], A, Zipper[A]] {      
      override def apply(parent: Option[Zipper[Node]]): Builder[ElemsWithContext[A],Zipper[A]] = new WithZipperBuilder[A](parent)
    }
  }
  
  implicit def canBuildFromDeep[A <: Node]: CanBuildFrom[Group[_], A, Zipper[A]] = {
    new CanBuildFrom[Group[_], A, Zipper[A]] with CanProduceZipper[Group[_], A, Zipper[A]] {
      def apply(from: Group[_]): Builder[A, Zipper[A]] = apply()
      def apply(): Builder[A, Zipper[A]] = Zipper.newBuilder[A]

      def lift = canBuildFromWithZipper
    }
  }
  
  def newBuilder[A <: Node] = VectorCase.newBuilder[A].mapResult({new Group(_).toZipper})
  
  /** Returns a "broken" zipper which contains the specified nodes but cannot be unselected */
  private[antixml] def brokenZipper[A <: Node](nodes: VectorCase[A]): Zipper[A] = {
    val fakePath = VectorCase(0)
    new Group[A](nodes) with Zipper[A] {
      override def parent = None      
      override val time = 0
      override val metas = constant((fakePath,0), nodes.length)
      override val pathIndex = SortedMap( fakePath -> (0 until nodes.length, 0))
    }
  }
  
  private def constant[A](a: A, sz: Int) = new IndexedSeq[A] {
    override def apply(i: Int) = a
    override def length = sz
  }
  
  /**
   * The primary builder class used to construct Zippers. 
   */
  private class WithZipperBuilder[A <: Node](parent: Option[Zipper[Node]]) extends Builder[ElemsWithContext[A],Zipper[A]] { self =>
    private val innerBuilder = VectorCase.newBuilder[(Path, Time, A)]
    private var pathIndex = SortedMap.empty[Path,(IndexedSeq[Int], Time)]
    private var size = 0
    private var maxTime = 0
    
    override def += (ewc: ElemsWithContext[A]) = {      
      val ElemsWithContext(pseq, time, ns) = ewc
      val path = VectorCase.fromSeq(pseq)

      val items = ns.seq.toSeq.map(x => (path, time, x))(VectorCase.canBuildFrom)
      innerBuilder ++= items
      
      val (oldIndices, oldTime) = pathIndex.getOrElse(path, (VectorCase.empty,0))
      val (newIndices, newTime) = (math.max(oldTime, time), oldIndices ++ (size until (size + items.length)))
      pathIndex = pathIndex.updated(path, (newTime,newIndices))
      
      size += items.length
      maxTime = math.max(maxTime, time)
      this            
    }
    override def clear() {
      innerBuilder.clear()
      pathIndex = SortedMap.empty
      size = 0
      maxTime = 0
    }
    override def result(): Zipper[A] = {
      val res = innerBuilder.result()
      new Group[A](res map {case (_,_,node) => node}) with Zipper[A] {
        override def parent = self.parent      
        override val time = maxTime
        override val metas = res map {case (path,time,_) => (path,time)}
        override val pathIndex = self.pathIndex
      }
    }
  }
  
}