package com.codecommit.antixml
package zipper

import Zipper._
import CanBuildFromWithZipper.ElemsWithContextVisible
import scala.collection.generic.{CanBuildFrom, FilterMonadic}
import scala.collection.GenTraversableOnce
import scala.annotation.tailrec

/** Contains methods on [[com.codecommit.antixml.Group]] that are overridden in
 *  the zipper's implementation.
 */
private[antixml] trait ZipperGroupOverrides[+A <: Node] { self: Zipper[A] =>
  
  override protected[this] def newBuilder = Zipper.newBuilder[A]
  
  override def updated[B >: A <: Node](index: Int, node: B): Zipper[B] = context match {
    case Some(Context(parent, lastUpdate, metas, additionalHoles, hiddenNodes)) => {
      val updatedTime = lastUpdate + 1
      val (updatedPath, _) = metas(index)
      val updatedMetas = metas.updated(index, (updatedPath, updatedTime))
      val ctx = Context(parent, updatedTime, updatedMetas, additionalHoles, hiddenNodes)

      new Group(nodes.updated(index, node)) with Zipper[B] {
        val context = Some(ctx)
      }
    }
    case None => brokenZipper(nodes.updated(index, node))
  }
  
  override def slice(from: Int, until: Int): Zipper[A] = context match {
    case Some(Context(parent, lastUpdate, metas, additionalHoles, hiddenNodes)) => {
      val lo = math.min(math.max(from, 0), nodes.length)
      val hi = math.min(math.max(until, lo), nodes.length)
      val cnt = hi - lo
      
      //Put the ZipperPaths from the elided `metas` entries into additionalHoles
      val ahs = new AdditionalHolesBuilder()
      ahs ++= additionalHoles
      for(i <- 0 until lo)
        ahs += ((metas(i)._1, lastUpdate + 1 + i))
      for(i <- hi until nodes.length)
        ahs += ((metas(i)._1, lastUpdate + 1 + i - cnt))
      
      val ctx = Context(parent, lastUpdate + length - cnt, metas.slice(from, until), ahs.result(), hiddenNodes)
      
      new Group(nodes.slice(from,until)) with Zipper[A] {
        val context = Some(ctx)
      }
    }
    case None => brokenZipper(nodes.slice(from,until))
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

  override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit cbf: CanBuildFrom[Zipper[A], B, That]): That = cbf match {
    case cpz: CanProduceZipper[Zipper[A], B, That] if context.isDefined => {
      val Context(parent, lastUpdate, metas, additionalHoles, hiddenNodes) = context.get
      val b = cpz.lift(Some(parent), this)
      for(i <- 0 until nodes.length) {
        val (path,_) = metas(i)
        b += ElemsWithContextVisible(path, lastUpdate+i+1, f(nodes(i)))
      }

      for ((path, time) <- additionalHoles) {
        b += ElemsWithContextVisible[B](path, time, util.Vector0)
      }

      b ++= hiddenNodes 

      b.result()
    }
    case _ => {
      val b = cbf(this)
      for(n <- nodes)
        b ++= f(n).seq
      b.result()
    }
  }
  
  override def withFilter(f: A => Boolean) = new WithFilter(List(f))
  
  class WithFilter(filters: List[A => Boolean]) extends FilterMonadic[A, Zipper[A]] {
    private[this] def sat(a: A) = filters forall { _(a) }
    
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self flatMap {a => if (sat(a)) Seq(f(a)) else Nil } 
    
    def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Zipper[A], B, That]) =
      self flatMap {a => if (sat(a)) f(a) else Nil}
    
    def foreach[B](f: A => B) = self foreach {a => if (sat(a)) f(a)}
    
    def withFilter(p: A => Boolean) = new WithFilter(p :: filters)
  }

  override def toZipper = this
  
  /**
   * Optionally replaces each node with 0 to many nodes. Used by `unselect`.  See same-named function in `Group` for more details.
   */
  private [antixml] override def conditionalFlatMapWithIndex[B >: A <: Node] (f: (A, Int) => Option[scala.collection.Seq[B]]): Zipper[B] = {
    /* See the Group implementation for information about how this function is optimized. */ 
    context match {
      case None => brokenZipper(new Group(nodes).conditionalFlatMapWithIndex(f).nodes)
      case Some(Context(parent, lastUpdate, metas, additionalHoles, hiddenNodes)) => {
        //Optimistic function that uses `update`
        @tailrec
        def update(z: Zipper[B], index: Int): Zipper[B] = {
          if (index<z.length)
            f(nodes(index), index) match {
              case None => update(z,index + 1)
              case Some(r) if r.lengthCompare(1)==0 => update(z.updated(index, r.head), index + 1)
              case Some(r) => build(z, r, index)
            }
          else
            z
        }
        
        //Fallback function that uses a builder
        def build(z: Zipper[B], currentReplacements: Seq[B], index: Int): Zipper[B] = {
          val b = newZipperContextBuilder[B](Some(parent))
          val zc = z.context.get 
          
          for(i <- 0 until index) {
            val (p,t) = zc.metas(i)
            b += ElemsWithContextVisible(p,t,util.Vector1(z(i)))
          }
          b += ElemsWithContextVisible(metas(index)._1, zc.lastUpdate+1,currentReplacements)
          for(i <- (index + 1) until nodes.length) {
            val n = nodes(i)
            val m = metas(i)
            f(n,i) match {
              case None => b += ElemsWithContextVisible(m._1, m._2, util.Vector1(n))
              case Some(r) => b += ElemsWithContextVisible(m._1, zc.lastUpdate + 1 + i - index, r)
            }
          }
          
          for((p,t) <- additionalHoles) {
            b += ElemsWithContextVisible(p,t,util.Vector0)
          }
          
          b ++= hiddenNodes
            
          b.result
        }
        
        update(this, 0)
      }
    }    
  }
}