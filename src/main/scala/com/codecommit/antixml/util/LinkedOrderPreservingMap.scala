package com.codecommit.antixml.util

import scala.annotation.tailrec
import scala.collection.immutable.{Map, MapLike}
import scala.collection.generic.{ImmutableMapFactory, CanBuildFrom}

private[antixml] object LinkedOrderPreservingMap extends ImmutableMapFactory[LinkedOrderPreservingMap] {

  private[util] sealed abstract class Link[A,+B] {
    def prev: Option[A]
    def next: Option[A]
    val value: B
  }
  
  private object Link {
    def apply[A,B](value: B, prev: Option[A], next: Option[A]): Link[A,B] = (prev,next) match {
      case (Some(p),Some(n)) => Middle(value,p,n)
      case (Some(p),None) => Last(value,p)
      case (None,Some(n)) => First(value,n)
      case (None,None) => Singleton(value)
    }
    
    def unapply[A,B](link: Link[A,B]): Option[(B,Option[A],Option[A])] = 
      Some((link.value,link.prev,link.next))
  }
  
  /*
  * The following 4 cases correspond to the possible combinations of prev==None and next==None.  Splitting
  * these out allows us to avoid the overhead of storing Option instances directly on the Link.  On Hotspot,
  * this cuts our per-entry overhead nearly in half.
  */
  
  private case class First[A,+B](override val value: B, nextKey: A) extends Link[A,B] {
    override def prev: None.type = None
    override def next: Some[A] = Some(nextKey)
  }
  
  private case class Middle[A,+B](override val value: B, prevKey: A, nextKey: A) extends Link[A,B] {
    override def prev: Some[A] = Some(prevKey)
    override def next: Some[A] = Some(nextKey)
  }

  private case class Last[A,+B](override val value: B, prevKey: A) extends Link[A,B] {
    override def prev: Some[A] = Some(prevKey)
    override def next: None.type = None
  }

  private case class Singleton[A,+B](override val value: B) extends Link[A,B] {
    override def prev: None.type = None
    override def next: None.type = None
  }
  
  
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedOrderPreservingMap[A, B]] = new MapCanBuildFrom[A, B]
  
  override def empty [A,B]: LinkedOrderPreservingMap[A,B] = new LinkedOrderPreservingMap(Map.empty,None,None)
  
  private def singleton[A,B] (k:A, v:B) : LinkedOrderPreservingMap[A,B] = {
    val sk = Some(k)
    new LinkedOrderPreservingMap[A,B](Map(k -> Singleton(v)),sk,sk)
  } 
  
  
}

/**
 * An simple `OrderPreservingMap` implementation that layers an immutable doubly linked-list structure on top of a default
 * scala immutable Map.  The additional indirection provided by the map allows for efficient local operations on the list.  
 * Asymptotic performance is the same as that of the underlying map.
 * 
 * Note that most code should use the [[com.codecommit.antixml.util.OrderPreservingMap]] companion object to produce 
 * OrderPreservingMaps rather than directly use LinkedOrderPreservingMap.  For large maps, the `OrderPreservingMap` companion
 * produces LinkedOrderPreservingMaps, but for small maps it produces more efficient specializations.
 */
private[antixml] class LinkedOrderPreservingMap[A,+B] private (delegate:Map[A,LinkedOrderPreservingMap.Link[A,B]], firstKey:Option[A], lastKey:Option[A]) extends OrderPreservingMap[A,B] with MapLike[A,B,LinkedOrderPreservingMap[A,B]] {
  
  import LinkedOrderPreservingMap.{Link, singleton}
  
  require({
    if (delegate.isEmpty)
      firstKey == None && lastKey == None
    else
      firstKey != None && lastKey != None
  }, (delegate,firstKey,lastKey))
      
  private def mustGetLink(a: A): Link[A,B] = delegate.get(a) match {
    case Some(e) => e
    case None => throw new AssertionError("map should have contained "+a)
  }
  private def mustGetEntry(a: A): (A,B) = delegate.get(a) match {
    case Some(Link(b,_,_)) => (a,b)
    case None => throw new AssertionError("map should have contained "+a)
  }
  
  private def firstInOrder: Option[(A,B)] = firstKey match {
    case Some(a) => Some(mustGetEntry(a))
    case None => None
  }
  
  private def nextInOrder(key: A): Option[(A,B)] = delegate.get(key) match {
    case Some(Link(_,_,Some(next))) => Some(mustGetEntry(next))
    case Some(Link(_,_,None)) => None
    case None => None
  }
  
  override def iterator: Iterator[(A,B)] = {
    type Cursor = (A,Link[A,B]) 
    def cursor(k: Option[A]): Option[Cursor] = k map {(key: A) => (key, mustGetLink(key))}
    new Iterator[(A,B)] {
      private var curs = cursor(firstKey)
      override def hasNext: Boolean = (curs != None)
      override def next: (A,B) = curs match {
        case Some((key,Link(value,_,nextKey))) => {
          curs = cursor(nextKey)
          (key,value)
        }
        case None => throw new NoSuchElementException()
      }
    }
  }

  override def foreach [U] (f: ((A,B)) => U) = {
    type Cursor = (A,Link[A,B]) 
    def cursor(k: Option[A]): Option[Cursor] = k map {(key: A) => (key, mustGetLink(key))}
    @tailrec def step(curs: Option[Cursor]):Unit = curs match {
      case None =>
      case Some((key,Link(value,_,nextKey))) => {
        f((key,value))
        step(cursor(nextKey))
      }
    }
    step(cursor(firstKey))
  }

  override def empty:LinkedOrderPreservingMap[A,B] = LinkedOrderPreservingMap.empty
  
  override def + [B1 >: B] (kv: (A, B1)): LinkedOrderPreservingMap[A, B1] = {
    val (k,v) = kv
    delegate.get(k) match {
      case Some(Link(old,prev,next)) =>
        new LinkedOrderPreservingMap(delegate.updated(k,Link(v,prev,next)), firstKey, lastKey)
      case None => {
        lastKey match {
          case None => singleton(k,v)
          case Some(tlk) => {
            val Link(tlv,tlp,_) = mustGetLink(tlk)
            val link1 = Link(tlv,tlp,Some(k))
            val link2 = Link(v,Some(tlk),None)
            new LinkedOrderPreservingMap(delegate.updated(tlk,link1).updated(k,link2),firstKey,Some(k))
          } 
        }
      }
    }
  }
  
  override def - (key: A): LinkedOrderPreservingMap[A, B] = delegate.get(key) match {
    case None => this
    case Some(Link(_, None, None)) => empty
    case Some(Link(_, prev, next)) => {
      val (delg1, nhead) = prev match {
        case None => (delegate, next)
        case Some(pk) => {
          val Link(pv, pp, _) = mustGetLink(pk)
          (delegate.updated(pk,Link(pv,pp,next)), firstKey)
        }
      }
      val (delg2, ntail) = next match {
        case None => (delg1, prev)
        case Some(nk) => {
          val Link(nv, _, nn) = mustGetLink(nk)
          (delg1.updated(nk,Link(nv,prev,nn)), lastKey)
        }
      }
      new LinkedOrderPreservingMap(delg2 - key,nhead,ntail)
    }
     
  }
  
  override def get (key: A): Option[B] = delegate.get(key) match {
    case Some(Link(v,_,_)) => Some(v)
    case None => None
  }
  
  override def head:(A,B) = firstKey match {
    case Some(key) => mustGetEntry(key)
    case None => throw new NoSuchElementException()
  } 
  
  override def last:(A,B) = lastKey match {
    case Some(key) => mustGetEntry(key)
    case None => throw new NoSuchElementException()
  }
  
  override def tail: LinkedOrderPreservingMap[A,B] = firstKey match {
    case Some(key) => this - key
    case None => this
  }
  
  override def init: LinkedOrderPreservingMap[A,B] = lastKey match {
    case Some(key) => this - key
    case None => this
  }
  
  //TODO: optimize take/drop family?
  
  override def size:Int = delegate.size
}

