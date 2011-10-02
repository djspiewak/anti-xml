/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of "Anti-XML" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.antixml.util

import scala.annotation.tailrec
import scala.collection.immutable.{Map, MapLike}
import scala.collection.mutable.{Builder}
import scala.collection.generic.{ImmutableMapFactory, CanBuildFrom}

/**
 * An `OrderPreservingMap` implementation that layers an immutable doubly linked-list structure on top of a default
 * scala immutable Map.  The additional indirection provided by the map allows for efficient local operations on the list.  
 * Asymptotic performance is the same as that of the underlying map.  Sufficiently small maps have their own, optimized,
 * representations.
 * 
 */
private[antixml] sealed trait LinkedOrderPreservingMap[A,+B] extends OrderPreservingMap[A,B] with MapLike[A,B,LinkedOrderPreservingMap[A,B]] {
  override def empty: LinkedOrderPreservingMap[A,B] = LinkedOrderPreservingMap.empty

  override def + [B1 >: B] (kv: (A, B1)): LinkedOrderPreservingMap[A, B1]

}

private[antixml] object LinkedOrderPreservingMap extends ImmutableMapFactory[LinkedOrderPreservingMap] {
  
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedOrderPreservingMap[A, B]] = new MapCanBuildFrom[A, B]
  
  private val theEmpty = new Linked0[Any,Nothing]
  
  override def empty[A,B] = theEmpty.asInstanceOf[LinkedOrderPreservingMap[A, B]] //Ugly, but should be OK
  
  /** Represents an interior link or boundary of the linked list.*/
  private sealed trait LinkOrBoundary[+A,+B] {
    val value: B
    def maybePrev: Option[A]
    def maybeNext: Option[A]
  }

  /** Represents an interior node of the linked list.*/
  private case class Link[+A,+B] (override val value: B, prevKey: A, nextKey: A) extends LinkOrBoundary[A,B] {
    override def maybePrev = Some(prevKey)
    override def maybeNext = Some(nextKey)
  }
  
  /** 
   * Represents either the first or last node of the linked list.  Lists of size 1 use a specialized
   * represention, so a node will never be simultaneously first and last.
   */
  private sealed trait Boundary[+A,+B] extends LinkOrBoundary[A,B]
  
  private case class First[+A,+B] (key: A, override val value: B, nextKey: A) extends Boundary[A,B] {
    override def maybePrev = None
    override def maybeNext = Some(nextKey)
  }
  
  private case class Last[+A,+B] (key: A, override val value: B, prevKey: A) extends Boundary[A,B] {
    override def maybePrev = Some(prevKey)
    override def maybeNext = None
  }
  
  /**
   * Implementation for maps of size > 4.  Interior nodes are maintained in a `Map[A,Link[A,B]]` structure.
   * Boundary nodes are maintained in their own fields.  
   */
  private class LinkedN[A,+B] (interior:Map[A,Link[A,B]],
                              firstLink: First[A,B],
                              lastLink: Last[A,B]) extends LinkedOrderPreservingMap[A,B] {

    //interior.size < 3 implies this.size < 5 implies we should be using a small map implementation                        
    require(interior.size >= 3, (interior, firstLink, lastLink))
    

    private def mustGetLinkOrBoundary(a: A): LinkOrBoundary[A,B] = interior.get(a) match {
      case Some(e) => e
      case None => {
        if (a == firstLink.key)
          firstLink
        else if (a == lastLink.key)
          lastLink
        else
          throw new AssertionError("map should have contained "+a)
      }
    }
    
    override def iterator = {
      type Cursor = (A,LinkOrBoundary[A,B]) 
      def cursor(k: Option[A]): Option[Cursor] = k map {(key: A) => (key, mustGetLinkOrBoundary(key))}
      new Iterator[(A,B)] {
        private var curs: Option[Cursor] = Some((firstLink.key,firstLink))
        override def hasNext: Boolean = (curs != None)
        override def next: (A,B) = curs match {
          case Some((key,link)) => {
            curs = cursor(link.maybeNext)
            (key,link.value)
          }
          case None => throw new NoSuchElementException()
        }
      }
    }
  
    override def foreach [U] (f: ((A,B)) => U) = {
      type Cursor = (A,LinkOrBoundary[A,B]) 
      def cursor(k: Option[A]): Option[Cursor] = k map {(key: A) => (key, mustGetLinkOrBoundary(key))}
      @tailrec def step(curs: Option[Cursor]) {
        curs match {
          case None =>
          case Some((key,link)) => {
            f((key,link.value))
            step(cursor(link.maybeNext))
          }
        }
      }
      step(Some((firstLink.key,firstLink)))
    }
  
    override def empty = LinkedOrderPreservingMap.empty
    
    override def + [B1 >: B] (kv: (A, B1)): LinkedOrderPreservingMap[A, B1] = {
      val (k,v) = kv
      interior.get(k) match {
        case Some(Link(old,prev,next)) =>
          new LinkedN(interior.updated(k,Link(v,prev,next)), firstLink, lastLink)
        case None => {
          if (k==firstLink.key) {
            new LinkedN(interior, firstLink.copy(value=v), lastLink)
          } else if (k==lastLink.key) {
            new LinkedN(interior, firstLink, lastLink.copy(value=v))
          } else {
            val interior2 = interior.updated(lastLink.key,Link(lastLink.value,lastLink.prevKey,k))
            new LinkedN(interior2, firstLink, Last(k, v, lastLink.key))
          }
        }
      }
    }
    
    override def - (key: A) = {
      if (size == 5) {
        //This case is special because we may be switching to a small map.
        val buf = toBuffer filter {x => key != x._1}
        if (buf.size == 4) {
          val ((k0,v0),(k1,v1),(k2,v2),(k3,v3)) = (buf(0),buf(1),buf(2),buf(3))
          new Linked4(k0,v0,k1,v1,k2,v2,k3,v3)
        } else {
          this
        }
      } else interior.get(key) match  {        
        case Some(Link(_, prev, next)) => {
          val (interior2, newFirst) = interior.get(prev) match {
            case Some(link) => (interior.updated(prev, link.copy(nextKey = next)), firstLink)
            case None => (interior, firstLink.copy(nextKey = next))
          }
          val (interior3, newLast) = interior.get(next) match {
            case Some(link) => (interior2.updated(next, link.copy(prevKey = prev)), lastLink)
            case None => (interior2, lastLink.copy(prevKey = prev))
          }
          new LinkedN(interior3 - key, newFirst, newLast)
        }
        case None => {
          if (key==lastLink.key) {
            val lk = lastLink.prevKey
            val Link(lv,lkp,_) = interior(lk)
            new LinkedN(interior - lk, firstLink, Last(lk, lv, lkp))
          } else if (key==firstLink.key) {
            val fk = firstLink.nextKey
            val Link(fv,_,fkn) = interior(fk)
            new LinkedN(interior - fk, First(fk,fv,fkn), lastLink)
          } else
            this
        }
      }
    }
    
    
    override def get (key: A) = interior.get(key) match {
      case Some(Link(v,_,_)) => Some(v)
      case None => {
        if (lastLink.key==key)
          Some(lastLink.value)
        else if (firstLink.key==key)
          Some(firstLink.value)
        else
          None
      }
    }
    
    override def head:(A,B) = (firstLink.key, firstLink.value)
    
    override def last:(A,B) = (lastLink.key, lastLink.value)
    
    override def tail = this - firstLink.key
    
    override def init = this - lastLink.key
    
    //TODO: optimize take/drop family?  toSeq?
    
    override def size:Int = 2 + interior.size      

  }
  
  /**
   * Implementation for LinkedOrderPreservingMaps of size 0.
   */
  private class Linked0[A,+B] extends LinkedOrderPreservingMap[A, B] {
    override def iterator = Iterator()
    override def foreach [U] (f: ((A, B)) => U) {}
    override def empty = this
    override def + [B1 >: B] (kv: (A, B1)) = new Linked1(kv._1,kv._2)
    override def - (key: A) = this
    override def get (key: A) = None  
    override def head = Seq().head  
    override def last = Seq().last  
    override def tail = this
    override def init = this
    override def size = 0
  }
  
  /**
   * Implementation for LinkedOrderPreservingMaps of size 1.
   */
  private class Linked1[A,+B](k0: A, v0: B) extends LinkedOrderPreservingMap[A,B] {
    override def iterator = Iterator((k0,v0))
    override def foreach [U] (f: ((A,B)) => U) {
      f((k0,v0))
    }
    override def empty = LinkedOrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)) = kv match {
      case (`k0`,v) => new Linked1(k0,v)
      case (k,v) => new Linked2(k0,v0,k,v)
    }
    override def - (key: A) = key match {
      case `k0` => LinkedOrderPreservingMap.empty
      case _ => this
    }
    override def get (key: A) = key match {
      case `k0` => Some(v0)
      case _ => None
    }
    override def head = (k0,v0)  
    override def last = (k0,v0)
    override def tail = LinkedOrderPreservingMap.empty
    override def init = LinkedOrderPreservingMap.empty
    override def size:Int = 1
  }
  
  /**
   * Implementation for LinkedOrderPreservingMaps of size 2.
   */
  private class Linked2[A,+B](k0: A, v0: B, k1: A, v1: B) extends LinkedOrderPreservingMap[A,B] {
    override def iterator = Iterator((k0,v0),(k1,v1))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
    }
    override def empty = LinkedOrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)) = kv match {
      case (`k0`,value) => new Linked2(k0,value,k1,v1)
      case (`k1`,value) => new Linked2(k0,v0,k1,value)
      case (key,value) => new Linked3(k0,v0,k1,v1,key,value)
    }
    override def - (key: A) = key match {
      case `k0` => new Linked1(k1,v1)
      case `k1` => new Linked1(k0,v0)
      case _ => this
    }
    override def get (key: A) = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case _ => None
    }  
    override def head = (k0,v0)  
    override def last = (k1,v1)
    override def tail = new Linked1(k1,v1)
    override def init = new Linked1(k0,v0)
    override def size:Int = 2
  }
  
  /**
   * Implementation for LinkedOrderPreservingMaps of size 3/
   */
   private class Linked3[A,+B](k0: A, v0: B, k1: A, v1: B, k2: A, v2: B) extends LinkedOrderPreservingMap[A,B] {
    override def iterator = Iterator((k0,v0),(k1,v1),(k2,v2))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
      f((k2,v2))
    }
    override def empty = LinkedOrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)) = kv match {
      case (`k0`,value) => new Linked3(k0,value,k1,v1,k2,v2)
      case (`k1`,value) => new Linked3(k0,v0,k1,value,k2,v2)
      case (`k2`,value) => new Linked3(k0,v0,k1,v1,k2,value)
      case (key,value)  => new Linked4(k0,v0,k1,v1,k2,v2,key,value)
    }
    override def - (key: A) = key match {
      case `k0` => new Linked2(k1,v1,k2,v2)
      case `k1` => new Linked2(k0,v0,k2,v2)
      case `k2` => new Linked2(k0,v0,k1,v1)
      case _ => this
    }
    override def get (key: A) = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case `k2` => Some(v2)
      case _ => None
    }  
    override def head = (k0,v0)  
    override def last = (k2,v2)
    override def tail = new Linked2(k1,v1,k2,v2)
    override def init = new Linked2(k0,v0,k1,v1)
    override def size:Int = 3
  }

  /**
   * Implementation for LinkedOrderPreservingMaps of size 4.
   */
  private class Linked4[A,+B](k0: A, v0: B, k1: A, v1: B, k2: A, v2: B, k3: A, v3: B) extends LinkedOrderPreservingMap[A,B] {
    override def iterator = Iterator((k0,v0),(k1,v1),(k2,v2),(k3,v3))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
      f((k2,v2))
      f((k3,v3))
    }
    override def empty = LinkedOrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)) = kv match {
      case (`k0`,value) => new Linked4(k0,value,k1,v1,k2,v2,k3,v3)
      case (`k1`,value) => new Linked4(k0,v0,k1,value,k2,v2,k3,v3)
      case (`k2`,value) => new Linked4(k0,v0,k1,v1,k2,value,k3,v3)
      case (`k3`,value) => new Linked4(k0,v0,k1,v1,k2,v2,k3,value)
      case (k4,v4) => {
        val interior = Map[A,Link[A,B]](k1->Link(v1,k0,k2), k2->Link(v2,k1,k3), k3->Link(v3,k2,k4))
        new LinkedN(interior, First(k0, v0, k1), Last(k4,v4,k3))
      }
    }
    override def - (key: A) = key match {
      case `k0` => new Linked3(k1,v1,k2,v2,k3,v3)
      case `k1` => new Linked3(k0,v0,k2,v2,k3,v3)
      case `k2` => new Linked3(k0,v0,k1,v1,k3,v3)
      case `k3` => new Linked3(k0,v0,k1,v1,k2,v2)
      case _ => this
    }
    override def get (key: A) = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case `k2` => Some(v2)
      case `k3` => Some(v3)
      case _ => None
    }  
    override def head = (k0,v0)
    override def last = (k3,v3)
    override def tail = new Linked3(k1,v1,k2,v2,k3,v3)
    override def init = new Linked3(k0,v0,k1,v1,k2,v2)
    override def size:Int = 4
  }
}


