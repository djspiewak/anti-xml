package com.codecommit.antixml.util

import scala.collection.immutable.{Map, MapLike}
import scala.collection.generic.{ImmutableMapFactory, CanBuildFrom}

/**
 * An immutable `Map` specialization that guarantees its iteration order.
 *
 * Loosely speaking, an OrderPreservingMap's iteration order is the order in which its keys where "added" to the map.
 * Merely changing the value of a key does not alter its position in the iteration order.
 *
 * More precisely, Given an OrderPreservingMap, `map` and items `k` and `v`:
 *
 * * If `map.isDefinedAt(k)` then:
 * ** The iteration order of `map + (k,v)` is the same as that of `map` with `(k,v)` taking the place of `(k,map(k))`.
 * ** The iteration order of `map - k` is the same as that of `map` with `(k,map(k))` elided.
 * * Otherwise:
 * ** The iteration order of `map + (k,v)` is the same as that of `map` followed by `(k,v)`.
 * ** The iteration order of `map - k` is the same as that of `map`.
 * 
 * The behavior of other map operations with respect to order is derived from the above rules in the natural way.
 */
private[antixml] trait OrderPreservingMap[A,+B] extends Map[A,B] with MapLike[A,B,OrderPreservingMap[A,B]] { self =>
  override def empty:OrderPreservingMap[A,B] = LinkedOrderPreservingMap.empty

  override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A, B1]
}

private[antixml] object OrderPreservingMap extends ImmutableMapFactory[OrderPreservingMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), OrderPreservingMap[A, B]] = new MapCanBuildFrom[A, B]
  
  override def empty [A,B]: OrderPreservingMap[A,B] = Map0.asInstanceOf[OrderPreservingMap[A, B]]
  
  private object Map0 extends OrderPreservingMap[Any, Nothing] {
    override def iterator: Iterator[(Any, Nothing)] = Iterator()
    override def foreach [U] (f: ((Any, Nothing)) => U) = {}
    override def empty = this
    override def + [B1] (kv: (Any, B1)): OrderPreservingMap[Any, B1] = new Map1(kv._1,kv._2)
    override def - (key: Any): OrderPreservingMap[Any, Nothing] = this
    override def get (key: Any): None.type = None  
    override def head:(Any,Nothing) = Seq().head  
    override def last:(Any,Nothing) = Seq().last  
    override def tail: OrderPreservingMap[Any, Nothing] = this
    override def init: OrderPreservingMap[Any, Nothing] = this
    override def size:Int = 0
  }
  
  private class Map1[A,B](k0: A, v0: B) extends OrderPreservingMap[A,B] {
    override def iterator: Iterator[(A,B)] = Iterator((k0,v0))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
    }
    override def empty = OrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A,B1] = kv match {
      case (`k0`,v) => new Map1(k0,v)
      case (k,v) => new Map2(k0,v0,k,v)
    }
    override def - (key: A): OrderPreservingMap[A, B] = key match {
      case `k0` => OrderPreservingMap.empty
      case _ => this
    }
    override def get (key: A): Option[B] = key match {
      case `k0` => Some(v0)
      case _ => None
    }  
    override def head:(A,B) = (k0,v0)  
    override def last:(A,B) = (k0,v0)
    override def tail: OrderPreservingMap[A,B] = OrderPreservingMap.empty
    override def init: OrderPreservingMap[A,B] = OrderPreservingMap.empty
    override def size:Int = 1
  }
  
  private class Map2[A,B](k0: A, v0: B, k1: A, v1: B) extends OrderPreservingMap[A,B] {
    override def iterator: Iterator[(A,B)] = Iterator((k0,v0),(k1,v1))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
    }
    override def empty = OrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A,B1] = kv match {
      case (`k0`,value) => new Map2(k0,value,k1,v1)
      case (`k1`,value) => new Map2(k0,v0,k1,value)
      case (key,value) => new Map3(k0,v0,k1,v1,key,value)
    }
    override def - (key: A): OrderPreservingMap[A, B] = key match {
      case `k0` => new Map1(k1,v1)
      case `k1` => new Map1(k0,v0)
      case _ => this
    }
    override def get (key: A): Option[B] = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case _ => None
    }  
    override def head:(A,B) = (k0,v0)  
    override def last:(A,B) = (k1,v1)
    override def tail: OrderPreservingMap[A,B] = new Map1(k1,v1)
    override def init: OrderPreservingMap[A,B] = new Map1(k0,v0)
    override def size:Int = 2
  }
  
  private class Map3[A,B](k0: A, v0: B, k1: A, v1: B, k2: A, v2: B) extends OrderPreservingMap[A,B] {
    override def iterator: Iterator[(A,B)] = Iterator((k0,v0),(k1,v1),(k2,v2))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
      f((k2,v2))
    }
    override def empty = OrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A,B1] = kv match {
      case (`k0`,value) => new Map3(k0,value,k1,v1,k2,v2)
      case (`k1`,value) => new Map3(k0,v0,k1,value,k2,v2)
      case (`k2`,value) => new Map3(k0,v0,k1,v1,k2,value)
      case (key,value)  => new Map4(k0,v0,k1,v1,k2,v2,key,value)
    }
    override def - (key: A): OrderPreservingMap[A, B] = key match {
      case `k0` => new Map2(k1,v1,k2,v2)
      case `k1` => new Map2(k0,v0,k2,v2)
      case `k2` => new Map2(k0,v0,k1,v1)
      case _ => this
    }
    override def get (key: A): Option[B] = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case `k2` => Some(v2)
      case _ => None
    }  
    override def head:(A,B) = (k0,v0)  
    override def last:(A,B) = (k2,v2)
    override def tail: OrderPreservingMap[A,B] = new Map2(k1,v1,k2,v2)
    override def init: OrderPreservingMap[A,B] = new Map2(k0,v0,k1,v1)
    override def size:Int = 3
  }

  private class Map4[A,B](k0: A, v0: B, k1: A, v1: B, k2: A, v2: B, k3: A, v3: B) extends OrderPreservingMap[A,B] {
    override def iterator: Iterator[(A,B)] = Iterator((k0,v0),(k1,v1),(k2,v2),(k3,v3))
    override def foreach [U] (f: ((A,B)) => U) = {
      f((k0,v0))
      f((k1,v1))
      f((k2,v2))
      f((k3,v3))
    }
    override def empty = OrderPreservingMap.empty
    override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A,B1] = kv match {
      case (`k0`,value) => new Map4(k0,value,k1,v1,k2,v2,k3,v3)
      case (`k1`,value) => new Map4(k0,v0,k1,value,k2,v2,k3,v3)
      case (`k2`,value) => new Map4(k0,v0,k1,v1,k2,value,k3,v3)
      case (`k3`,value) => new Map4(k0,v0,k1,v1,k2,v2,k3,value)
      case _ => LinkedOrderPreservingMap((k0,v0),(k1,v1),(k2,v2),(k3,v3),kv)
    }
    override def - (key: A): OrderPreservingMap[A, B] = key match {
      case `k0` => new Map3(k1,v1,k2,v2,k3,v3)
      case `k1` => new Map3(k0,v0,k2,v2,k3,v3)
      case `k2` => new Map3(k0,v0,k1,v1,k3,v3)
      case `k3` => new Map3(k0,v0,k1,v1,k2,v2)
      case _ => this
    }
    override def get (key: A): Option[B] = key match {
      case `k0` => Some(v0)
      case `k1` => Some(v1)
      case `k2` => Some(v2)
      case `k3` => Some(v3)
      case _ => None
    }  
    override def head:(A,B) = (k0,v0)
    override def last:(A,B) = (k3,v3)
    override def tail: OrderPreservingMap[A,B] = new Map3(k1,v1,k2,v2,k3,v3)
    override def init: OrderPreservingMap[A,B] = new Map3(k0,v0,k1,v1,k2,v2)
    override def size:Int = 4
  }
}

