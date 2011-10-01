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
 *
 * The `OrderPreservingMap` companion object provides factory methods for creating general-purpose OrderPreservingMaps. 
 */
private[antixml] trait OrderPreservingMap[A,+B] extends Map[A,B] with MapLike[A,B,OrderPreservingMap[A,B]] { self =>
  override def empty:OrderPreservingMap[A,B] = OrderPreservingMap.empty

  override def + [B1 >: B] (kv: (A, B1)): OrderPreservingMap[A, B1]
  
  override def + [B1 >: B] (kv1: (A, B1), kv2: (A, B1), kvs: (A, B1) *): OrderPreservingMap[A, B1] =
    self + kv1 + kv2 ++ kvs

  override def updated [B1 >: B] (key: A, value: B1): OrderPreservingMap[A, B1] = self + ((key,value))

}

/**
 * Factory methods for creating general-purpose OrderPreservingMaps.
 */
private[antixml] object OrderPreservingMap extends ImmutableMapFactory[OrderPreservingMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), OrderPreservingMap[A, B]] = new MapCanBuildFrom[A, B]
    
  override def empty [A,B]: OrderPreservingMap[A,B] = LinkedOrderPreservingMap.empty
  
  override def apply [A, B] (elems: (A, B)*) = LinkedOrderPreservingMap(elems: _*)
  
  override def newBuilder[A, B] = LinkedOrderPreservingMap.newBuilder
  
}

