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

