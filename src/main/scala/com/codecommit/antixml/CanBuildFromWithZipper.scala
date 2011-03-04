package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Vector
import scala.collection.mutable.Builder

trait CanBuildFromWithZipper[-From, -Elem, To] extends CanBuildFrom[From, Elem, To] {
  def apply(from: From): Builder[Elem, To] = apply(from, Vector())
  def apply(): Builder[Elem, To] = apply(Vector())
  
  def apply(from: From, map: Vector[(Int, Int, Group[Node] => Node)]): Builder[Elem, To]
  def apply(map: Vector[(Int, Int, Group[Node] => Node)]): Builder[Elem, To]
}

object CanBuildFromWithZipper {
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithZipper[From, Elem, To] = new CanBuildFromWithZipper[From, Elem, To] {
    def apply(from: From, map: Vector[(Int, Int, Group[Node] => Node)]) = cbf.apply(from)
    def apply(map: Vector[(Int, Int, Group[Node] => Node)]) = cbf.apply()
  }
}
