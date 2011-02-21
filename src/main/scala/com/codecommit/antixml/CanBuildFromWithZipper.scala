package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Vector
import scala.collection.mutable.Builder

trait CanBuildFromWithZipper[-From, -Elem, To] extends CanBuildFrom[From, Elem, To] {
  def apply(from: From): Builder[Elem, To] = apply(from, nilRebuild, Nil)
  def apply(): Builder[Elem, To] = apply(nilRebuild, Nil)
  private def nilRebuild(to: To) = Group()
  
  def apply(from: From, rebuild: To => Group[Node], path: List[Group[Node] => Group[Node]]): Builder[Elem, To]
  def apply(rebuild: To => Group[Node], path: List[Group[Node] => Group[Node]]): Builder[Elem, To]
  def rebuild(former: Group[Node], map: Vector[(Int, Int, Group[Node] => Node)])(children: To): Group[Node]
}

object CanBuildFromWithZipper {
  implicit def identityCanBuildFrom[From, Elem, To](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithZipper[From, Elem, To] = new CanBuildFromWithZipper[From, Elem, To] {
    def apply(from: From, rebuild: To => Group[Node], path: List[Group[Node] => Group[Node]]) = cbf.apply(from)
    def apply(rebuild: To => Group[Node], path: List[Group[Node] => Group[Node]]) = cbf.apply()
    def rebuild(former: Group[Node], map: Vector[(Int, Int, Group[Node] => Node)])(children: To): Group[Node] = Group()   // TODO
  }
}
