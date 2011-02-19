package com.codecommit.antixml

import scala.collection.mutable.Builder
import scala.collection.immutable.Vector

trait CanBuildFromWithZipper[-From, -Elem, To] {
  def apply(from: From, path: List[To => Group[Node]]): Builder[Elem, To]
  def apply(path: List[To => Group[Node]]): Builder[Elem, To]
  def rebuild(former: Group[Node], map: Vector[(Int, Int, Group[Node] => Node)])(children: To): Group[Node]
}
