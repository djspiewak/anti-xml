
package com.codecommit.antixml

import scala.collection.immutable.Seq
import scala.math.Ordering
import util._

/*

private[antixml] case class SimplePath(path: VectorCase[Int]) {
  
  def liesIn(z: DeepZipper[_]) = z.containsPath(this)
  
  def liesAbove(z: DeepZipper[_]) = z.isBeneathPath(this)
    
  def :+ (index: Int) = SimplePath(path :+ index)
  
  def length = path.length
  
  def startsWith(s: SimplePath) = path.startsWith(s.path)
}

private[antixml] object SimplePath extends (VectorCase[Int] => SimplePath) {
  
  //Lexicographic order
  implicit object SimplePathOrdering extends Ordering[SimplePath] {
    override def compare(x: SimplePath, y: SimplePath) =
      Ordering.Iterable[Int].compare(x.path, y.path)
  }
  
  val empty = SimplePath(VectorCase.empty)
  
}
*/
