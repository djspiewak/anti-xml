package com.codecommit.antixml.util

import org.scalacheck._
import scala.collection.immutable.Vector

object OrderPreservingMapGenerators {
  import Arbitrary.arbitrary
  import Gen._
  
  /**
   * The largest size for which `OrderPreservingMap.apply` will produce a small-map specialization.
   */
  private val smallMapMax = 4
  
  case class Entries[A,B](entries: Vector[(A,B)]) {
    val (keys,values) = entries.unzip
    val size = entries.size
    require(keys.distinct == keys, entries)
  }

  def byteKeyEntries[A](minSize:Int,maxSize:Int)(implicit arbA: Arbitrary[A]): Gen[Entries[Byte,A]] = {
    require(minSize<=maxSize && minSize>=0)
    //Want equal frequency between small and large maps if the range intersects both kinds
    val sizeGen = if (minSize<=smallMapMax && maxSize>smallMapMax)
      frequency((1,choose(minSize,smallMapMax)),(1,choose(smallMapMax+1,maxSize)))
    else
      choose(minSize,maxSize)
    for (
      size <- sizeGen;
      entries <- entriesFor(seqOfNDistinctBytes(size),arbA.arbitrary)
    ) yield Entries(entries)
  }
  
  def entriesFor[A,B](keyGen: => Gen[Seq[A]], valGen: => Gen[B]): Gen[Vector[(A,B)]] = for {
    keys <- keyGen;
    vals <- listOfN(keys.size, valGen)
  } yield Vector(keys.zip(vals): _*)
  
  val allBytes = Range.inclusive(Byte.MinValue,Byte.MaxValue) map {_.toByte}
  
  def complement(src: Traversable[Byte]):IndexedSeq[Byte] = allBytes.filterNot(Set(src.toSeq:_*))
  
  def seqOfNDistinctBytes(n: Int):Gen[Seq[Byte]] = pick(n,allBytes)
  
}


