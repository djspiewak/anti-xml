package com.codecommit.antixml

import scala.collection.immutable.BitSet
import scala.util.Random

object BloomFilter {

  def apply[A](elements: Seq[A] = Nil)(implicit conf: BloomFilterConfiguration): BloomFilter[A] = {
    require(elements != null, "elements must not be null!")
    new BloomFilter(elements, conf)
  }
}

private[antixml] class BloomFilter[A](elements: Seq[A], conf: BloomFilterConfiguration) {
  import conf._
  import math._

  def contains(element: A): Boolean =
    hash(element) forall bits.contains

  def +[B >: A](element: B): BloomFilter[B] =
    new BloomFilter(element +: elements, conf)

  override lazy val toString = "BloomFilter: n=%s, m=%s, k=%s".format(elements.size, m, k)

  private val (m, k) = {
    val n = elements.size
    val m = {
      val m = round((- n * log(p) / pow(log(2), 2)).toFloat)
      if (m > 0) m else 1
    }
    val k =
      if (n == 0) {
        0
      } else {
        val k = round(log(2).toFloat * m / n)
        if (k > 0) k else 1
      }
    (m, k)
  }

  private val bits = BitSet(hash(elements): _*) // Pay attention to initialization order!

  private def hash(elements: Seq[A]): Seq[Int] = elements flatMap hash

  private def hash(element: A): Seq[Int] = {
    // TODO Is tihs approach valid and if so does it offer enough performance?
    val rnd = new Random(0)
    val hashCode = element.hashCode
    (1 until k) map { _ => abs(hashCode ^ rnd.nextInt) % m }
  }
}

object BloomFilterConfiguration {

  // Values > 0.18f lead towards k == 2
  // Values < 0.25f tend to show more false trues than expected
  // 0.33f leads to significantly faster creation than 0.25f for 100000 elements
  implicit val default: BloomFilterConfiguration =
    BloomFilterConfiguration(0.33f)
}

case class BloomFilterConfiguration(p: Float) {
  require(p > 0, "p must be positive!")
  require(p < 1, "p must be less than 1!")
}
