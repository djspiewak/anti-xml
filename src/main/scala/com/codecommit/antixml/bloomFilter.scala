package com.codecommit.antixml

import scala.collection.immutable.BitSet

object BloomFilter {

  def apply[A](element: A)(implicit factory: BloomFilterFactory) = {
    require(element != null, "element must not be null!")
    require(factory != null, "factory must not be null!")
    factory newBloomFilter List(element)
  }

  def apply[A](elements: Seq[A] = Nil)(implicit factory: BloomFilterFactory) = {
    require(elements != null, "elements must not be null!")
    require(factory != null, "factory must not be null!")
    factory newBloomFilter elements
  }

}

trait BloomFilter[A] {

  def contains(element: A): Boolean

  def +[B >: A](element: B): BloomFilter[B]
}

object BloomFilterFactory {

  implicit val defaultBloomFilterFactory: BloomFilterFactory =
    new BloomFilterFactory {
      override def newBloomFilter[A](elements: Seq[A]) =
        new DefaultBloomFilter(elements)
    }
}

trait BloomFilterFactory {

  def newBloomFilter[A](elements: Seq[A]): BloomFilter[A]
}

private[antixml] class DefaultBloomFilter[A](elements: Seq[A]) extends BloomFilter[A] {

  def contains(element: A): Boolean = {
    require(element != null, "element must not be null!")
    hash(element) forall bits.contains
  }

  def +[B >: A](element: B): BloomFilter[B] = {
    require(element != null, "element must not be null!")
    new DefaultBloomFilter(element +: elements)
  }

  private val bits = BitSet(hash(elements): _*)

  private def hash(elements: Seq[A]): Seq[Int] = {
    elements flatMap hash
  }

  private def hash(element: A): Seq[Int] = {
    // TODO Use k different hash functions!
    List(math.abs(element.hashCode))
  }
}
