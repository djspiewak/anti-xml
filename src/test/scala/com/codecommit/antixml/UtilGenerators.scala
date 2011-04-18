package com.codecommit.antixml

import org.scalacheck._

trait UtilGenerators {
  import Arbitrary.arbitrary
  import Gen._
  
  implicit def arbPartialFunctionp[A, B](implicit arbF: Arbitrary[A => Option[B]]): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(partialFunctionGenerator)
  
  def partialFunctionGenerator[A, B](implicit arbF: Arbitrary[A => Option[B]]) = for {
    f <- arbF.arbitrary
  } yield new PartialFunction[A, B] {
    def apply(a: A) = f(a).get
    def isDefinedAt(a: A) = f(a).isDefined
  }
}
