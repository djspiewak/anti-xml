package com.codecommit.antixml

import org.scalacheck._
import scala.io.Source

trait XMLGenerators {
  import Arbitrary.arbitrary
  import Gen._
  
  val MaxGroupDepth = 3
  
  val identifiers = (Source fromURL (getClass getResource ("/identifiers.txt")) getLines).toList
  
  implicit val arbSelector: Arbitrary[Selector[Node, Zipper[Node]]] =
    Arbitrary(oneOf(nodeSelectorGenerator, elemSelectorGenerator))
  
  implicit def arbGroup[A <: Node](implicit arb: Arbitrary[A]): Arbitrary[Group[A]] =
    Arbitrary(groupGenerator[A])
  
  implicit val arbNode: Arbitrary[Node] = Arbitrary(nodeGenerator(0))
  
  implicit val arbProcInstr: Arbitrary[ProcInstr] = Arbitrary(procInstrGenerator)
  implicit val arbElem: Arbitrary[Elem] = Arbitrary(elemGenerator(0))
  implicit val arbText: Arbitrary[Text] = Arbitrary(textGenerator)
  implicit val arbEntityRef: Arbitrary[EntityRef] = Arbitrary(entityRefGenerator)
  
  lazy val elemSelectorGenerator = oneOf(identifiers) map stringToSelector
  
  lazy val nodeSelectorGenerator: Gen[Selector[Node, Zipper[Node]]] = for {
    flag <- arbitrary[Boolean]
    xform <- arbitrary[Node => Node]
  } yield Selector({
    case n if flag => xform(n)
  })
  
  def groupGenerator[A <: Node](implicit arb: Arbitrary[A]): Gen[Group[A]] =
    listOf(arb.arbitrary) map Group.fromSeq
  
  def nodeGenerator(depth: Int = 0): Gen[Node] = frequency(
    3 -> procInstrGenerator, 
    30 -> elemGenerator(depth), 
    50 -> textGenerator, 
    17 -> entityRefGenerator)
  
  lazy val procInstrGenerator: Gen[ProcInstr] = for {
    target <- genSaneString
    data <- genSaneString
  } yield ProcInstr(target, data)
  
  def elemGenerator(depth: Int = 0): Gen[Elem] = for {
    ns <- genSaneOptionString
    name <- genSaneString
    attrs <- genSaneMapStringString
    children <- if (depth > MaxGroupDepth) value(Group()) else (listOf(nodeGenerator(depth + 1)) map Group.fromSeq)
  } yield Elem(ns, name, attrs, children)
  
  lazy val textGenerator: Gen[Text] = genSaneString map Text
  
  lazy val entityRefGenerator: Gen[EntityRef] = genSaneString map EntityRef
  
  private lazy val genSaneString: Gen[String] = oneOf(identifiers)
  
  private lazy val genSaneOptionString: Gen[Option[String]] =
    frequency(5 -> (genSaneString map { Some(_) }), 1 -> None)
  
  private lazy val genSaneMapStringString: Gen[Map[String, String]] = {
    val genTuple = for {
      _1 <- genSaneString
      _2 <- genSaneString
    } yield (_1, _2)
    
    listOf(genTuple) map { Map(_: _*) }
  }
}
