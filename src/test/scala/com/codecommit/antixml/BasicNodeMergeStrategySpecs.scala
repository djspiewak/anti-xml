package com.codecommit.antixml

import org.specs2.mutable._
import org.scalacheck._
import org.specs2.ScalaCheck

class BasicNodeMergeStrategySpecs extends SpecificationWithJUnit  with ScalaCheck with XMLGenerators {
	implicit val params = set(maxSize -> 10, minTestsOk -> 20)
	
    "A basic merge startegy" should {
    def maxTime(s: Seq[(Node, Int)]) = s.maxBy(_._2)

    "fail on empty lists" in {
      val n = Text("foo")
      BasicNodeMergeStrategy(n, List[(Node, Int)]()) must throwA[Exception]
    }

    implicit def timeNodes = Arbitrary { for { n <- nodeGenerator(1); t <- Gen.posNum[Int] } yield (n, t) }

    "select the newest node when the original is not an elem" in check { alts: List[(Node, Int)] =>
      !alts.isEmpty ==> {
        (BasicNodeMergeStrategy(_: Node, alts) mustEqual maxTime(alts)).forall {
          Seq(ProcInstr("foo", "bar"), Text("baz"), CDATA("qux"), EntityRef("somethin"))
        }
      }
    }

    "select the newest node when no elems are present in the alternatives" in check { alts: List[(Node, Int)] =>
      val noElems = alts.filterNot(_._1.isInstanceOf[Elem])
      !noElems.isEmpty ==> {
        BasicNodeMergeStrategy(elem("fo0"), noElems) mustEqual noElems.maxBy(_._2)
      }
    }

    val orig = elem("orig", Text("foo"))
    val alt1 = (orig.copy(name = "alt1"), 2)
    val alt2 = (orig.copy(name = "alt2"), 5)
    val alt3 = (orig.copy(name = "orig", children = Group(Text("bar"), Text("baz"))), 4)
    val alt4 = (orig.copy(name = "alt4", children = Group()), 3)
    val text = (Text("baz"), 3)

    "merge elems by taking the most recent children and most recent non child modification" in {
      val alts1 = Seq(alt1, alt2, text, alt3, alt4)
      BasicNodeMergeStrategy(orig, alts1) mustEqual (orig.copy(name = alt2._1.name, children = alt3._1.children), 5)
    }

    "keep the children intact if they were not modified" in {
      val alts2 = Seq(alt1, alt2, text)
      BasicNodeMergeStrategy(orig, alts2) mustEqual (orig.copy(name = alt2._1.name), 5)
    }

    "keep non children fields intact if they were not modified" in {
      val alts3 = Seq(text, alt3, alt4)
      BasicNodeMergeStrategy(orig, alts3) mustEqual (orig.copy(children = alt3._1.children), 4)
    }
  }
    
  def elem(name: String) = Elem(None, name, Attributes(), Map(), Group())
  def elem(name: String, children: Node*) = Elem(None, name, Attributes(), Map(), Group(children: _*))
}