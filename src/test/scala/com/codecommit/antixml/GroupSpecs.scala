package com.codecommit.antixml

import org.specs._
import org.scalacheck._

object GroupSpecs extends Specification with ScalaCheck with XMLGenerators with UtilGenerators {
  import Prop._
  import XML._
  
  noDetailedDiffs()
  
  val numProcessors = Runtime.getRuntime.availableProcessors
  
  "shallow selector" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
      ns \ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = NodeSeq(elem("a"), elem("a"), elem("a"))
      ns \ "a" mustEqual result
    }
    
    "be fully specified by flatMap / collect" in {
      val prop = forAll { (ns: Group[Node], selector: Selector[Node, Zipper[Node]]) =>
        val result = ns \ selector
        val expected = ns flatMap {
          case Elem(_, _, _, children) => children collect selector
          case _ => Group()
        }
        
        result.toList mustEqual expected.toList
      }
      
      prop must pass(set(minTestsOk -> (numProcessors * 200), maxSize -> 30, workers -> numProcessors))
    }
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \ text
      strs mustEqual Vector("Some text", "More text")
    }
  }
  
  "deep selector" should {
    "find an immediate descendant" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \\ "parent" mustEqual NodeSeq(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = NodeSeq(elem("a"), elem("a"), elem("a"))
      ns \\ "a" mustEqual result
    }
    
    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val result = fromString("<parent><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><target>outside</target><target>sub1</target><target>top3-outer</target><target>phoney</target><target>top1</target><target>top2</target></parent>")
      ns \\ "target" mustEqual result.head.children
    }
    
    "be fully specified by recursive flatMap / collect" in {
      val prop = forAll { (ns: Group[Node], selector: Selector[Node, Zipper[Node]]) =>
        def loop(ns: Group[Node]): Group[Node] = {
          val recursive = ns flatMap {
            case Elem(_, _, _, children) => loop(children)
            case _ => Group()
          }
          
          val shallow = ns flatMap {
            case Elem(_, _, _, children) => children collect selector
            case _ => Group()
          }
          
          shallow ++ recursive
        }
        
        val result = ns \\ selector
        val expected = loop(ns)
        
        result.toList mustEqual expected.toList
      }
      
      prop must pass(set(minTestsOk -> (numProcessors * 200), maxSize -> 30, workers -> numProcessors))
    }
    
    "work with an alternative selector" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val strs = ns \\ text
      strs mustEqual Vector("Some text", "More text", "top", "outside", "sub1", "top3-outer", "top1", "top2", "phoney")
    }
  }
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), NodeSeq(children: _*))
}
