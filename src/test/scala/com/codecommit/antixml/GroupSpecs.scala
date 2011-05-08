/*
 * Copyright (c) 2011, Daniel Spiewak
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer. 
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * - Neither the name of the <ORGANIZATION> nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
      ns \ "parent" mustEqual Group(elem("parent"))
    }
    
    "be referentially transparent" in {
      val ns = fromString("<parent><parent/></parent>")
      ns \ "parent" mustEqual Group(elem("parent"))
      ns \ "parent" mustEqual Group(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \ "a" mustEqual result
    }
    
    "be fully specified by flatMap / collect" in {
      val prop = forAll { (ns: Group[Node], selector: Selector[Node]) =>
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
      ns \\ "parent" mustEqual Group(elem("parent"))
    }
    
    "find a subset of nodes" in {
      val ns = fromString("<parent>Some<a/>text<b/>to\nreally<c/>confuse<a/><b/><d/>things<e/><a/><f/></parent>")
      val result = Group(elem("a"), elem("a"), elem("a"))
      ns \\ "a" mustEqual result
    }
    
    "find and linearize a deep subset of nodes" in {
      val ns = fromString("<parent>Some text<sub1><target>sub1</target></sub1><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><phoney><target>phoney</target></phoney>More text<target>outside</target></parent>")
      val result = fromString("<parent><target>top<sub1><target>top1</target><target>top2</target></sub1><target>top3-outer</target></target><target>outside</target><target>sub1</target><target>top3-outer</target><target>phoney</target><target>top1</target><target>top2</target></parent>")
      ns \\ "target" mustEqual result.children
    }
    
    "be fully specified by recursive flatMap / collect" in {
      val prop = forAll { (ns: Group[Node], selector: Selector[Node]) =>
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
  
  def elem(name: String, children: Node*) = Elem(None, name, Map(), Group(children: _*))
}
