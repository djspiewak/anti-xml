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
 * - Neither the name of "Anti-XML" nor the names of its contributors may
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
package performance

import javax.xml.parsers.DocumentBuilderFactory

import SelectionTrial.JavaPimps._  //Adds \ and \\ operators to dom Documents

trait SelectionTrial {self: Trial =>
  def xmlResource: java.net.URL
  
  trait Inst extends self.TrialInstance[Seq[Seq[_]]] {
    override def testDataDescription = XmlCounts(antiTree).report
    override def resultDescription(results: Seq[Seq[_]]) = {
      val cnt = (0 /: results) { (sum,result) => sum + result.length }
      "Selected "+cnt+" nodes"
    }
    def antiTree: Elem
    def scalaTree: scala.xml.Elem
    def domTree: org.w3c.dom.Document
    
    val lowPriorityImpls:Set[AnyImpl]
    
    override def runLevel(impl: AnyImpl) = 
      super.runLevel(impl) + (if (lowPriorityImpls(impl)) 1 else 0) 

  }
  def create: Inst
}

object SelectionTrial {
  class JavaDocumentPimp(doc: org.w3c.dom.Document) {
    def \\(name: String): JavaNodeSeqWithIndexedSeq = 
      JavaNodeSeqWithIndexedSeq.copy(doc.getElementsByTagName(name)) //Using `copy` to force list to be realized
    def \(name: String): JavaNodeSeqWithIndexedSeq = JavaNodeSeqWithIndexedSeq.wrap(doc.getChildNodes) \ name
  }

  object JavaPimps {
    implicit def wrap(doc: org.w3c.dom.Document): JavaDocumentPimp = new JavaDocumentPimp(doc)
  }
}

trait ShallowSelectionTrial extends SelectionTrial {self: Trial =>
  val selections: List[List[String]] //always use List to ensure all trials are consistent 
  
  def applySelections[A](a: A)(fSel: (A, String) => A) = 
    selections map {ss => (a /: ss)(fSel)}  
  
  trait Inst extends super.Inst {
    val antiXml = implemented by "anti-xml" preload antiTree in { xml =>
      applySelections(xml.toGroup) { (x,s) => x \ s }
    }
    val scalaXml = implemented by "scala.xml" preload scalaTree in { xml =>
      applySelections(xml:scala.xml.NodeSeq) { (x,s) => x \ s }
    }
    val javaXml = implemented by "javax.xml" preload domTree in {xml =>
      applySelections(JavaNodeSeqWithIndexedSeq.wrap(xml.getChildNodes)) { (x,s) => x \ s }
    }
    val antiXmlNB = implemented by "anti-xml - no bloom" preload antiTree in { xml =>
      applySelections(xml.toGroup) { (x,s) => x \ noBloom(s) }
    }
    val antiXmlNZ = implemented by "anti-xml - no zipper" preload antiTree in { xml =>
      val cbfwz = withoutZipperContext[Elem]
      applySelections(xml.toGroup) { (x,s) => x.\(s)(cbfwz) }
    }
  }
  def create: Inst
}

trait DeepSelectionTrial extends SelectionTrial {self: Trial =>
  val selections: List[String] //always use List to ensure all trials are consistent 
  
  def applySelections[A,B](a: A)(fSel: (A, String) => B) = 
    selections map {ss => fSel(a,ss)}  
  
  trait Inst extends super.Inst {
    val antiXml = implemented by "anti-xml" preload antiTree in { xml =>
      applySelections(xml) { (x,s) => x \\ s }
    }
    val scalaXml = implemented by "scala.xml" preload scalaTree in { xml =>
      applySelections(xml:scala.xml.NodeSeq) { (x,s) => x \\ s }
    }
    val javaXml = implemented by "javax.xml" preload domTree in {xml =>
      applySelections(xml) { (x,s) => x \\ s }
    }
    val antiXmlNZ = implemented by "anti-xml - no zipper" preload antiTree in { xml =>
      val cbfwz = withoutZipperContext[Elem]
      applySelections(xml) { (x,s) => x.\\(s)(cbfwz) }
    }
    val antiXmlNB = implemented by "anti-xml - no bloom" preload antiTree in { xml =>
      applySelections(xml) { (x,s) => x \\ noBloom(s) }
    }
  }
  def create:Inst
}


trait ColdShallowSelectionTrial extends ShallowSelectionTrial {self: Trial =>
  class Inst extends super.Inst {
    override def antiTree = from(xmlResource) {XML.fromInputStream(_)}
    override def scalaTree = from(xmlResource) {scala.xml.XML.load(_)}
    override def domTree = from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    override val lowPriorityImpls:Set[AnyImpl] = Set(antiXmlNZ)
  }
  override def create:Inst = new Inst
}

trait WarmShallowSelectionTrial extends ShallowSelectionTrial { self: Trial =>
  class Inst extends super.Inst {
    override val antiTree = from(xmlResource) {XML.fromInputStream(_)}
    override val scalaTree = from(xmlResource) {scala.xml.XML.load(_)}
    override val domTree = from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    override val lowPriorityImpls:Set[AnyImpl] = Set(antiXmlNB,antiXmlNZ)
  }
  override def create:Inst = new Inst
}

trait ColdDeepSelectionTrial extends DeepSelectionTrial {self: Trial =>
  class Inst extends super.Inst {
    override def antiTree = from(xmlResource) {XML.fromInputStream(_)}
    override def scalaTree = from(xmlResource) {scala.xml.XML.load(_)}
    override def domTree = from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    override val lowPriorityImpls:Set[AnyImpl] = Set(antiXmlNZ)
  }
  override def create:Inst = new Inst
}

trait WarmDeepSelectionTrial extends DeepSelectionTrial { self: Trial =>
  class Inst extends super.Inst {
    override val antiTree = from(xmlResource) {XML.fromInputStream(_)}
    override val scalaTree = from(xmlResource) {scala.xml.XML.load(_)}
    override val domTree = from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    override val lowPriorityImpls:Set[AnyImpl] = Set(antiXmlNB,antiXmlNZ)
  }
  override def create:Inst = new Inst
}

