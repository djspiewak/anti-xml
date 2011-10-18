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

case class XmlCounts(nodes: Int, elements: Int, attributes: Int) {
  def +(rhs: XmlCounts) = XmlCounts(nodes+rhs.nodes,elements+rhs.elements,attributes+rhs.attributes)
  
  def report:String = nodes+" nodes, "+elements+" elems, "+attributes+" attrs"
}

object XmlCounts {
  def apply(a: Any): XmlCounts = a match {
    case g: Group[_] => (XmlCounts(0,0,0) /: g) { (acc,n) => acc + count(n) }
    case n: Node => count(n)
    case sn: scala.xml.Node => count(sn)
    case jn: org.w3c.dom.Node => count(jn)
    case _ => XmlCounts(-1,-1,-1)
  }
  private def count(n: Node): XmlCounts = {
    val top = n match {
      case Elem(_,_,a,_,_) => XmlCounts(1,1,a.size)
      case _ => XmlCounts(1,0,0)
    }
    (top /: n.children) { (acc,child) => acc + count(child) }
  }
  private def count(n: scala.xml.Node): XmlCounts = n match {
    case e: scala.xml.Elem => 
      (XmlCounts(1,1,e.attributes.length) /: e.child) { (acc,c) => acc + count(c) }
    case _ => XmlCounts(1,0,0)
  }
  private def count(n: org.w3c.dom.Node): XmlCounts = n match {
    case e: org.w3c.dom.Element =>
      (XmlCounts(1,1,e.getAttributes.getLength) /: JavaNodeSeqWithIndexedSeq.wrap(e.getChildNodes)) { (acc,c) => acc + count(c) }
    case d: org.w3c.dom.Document =>
      (XmlCounts(0,0,0) /: JavaNodeSeqWithIndexedSeq.wrap(d.getChildNodes)) { (acc,c) => acc + count(c) }
    case _ => XmlCounts(1,0,0)      
  }
}
