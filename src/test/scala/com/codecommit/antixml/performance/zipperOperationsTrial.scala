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

trait ZipperOpsTrial { self: Trial =>
  
  def xmlResource: java.net.URL
  def createZipper(xml: Elem): Zipper[Node]
  def unselectCount = 1
  //If necessary, override nodePred so that about half the nodes match.
  def nodePred(n: Node): Boolean = n match {
    case e:Elem => (e.name.length & 1) == 0
    case _ => false
  }        
  
  class ZipperOpsInstance extends TrialInstance[Seq[Node]] {      
    val zipper = createZipper(from(xmlResource) {XML.fromInputStream(_)})
    val nonZipper = zipper.stripZipper
    val unselectCount = ZipperOpsTrial.this.unselectCount
    val sliceRange = ((zipper.length / 4), 3*(zipper.length / 4))
    
    override def resultDescription(s: Seq[Node]) = "result has length="+s.length+" and "+XmlCounts(s).report
    override def testDataDescription = "zipper has length="+zipper.length+" and "+XmlCounts(zipper).report

    val unselect = implemented by "zipper.unselect" in {
      (zipper /: (0 until unselectCount)) { (z,_) => z.unselect}
    }
    val zupdate = implemented by "zipper.updated" in {
      (zipper /: (0 until zipper.length)) { (z,i) => z.updated(i,Text("updated")) }
    }
    val gupdate = implemented by "group.updated" in {
      (nonZipper /: (0 until zipper.length)) { (z,i) => z.updated(i,Text("updated")) }
    }
    val zmap = implemented by "zipper.map" in {
      zipper map {_ => Text("replaced")}
    }
    val gmap = implemented by "group.map" in {
      nonZipper map {_ => Text("replaced")}
    }
    val zfmap = implemented by "zipper.flatmap" in {
      zipper flatMap {n => if (nodePred(n)) Seq(Text("rep1"),Text("rep2")) else Seq() }
    }
    val nzfmap = implemented by "group.flatmap" in {
      nonZipper flatMap {n => if (nodePred(n)) Seq(Text("rep1"),Text("rep2")) else Seq() }
    }
    val zslice = implemented by "zipper.slice" in {
      zipper.slice(sliceRange._1, sliceRange._2)
    }
    val nslice = implemented by "group.slice" in {
      nonZipper.slice(sliceRange._1, sliceRange._2)
    }
    val zfilter = implemented by "zipper.filter" in {
      zipper filter nodePred
    }
    val gfilter = implemented by "group.filter" in {
      nonZipper filter nodePred
    }
  }
  
  def create: ZipperOpsInstance = new ZipperOpsInstance

}
  
