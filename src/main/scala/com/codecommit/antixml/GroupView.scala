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

import util.CatamorphicVector
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants._
import scala.collection.IndexedSeqLike

class GroupNodeView private[antixml](xmlReader: XMLStreamReader) extends IndexedSeq[NodeView] with IndexedSeqLike[NodeView, GroupNodeView] {
  
  override protected[this] def newBuilder = GroupView.newBuilder[NodeView]
  
  private val nodes: CatamorphicVector[XMLStreamReader, NodeView] = {
    val back = CatamorphicVector(xmlReader) { xmlReader =>
      if (xmlReader.hasNext) {
        (xmlReader.next match {
          case `START_ELEMENT` => Some(xmlReader, new ElemView(xmlReader))
          case `CHARACTERS` => Some(xmlReader, new TextView(xmlReader))
          case `END_ELEMENT` => None
          case `END_DOCUMENT` => None
        })
      } else {
        None
      }
    }
    
    // ensure subtrees are forced before continuing parsing
    back map { node: NodeView =>
      node.force()
      node
    }
  }

  override def apply(index: Int): NodeView = nodes(index)
  
  override def length = nodes.length
  
  override def toString = "GroupNodeView(" + nodes + ")"
}

object GroupView {
  def newBuilder[A <: NodeView] = null
}
