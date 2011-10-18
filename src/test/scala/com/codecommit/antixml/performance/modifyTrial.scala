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

trait ModifyTrial {self: Trial =>
  def xmlResource: java.net.URL
  
  trait ModifyInstance extends TrialInstance[Any] {
    val antiTree = from(xmlResource) {XML.fromInputStream(_)}
    val scalaTree = from(xmlResource) {scala.xml.XML.load(_)}
    val domTree = from(xmlResource) {DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(_)}
    override def resultDescription(a: Any) = "result has "+XmlCounts(a).report
  }
}

trait DeepModifyTrial extends ModifyTrial {self: Trial =>
  val selection: String
  val attributeToSet: (String,String)
  
  class ModifyInstance extends super.ModifyInstance {
    override def testDataDescription = "modify "+(antiTree \\ selection).length+" nodes"
    
    val antiXml = implemented by "anti-xml" in {
      val sels = (antiTree \\ selection) 
      val mods = sels map {e => e.copy(attrs = e.attrs + attributeToSet)}
      mods.unselect
    }
    
    /** Naive scala.xml implementation */ 
    val scalaXml = implemented by "scala.xml" in {
      import scala.xml.{Node, Elem, Attribute}
      def deepMap(node: Node)(f: Node => Node):Node = f(node) match {
        case e: Elem => e.copy(child=e.child.map(n => deepMap(n)(f)))
        case n => n
      }
      deepMap(scalaTree) {
        case e@Elem(_,`selection`,attrs,_,_) => e.asInstanceOf[Elem] % (Attribute("",attributeToSet._1,attributeToSet._2,attrs))
        case n => n
      }
    }
    
    /** Optimized scala.xml implementation.  Does not call map on seqs that do not contain children. */ 
    val scalaXml2 = implemented by "scala-xml, opt" in {
      import scala.xml.{Node, Elem, Attribute}
      def modify(g: Seq[Node], sel: String)(f: Elem => Elem):Seq[Node] = g map {
        case e: Elem => {
          val e2 = if (e.label == sel) f(e) else e
          if (e2.child.forall({! _.isInstanceOf[Elem]})) e2 else
            e2.copy(child=modify(e2.child,sel)(f))
        }
        case n => n
      }
      scalaTree.copy(child = modify(scalaTree.child, selection) { e =>
        e % (Attribute("",attributeToSet._1,attributeToSet._2,e.attributes))
      })
    }

    /** javax.xml implementation, which makes a clone of the dom and then mutates the result.
     *  The `getElementsByTagName` method is used to find the elements to update. 
     */ 
    val javaXml = implemented by "javax.xml" in {
      val domCopy = domTree.cloneNode(true).asInstanceOf[org.w3c.dom.Document]
      val sels = domCopy.getElementsByTagName(selection)
      for(i <- 0 until sels.getLength) {
        val e = sels.item(i).asInstanceOf[org.w3c.dom.Element]
        e.setAttribute(attributeToSet._1,attributeToSet._2)
      }
      domCopy
    }

    /** Analog to the naive scala.xml implementation */
    val antiXml2 = implemented by "anti-xml no-zip" in {
      def deepMap(node: Node)(f: (Node) => Node):Node = f(node) match {
        case e: Elem => e.copy(children=e.children.map(n => deepMap(n)(f)))
        case n => n
      }
      deepMap(antiTree) {
        case e@Elem(_,`selection`,attrs,_,_) => e.asInstanceOf[Elem].copy(attrs = e.attrs +attributeToSet)
        case n => n
      }
    }

    /** Analog to the optimized scala.xml implementation */
    val antiXml3 = implemented by "anti-xml no-zip, opt" in {
      def modify(g: Group[Node], sel: String)(f: Elem => Elem):Group[Node] = g map {
        case e: Elem => {
          val e2 = if (e.name == sel) f(e) else e
          if (e2.children.forall({! _.isInstanceOf[Elem]})) e2 else
            e2.copy(children=modify(e2.children,sel)(f))
        }
        case n => n
      }
      antiTree.copy(children = modify(antiTree.children, selection) { e =>
        e.copy(attrs = e.attrs +attributeToSet)
      })
    }

    /** Alternate analog to optimized scala.xml, which uses selective update rather than map */
    val antiXml4 = implemented by "anti-xml no-zip, update" in {
      def modify(g: Group[Node], sel: String)(f: Elem => Elem):Group[Node] = {
        (g /: (0 until g.length)) { (acc,i) => 
          acc(i) match {
            case e: Elem => {
              val e2 = if (e.name == sel) f(e) else e
              val e3c = modify(e2.children,sel)(f)
              val e3 = if (e3c eq e2.children) e2 else e2.copy(children=e3c)
              if (e3 eq e) acc else acc.updated(i,e3)
            }
            case n => acc
          }
        }
      }
      antiTree.copy(children = modify(antiTree.children, selection) { e =>
        e.copy(attrs = e.attrs +attributeToSet)
      })
    }
    
    /* The alternate impls get suppressed at low run levels: */
    
    val lowPriorityImpls:Set[AnyImpl] = Set(scalaXml2,antiXml2,antiXml3,antiXml4)
    
    override def runLevel(impl: AnyImpl) = 
      super.runLevel(impl) + (if (lowPriorityImpls(impl)) 1 else 0) 

  }
  override def create:ModifyInstance = new ModifyInstance
}

trait ShallowModifyTrial extends ModifyTrial {self: Trial =>
  val selection: List[String]
  val attributeToSet: (String,String)
  
  class ModifyInstance extends super.ModifyInstance {
    override def testDataDescription = "modify "+((antiTree.toGroup /: selection) { (g,s) => g \ s }).length+" nodes"
    
    val antiXml = implemented by "anti-xml" in {
      val s2 = antiTree.toGroup \ selection.head
      val sels = (s2 /: selection.tail) { (g,s) => g \ s }
      val mods = sels map {e => e.copy(attrs = e.attrs + attributeToSet)}
      ((mods:Zipper[Node]) /: selection) { (z, _) => z.unselect}
    }
    
    /** Scala.xml implementation */
    val scalaXml = implemented by "scala.xml" in {
      import scala.xml.{Node, Elem, Attribute}
      def modify(ns: Seq[Node], sels: List[String])(f: Elem => Elem): Seq[Node] = ns map {
        case e: Elem => sels match {
          case e.label :: Nil => f(e)
          case e.label :: rest => e.copy(child=modify(e.child,rest)(f))
          case _ => e
        }
        case n => n
      }
      scalaTree.copy(child = modify(scalaTree.child,selection) {e =>
         e % Attribute("",attributeToSet._1,attributeToSet._2,e.attributes)
      })
    }
    
    /** javax.xml implementation, which makes a clone of the dom and then mutates the result */     
    val javaXml = implemented by "javax.xml" in {
      import org.w3c.dom.{Document, NodeList, Node, Element}
      val domCopy = domTree.cloneNode(true).asInstanceOf[Document]
      def modify(nl: NodeList, sels: List[String])(f: Element => Any) {
        for(indx <- 0 until nl.getLength) nl.item(indx) match {
          case e: Element => {
            val name = simpleNameOf(e)
            sels match {
              case `name` :: Nil => f(e)
              case `name` :: rest => modify(e.getChildNodes(), rest)(f)
              case _ => ()
            }
          }
          case _ => ()
        }
      }
      modify(domCopy.getChildNodes().item(0).getChildNodes(), selection) { e =>
        e.setAttribute(attributeToSet._1,attributeToSet._2)
      }
      domCopy
    }
    
    /** Analog to the scala.xml implementation */
    val antiXml2 = implemented by "anti-xml no-zip" in {
      def modify(g: Group[Node], sels: List[String])(f: Elem => Elem): Group[Node] = g map {
        case e: Elem => sels match {
          case e.name :: Nil => f(e)
          case e.name :: rest => e.copy(children=modify(e.children,rest)(f))
          case _ => e
        }
        case n => n
      }
      antiTree.copy(children = modify(antiTree.children,selection) {e =>
         e.copy(attrs = e.attrs + attributeToSet) 
      })
    }
    
    /* The alternate impls get suppressed at low run levels: */
    
    val lowPriorityImpls:Set[AnyImpl] = Set(antiXml2)
    
    override def runLevel(impl: AnyImpl) = 
      super.runLevel(impl) + (if (lowPriorityImpls(impl)) 1 else 0) 

  }
  override def create:ModifyInstance = new ModifyInstance
}

