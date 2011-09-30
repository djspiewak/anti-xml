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

import java.io.Writer
/**
 * Root of the `Node` ADT, representing the different types of supported XML
 * nodes which may appear in an XML fragment.  The ADT itself has the following
 * shape (Haskell syntax):
 *
 * {{{
 * type Prefix = Maybe String
 * type Scope = Map String String
 *
 * data Node = ProcInstr String String
 *           | Elem Prefix String Attributes Scope (Group Node)
 *           | Text String
 *           | CDATA String
 *           | EntityRef String
 * }}}
 *
 * For those that don't find Haskell to be the clearest explanation of what's
 * going on in this type, here is a more natural-language version.  The `Node`
 * trait is sealed and has exactly five subclasses, each implementing a different
 * type of XML node.  These five classes are as follows:
 *
 * <ul>
 * <li>[[com.codecommit.antixml.ProcInstr]] – A processing instruction consisting
 * of a target and some data</li>
 * <li>[[com.codecommit.antixml.Elem]] – An XML element consisting of an optional
 * prefix, a name (or identifier), a set of attributes, a set of namespace mappings 
 * in scope and a sequence of child nodes</li>
 * <li>[[com.codecommit.antixml.Text]] – A node containing a single string, representing
 * character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.CDATA]] – A node containing a single string, representing
 * ''unescaped'' character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.EntityRef]] – An entity reference (e.g. `&amp;`)</li>
 * </ul>
 */
sealed trait Node {
  /** Returns the children of this node. If the node is an [[com.codecommit.antixml.Elem]], then this method returns the element's children, 
   * otherwise it returns return an empty [[com.codecommit.antixml.Group]] */
  def children = Group.empty[Node]
}

private[antixml] object Node {
  // TODO we should probably find a way to propagate custom entities from DTDs
  def escapeText(text: String) = text flatMap {
    case '"' => "&quot;"
    case '&' => "&amp;"
    case '\'' => "&apos;"
    case '<' => "&lt;"
    case '>' => "&gt;"
    case c => List(c)
  }
  
}
 
/**
 * A processing instruction consisting of a `target` and some `data`.  For example:
 *
 * {{{
 * <?xml version="1.0"?>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * ProcInstr("xml", "version=\"1.0\"")
 * }}}
 */
case class ProcInstr(target: String, data: String) extends Node {
  override def toString = "<?" + target + " " + data + "?>"
}

/**
 * An XML element consisting of an optional namespace prefix, a name (or identifier), a
 * set of attributes, a namespace prefix scope (mapping of prefixes to namespace URIs),
 * and a sequence of child nodes.
 * For example:
 *
 * {{{
 * <span id="foo" class="bar">Lorem ipsum</span>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * Elem(None, "span", Attributes("id" -> "foo", "class" -> "bar"), Map(), Group(Text("Lorem ipsum")))
 * }}}
 */
case class Elem(prefix: Option[String], name: String, attrs: Attributes, scope: Map[String, String], override val children: Group[Node]) extends Node with Selectable[Elem] {
  import Elem.NameRegex
  
  for (p <- prefix) {
    if (NameRegex.unapplySeq(p).isEmpty) {
      throw new IllegalArgumentException("Illegal element prefix, '" + p + "'")
    }
  } 
  
  if (NameRegex.unapplySeq(name).isEmpty) {
    throw new IllegalArgumentException("Illegal element name, '" + name + "'")
  }
  
  /**
   * See the `canonicalize` method on [[com.codecommit.antixml.Group]].
   */
  def canonicalize = copy(children=children.canonicalize)
  
  override def toString = {
    val sw = new java.io.StringWriter() 
    val xs = XMLSerializer()
    xs.serialize(this, sw)
    sw.toString
  }
  
  override val hashCode = runtime.ScalaRunTime._hashCode(this)
  
  def toGroup = Group(this)
}

object Elem extends ((Option[String], String, Attributes, Map[String, String], Group[Node]) => Elem) {
  val NameRegex = {
    val nameStartChar = """:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD"""
    "[" + nameStartChar + "][" + nameStartChar + """\-\.0-9\u00B7\u0300-\u036F\u203F-\u2040]*"""r
  }
}

/**
 * A node containing a single string, representing character data in the XML tree.
 * For example:
 *
 * {{{
 * Lorem ipsum &amp; dolor sit amet
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * Text("Lorem ipsum & dolor sit amet")
 * }}}
 *
 * Note that reserved characters (as defined by the XML 1.0 spec) are escaped
 * when calling `toString`.  Thus, if you invoke `toString` on the `Text` node
 * given in the example above, the result will reverse back into the original
 * text, including the `&amp;` escape.  If you need a text representation which
 * does ''not'' escape characters on output, use [[com.codecommit.antixml.CDATA]].
 */
case class Text(text: String) extends Node {
  override def toString = Node.escapeText(text)
}

/**
 * A node containing a single string, representing unescaped character data in
 * the XML tree.  For example:
 *
 * {{{
 * <![CDATA[Lorem ipsum & dolor sit amet]]>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * CDATA("Lorem ipsum & dolor sit amet")
 * }}}
 *
 * Note that reserved characters (as defined by the XML 1.0 spec) are ''not''
 * escaped when calling `toString`.  If you need a text representation which
 * performs escaping, use [[com.codecommit.antixml.Text]]
 */
case class CDATA(text: String) extends Node {
  if (text.contains("]]>"))
    throw new IllegalArgumentException("CDATA nodes can't contain ']]>'")
  override def toString = "<![CDATA[" + text + "]]>"
}

/**
 * A node representing an entity reference. For example:
 *
 * {{{
 * &hellip;
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * EntityRef("hellip")
 * }}}
 */
case class EntityRef(entity: String) extends Node {
  override def toString = "&" + entity + ";"
}
