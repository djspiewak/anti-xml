package com.codecommit.antixml

/**
 * Root of the `Node` ADT, representing the different types of supported XML
 * nodes which may appear in an XML fragment.  The ADT itself has the following
 * shape (Haskell syntax):
 *
 * {{{
 * data Node = ProcInstr String String
 *           | Elem (Maybe String) String (Map String String) (Group Node)
 *           | Text String
 *           | CDATA String
 *           | EntityRef String
 * }}}
 *
 * For those that don't find Haskell to be the clearest explanation of what's
 * going on in this type, here is a more natural-language version.  The `Node`
 * trait is sealed and has exactly four subclasses, each implementing a different
 * type of XML node.  These four classes are as follows:
 *
 * <ul>
 * <li>[[com.codecommit.antixml.ProcInstr]] – A processing instruction consisting
 * of a target and some data</li>
 * <li>[[com.codecommit.antixml.Elem]] – An XML element consisting of an optional
 * namespace, a name (or identifier), a set of attributes and a sequence of child nodes</li>
 * <li>[[com.codecommit.antixml.Text]] – A node containing a single string, representing
 * character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.CDATA]] – A node containing a single string, representing
 * ''unescaped'' character data in the XML tree</li>
 * <li>[[com.codecommit.antixml.EntityRef]] – An entity reference (e.g. `&amp;`)</li>
 * </ul>
 */
sealed trait Node

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
 * An XML element consisting of an optional namespace, a name (or identifier), a
 * set of attributes and a sequence of child nodes. For example:
 *
 * {{{
 * <span id="foo" class="bar">Lorem ipsum</span>
 * }}}
 * 
 * This would result in the following node:
 *
 * {{{
 * Elem(None, "span", Map("id" -> "foo", "class" -> "bar"), Group(Text("Lorem ipsum")))
 * }}}
 */
case class Elem(ns: Option[String], name: String, attrs: Map[String, String], children: Group[Node]) extends Node {
  override def toString = {
    val prefix = ns map { _ + ':' } getOrElse ""
    val qName = prefix + name
    
    val attrStr = if (attrs.isEmpty) 
      ""
    else
      " " + (attrs map { case (key, value) => key + "=\"" + value + '"' } mkString " ")
    
    val partial = "<" + qName + attrStr
    if (children.isEmpty)
      partial + "/>"
    else
      partial + '>' + children.toString + "</" + qName + '>'
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
  override def toString = text flatMap {
    case '"' => "&quot;"
    case '&' => "&amp;"
    case '\'' => "&apos;"
    case '<' => "&lt;"
    case '>' => "&gt;"
    case c => List(c)
  }
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
