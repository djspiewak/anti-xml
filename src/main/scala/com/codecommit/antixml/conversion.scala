package com.codecommit
package antixml

/**
 * Pimp container for the explicit conversions into Anti-XML types.  Out of the
 * box, conversions are provided from `scala.xml` types.  However, this mechanism
 * is very extensible due to the use of a typeclass ([[com.codecommit.antixml.XMLConvertable]])
 * to represent the actual conversion.  Thus, it is possible to add conversions
 * by defining an implicit instance of the typeclass and having it in scope.  It
 * is even possible to override the built-in conversions for `scala.xml` types
 * simply by shadowing the conversions for types like [[scala.xml.Elem]].  The
 * built-in conversions are defined in such a way that Scala's implicit resolution
 * will give precedence to almost anything you define, as long as it is somehow
 * in scope.
 */
class Converter[A](a: A) {
  
  /**
   * Converts a target type `A` into some result type B (presumably in the Anti-XML
   * API).  Technically, this function is not just restricted to converting into
   * Anti-XML types.  However, it would probably minimize confusion if it were
   * exclusively used for this purpose.  This generality comes from the fact that
   * the `anti` function itself doesn't perform any conversion, but merely delegates
   * directly to the `apply` method on whatever instance of `XMLConvertable` it
   * happens to be passed.
   *
   * '''Note:''' If no conversion is available for the target type, then the compiler
   * will reject this method call.  Similarly, if more than one conversion is in
   * scope and neither has implicit precedence over the other, then the compiler
   * will reject this method call as ambiguous.  In such cases, it is always
   * possible to pass the conversion explicitly.
   *
   * @see [[com.codecommit.antixml.XMLConvertable]]
   * @usecase def anti: Node 
   */
  def anti[B](implicit conversion: XMLConvertable[A, B]) = conversion(a)
}


trait XMLConvertable[-A, +B] {      // note: doesn't extend Function1 to avoid coercion
  def apply(a: A): B
}

object XMLConvertable extends SecondPrecedenceConvertables {
  implicit object ElemConvertable extends XMLConvertable[xml.Elem, Elem] {
    def apply(e: xml.Elem) = {
      val ns = if (e.prefix == null) None else Some(e.prefix)
      val attrs = e.attributes.asAttrMap
      val children = NodeSeqConvertable(xml.NodeSeq fromSeq e.child)
      Elem(ns, e.label, attrs, children)
    }
  }
  
  implicit object TextConvertable extends XMLConvertable[xml.Atom[String], Text] {
    def apply(t: xml.Atom[String]) = Text(t.text)
  }
  
  implicit object EntityRefConvertable extends XMLConvertable[xml.EntityRef, EntityRef] {
    def apply(ref: xml.EntityRef) = EntityRef(ref.entityName)
  }
}

// it really amazes me that this even works
private[antixml] sealed trait SecondPrecedenceConvertables extends ThirdPrecedenceConvertables { this: XMLConvertable.type =>
  implicit object NodeConvertable extends XMLConvertable[xml.Node, Node] {
    def apply(n: xml.Node) = n match {
      case e: xml.Elem => ElemConvertable(e)
      case a: xml.Atom[String] => TextConvertable(a)
      case r: xml.EntityRef => EntityRefConvertable(r)
      case g: xml.Group => error("xml.Group should never have been a Node; there is no sane conversion")
    }
  }
}

private[antixml] sealed trait ThirdPrecedenceConvertables { this: XMLConvertable.type =>
  // written against Seq[xml.Node] rather than NodeSeq since scala.xml isn't consistent
  implicit object NodeSeqConvertable extends XMLConvertable[Seq[xml.Node], Group[Node]] {
    def apply(ns: Seq[xml.Node]) = Group(ns map NodeConvertable.apply: _*)
  }
}
