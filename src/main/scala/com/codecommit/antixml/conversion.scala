package com.codecommit
package antixml

class Converter[A](a: A) {
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
  implicit object NodeSeqConvertable extends XMLConvertable[xml.NodeSeq, Group[Node]] {
    def apply(ns: xml.NodeSeq) = Group(ns map NodeConvertable.apply: _*)
  }
}
