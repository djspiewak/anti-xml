package com.codecommit

/**
 * Base package for the Anti-XML framework.  Note that importing this package
 * brings in a number of implicit conversions.  Specifically:
 *
 * <ul>
 * <li>`String => Selector[Elem, Zipper[Elem]]` – Used to allow string-based
 * selection syntax with [[com.codecommit.antixml.Group]].  (e.g. `ns \ "name"`)</li>
 * <li>`Symbol => Selector[Elem, Zipper[Elem]]` – Used to allow symbol-based
 * selection syntax with [[com.codecommit.antixml.Group]].  (e.g. `ns \ 'name`)</li>
 * <li>∀`A` . `A => Converter[A]` – Implements ''explicit'' conversions from
 * `scala.xml` types to Anti-XML correspondents (where applicable).  This
 * technically makes the `anti` method available on all types.  However, that
 * method will only be callable on very specific types in the `scala.xml`
 * library, and thus it shouldn't cause any collsion issues.</li>
 * </ul>
 */
package object antixml {
  // (from, to, rebuild, internal map)
  private[antixml] type ZContext = (Int, Int, (Group[Node], Map[Int, Set[Int]]) => Node, Map[Int, Set[Int]])

  /**
   * Implicitly lifts a [[scala.String]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ "name"`
   */
  implicit def stringToSelector(name: String): Selector[Elem, Zipper[Elem]] =
    Selector({ case e @ Elem(_, `name`, _, _) => e }, Some(name))

  /**
   * Implicitly lifts a [[scala.Symbol]] into an instance of [[com.codecommit.antixml.Selector]]
   * which can then be passed to the appropriate methods on [[com.codecommit.antixml.Group]].
   * For example: `ns \ 'name`
   */
  implicit def symbolToSelector(sym: Symbol): Selector[Elem, Zipper[Elem]] = {
    val Symbol(name) = sym
    stringToSelector(name)
  }
  
  /**
   * Pimps the `anti` method onto any object for which there exists a conversion
   * into Anti-XML.  Note that this conversion is an implicit value, statically
   * enforced and thus shouldn't be the source of any collision issues.  It should
   * actually be possible to have another implicit conversion in scope which
   * pimps the `anti` method without seeing conflicts.
   * 
   * @see [[com.codecommit.antixml.XMLConvertable]]
   */
  implicit def nodeSeqToConverter[A](a: A): Converter[A] = new Converter(a)

  /**
   * Wildcard selector which passes ''all'' nodes unmodified.  This is analogous
   * to the `"_"` selector syntax in `scala.xml`.  For example: `ns \ * \ "name"`
   */
  val `*`: Selector[Node, Zipper[Node]] = Selector({ case n: Node => n })
  
  /**
   * Non-node selector which finds exclusively [[com.codecommit.antixml.Text]]
   * nodes and pulls out their `String` content.  Unlike most selectors, the
   * result of using this selector is not a [[com.codecommit.antixml.Group]], but
   * a generic [[scala.collection.Traversable]]`[String]`.  This selector can
   * be used to emulate the `NodeSeq#text` method provided by `scala.xml`.  For
   * example: `ns \\ text mkString` (this is analogous, but not quite equivalent
   * to calling `ns.text` in `scala.xml`).
   */
  val text: Selector[String, Traversable[String]] = Selector({ case Text(str) => str })
}
