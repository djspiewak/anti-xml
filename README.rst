Anti-XML is a replacement for the ``scala.xml`` package in the Scala standard
library.  The goal is to learn from the mistakes made in the implementation of
the original library and finally provide an implementation which corrects them.
The problems with ``scala.xml`` boil down to three major categories:

* Clumsy API
* Unreliable semantics (partially stemming from the secret use of mutable data
  structures under the surface)
* Extremely poor performance (especially in terms of memory use)

There are numerous outstanding issues which fall into each category, but this is
the gist of it.  In short, there are far too many fundamental problems with
``scala.xml`` to simply fix.  We need to start from scratch, unburdened by the
need for backward compatibility or prior assumptions.

That is precisely what this project intends to do.  As an example, the core data
structures are implemented on top of ``Vector`` rather than ``ArrayBuffer``.
This not only ensures thread-safety, it also provides substantially better
performance and a lower memory footprint.  We've also been re-thinking some
fundamental aspects of the API.  For example, ``Node`` no longer extends ``NodeSeq``.
In fact, there is *no* ``NodeSeq``.  Instead, anti-xml provides a ``Group[+A <: Node]``
type which is far more general and far more convenient convenient in practice.
(for example, if you really miss ``NodeSeq``, you can just define ``type NodeSeq = Group[Node]``)
Selectors have also been revamped and now satisfy a rigidly consistent contract.
They are also only defined on ``Group`` (*not* ``Node``).  More on this below_.

We have a `large set of ideas`_ that we're exploring for this project.  Not all of
them will make it (certainly not in their current form), but it's a start.  It
will be interesting to see where things go!


Usage
=====

We haven't pushed any compiled binaries as of yet.  You will need to clone the
project using Git.  Then, use SBT to build a JAR for the project.  Everything
you need will be in the ``com.codecommit.antixml`` package.  Enjoy!

The API should look fairly familiar to anyone who has used the ``scala.xml``
package.  For example::
    
    import com.codecommit.antixml._
    
    val xml = XML.fromFile("src/test/resources/bookstore.xml")
    val titles = (xml \ "book" \ "title" \ *) map { _.toString }
    val firstAuthor = (xml \\ "author" \ *) map { _.toString } headOption

Well, maybe not *exactly* like ``scala.xml``.  This example uses the bookstore.xml_
file from the test suite.  The ``titles`` and ``firstAuthor`` fields will have
the following values:

* ``titles`` = ``Vector("For Whom the Bell Tolls", "I, Robot", "Programming Scala")``
* ``firstAuthor`` = ``Some("Hemmingway")``

The ``\`` and ``\\`` operators make it possible to perform complex queries against
XML trees simply and efficiently (see the section on selectors_).  If you pass
a ``String`` value to these selectors, then the result will be a ``Group[Elem]``.
You can also pass a ``Symbol`` if that is more convenient::
    
    val xml = ...
    val books = xml \ "book"
    val books2 = xml \ 'book       // equivalent to `books`
    
You will also note that the ``*`` character is used as a wildcard selector, rather
than the magical string value ``"_"`` (as in ``scala.xml``).  This is because
selectors do not do *any* string parsing (at all)!  If you pass a string as a
selector, it assumes that string value to be an element name, no more, no less.
This dramatically simplifies the selection semantics and also serves to make the
behavior (and performance) of the library quite a bit more predictable.

It is also worth noting that the ``Node`` hierarcy has been dramatically
simplified.  You can see this for yourself by looking at the node.scala_ file.
Basically, ``Node`` is now a proper `algebraic data type`_ with a very straightforward
(and lightweight) structure.  This makes a lot of common tasks quite a bit easier.
For example, if I wanted to get the value of the ``popular`` attribute of the
first ``book`` element, I could do so very easily::
    
    val xml = ...
    val popular: String = (xml \ "book").head.attrs("popular")
    
We're still working out the best way to incorporate namespace information into
this representation.  If you have any ideas, please fork and demonstrate!

One very important aspect of the API is that ``Group`` is based on ``scala.collection.immutable.Vector``.
As such, it is possible to access any node within a ``Group`` in constant time.
It is also possible to *update* (by which I mean "derive a new ``Group`` with
revised data") any node in constant time.  This can be very useful sometimes::
    
    val xml = ...
    val books = xml \ "book"
    val books2 = books.updated(2, books(2).copy(attrs=Map("updated" -> "yes")))
    
Note that this snippet makes use of the ``copy`` method which we get for free on
``Elem`` because of its nature as a case class.

There are a lot more things to see and a large number of improvements over Scala's
built-in XML support.  For example, we actually provide a mechanism for taking
the ``books2`` value in the above example and reconstructing the original ``xml``
tree around it, ariving at the original structure modulo the change made to the
third ``<book>`` element deep inside the tree.  For more details, see some of
the following sections.

.. _bookstore.xml: https://github.com/djspiewak/anti-xml/blob/master/src/test/resources/bookstore.xml
.. _node.scala: https://github.com/djspiewak/anti-xml/blob/master/src/main/scala/com/codecommit/antixml/node.scala
.. _algebraic data type: http://en.wikipedia.org/wiki/Algebraic_data_type

.. _below:
.. _selectors:

Selectors
=========

Anti-XML provides a very general mechanism for selectors.  However, before we get
into that, we need to settle a little bit of terminology.  Consider the following
snippet::
    
    val xml: Group[Node] = ...
    xml \ * \ "book"
    xml \\ "author"
    
In this snippet, there are three selectors and two select operators.  The two
select operators are ``\`` and ``\\``, which are "shallow-select" and "deep-select"
respectively.  The selectors are ``*`` (the wildcard selector), ``"book"`` and
``"author"``.  Thus, select operators are defined on ``Group`` each as a function
which takes a selector and returns a new ``Group``.


Select Definitions
------------------

Shallow- and deep-select are both defined in full generality.  Shallow-select
is (in principle) defined as the following::
    
    def \(selector: Selector) = {
      nodes flatMap {
        case Elem(_, _, _, children) => children collect selector
        case _ => Group()
      }
    }
    
This is to say, shallow-select finds all of the ``Elem``(s) in the current ``Group``
and filters their children against the selector (which extends ``PartialFunction``).
The filtered children are then concatenated together into a single ``Group``.

Deep-select is (in principle) defined as the following::
    
    def \\(selector: Selector) = {
      val recursive = nodes flatMap {
        case Elem(_, _, _, children) => children \\ selector
        case _ => Group()
      }
      
      (this \ selector) ++ recursive
    }
    
This is to say that deep-select is equivalent to applying shallow-select at every
level of the XML tree, recursively.  It is important to note that if a selector
matches some ``Elem`` *a* which in turn contains a child ``Elem`` *b* which is
also matched by the selector, both *a* and *b* will be returned by ``\\``.


Selectors
---------

A selector is an object of type ``Selector[A, Coll]``, which is really just a
``PartialFunction[Node, A]`` with some extra trimming (for optimization).  This
function is used to search and transform (in a single pass) the result set on a
select.  In principle, selectors can return *any* results.  For example, one could
write a ``text`` selector which results in a ``List[String]`` object containing
the respective contents of the ``Text`` and ``Whitespace`` nodes in the tree.
This selector would be defined in the following way::
    
    val text: Selector[String, List[String]] = Selector({
      case Text(str) => str
      case Whitespace(str) => str
    })
    
This selector could then be used just like any other::
    
    val xml: Group[Node] = ...
    xml \ text        // => List[String]
    
In this way, the selector mechanism is fully extensible to almost any use-case.
There are three build-in selectors:

* Select only ``Elem`` nodes based on name

  * Implicit conversion from ``String`` (e.g. ``xml \ "book"``)
  * Implicit conversion from ``Symbol`` (e.g. ``xml \ 'book``)
  
* Select all nodes (basically, the identity selector)

  * Defined as the ``*`` operator


Type Safety
-----------

Every selector is typed on a resulting element and collection type.  For example::
    
    val `*`: Selector[Node, Group[Node]] = ...
    
    implicit def strToSelector(str: String): Selector[Elem, Group[Elem]] = ...
    implicit def symToSelector(str: Symbol): Selector[Elem, Group[Elem]] = ...

Notably, any select method will return a collection of the type specified by
the selector.  This is quite useful in many ways.  For example, if you select
using one of the name selectors (using ``String`` or ``Symbol``), then the
collection resulting from the select will be of type ``Group[Elem]``::
    
    val xml: Group[Node] = ...
    val results: Group[Elem] = xml \ "book"
    
However, if you select using the wildcard selector (``*``), the result will
naturally be of type ``Group[Node]`` since every node (including non-``Elem``(s))
will be returned::
    
    val xml: Group[Node] = ...
    val results: Group[Node] = xml \ *


Zippers
=======

**TODO**


.. _large set of ideas:

The Task List
=============

The task list for this project is maintained as a public message in `Novell Vibe`_.
To access this message, simply sign up for a free account (if you haven't already)
and then hit the following URL: https://vibe.novell.com/thread/41cf4424-15c6-40dd-b79f-497bcbd8e147


.. _Novell Vibe: https://vibe.novell.com
