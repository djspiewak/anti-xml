==================================
Anti-XML: The Non-Violent Solution
==================================

Anti-XML is a replacement for the ``scala.xml`` package in the Scala standard
library.  The goal is to learn from the mistakes made in the implementation of
the original library and finally provide an implementation which corrects them.
The problems with ``scala.xml`` boil down to three major categories:

* **Clumsy API**

  * The ``\`` and ``\\`` operators do not behave consistently, a flaw which stems
    from the misguided desire to emulate XPath (something which cannot be done
    in a combinatorial setting)
  * ``Node <: NodeSeq <: Seq[Node]``
  
    * Oh yeah, and ``Node`` isn't actually an ADT_!
  
  * The implicit conversion from ``Seq[Node]`` to ``NodeSeq`` means you're never
    really sure which type you're working with in non-trivial code
  * ``NodeSeq`` fails to leverage the 2.8 collections library, meaning that many
    of its methods return ``Seq[Node]`` rather than ``NodeSeq``, a problem
    compounded by the previous point
  * One word: ``MetaData``
  * One more word: ``Atom[String]``

* **Unreliable semantics** (partially stemming from the secret use of mutable data
  structures under the surface)
  
  * Pervasive concurrency bugs and race conditions
  * Very surprising (and buggy) ``equals`` implementations
  * Use of mutability means that bugs can *occasionally* create recursive XML
    trees (not seen since Scala 2.7.5, but I thought I'd mention it)
  
* **Extremely poor performance** (especially in terms of memory use)

  * `Novell Vibe`_ once had a chunk of XML which was 16 MB on disk and required
    *250 MB* of heap space!
  * Selectors are generally very slow
  * The formatted-string selector semantics for attribute querying mean that
    *every* string gets boxed into a ``Text``, which creates enormous heap crush

There are many more outstanding issues which fall into each category, but this is
the gist of it.  In short, there are far too many fundamental problems with
``scala.xml`` to simply fix.  We need to start from scratch, unburdened by the
need for backward compatibility or prior assumptions (like the fake XPath support).

That is precisely what this project intends to do.  As an example, the core data
structures are implemented on top of ``Vector`` rather than ``ArrayBuffer``.
This not only ensures thread-safety, it also provides substantially better
performance and a lower memory footprint.  We've also been re-thinking some
fundamental aspects of the API.  For example, ``Node`` no longer extends ``NodeSeq``.
In fact, there is *no* ``NodeSeq``.  Instead, anti-xml provides a ``Group[+A <: Node]``
type which is far more general and far more convenient in practice. (for example,
if you really miss ``NodeSeq``, you can just define ``type NodeSeq = Group[Node]``)
Selectors have also been revamped and now satisfy a rigidly consistent contract.
They are also only defined on ``Group`` (*not* ``Node``).  More on this below_.

We have a `large set of ideas`_ that we're exploring for this project.  Not all of
them will make it (certainly not in their current form), but it's a start.  It
will be interesting to see where things go!

.. _ADT: http://en.wikipedia.org/wiki/Algebraic_data_type
.. _large set of ideas: http://dl.dropbox.com/u/1679797/anti-xml-todo.html


Usage
=====

* `Scaladoc for the master branch`_
* `Scaladoc for the latest stable release`_

The Maven artifact descriptor for the latest *stable* version of
Anti-XML is as follows: ``com.codecommit:anti-xml_2.8.1:0.2``.  We also regularly
push ``-SNAPSHOT`` releases to the Scala-Tools_ "snapshots" repository, for all
five of you who like to live dangerously.  You should be able to use this
descriptor to easily add Anti-XML as a dependency to any project with a
Maven-compatible build system (Maven, Buildr, SBT, Gradle, Ivy, etc). The stable
artifacts themselves are hosted in the Scala-Tools_ "releases" repository.  For
reference, here are a few copy/paste snippets you can use for some of the common
build systems.

**SBT**::
  
    val antiXML = "com.codecommit" %% "anti-xml" % "0.2"
  
**Buildr**::
  
    compile.with "com.codecommit:anti-xml_#{Scala.version}:jar:0.2"
  
**Maven2**::
  
    <dependency>
      <groupId>com.codecommit</groupId>
      <artifactId>anti-xml_2.8.1</artifactId>
      <version>0.2</version>
    </dependency>
    
  
Supported Versions of Scala
---------------------------

Anti-XML is cross-built_ for the following Scala versions:

* **2.9.0-1** (0.3-SNAPSHOT release only)
* **2.9.0**
* **2.8.1**

While it is theoretically possible to add support for 2.8.0, we have no plans to
do so at this time.  The reason being that Specs2_ – the testing framework used
by Anti-XML – has not been cross-built for 2.8.0.  Additionally, ScalaCheck_ has
not updated its 2.8.0 cross-build in several months.  All that combined with the
fact that 2.8.1 is a nearly-completely backwards compatible update with 2.8.0 has
led to the conclusion that cross-building for 2.8.0 just isn't worth the effort.

.. _cross-built: http://code.google.com/p/simple-build-tool/wiki/CrossBuild
.. _Specs2: http://etorreborre.github.com/specs2/
.. _ScalaCheck: http://code.google.com/p/scalacheck/

Random Snippets
---------------

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
    val books2 = xml \ 'book       // `books2` is equivalent to `books`
    
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

.. _Scaladoc for the master branch: http://www.danielspiewak.com/anti-xml/doc
.. _Scaladoc for the latest stable release: http://www.danielspiewak.com/anti-xml/v0.1/doc
.. _Scala-Tools: http://scala-tools.org
.. _the project CI server: http://hudson.danielspiewak.org/job/anti-xml/
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
    
This is to say, shallow-select finds all of the ``Elem`` in the current ``Group``
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

A selector is an object of type ``Selector[A]``, which is really just a
``PartialFunction[Node, A]`` with some extra trimming (for optimization).  This
function is used to search and transform (in a single pass) the result set on a
select.  In principle, selectors can return *any* results.  For example, one could
write a ``text`` selector which produces a collection of ``String`` representing
the contents of all of the ``Text`` nodes in the tree. This selector would be
defined in the following way::
    
    val text: Selector[String] = Selector({
      case Text(str) => str
    })
    
This selector could then be used just like any other::
    
    val xml: Group[Node] = ...
    xml \ text        // => Vector[String]
    
In this way, the selector mechanism is fully extensible to almost any use-case.
There are four build-in selectors:

* Select only ``Elem`` nodes based on name

  * Implicit conversion from ``String`` (e.g. ``xml \ "book"``)
  * Implicit conversion from ``Symbol`` (e.g. ``xml \ 'book``)
  
* Select all nodes (basically, the identity selector)

  * Defined as the ``*`` operator
  
* Select the *contents* of ``Text`` and ``CDATA`` nodes

  * Defined as ``text`` (e.g. ``xml \ text``)
  * Very close to the example given above


Type Safety
-----------

Every selector is typed on a resulting element and collection type.  For example::
    
    val `*`: Selector[Node] = ...
    
    implicit def strToSelector(str: String): Selector[Elem] = ...
    implicit def symToSelector(str: Symbol): Selector[Elem] = ...

Notably, any select method will return a collection of the type specified by
the selector.  This is quite useful in many ways.  For example, if you select
using one of the name selectors (using ``String`` or ``Symbol``), then the
collection resulting from the select will be of type ``Group[Elem]``::
    
    val xml: Group[Node] = ...
    val results: Group[Elem] = xml \ "book"
    
However, if you select using the wildcard selector (``*``), the result will
naturally be of type ``Group[Node]`` since every node (including non-``Elem`` )
will be returned::
    
    val xml: Group[Node] = ...
    val results: Group[Node] = xml \ *

The result of a selection need not be of type ``Group``!  For example, consider
the ``text`` selector::
    
    val xml: Group[Node] = ...
    val results: IndexedSeq[String] = xml \ text
    
This is logical since selection using ``text`` will return a sequence of ``String``,
which obviously cannot be contained within a ``Group``.  The exact return type
is based on the instance of ``CanBuildFromWithZipper`` which is in implicit
scope at the call-site.  Any selector which produces ``Node`` (or a subtype) will
match the default instance of ``CanBuildFromWithZipper`` which produces an object
of type ``Group`` (actually, ``Zipper``; see below).  Selectors which produce
other types (such as ``String``, in the case of the ``text`` selector) will fall
back on an implicit "lift" of ``CanBuildFrom`` to ``CanBuildFromWithZipper``.
Thus, the fallback resolution is for the compiler to find an instance of
``CanBuildFrom`` in implicit scope at the call-site and lift that into an instance
of ``CanBuildFromWithZipper``.  Since there is a ``CanBuildFrom`` defined for
elements of type ``String`` which produces an ``IndexedSeq[String]``, that becomes
the type of the resultant of applying a selector of type ``Selector[String]``.

There is a slight catch to this implicit "lift" mechanism: it only works for
result types which are implicitly convertable to ``Traversable[A]``, where the
selector is of type ``Selector[A]``.  In practice, the only instances of
``CanBuildFrom`` which are particularly useful are those which return collections
(or objects implicitly convertable to collections).  However, it is theoretically
possible to have a ``CanBuildFrom`` which produces something which is not a
``Traversable``.  In this case, you will need to define a custom implicit
``CanBuildFromWithZipper`` for that type, rather than relying on the built-in
lifting.  Such a definition is almost exactly the same as defining an instance
of ``CanBuildFrom``.  The primary difference is that the result type must be
monoidal.  This is to say, it must be possible to define a function of type
``(A, A) => A`` where ``A`` is the result type of the ``CanBuildFromWithZipper``,
and this function definition must behave according to the monoidal laws regarding
composition (or at least, it must if you want deep-select to return sane results).


Zippers
=======

Most of us have heard the term "zipper" at one point or another.  Unfortunately,
it's a very overloaded term and can mean anything from a popular clothing fastener
to a collections utility method to a data structure.  In this context, "zipper"
refers to the functional data structure allowing efficient and convenient in-place
updates to immutable trees.

Fortunately, you don't need to understand what that means in order to make use of
this powerful concept.  In fact, you don't even need to know that it's there!

::
    
    val xml: Group[Elem] = ...
    val results = xml \ "book"
    
In this example, ``results`` will of course be of type ``Group[Elem]``...sort of.
It will actually be of a more specific type: ``Zipper[Elem]``.  ``Zipper`` extends
``Group``, so if you want to just ignore the zipper data structure and use selector
results as a ``Group``, then by all means go right ahead!  However, by leveraging
the power of the zipper, it is possible to perform some really amazing tasks which
are difficult almost to the point of impossibility with ``scala.xml``.

As an example, imagine we had selected all of the ``<book/>`` elements (as handled
by the above snippet) and we wanted to grab just the first of those elements and
give it a new attribute (say, ``first="yes"``).  Of course, XML trees are immutable,
but it's easy enough to derive a new version of ``results`` which has the
modification::
    
    val results = xml \ "book"
    val book2 = results.head.copy(attrs=Map("first" -> "yes"))
    val results2 = results.updated(0, book2)
    
The ``results2`` variable will be of type ``Group[Elem]`` and will contain exactly
the same contents as ``results``, except that the first ``<book/>`` will now have
our ``first="yes"`` attribute.  So far, so good...

Now comes the tricky part.  Let's say that instead of getting the updated results,
what we *really* wanted was the updated ``xml`` value.  In other words, we started
with an XML tree, we drilled down into that tree using a selector, we derived a
new version of that result set with some modifications (in our case, a new attribute),
and now we want to go *back* to the tree we originally had, except with the modifications
we made way down in the bowels.  This is what a zipper is for::
    
    val results2 = results.updated(0, book2)
    val xml2 = results2.unselect
    
That's all there is to it!  Imagine the contents of ``xml`` had been the following::
    
    <bookstore>
        <book>
            <title>For Whom the Bell Tolls</title>
            <author>Hemmingway</author>
        </book>
        <book>
            <title>I, Robot</title>
            <author>Isaac Asimov</author>
        </book>
        <book>
            <title>Programming Scala</title>
            <author>Dean Wampler</author>
            <author>Alex Payne</author>
        </book>
    </bookstore>
    
We selected all of the ``<book/>`` elements and then "changed" (well, derived a
new version of) the first one to have the ``first="yes"`` attribute.  We then
used the ``unselect`` zipper method to go *back* to our original tree (modulo
modifications), which means that ``xml2`` will contain the following::
    
    <bookstore>
        <book first="yes">
            <title>For Whom the Bell Tolls</title>
            <author>Hemmingway</author>
        </book>
        <book>
            <title>I, Robot</title>
            <author>Isaac Asimov</author>
        </book>
        <book>
            <title>Programming Scala</title>
            <author>Dean Wampler</author>
            <author>Alex Payne</author>
        </book>
    </bookstore>
    
If you were doing this with ``scala.xml``, you would be stuck rebuilding the
``<bookstore>...</bookstore>`` parent by hand.  Now in this case, that's not so
bad, but imagine we were doing something more complicated.  For example, what if
we were to traverse all the way down to the ``<title/>`` elements and play the
same trick::
    
    val xml: Group[Elem] = ...
    val results = xml \ "book" \ "title"
    val results2 = results.updated(0, results.head.copy(attrs=Map("first" -> "yes")))
    val xml2 = results2.unselect.unselect
    
The only difference here is the fact that we had to call ``unselect`` twice rather
than once.  This is because we actually selected (using the ``\`` operator) twice
rather than once.  Thus, ``unselect`` is like an undo function for selection.
And the final result?

::
    
    <bookstore>
        <book>
            <title first="yes">For Whom the Bell Tolls</title>
            <author>Hemmingway</author>
        </book>
        <book>
            <title>I, Robot</title>
            <author>Isaac Asimov</author>
        </book>
        <book>
            <title>Programming Scala</title>
            <author>Dean Wampler</author>
            <author>Alex Payne</author>
        </book>
    </bookstore>
    
Imagine trying to handle *that* with ``scala.xml``!  We could make this even more
complicated by adding other elements under ``<bookstore>...</bookstore>``, or by
using ``unselect`` followed by a subsequent selection, modification, ``unselect``,
etc.  The zipper keeps track of all of the context required to get back to where
we started modulo all of the "changes" we have made.  In the end, it's all of the
convenience of working with a mutable XML tree without any of the concurrency
issues or murky reasoning.

Of course, maintaining all of that context doesn't come free, and zipper does use
quite a bit of memory.  It's not *huge*, but it's also not something you can ignore
if you're working with very large trees.  The most serious impact is that the
results of a selection maintain a pointer to the original parent ``Group``.  Thus,
you cannot take a large XML tree, select into it, discard the parent pointer and
expect the majority of the tree to be GC'd.  The zipper parent pointer(s) will
prevent that.  That is, unless you throw away the context::
    
    val xml: Group[Elem] = ...
    val results = xml \ "book"
    val trim = results.stripZipper
    
The ``trim`` variable will contain exactly the same elements as ``results``.
The only difference is that it will be of type ``Group[Elem]`` rather than
``Zipper[Elem]``, and as the types would suggest, it does not contain the zipper
context needed to reconstitute the parent (and surrounding) tree.  This has the
advantage of allowing the garbage collector to clean up the parent tree if in fact
you have released all other references.  Of course, you cannot use the ``unselect``
method on the ``trim`` object (and the compiler will ensure this), but depending
on your performance needs, that may be an acceptable sacrifice.  The choice is
yours.

Supported "Modifications"
-------------------------

It's worth noting that while all collection methods supported by ``Vector`` are
also supported by ``Group`` (and by extension, ``Zipper``), *not* all of those
methods are able to preserve the zipper context.  Obviously, things like ``fold``,
``reduce``, ``mkString`` and so on are not going to be able to carry any special
information (nor would it make sense to do so).  Also, if you do something like
``map`` over a ``Zipper`` and have a function which returns ``Int`` (or anything
else which cannot be stored in a ``Group``), then clearly the zipper context will
be lost in that case as well.

However, any ``map`` which returns something of type ``Node`` (or any of its
subtypes) will preserve the zipper context and you will be able to ``unselect``
on the resulting collection.  Similarly (and as we saw in the examples), the
``updated`` method is also able to preserve context.  Unfortunately, methods like
``:+`` and ``+:`` (append and prepend, respectively), and so on are *not* able
to preserve context.  A full list of context-preserving methods follows below:

* ``collect``
* ``filter``
* ``flatMap``
* ``map``
* ``updated``
* ``withFilter``

We're working to add more methods to this list.  A large number of collection-returning
utility methods can be implemented in terms of ``flatMap``.  Implementing these
methods is largely a matter of just writing a few lines of code with the appropriate
delegation.

Other Selectors
---------------

Right now, only the ``\`` method returns a zippable result.  The ``\\`` method
(deep-select) will certainly return something of *type* ``Zipper[A <: Node]``
(assuming that an appropriate selector is specified), but the result will not
contain any zipper context.  Here again, we are working to rectify this issue.
Unfortunately, getting the zipper to work with deep selection is very, *very*
non-trivial and requires a great deal of experimentation and design.  If you're
interested in playing with the work-in-progress, you can grab the deep-zipper_
branch of the main GitHub repository.

.. _deep-zipper: https://github.com/djspiewak/anti-xml/tree/deep-zipper


Performance
===========

Performance is one of the most important features of a framework, particularly
one operating at a low-level on comparatively sizable data sets (like XML).  This
is why we have made benchmarking and rigorous performance testing an integral
part of our development process.  We're still adding tests and optimizing, but
the results are already very promising.

One feature of Anti-XML which is important to remember is the fact that we use
bloom filters to optimize selection over arbitrarily large trees.  This is why
both shallow and deep selection are almost unacountably fast under Anti-XML (when
compared to ``scala.xml`` and even ``javax.xml``).  Unfortunately, it is also why
Anti-XML trees require noticably more memory than ``scala.xml``, and why Anti-XML
parse times tend toward the long side.

All of the tests below were performed on a 2010 MacBook Pro with a Dual core,
2.66 Ghz Core i7 (Turbo up to 3 Ghz) and hyperthreading enabled, 8 GB of 1067 Mhz
DDR3 RAM and a 256 GB 3 Gbps SATA2 SSD.  The sources for all of the performance
tests can be found in the repository.

Memory
------

.. image:: https://chart.googleapis.com/chart?cht=bvg&chco=00B88A,4D89F9,C6D9FD&chbh=25,4,35&chs=600x300&chdl=Anti-XML|scala.xml|javax.xml&chxt=x,y&chxs=1N*f*+MB&chxr=1,0,300,50&chds=0,300,0,300,0,300&chd=t:50.39,250.9|45.33,197.5|37.89,168.1&chxl=0:|spending.xml+(7+MB)|discogs.xml+(30+MB)
   :height: 300px
   :width:  600px

===========     ========        =============       =============
Source Size     Anti-XML        ``scala.xml``       ``javax.xml``
===========     ========        =============       =============
7.1 MB          50.39 MB        45.33 MB            37.89 MB
32 MB           250.9 MB        179.5 MB            168.1 MB
===========     ========        =============       =============


Runtime
-------

spending.xml
~~~~~~~~~~~~

.. image:: https://chart.googleapis.com/chart?cht=bvg&chco=00B88A,4D89F9,C6D9FD&chbh=25,4,35&chs=600x300&chdl=Anti-XML|scala.xml|javax.xml&chxt=x,y&chxs=1N*f*+ms&chxr=1,0,300,50&chds=0,300,0,300,0,300,0,300&chd=t:195,6,5|232,15,265|97,_,16&chxl=0:|Parse|Shallow-Select|Deep-Select
   :height: 300px
   :width:  600px

==============     ========        =============       =============
Action             Anti-XML        ``scala.xml``       ``javax.xml``
==============     ========        =============       =============
Parse              195 ms          232 ms              97 ms
Shallow-Select     6 ms            15 ms               ``-``
Deep-Select        5 ms            265 ms              16 ms
==============     ========        =============       =============

discogs.xml
~~~~~~~~~~~

.. image:: https://chart.googleapis.com/chart?cht=bvg&chco=00B88A,4D89F9,C6D9FD&chbh=25,4,35&chs=600x300&chdl=Anti-XML|scala.xml|javax.xml&chxt=x,y&chxs=1N*f*+ms&chxr=1,0,1500,300&chds=0,1500,0,1500,0,1500,0,1500&chd=t:1119,620,342|1161,84,1220|692,_,50&chxl=0:|Parse|Shallow-Select|Deep-Select
   :height: 300px
   :width:  600px

==============     ========        =============       =============
Action             Anti-XML        ``scala.xml``       ``javax.xml``
==============     ========        =============       =============
Parse              1119 ms         1161 ms             692 ms
Shallow-Select     620 ms          84 ms               ``-``
Deep-Select        342 ms          1220 ms             50 ms
==============     ========        =============       =============


.. _spending.xml: https://github.com/djspiewak/anti-xml/blob/master/src/test/resources/spending.xml


The Task List
=============

The task list for this project was formerly maintained as a message in `Novell Vibe`_.
However, some database issues caused us to repeatedly lose access to that message,
and so I've made a manual dump-and-render into an HTML file in Dropbox.  Eventually,
this will move somewhere else...  http://dl.dropbox.com/u/1679797/anti-xml-todo.html

Note: before you start contributing to the project, you really should read the
CONTRIBUTING.rst_ document.  This outlines some basic guidelines, as well as the
legal mumbo-jumbo required to ensure we all have our copyrights straight.


.. _Novell Vibe: https://vibe.novell.com
.. _CONTRIBUTING.rst: anti-xml/tree/master/CONTRIBUTING.rst
