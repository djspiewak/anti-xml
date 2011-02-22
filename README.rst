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


.. _below:

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
