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

**TODO**


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
