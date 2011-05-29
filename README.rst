==================================
Anti-XML: The Non-Violent Solution
==================================

**Documentation and usage can be found at http://anti-xml.org**

Anti-XML is a proposed replacement for the ``scala.xml`` package in
the Scala standard library. The standard package is outdated and
beyond fixing. We need to start over, on solid foundations and
unburdened by backward compatibility. Anti-XML aims for quality in
three major areas:

* **Usability**

  * Leverage powerful ideas like combinators and proper ADTs_
  * Leverage the 2.8 collections library
  * Provide consistent selector behavior
  * Avoid these missteps: ``Node <: NodeSeq <: Seq[Node]``,
    ``MetaData``, ``Atom[String]``

* **Reliability**

  * Use immutable data structures (in the API and under the hood too)
  * Avoid bugs and race conditions
  * Provide a proper ``equals``
  
* **Performance**

  * Lower memory usage (`Novell Vibe`_ once had a 16 MB chunk of XML
    use 250 MB of heap!)
  * Provide efficient selectors

We are exploring `many ideas`_ for this project. It will be
interesting to see where things go!

.. _ADTs: http://en.wikipedia.org/wiki/Algebraic_data_type
.. _many ideas: http://dl.dropbox.com/u/1679797/anti-xml-todo.html
.. _Novell Vibe: http://vibe.novell.com


Usage
=====

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


Documentation
=============

Usage information, examples, performance results and more can be found on the
Anti-XML website: http://anti-xml.org


Contributing
============

Contributions are most welcome!  Fork, hack, request pull, rinse and repeat.  If
you're looking for things to work on, I would check the `issue tracker`_ or the
`official TODO list`_.  However, before you get started, be sure to read the
information in CONTRIBUTING.rst_.  It offers some basic code guidelines (don't
worry, curly braces aren't a religious issue here) and explains the legal
mumbo-jumbo involved in contributing.

.. _issue tracker:
.. _official TODO list: http://dl.dropbox.com/u/1679797/anti-xml-todo.html
.. _CONTRIBUTING.rst: anti-xml/tree/master/CONTRIBUTING.rst
