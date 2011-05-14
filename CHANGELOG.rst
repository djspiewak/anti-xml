=========
CHANGELOG
=========

0.2
===

This release adds support for Scala 2.9.0.  100% compatibility with 2.8.1 is
still maintained.  Note that this *is* a pre-1.0 release, and so a number of
secondary APIs have been adjusted, renamed or simply removed, generally without
documenting here.

* **Features**

  * ``Group`` canonicalization (see scaladoc for ``Group#canonicalize``) 
  * Proper namespace/prefix support
  
    * Scope retention on ``Elem``
  
  * Namespace support on element attributes
  * Removed ``StAXIterator``
  * ``Selector`` is no longer restricted to a single resulting container type
  * Performance improvements
  
    * StAX parser backend is now the default
    * Lazy bloom filter creation

  * BSD Licensing
  
* **Bug Fixes**

  * `Issue #13`_ – Zipper With Empty Context Does Not Define flatMap
  * ``Zipper#unselect`` with a top-level select miss
  * SAX2 sometimes reuses Attributes objects, confusing ``NodeSeqSAXHandler``
  
  
.. _Issue #13: https://github.com/djspiewak/anti-xml/issues/13
