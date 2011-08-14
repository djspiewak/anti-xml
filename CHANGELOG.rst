=========
CHANGELOG
=========

0.3
===

* New utility methods supported by Zipper

  * ``drop``
  * ``slice``
  * ``splitAt``
  * ``take``
  
* Deep-select now returns a ``Group`` rather than a ``Zipper`` with an invalid
  context
* Implicit conversions from ``String`` and ``Symbol`` to ``Selector`` are now
  hidden in the ``Selector`` companion object
* Explicit converters now use ``convert`` instead of ``anti``
* **Bug Fixes**

  * `Issue #36`_ – Zipper.unselect does not preserve order of flatMapped children
  * `Issue #19`_ – Attributes Should Be Order-Preserving
  * `Issue #26`_ – Zipper unselect is incorrect after multiple operations
  * ``Zipper#unselect`` fails to rebuild siblings at the second level when some
    results have been dropped
  * ``Zipper#unselect`` on empty selection results no functions appropriately
  * `Issue 12`_ – Utility Operations on Group Return an Invalid Zipper

    * Utility methods on ``Group`` now return ``Group`` when possible, rather
	  than ``Zipper``.  This also changes the ``CanBuildFromWithZipper`` API
	  by splitting it entirely from ``CanBuildFrom``.


.. _Issue #36: https://github.com/djspiewak/anti-xml/issues/36
.. _Issue #19: https://github.com/djspiewak/anti-xml/issues/19
.. _Issue #26: https://github.com/djspiewak/anti-xml/issues/26
.. _Issue #12: https://github.com/djspiewak/anti-xml/issues/12


0.2
===

This release adds support for Scala 2.9.0.  100% compatibility with 2.8.1 is
still maintained.  Note that this *is* a pre-1.0 release, and so a number of
secondary APIs have been adjusted, renamed or simply removed, generally without
documenting here.

* **Features**

  * ``XMLSerializer`` for proper (non-``String``) serialization support
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

  * `Issue #16`_ – Implement Identifier Checking (for reserved characters)
  * `Issue #13`_ – Zipper With Empty Context Does Not Define flatMap
  * ``Zipper#unselect`` with a top-level select miss
  * SAX2 sometimes reuses Attributes objects, confusing ``NodeSeqSAXHandler``
  

.. _Issue #16: https://github.com/djspiewak/anti-xml/issues/16
.. _Issue #13: https://github.com/djspiewak/anti-xml/issues/13
