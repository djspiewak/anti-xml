=========
CHANGELOG
=========

0.2
===

* Features

  * ``Selector`` is no longer restricted to a single resulting container type
  * Performance improvements
  
    * StAX parser backend is now the default
    * Lazy bloom filter creation

  * BSD Licensing
  
* Bug Fixes

  * Zipper#unselect with a top-level select miss
  * SAX2 sometimes reuses Attributes objects, confusing NodeSeqSAXHandler
