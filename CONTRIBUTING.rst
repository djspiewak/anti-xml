============
Contributing
============

Any and everyone is very welcome to contribute to Anti-XML!  This is part of the
reason we're using GitHub to host the source.  If you see something that you feel
needs to be done, or you just want to get your hands dirty with a really neat
Scala project, go ahead and create your own fork, commit and push.  Once your
happy with your changes, submit a pull request!  Just as a curtesy, please try
to keep your changes organized (preferably in a `feature branch`_) to make it
easier for me to review and merge them.  While I *can* handle pull requests for
dozens of unrelated commits all mixed together in the same branch, I really prefer
not to.  So, just keep to common Git best-practices and you should be fine!


Guidelines
==========

Note that these are hints and suggestions, not necessarily rules!  In other words,
I won't reject your pull request simply for having a colon out of place.

* **Observe "Good" Scala Style** – This is an intensely subjective thing, so I'm
  not going to say too much about it.  I try to follow the `Scala Style Guide`_,
  and I suggest you do the same, but I won't go crazy if you don't.
* **Aim for Purely-Functional** – Make every attempt to write your code in a
  purely-functional style.  Code written in this style is much easier to review
  (because its effect is self-contained) and vastly easier to maintain in the
  long run.  There are certainly times when the imperative style is appropriate
  (e.g. for performance reasons), but those times are far less frequent than you
  might think.  Default to purely-functional, and only convert to imperative if
  you *absolutely* must.  Note that even if your implementations are imperative,
  the exposed API *must* be purely-functional.  All side-effects must be resolved
  within the scope of whatever function you're writing.
* **Do Something Cool** – I think this should go without saying.


Legal Nastiness
===============

Alright, here comes a mountain of boring; it's important though, so **please read!**
As you should have already seen, Anti-XML is licensed under the `BSD License`_.
This is very important to understand: if you contribute code to Anti-XML, you
are agreeing to assign a non-exclusive, worldwide, royalty-free copyright license
to the Anti-XML project, collectively represented by *Daniel Spiewak*.  In
plain-English, this means that if you contribute, you're letting Anti-XML incorporate,
control and extend the contributions you submit.  There is a more precise, legal
definition of this assignment below (drawn from the `Common Public License`_).

All of this is pretty standard for an open-source project.  Basically, there is
an understanding that if you contribute, you're giving away that source code
under the terms of the project license.  The only reason I have to spell it out
here is to prevent law suits (and also to ensure that the code provenance is clear
for any corporation which wishes to make use of the library).  There is nothing
required or implied here that is particularly out of the ordinary.

The important point here is that you implicitly agree to these terms when you
submit a pull request to the project.  In other words, if you *ask* me to include
your contributions into the main Git repository, you are assumed to have agreed
to these terms.  This is analogous to the "shrink wrap" agreement used by certain
hardware OEMs.  If I could make GitHub show this agreement when you submit the
pull request, I would.

If, for whatever reason, you decide after the fact that you *don't* agree to
these terms on your contribution, let me know within 30 days of the pull
request and I'll remove your code from the repository, no questions asked.  If
you discover the mistake after the 30 day window, I'll still try to accomodate
you, but I can't make any guarantees given the fact that additional code may have
been layered on top of your code by that point.

If you have any questions about this implicit contributor agreement, please do
not hesitate to email me.  I'm not a lawyer, but I'll try to answer your questions
as best I can.  My hope is that this scheme represents the most painless way to
accomplish all this legal mumbo-jumbo, allowing everyone to focus on more
interesting things, like the actual code.

`Daniel Spiewak`_


Legalese
========

(shamelessly stolen from the text of the `Common Public License`_)


Definitions
-----------

"Contribution" means:

* in the case of the initial Contributor, the initial code and
  documentation distributed under this Agreement, and
* in the case of each subsequent Contributor:

  i) changes to the Program, and
  ii) additions to the Program;

where such changes and/or additions to the Program originate
from and are distributed by that particular Contributor. A
Contribution 'originates' from a Contributor if it was added to the
Program by such Contributor itself or anyone acting on such
Contributor's behalf. Contributions do not include additions to the
Program which: (i) are separate modules of software distributed in
conjunction with the Program under their own license agreement,
and (ii) are not derivative works of the Program.

"Contributor" means any person or entity that distributes the Program.

"Licensed Patents " mean patent claims licensable by a Contributor
which are necessarily infringed by the use or sale of its Contribution
alone or when combined with the Program.

"Program" means the Contributions distributed in accordance with this Agreement.

"Recipient" means anyone who receives the Program under this
Agreement, including all Contributors.


Agreement
---------

* Subject to the terms of this Agreement, each Contributor
  hereby grants Recipient a non-exclusive, worldwide, royalty-free
  copyright license to reproduce, prepare derivative works of, publicly
  display, publicly perform, distribute and sublicense the Contribution
  of such Contributor, if any, and such derivative works, in source code
  and object code form.
* Subject to the terms of this Agreement, each Contributor
  hereby grants Recipient a non-exclusive, worldwide, royalty-free
  patent license under Licensed Patents to make, use, sell, offer to
  sell, import and otherwise transfer the Contribution of such
  Contributor, if any, in source code and object code form. This patent
  license shall apply to the combination of the Contribution and the
  Program if, at the time the Contribution is added by the Contributor,
  such addition of the Contribution causes such combination to be
  covered by the Licensed Patents. The patent license shall not apply to
  any other combinations which include the Contribution. No hardware per
  se is licensed hereunder.
* Recipient understands that although each Contributor grants
  the licenses to its Contributions set forth herein, no assurances are
  provided by any Contributor that the Program does not infringe the
  patent or other intellectual property rights of any other entity. Each
  Contributor disclaims any liability to Recipient for claims brought by
  any other entity based on infringement of intellectual property rights
  or otherwise. As a condition to exercising the rights and licenses
  granted hereunder, each Recipient hereby assumes sole responsibility
  to secure any other intellectual property rights needed, if any. For
  example, if a third party patent license is required to allow
  Recipient to distribute the Program, it is Recipient's responsibility
  to acquire that license before distributing the Program.
* Each Contributor represents that to its knowledge it has
  sufficient copyright rights in its Contribution, if any, to grant the
  copyright license set forth in this Agreement.


.. _feature branch: http://nvie.com/posts/a-successful-git-branching-model/
.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _BSD License: http://www.opensource.org/licenses/bsd-license.php
.. _Daniel Spiewak: mailto:djspiewak@gmail.com
.. _Common Public License: http://www.opensource.org/licenses/cpl1.0
