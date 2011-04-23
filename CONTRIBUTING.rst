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
The copyright holder for all of the code in the Git repository *Daniel Spiewak*.
This is very important to understand: if you contribute code to Anti-XML, you
implicitly agree to transfer ownership of that code to Daniel Spiewak!  In this
context, "contribute code" means opening a pull request on GitHub, informally via
email, or any other method by which code in your purview enters the primary Git
repository for Anti-XML.  If you create a pull request for commits on which someone
else is the author, you are certifying that you have obtained the relevant
permission and are empowered to release the copyright on that code (as well as
your own) to Daniel Spiewak.

To be clear, this implicit contributor agreement is not because I'm a copyright-hungry
control freak who simply must posess the fairest code in the land.  Rather, it is
so that I have the ability to steer the project and control the licensing as
necessary (e.g. applying the BSD License is a decision which requires such ownership).
If anything, this makes it slightly harder on me, since I am now legally
responsible for *everything* in the repository.  So, if some company decides we're
stepping on their patents, I'm the one who is held accountable.

Most large projects (e.g. Apache, Eclipse, Scala) have committer agreements which
must be signed and filed basically stating the majority of the above.  We're not
going to have that for Anti-XML, simply because it's a lot of work and it imposes
a huge barrier to contribution.  Instead, we're going to rely on the implicit
assumption that if you ask me to pull your commits into the Git repository, you
are (at the same time) transfering the copyright on that code to Daniel Spiewak.
For my part, I will not pull a commit into the repository without someone actually
asking me to do so.  This prevents me from "stealing" code out from under you.
It also makes it possible for someone to fork Anti-XML without worrying about me
taking their work without permission.

If, for whatever reason, you decide after the fact that you *don't* want to
assign copyright on your code over to me, let me know within 30 days of the pull
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


.. _feature branch: http://nvie.com/posts/a-successful-git-branching-model/
.. _Scala Style Guide: http://davetron5000.github.com/scala-style/
.. _BSD License: http://www.opensource.org/licenses/bsd-license.php
.. _Daniel Spiewak: mailto:djspiewak@gmail.com
