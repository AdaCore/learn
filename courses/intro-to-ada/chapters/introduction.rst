:code-config:`run_button=True;prove_button=False;accumulate_code=True`

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Introduction
============

History
-------

In the seventies, the American department of Defense suffered from an
explosion of the number of languages, each team using a different set of
idioms/scripts. The DOD decided to solve this by issuing a request for
proposal to big companies of the time. The proposal that was selected by
the DOD in the end was Jean Ichbiah's proposal, on behalf of HoneyWell
bull.

The first standard for the language was issued in 1983, with revisions
being made in 1995, 2005 and 2012, each time adding major features to
the language.

This tutorial will focus on Ada 2012 as a whole, rather than teach
different versions of the language.

Ada today
---------

Today, Ada is mainly used in real-time/safety critical systems, with a
general focus on embedded systems. While Ada is and can be used as a
general purpose language, it will really shine in low level
applications:

-  Embedded systems with low memory requirements (no garbage collector
   allowed).
-  Direct interfacing with hardware.
-  Soft or hard real-time systems.
-  Low level systems programming.

This list is intentionally abstract. Today,  Ada has certain domains /
niches where it is used a lot, like Aerospace & Defense, civil aviation,
public transportation, etc. These are domains that require a high degree
of safety: a software defect in this kind of applications are not just an
annoyance, but may have severe consequences. Ada provides safety features
that allow for detecting defects at an early stage --- usually, at
compilation time or using static analysis tools. In addition, Ada can also
be used in to create applications in varied categories, such as:

-  `Video game programming <https://github.com/AdaDoom3/AdaDoom3>`_
-  `Real-time audio <http://www.electronicdesign.com/embedded-revolution/assessing-ada-language-audio-applications>`_
-  `Kernel modules <http://www.nihamkin.com/tag/kernel.html>`_

This is a non-comprehensive list that hopefully sheds light on which
kind of programming Ada is good at.

In terms of modern languages, the closest in terms of targets and level
of abstraction are probably
`C++ <https://en.wikipedia.org/wiki/C%2B%2B>`_ and
`Rust <https://www.rust-lang.org/en-US/>`_.

Philosophy
----------

A word should be said about Ada's philosophy, because it is very
different from the one of a lot of other languages. In Ada it is
considered that:

-  Readability is more important than conciseness. Syntactically this
   shows through the fact that keywords are preferred to symbols, that no
   keyword is an abbreviation, etc.

-  Very strong typing. It is very easy to introduce new types in Ada,
   sometimes more than reusing existing ones.

    It is in the same ball-park as many functional languages in that regard,
    except that the programmer has to be much more explicit about typing, because
    there is almost no type inference.

.. AI for amiard: Develop a little bit. Put in the proper format when we have decided about it (issue #4)

-  Explicit is better than implicit: Although weirdly this is a
   `Python <www.TODOpython.com>`_ commandment, Ada takes it way further
   than any language we know of:

   -  There is mostly no structural typing, and most types need to be
      explicitly named by the programmer.

   -  As previously said, mostly no type inference.

   -  Semantics are very well defined, and undefined behavior is limited
      to an absolute minimum.

   -  The programmer can generally give a *lot* of information about
      what his program means to the compiler (and other programmers).
      This allows the compiler to be extremely helpful (read: strict)
      with the programmer.

During this course, we will explain the individual language features that
are building blocks for that philosophy.

SPARK
-----

While this class is solely about the Ada language, it is worth mentionning that
another language, extremely close, and completely interoperable with Ada,
exists: The SPARK language.

SPARK is a subset of Ada, that is designed so that the code written in SPARK is
amenable to automatic proof, which provides a level of insurance with regards
to the correctness of your code that is much higher than with a regular
programming language.

There is a
`dedicated class for the SPARK language <https://TODOLINKTOSPARKCOURSE>`_,
but keep in mind that everytime we speak
about the specification power of Ada during this course, it is power that you
can leverage in SPARK to help proving your programs correct.
