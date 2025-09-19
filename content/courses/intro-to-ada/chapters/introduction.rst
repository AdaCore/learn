:prev_state: False

.. include:: ../../../global.txt

Introduction
============

History
-------

In the 1970s the United States Department of Defense (DOD) suffered from an
explosion of the number of programming languages, with different projects using
different and non-standard dialects or language subsets / supersets. The DOD
decided to solve this problem by issuing a request for proposals for a common,
modern programming language. The winning proposal was one submitted by Jean
Ichbiah from CII Honeywell-Bull.

The first Ada standard was issued in 1983; it was subsequently revised and
enhanced in 1995, 2005 and 2012, with each revision bringing useful new
features.

This tutorial will focus on Ada 2012 as a whole, rather than teaching different
versions of the language.

Ada today
---------

Today, Ada is heavily used in embedded real-time systems, many of which are
safety critical. While Ada is and can be used as a general-purpose language, it
will really shine in low-level applications:

- Embedded systems with low memory requirements (no garbage collector allowed).
- Direct interfacing with hardware.
- Soft or hard real-time systems.
- Low-level systems programming.

Specific domains seeing Ada usage include Aerospace & Defense, civil aviation,
rail, and many others. These applications require a high degree of safety: a
software defect is not just an annoyance, but may have severe consequences. Ada
provides safety features that detect defects at an early stage |mdash| usually at
compilation time or using static analysis tools. Ada can also be used to create
applications in a variety of other areas, such as:

-  `Video game programming <https://github.com/AdaDoom3/AdaDoom3>`_
-  `Real-time audio <http://www.electronicdesign.com/embedded-revolution/assessing-ada-language-audio-applications>`_
-  `Kernel modules <http://www.nihamkin.com/tag/kernel.html>`_

This is a non-comprehensive list that hopefully sheds light on which
kind of programming Ada is good at.

In terms of modern languages, the closest in terms of targets and level
of abstraction are probably
:wikipedia:`C++ <C%2B%2B>` and
`Rust <https://www.rust-lang.org/en-US/>`_.

Philosophy
----------

Ada's philosophy is different from most other languages. Underlying Ada's
design are principles that include the following:

-  Readability is more important than conciseness. Syntactically this
   shows through the fact that keywords are preferred to symbols, that no
   keyword is an abbreviation, etc.

-  Very strong typing. It is very easy to introduce new types in Ada, with the
   benefit of preventing data usage errors.

   - It is similar to many functional languages in that regard, except that the
     programmer has to be much more explicit about typing in Ada, because there
     is almost no type inference.

-  Explicit is better than implicit. Although this is a
   `Python <https://www.python.org>`_ commandment, Ada takes it way further
   than any language we know of:

   -  There is mostly no structural typing, and most types need to be
      explicitly named by the programmer.

   -  As previously said, there is mostly no type inference.

   -  Semantics are very well defined, and undefined behavior is limited
      to an absolute minimum.

   -  The programmer can generally give a *lot* of information about
      what their program means to the compiler (and other programmers).
      This allows the compiler to be extremely helpful (read: strict)
      with the programmer.

.. AI for amiard: Create admonition with more details about comparison to
   functional languages (referred in paragraph about strong typing).

During this course, we will explain the individual language features that
are building blocks for that philosophy.

SPARK
-----

While this class is solely about the Ada language, it is worth mentioning that
another language, extremely close to and interoperable with Ada, exists: the
SPARK language.

SPARK is a subset of Ada, designed so that the code written in SPARK is
amenable to automatic proof. This provides a level of assurance with regard to
the correctness of your code that is much higher than with a regular
programming language.

There is a dedicated
:doc:`course for the SPARK language </courses/intro-to-spark/index>`
but keep in mind that every time we speak about the specification power of Ada
during this course, it is power that you can leverage in SPARK to help proving
the correctness of program properties ranging from absence of run-time errors
to compliance with formally specified functional requirements.
