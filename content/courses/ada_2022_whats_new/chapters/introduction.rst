Introduction
============

.. include:: ../../global.txt

This is a collection of short code snippets demonstrating new
features of ongoing `Ada 2022 Standard`_ as they are implemented
in GNAT Ada compiler.

Some of these features require a compiler command line switch or
pragma to work. Compilers since `GNAT Community Edition 2021`_ or
`GCC 11`_ uses :ada:`pragma Ada_2022;` or `-gnat2022` swith.
Older compilers use :ada:`pragma Ada_2020;` or `-gnat2020` instead.
Square brackets syntax and :ada:`'Reduce` expression also need
:ada:`pragma Extensions_Allowed (On);` or `-gnatX` swith.

References:
-----------

* Draft `Ada 2022 Standard`_
* `Ada 202x support in GNAT`_ blog post

.. _`GNAT Community Edition 2021`: https://blog.adacore.com/gnat-community-2021-is-here
.. _`GCC 11`: https://gcc.gnu.org/gcc-11/
.. _`Ada 202x support in GNAT`: https://blog.adacore.com/ada-202x-support-in-gnat
.. _`Ada 2022 Standard`: http://www.ada-auth.org/standards/2xaarm/html/AA-TTL.html