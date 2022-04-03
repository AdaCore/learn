Introduction
============

.. include:: ../../global.txt

This is a collection of short code examples demonstrating new
features of the `Ada 2022 Standard`_ as they are implemented
in GNAT Ada compiler.

To use some of these features, you may need to use a compiler command
line switch or pragma. Compilers starting with `GNAT Community Edition
2021`_ or `GCC 11`_ use :ada:`pragma Ada_2022;` or the ``-gnat2022``
switch.  Older compilers use :ada:`pragma Ada_2020;` or
``-gnat2020``. To use the square brackets syntax or :ada:`'Reduce`
expressions, you need :ada:`pragma Extensions_Allowed (On);` or the
``-gnatX`` switch.

References
----------

* Draft `Ada 2022 Standard`_
* `Ada 202x support in GNAT`_ blog post

.. _`GNAT Community Edition 2021`: https://blog.adacore.com/gnat-community-2021-is-here
.. _`GCC 11`: https://gcc.gnu.org/gcc-11/
.. _`Ada 202x support in GNAT`: https://blog.adacore.com/ada-202x-support-in-gnat
.. _`Ada 2022 Standard`: http://www.ada-auth.org/standards/2xaarm/html/AA-TTL.html
