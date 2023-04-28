:prev_state: False
:next_state: False

Introduction To SPARK
=====================

.. include:: ../global.txt

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2018 |ndash| 2022, AdaCore

        This book is published under a CC BY-SA license, which means that you
        can copy, redistribute, remix, transform, and build upon the content
        for any purpose, even commercially, as long as you give appropriate
        credit, provide a link to the license, and indicate if changes were
        made. If you remix, transform, or build upon the material, you must
        distribute your contributions under the same license as the original.
        You can find license details
        `on this page <http://creativecommons.org/licenses/by-sa/4.0>`_

        .. image:: ../../images/ccheart_black.png
            :width: 108pt

.. container:: content-description

    This tutorial is an interactive introduction to the SPARK programming
    language and its formal verification tools. You will learn the difference
    between Ada and SPARK and how to use the various analysis tools that come
    with SPARK.

    This document was prepared by Claire Dross and Yannick Moy.

    .. info::

        The code examples in this course use a 80-column limit, which is a
        typical limit for Ada code. Note that, on devices with a small screen
        size, some code examples might be difficult to read.


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/intro-to-spark.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/intro-to-spark.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    Overview <chapters/01_Overview>
    Flow Analysis <chapters/02_Flow_Analysis>
    Proof of Program Integrity <chapters/03_Proof_Of_Program_Integrity>
    State Abstraction <chapters/04_State_Abstraction>
    Proof of Functional Correctness <chapters/05_Proof_Of_Functional_Correctness>
