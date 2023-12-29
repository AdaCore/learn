:prev_state: False
:next_state: False

Introduction to the GNAT Toolchain
==================================

.. include:: ../global.txt

.. only:: no_hidden_books

    .. warning::

        This version of the website contains UNPUBLISHED contents.
        Please do not share it externally!

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2019 |ndash| 2023, AdaCore

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

    This course presents an introduction to the GNAT toolchain. The course
    includes first steps to get started with the toolchain and some details on
    the project manager (GPRbuild) and the integrated development environment
    (GNAT Studio).

    This document was written by Gustavo A. Hoffmann, with contributions and
    review from Richard Kenner and Robert Duff.

    .. note::

        The code examples in this course use an 80-column limit, which is a
        typical limit for Ada code. Note that, on devices with a small screen
        size, some code examples might be difficult to read.


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/GNAT_Toolchain_Intro.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/GNAT_Toolchain_Intro.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    GNAT Toolchain Basics <chapters/gnat_toolchain_basics>
    GPRbuild <chapters/gprbuild>
    GNAT Studio <chapters/gnat_studio>
    GNAT Tools <chapters/gnat_tools>
