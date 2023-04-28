:prev_state: False
:next_state: False

Introduction to GNAT Toolchain
==============================

.. include:: ../global.txt

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2019 |ndash| 2022, AdaCore

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

    This course presents an introduction to the GNAT toolchain, which is
    included in the GNAT Community 2020 edition. The course includes first steps
    to get started with the toolchain and some details on the project manager
    (GPRbuild) and the integrated development environment (GNAT Studio).

    This document was written by Gustavo A. Hoffmann, with contributions and
    review from Richard Kenner and Robert Duff.

    .. info::

        The code examples in this course use a 80-column limit, which is a
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

    GNAT Community <chapters/gnat_community>
    GPRbuild <chapters/gprbuild>
    GNAT Studio <chapters/gnat_studio>
    GNAT Tools <chapters/gnat_tools>
