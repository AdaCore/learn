:prev_state: False
:next_state: False

Creating C Bindings For Ada
=============================

.. include:: ../global.txt

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2023, AdaCore

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

    This course introduce some guidelines to be aware of when connecting
    a C library to Ada. This connection is done by creating Ada constructs
    that map directly to C constructs, as well as the Ada compiler/linker
    information necessary to make the connection.

    Once this interface ("thin binding") has been created, this document
    will help you understand how to create "thick bindings" - Ada-aware
    wrappers to the thin bindings that allow you to use the full power
    of the Ada language to more safely interact with the C library.

    This course was written by Michael Frank and Olivier Henley,
    with a major assist from REDACTED from REDACTED Corporation.

.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/Creating_C_Bindings_For_Ada.pdf">
                Download PDF
            </a>
            <br>
            <a class="ebook-download-button" href="/epub_books/courses/Creating_C_Bindings_For_Ada.epub">
                Download EPUB
            </a>

.. toctree::
   :maxdepth: 4

   Introduction <chapters/01_Introduction>
   Thin Bindings <chapters/02_Thin_Bindings>
   General Principles <chapters/03_General_Principles>
   Types <chapters/04_Types>
   Subprograms <chapters/05_Subprograms>
   Macros <chapters/06_Macros>
   Function Pointers <chapters/07_Function_Pointers>
   Special Cases <chapters/08_Special_Cases>
