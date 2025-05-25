:prev_state: False
:next_state: False

Guidelines for Safe and Secure Ada/SPARK
==========================================

.. include:: ../global.txt

.. only:: no_hidden_books

    .. meta::
        :robots: noindex, nofollow

    .. warning::

        This version of the website contains UNPUBLISHED contents.

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2024, AdaCore

        This book is published under a CC BY-SA license, which means that you
        can copy, redistribute, remix, transform, and build upon the content
        for any purpose, even commercially, as long as you give appropriate
        credit, provide a link to the license, and indicate if changes were
        made. If you remix, transform, or build upon the material, you must
        distribute your contributions under the same license as the original.
        You can find license details
        `on this page <http://creativecommons.org/licenses/by-sa/4.0>`_

.. container:: content-description

    This document provides a reasonable set of coding standards to be
    applied to Ada/SPARK source code. The contents can be used as-is,
    or customized for a particular project.

    This document was originally written by Patrick Rogers, and modified by
    Michael Frank.

.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button"
            href="/pdf_books/courses/Guidelines_for_Safe_and_Secure_Ada_SPARK.pdf">
                Download PDF
            </a>
            <br>
            <a class="ebook-download-button"
            href="/epub_books/courses/Guidelines_for_Safe_and_Secure_Ada_SPARK.epub">
                Download EPUB
            </a>


.. toctree::
   :maxdepth: 4

   Introduction <chapters/introduction>
   Definitions <chapters/guidelines/definitions>
   Dynamic Storage Management <chapters/guidelines/dynamic_storage_management>
   Safe Reclamation <chapters/guidelines/safe_reclamation>
   Concurrency <chapters/guidelines/concurrency>
   Robust Programming Practice <chapters/guidelines/robust_programming_practice>
   Exception Usage <chapters/guidelines/exception_usage>
   Object-Oriented Programming <chapters/guidelines/object_oriented_programming>
   Software Engineering <chapters/guidelines/software_engineering>
   References <chapters/references>
