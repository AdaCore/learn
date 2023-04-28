.. intro-to-ada documentation master file, created by
   sphinx-quickstart on Mon Feb 19 11:39:35 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

:prev_state: False
:next_state: False

Introduction to Ada
===================

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

    This course will teach you the basics of the Ada programming language and
    is intended for those who already have a basic understanding of programming
    techniques. You will learn how to apply those techniques to programming in
    Ada.

    This document was written by Raphaël Amiard and Gustavo A. Hoffmann, with
    review from Richard Kenner.

    .. info::

        The code examples in this course use a 50-column limit, which
        greatly improves the readability of the code on devices with a small
        screen size. This constraint, however, leads to an unusual coding
        style. For instance, instead of calling :ada:`Put_Line` in a single
        line, we have this:

        .. code-block:: ada

            Put_Line
              (" is in the northeast quadrant");

        or this:

        .. code-block:: ada

             Put_Line ("  (X => "
                       & Integer'Image (P.X)
                       & ")");

        Note that typical Ada code uses a limit of at least 79 columns.
        Therefore, please don't take the coding style from this course as a
        reference!


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/intro-to-ada.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/intro-to-ada.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    Introduction <chapters/introduction>
    Imperative Language <chapters/imperative_language>
    Subprograms <chapters/subprograms>
    Modular Programming <chapters/modular_programming>
    Strongly Typed Language <chapters/strongly_typed_language>
    Records <chapters/records>
    Arrays <chapters/arrays>
    More About Types <chapters/more_about_types>
    Access Types <chapters/access_types>
    More About Records <chapters/more_about_records>
    Fixed-Point Types <chapters/fixed_point_types>
    Privacy <chapters/privacy>
    Generics <chapters/generics>
    Exceptions <chapters/exceptions>
    Tasking <chapters/tasking>
    Design by contracts <chapters/contracts>
    Interfacing With C <chapters/interfacing_with_c>
    Object Oriented Programming <chapters/object_oriented_programming>
    Standard Library: Containers <chapters/standard_library_containers>
    Standard Library: Dates & Times <chapters/standard_library_dates_times>
    Standard Library: Strings <chapters/standard_library_strings>
    Standard Library: Files & Streams <chapters/standard_library_files_streams>
    Standard Library: Numerics <chapters/standard_library_numerics>
    Appendices <chapters/appendices>
