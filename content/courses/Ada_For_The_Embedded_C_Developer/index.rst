:prev_state: False
:next_state: False

Ada for the Embedded C Developer
===================================

.. include:: ../global.txt

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2020 |ndash| 2022, AdaCore

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

    This course introduces you to the Ada language by comparing it to C. It
    assumes that you have good knowledge of the C language. It also assumes
    that the choice of learning Ada is guided by considerations linked to
    reliability, safety or security. In that sense, it teaches you Ada
    paradigms that should be applied in replacement of those usually applied
    in C.

    This course also introduces you to the SPARK subset of the Ada programming
    language, which removes a few features of the language with undefined
    behavior, so that the code is fit for sound static analysis techniques.

    This course was written by Quentin Ochem, Robert Tice, Gustavo A. Hoffmann,
    and Patrick Rogers and reviewed by Patrick Rogers, Filip Gajowniczek, and
    Tucker Taft.

    .. info::

        The code examples in this course use a 80-column limit, which is a
        typical limit for Ada code. Note that, on devices with a small screen
        size, some code examples might be difficult to read.


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_Embedded_C_Developer.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_Embedded_C_Developer.epub">
                Download EPUB
            </a>

.. toctree::
   :maxdepth: 4

   Introduction <chapters/01_Introduction>
   The C Developer's Perspective <chapters/02_Perspective>
   Concurrency and Real-Time <chapters/03_Concurrency>
   Writing Ada on Embedded Systems <chapters/04_Embedded>
   Enhancing Verification with SPARK and Ada <chapters/05_SPARK>
   C to Ada Translation Patterns <chapters/06_Translation>
   Handling Variability and Re-usability <chapters/07_Reusability>
   Performance Considerations <chapters/08_Performance>
   Argumentation and Business Perspectives <chapters/09_Business>
   Conclusion <chapters/10_Conclusion>
   Hands-On: Object-Oriented Programming <chapters/Appendix_A_OOP>
