:nextprev_state: False

SPARK Ada for the MISRA C Developer
===================================

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

    This book presents the SPARK technology |mdash| the SPARK subset of Ada and
    its supporting static analysis tools |mdash| through an example-driven
    comparison with the rules in the widely known MISRA C subset of the C
    language.

    This document was prepared by Yannick Moy, with contributions and review
    from Ben Brosgol.

    .. info::

        The code examples in this course use a 80-column limit, which is a
        typical limit for Ada code. Note that, on devices with a small screen
        size, some code examples might be difficult to read.


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/SPARK_for_the_MISRA_C_Developer.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/SPARK_for_the_MISRA_C_Developer.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    Preface <chapters/01_preface>
    Enforcing Basic Program Consistency <chapters/02_program_consistency>
    Enforcing Basic Syntactic Guarantees <chapters/03_syntactic_guarantees>
    Enforcing Strong Typing <chapters/04_strong_typing>
    Initializing Data Before Use <chapters/05_initialization>
    Controlling Side Effects <chapters/06_side_effects>
    Detecting Undefined Behavior <chapters/07_undefined_behavior>
    Detecting Unreachable Code and Dead Code <chapters/08_unreachable_and_dead_code>
    Conclusion <chapters/09_conclusion>
    References <chapters/10_references>
