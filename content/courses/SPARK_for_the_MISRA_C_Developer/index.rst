:nextprev_state: False

SPARK Ada for the MISRA C Developer
===================================

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

        Copyright © 2018 |ndash| 2024, AdaCore

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

    .. note::

        The code examples in this course use an 80-column limit, which is a
        typical limit for Ada code. Note that, on devices with a small screen
        size, some code examples might be difficult to read.

    .. only:: builder_latex or builder_epub

        .. note::

            Each code example from this book has an associated "code block
            metadata", which contains the name of the "project" and an MD5 hash
            value. This information is used to identify a single code example.

            You can find all code examples in a zip file, which you can
            `download from the learn website <https://learn.adacore.com/zip/learning-ada_code.zip>`_.
            The directory structure in the zip file is based on the code block
            metadata. For example, if you're searching for a code example with
            this metadata:

            - Project: Courses.Intro_To_Ada.Imperative_Language.Greet

            - MD5: cba89a34b87c9dfa71533d982d05e6ab

            you will find it in this directory:

            :file:`projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/`

            In order to use this code example, just follow these steps:

            1. Unpack the zip file;
            2. Go to target directory;
            3. Start GNAT Studio on this directory;
            4. Build (or compile) the project;
            5. Run the application (if a main procedure is available in the
               project).


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
