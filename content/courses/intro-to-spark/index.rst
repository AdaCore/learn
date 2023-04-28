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

    .. only:: builder_latex or builder_epub

        .. info::

            Each code example from this book has an associated "code block
            metadata", which contains the name of the "project" and a MD5 hash
            value. This information is used to identify a single code example.

            You can find all code examples in a zip file, which you can
            `download from the learn website <https://learn.adacore.com/zip/learning-ada_code.zip>`_.
            The directory structure in the zip file is based on the code block
            metadata. For example, if you're searching for a code example with
            this metadata:

            Project: Courses.Intro_To_Ada.Imperative_Language.Greet
            MD5: cba89a34b87c9dfa71533d982d05e6ab

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
