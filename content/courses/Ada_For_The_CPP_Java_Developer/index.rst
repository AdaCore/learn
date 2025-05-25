:prev_state: False
:next_state: False

Ada for the C++ or Java Developer
===================================

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

        Copyright © 2013 |ndash| 2023, AdaCore

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

    This document will present the Ada language using terminology and examples
    that are familiar to developers that understand the C++ or Java languages.

    This document was prepared by Quentin Ochem, with contributions and review
    from Richard Kenner, Albert Lee, and Ben Brosgol.

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

            <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_CPP_Java_Developer.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_CPP_Java_Developer.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    Preface <chapters/01_Preface>
    Basics <chapters/02_Basics>
    Compilation Unit Structure <chapters/03_Compilation_Unit_Structure>
    Statements, Declarations, and Control Structures <chapters/04_Statements_Declarations_and_Control_Structures>
    Type System <chapters/05_Type_System>
    Functions and Procedures <chapters/06_Functions_and_Procedures>
    Packages <chapters/07_Packages>
    Classes and Object Oriented Programming <chapters/08_Classes_and_Object_Oriented_Programming>
    Generics <chapters/09_Generics>
    Exceptions <chapters/10_Exceptions>
    Concurrency <chapters/11_Concurrency>
    Low Level Programming <chapters/12_Low_Level_Programming>
    Conclusion <chapters/13_Conclusion>
    References <chapters/14_References>
