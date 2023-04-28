.. advanced-ada documentation master file.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

:prev_state: False
:next_state: False

Advanced Journey With Ada: A Flight In Progress
===============================================

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

    .. warning::

        **This is work in progress!**

        Information in this document is subject to change at any time without
        prior notification.  The editors/authors exclude all liability for the
        topicality, correctness, completeness or quality of the information
        provided in this course.

    .. note::

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

    .. only:: builder_latex or builder_epub

        .. note::

            Each code example from this book has an associated "code block
            metadata", which contains the name of the "project" and a MD5 hash
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


    This course will teach you advanced topics of the Ada programming language.
    The
    :doc:`Introduction to Ada </courses/intro-to-ada/index>`
    course is a prerequisite for this course.

    .. only:: not no_hidden_books

        This document was written by Gustavo A. Hoffmann and Robert A. Duff,
        with contributions from Franco Gasperoni, Gary Dismukes and
        Robert Dewar.

    .. only:: no_hidden_books

        This document was written by Gustavo A. Hoffmann and Robert A. Duff,
        with contributions from Arnaud Charlet, Emmanuel Briot,
        Franco Gasperoni, Gary Dismukes, Javier Miranda, Patrick Rogers,
        Quentin Ochem, Robert Dewar, and Yannick Moy.

    This document was reviewed by Patrick Rogers and Tucker Taft.

.. container:: content-changelog

    .. admonition:: CHANGELOG

        *Release 2023-04*

        - First draft release including following chapters:

            - Data Types
            - Control Flow
            - Modular Programming


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/advanced-ada.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/advanced-ada.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    chapters/data_types
    chapters/control_flow
    chapters/modular_prog

.. only:: no_hidden_books

    .. toctree::
        :maxdepth: 4
        :caption: Contents:

        chapters/resource_management
        chapters/abstraction_oriented_prog
        chapters/design_by_contracts
        chapters/initialization
        chapters/multithreading
        chapters/interfacing_external
        chapters/appendices
