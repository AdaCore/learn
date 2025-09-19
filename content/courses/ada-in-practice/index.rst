:prev_state: False
:next_state: False

.. _Ada_In_Practice_Course_Index:

Ada In Practice
===============

.. include:: ../../global.txt

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

        Copyright © 2025, AdaCore

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

    This course explores how to use the Ada language in real-world scenarios
    that extend beyond the complexity of lab exercises. The resulting solutions
    to these scenarios are idioms and techniques, some common to various
    programming languages and some specific to Ada. Multiple preliminary
    solutions to a given scenario (when they exist) are presented and analyzed
    for strengths and weaknesses, leading to the final preferred solution and
    its analysis. As a result, readers |mdash| especially those relatively new to
    the language |mdash| can learn how experienced Ada developers apply the
    language in actual practice. Prior knowledge of Ada is required, although
    explanations of the underlying semantics are provided when appropriate.

    This document was written by Patrick Rogers and reviewed by
    Gustavo A. Hoffmann, Steven Baird, Richard Kenner, Robert A. Duff, and Tucker Taft.

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

            <a class="ebook-download-button" href="/pdf_books/courses/ada-in-practice.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/ada-in-practice.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Contents:

    Introduction <chapters/introduction>
    Essential Design Idioms for Packages <chapters/essential_idioms_for_packages>
    Abstract Data Types <chapters/abstract_data_types>
    Abstract Data Machines <chapters/abstract_data_machines>
    Programming by Extension <chapters/programming_by_extension.rst>
    Constructor Functions For Abstract Data Types <chapters/constructor_functions_for_abstract_data_types>
    Controlling Object Initialization and Creation <chapters/controlling_obj_initialization_creation>
    Type Punning <chapters/type_punning>
    Expressing Inheritance Idioms <chapters/inheritance_idioms>
    Providing Component Access to Enclosing Record Objects <chapters/component_access_to_rec_objs>
    Interrupt Handling <chapters/interrupt_handling>
    Reducing Object Code from Generic Package Instantiations <chapters/reducing_object_code_from_generic_package_instantiations>
    Resource Acquisition Is Initialization (RAII) <chapters/raii>
    Using Streams for API Flexibility <chapters/streams_api_flexibility>
    Dealing with Silent Task Termination <chapters/silent_task_termination>
