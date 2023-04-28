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
