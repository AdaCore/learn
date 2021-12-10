.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

.. include:: <isopub.txt>

.. only:: builder_latex or builder_epub

    Everything Ada and SPARK
    ========================

.. only:: builder_html

    LEARN.ADACORE.COM
    ===================

    .. raw:: html

        <a href="https://github.com/AdaCore/learn"><i class="fab fa-github"></i> Edit on GitHub</a><br><br>

    What is Ada and SPARK?
    -----------------------

    Ada is a state-of-the art programming language that development teams worldwide
    are using for critical software: from microkernels and small-footprint,
    real-time embedded systems to large-scale enterprise applications, and
    everything in between.

    SPARK is formally analyzable subset of Ada |mdash| and toolset that brings
    mathematics-based confidence to software verification.

    Try Ada Now:
    -------------

    .. code:: ada run_button project=Introduction main=learn.adb

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Learn is

           subtype Alphabet is Character range 'A' .. 'Z';

        begin

           Put_Line ("Learning Ada from " & Alphabet'First & " to " & Alphabet'Last);

        end Learn;

    Check out the interactive courses and labs listed on the left side to learn
    more about Ada and SPARK.

    -------------

.. container:: content-blocks

    .. only:: builder_html

        .. toctree::
            :maxdepth: 4

            About <about>

    .. toctree::
        :maxdepth: 1
        :caption: Courses

        Introduction to Ada <courses/intro-to-ada/index>
        Introduction to SPARK <courses/intro-to-spark/index>
        Ada for the C++ or Java Developer <courses/Ada_For_The_CPP_Java_Developer/index>
        Ada for the Embedded C Developer <courses/Ada_For_The_Embedded_C_Developer/index>
        SPARK Ada for the MISRA C Developer <courses/SPARK_for_the_MISRA_C_Developer/index>
        Introduction to GNAT Toolchain <courses/GNAT_Toolchain_Intro/index>

    .. toctree::
        :maxdepth: 1
        :caption: Labs

        Introduction to Ada: Laboratories <labs/intro-to-ada/index>
        Bug Free Coding <labs/bug-free-coding/index>

.. only:: builder_html

    E-books
    ------------------------------

    You may download the courses and laboratories as e-books for offline reading.
    We offer you the following formats: PDF, EPUB and MOBI (for Kindle devices).

    .. container:: frontpage-ebooks

        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-intro-to-ada.jpeg
                    :alt: Introduction to Ada (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/intro-to-ada.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/intro-to-ada.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/intro-to-ada.mobi">
                                MOBI
                            </a>
                        </div>

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-intro-to-ada-labs.jpeg
                    :alt: Introduction to Ada: Laboratories (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/labs/intro-to-ada.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/labs/intro-to-ada.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/labs/intro-to-ada.mobi">
                                MOBI
                            </a>
                        </div>


        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-intro-to-spark.jpeg
                    :alt: Introduction to SPARK (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/intro-to-spark.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/intro-to-spark.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/intro-to-spark.mobi">
                                MOBI
                            </a>
                        </div>

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-GNAT_Toolchain_Intro.jpeg
                    :alt: Introduction to GNAT Toolchain (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/GNAT_Toolchain_Intro.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/GNAT_Toolchain_Intro.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/GNAT_Toolchain_Intro.mobi">
                                MOBI
                            </a>
                        </div>

        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-Ada_For_The_CPP_Java_Developer.jpeg
                    :alt: Ada for the C++ and Java Developer (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_CPP_Java_Developer.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_CPP_Java_Developer.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/Ada_For_The_CPP_Java_Developer.mobi">
                                MOBI
                            </a>
                        </div>

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-Ada_For_The_Embedded_C_Developer.jpeg
                    :alt: Ada for the Embedded C Developer (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_Embedded_C_Developer.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_Embedded_C_Developer.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/Ada_For_The_Embedded_C_Developer.mobi">
                                MOBI
                            </a>
                        </div>

        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-SPARK_for_the_MISRA_C_Developer.jpeg
                    :alt: SPARK for the MISRA-C Developer (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="/pdf_books/courses/SPARK_for_the_MISRA_C_Developer.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="/epub_books/courses/SPARK_for_the_MISRA_C_Developer.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="/mobi_books/courses/SPARK_for_the_MISRA_C_Developer.mobi">
                                MOBI
                            </a>
                        </div>

    -------------

    Download Ada and SPARK tools
    ------------------------------

    .. container:: download-button

        .. image:: images/GNAT-Community-download.png
            :target: https://www.adacore.com/download
            :alt: GNAT Community Download
            :width: 100pc

    **Try Ada and SPARK now with GNAT Community edition.**

    GNAT Community includes the Ada compiler and toolchain, the SPARK verifier and provers, and the GNAT Studio IDE.

    --------------

    GNAT Academic Program
    ------------------------

    **Teachers and graduate students** who are interested in teaching or using Ada or SPARK can take
    advantage of AdaCore's `GNAT Academic Program (GAP) <http://www.adacore.com/academia>`_.

    .. container:: gap-logo

        .. image:: images/gap_logo.png
            :target: http://www.adacore.com/academia
            :alt: GNAT Academic Program
            :width: 100pc

    GAP's primary objective is to help put Ada and SPARK at the forefront of university study by
    building a community of academic professionals. GAP members receive a comprehensive
    toolset and professional support package specifically designed to provide the tools
    needed to teach and use Ada and SPARK in an academic setting. Best of all, AdaCore
    provides the GAP Package to eligible members at no cost.
    `Register <https://www.adacore.com/academia/gap-registration>`_ for membership
    today and join over 100 member universities in 35 countries currently teaching
    Ada and SPARK using GAP.
