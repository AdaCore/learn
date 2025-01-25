.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

.. include:: <isopub.txt>

.. only:: builder_html

    LEARN.ADACORE.COM
    ===================

    .. only:: no_hidden_books

        .. warning::

            This version of the website contains UNPUBLISHED contents.
            Please do not share it externally!

.. only:: builder_latex or builder_epub

    Learning Ada
    ============

    .. only:: no_hidden_books

        .. warning::

            This version of the book contains UNPUBLISHED contents.
            Please do not share it externally!

.. only:: builder_html

    .. raw:: html

        <a href="https://github.com/AdaCore/learn"><i class="fab fa-github"></i> Edit on GitHub</a><br><br>

    What is Ada and SPARK?
    -----------------------

    Ada is a state-of-the art programming language that development teams worldwide
    are using for critical software: from microkernels and small-footprint,
    real-time embedded systems to large-scale enterprise applications, and
    everything in between.

    SPARK is a formally analyzable subset of Ada |mdash| and a toolset that brings
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
        Advanced Journey With Ada <courses/advanced-ada/index>
        Ada Idioms <courses/ada-idioms/index>
        Introduction to SPARK <courses/intro-to-spark/index>
        Introduction to Embedded Systems Programming <courses/intro-to-embedded-sys-prog/index>
        What's New in Ada 2022 <courses/whats-new-in-ada-2022/index>
        Ada for the C++ or Java Developer <courses/Ada_For_The_CPP_Java_Developer/index>
        Ada for the Embedded C Developer <courses/Ada_For_The_Embedded_C_Developer/index>
        SPARK Ada for the MISRA C Developer <courses/SPARK_for_the_MISRA_C_Developer/index>
        Introduction to the GNAT Toolchain <courses/GNAT_Toolchain_Intro/index>
        Guidelines for Safe and Secure Ada/SPARK <courses/Guidelines_for_Safe_and_Secure_Ada_SPARK/index>

    .. only:: no_hidden_books

        .. toctree::
            :maxdepth: 1
            :caption: Upcoming Courses

            Advanced Journey With Ada (UNPUBLISHED) <courses/advanced-ada/index_hidden>
            Advanced SPARK <courses/advanced-spark/index>

    .. toctree::
        :maxdepth: 1
        :caption: Labs

        Introduction to Ada: Laboratories <labs/intro-to-ada/index>
        Bug Free Coding <labs/bug-free-coding/index>

.. only:: builder_html

    E-books
    ------------------------------

    Download the contents of the entire website as an e-book for offline
    reading. Following formats are available: PDF and EPUB.

    .. container:: frontpage-ebooks

        .. container:: frontpage-ebook-and-buttons-block full-learning-ada-cover

            .. image:: images/page-1-of-learning-ada.jpeg
                :alt: Learning Ada (e-book)

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/learning-ada.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/learning-ada.epub">
                            EPUB
                        </a>

                        <a class="ebook-download-button" href="/zip/learning-ada_code.zip">
                            ZIP (source code)
                        </a>
                    </div>


    Alternatively, download individual courses and laboratories as e-books:

    .. container:: frontpage-ebooks

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-intro-to-ada.jpeg
                :alt: Introduction to Ada (e-book)
                :target: /courses/intro-to-ada/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/intro-to-ada.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/intro-to-ada.epub">
                            EPUB
                        </a>
                    </div>


        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-intro-to-ada-labs.jpeg
                :alt: Introduction to Ada: Laboratories (e-book)
                :target: /labs/intro-to-ada/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/labs/intro-to-ada.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/labs/intro-to-ada.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-advanced-ada-temp.jpeg
                :alt: Advanced Journey With Ada: A Flight In Progress (e-book)
                :target: /courses/advanced-ada/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/advanced-ada.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/advanced-ada.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-intro-to-spark.jpeg
                :alt: Introduction to SPARK (e-book)
                :target: courses/intro-to-spark/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/intro-to-spark.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/intro-to-spark.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-intro-to-embedded-sys-prog.jpeg
                :alt: Introduction to Embedded Systems Programming (e-book)
                :target: /courses/intro-to-embedded-sys-prog/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/intro-to-embedded-sys-prog.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/intro-to-embedded-sys-prog.epub">
                            EPUB
                        </a>
                    </div>


        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-whats-new-in-ada-2022.jpeg
                :alt: What's New in Ada 2022 (e-book)
                :target: /courses/whats-new-in-ada-2022/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/whats-new-in-ada-2022.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/whats-new-in-ada-2022.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-Ada_For_The_CPP_Java_Developer.jpeg
                :alt: Ada for the C++ and Java Developer (e-book)
                :target: /courses/Ada_For_The_CPP_Java_Developer/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_CPP_Java_Developer.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_CPP_Java_Developer.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-Ada_For_The_Embedded_C_Developer.jpeg
                :alt: Ada for the Embedded C Developer (e-book)
                :target: /courses/Ada_For_The_Embedded_C_Developer/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/Ada_For_The_Embedded_C_Developer.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/Ada_For_The_Embedded_C_Developer.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-SPARK_for_the_MISRA_C_Developer.jpeg
                :alt: SPARK for the MISRA-C Developer (e-book)
                :target: /courses/SPARK_for_the_MISRA_C_Developer/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/SPARK_for_the_MISRA_C_Developer.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/SPARK_for_the_MISRA_C_Developer.epub">
                            EPUB
                        </a>
                    </div>


        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-GNAT_Toolchain_Intro.jpeg
                :alt: Introduction to GNAT Toolchain (e-book)
                :target: courses/GNAT_Toolchain_Intro/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/GNAT_Toolchain_Intro.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/GNAT_Toolchain_Intro.epub">
                            EPUB
                        </a>
                    </div>

        .. container:: frontpage-ebook-and-buttons-block

            .. image:: images/page-1-of-Guidelines_for_Safe_and_Secure_Ada_SPARK.jpeg
                :alt: Guidelines for Safe and Secure Ada/SPARK (e-book)
                :target: courses/Guidelines_for_Safe_and_Secure_Ada_SPARK/index.html

            .. raw:: html

                    <div class="frontpage-ebook-download">
                        <a class="ebook-download-button" href="/pdf_books/courses/Guidelines_for_Safe_and_Secure_Ada_SPARK.pdf">
                            PDF
                        </a>

                        <a class="ebook-download-button" href="/epub_books/courses/Guidelines_for_Safe_and_Secure_Ada_SPARK.epub">
                            EPUB
                        </a>
                    </div>

    -------------

.. only:: builder_html and no_hidden_books

    E-books |mdash| Upcoming Courses
    -------------------------------------

    Download e-books of upcoming courses and laboratories:

    .. container:: frontpage-ebooks

         .. container:: frontpage-ebook-and-buttons-block

             .. image:: images/page-1-of-advanced-spark.jpeg
                 :alt: Advanced SPARK (e-book)
                 :target: /courses/advanced-spark/index.html

             .. raw:: html

                     <div class="frontpage-ebook-download">
                         <a class="ebook-download-button" href="/pdf_books/courses/advanced-spark.pdf">
                             PDF
                         </a>

                         <a class="ebook-download-button" href="/epub_books/courses/advanced-spark.epub">
                             EPUB
                         </a>
                     </div>


    -------------

.. only:: builder_html

    Ada Training
    ------------

    **Get professional Ada training** from
    `Adacore <https://www.adacore.com/training>`_.

    .. container:: ada-training-logo

        .. image:: images/ada_training_logo.png
            :target: https://www.adacore.com/training
            :alt: Ada training
            :width: 100pc

    Experience has shown that Ada is an extremely learnable language and that
    programmers with basic knowledge in other languages can quickly get up to
    speed with Ada. For programmers who already have some Ada experience,
    AdaCore offers advanced courses in Ada and GNAT Pro/GNAT Studio designed to
    help developers get the most out of the technology.

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
