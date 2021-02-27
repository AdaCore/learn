.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

.. include:: <isopub.txt>

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

Check out the interactive
:doc:`courses <courses/courses>`
and
:doc:`labs <labs/labs>`
to learn more about Ada and SPARK.

-------------

.. container:: content-blocks

    .. toctree::
       :maxdepth: 4

       About <about>
       Courses <courses/courses>
       Labs <labs/labs>

Books
------------------------------

Download the courses as PDF books for offline reading:

.. container:: pdf-row-1

    .. image:: images/page-1-of-intro-to-ada.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/intro-to-ada.pdf
        :alt: Introduction to Ada (PDF)
        :width: 149pt

    .. image:: images/page-1-of-intro-to-spark.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/intro-to-spark.pdf
        :alt: Introduction to SPARK (PDF)
        :width: 149pt

    .. image:: images/page-1-of-GNAT_Toolchain_Intro.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/GNAT_Toolchain_Intro.pdf
        :alt: Introduction to GNAT Toolchain (PDF)
        :width: 149pt

.. container:: pdf-row-2

    .. image:: images/page-1-of-Ada_For_The_CPP_Java_Developer.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/Ada_For_The_CPP_Java_Developer.pdf
        :alt: Ada_For_The_CPP_Java_Developer.pdf
        :width: 149pt

    .. image:: images/page-1-of-Ada_For_The_Embedded_C_Developer.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/Ada_For_The_Embedded_C_Developer.pdf
        :alt: Ada_For_The_Embedded_C_Developer.pdf
        :width: 149pt

    .. image:: images/page-1-of-SPARK_for_the_MISRA_C_Developer.jpeg
        :target: https://learn.adacore.com/pdf_books/courses/SPARK_for_the_MISRA_C_Developer.pdf
        :alt: SPARK_for_the_MISRA_C_Developer.pdf
        :width: 149pt

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

--------------

.. container:: mwac-banner

    .. image:: https://hackster.imgix.net/uploads/attachments/1164282/_UAa0j7WX8u.blob?auto=compress%2Cformat&w=1600&h=400&fit=min
        :target: https://www.hackster.io/contests/adacore3
        :width: 100pc

We’re calling on **developers across the globe** to build cool embedded applications using the Ada and SPARK programming languages and are offering **over $9,000 in total prizes!** Click on the banner above to learn more.
