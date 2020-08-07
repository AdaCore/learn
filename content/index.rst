.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

LEARN.ADACORE.COM
===================

.. raw:: html

  <a href="https://github.com/AdaCore/learn"><i class="fab fa-github"></i> Edit on GitHub</a><br><br>


**Learn.adacore.com is an interactive learning platform designed to teach the Ada and SPARK programming languages.**

.. code:: ada run_button project=Introduction

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Learn is

       subtype Alphabet is Character range 'A' .. 'Z';

    begin

       Put_Line ("Learning Ada from " & Alphabet'First & " to " & Alphabet'Last);

    end Learn;

.. container:: content-blocks

    .. toctree::
       :maxdepth: 4

       About <about>
       Courses <courses/courses>
       Labs <labs/labs>


-------------

.. container:: download-button

    .. image:: images/GNAT-Community-download.png
        :target: https://www.adacore.com/download
        :alt: Gnatcommunity Download
        :width: 100pc

**Try Ada and SPARK now with GNAT Community edition.**

GNAT Community includes the Ada compiler and toolchain, the SPARK verifier and provers, and the GNAT Studio IDE.

--------------

.. container:: mwac-banner

    .. image:: https://hackster.imgix.net/uploads/attachments/1164282/_UAa0j7WX8u.blob?auto=compress%2Cformat&w=1600&h=400&fit=min
        :target: https://www.hackster.io/contests/adacore3
        :width: 100pc

.. centered:: We’re calling on developers across the globe to build cool embedded applications using the Ada and SPARK programming languages and are offering over $9,000 in total prizes! Click on the banner above to learn more.
