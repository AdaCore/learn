.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

LEARN.ADACORE.COM
===================

.. raw:: html

  <a href="https://github.com/AdaCore/learn"><i class="fab fa-github"></i> Edit on GitHub</a><br><br>


**Learn.adacore.com is an interactive learning platform designed to teach the Ada and SPARK programming languages.**

.. code:: ada run_button

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

    .. toctree::
       :hidden:

       Tests <engine/tests/index>


-------------

.. container:: download-button

    .. image:: images/GNAT-Community-download.png
        :target: https://www.adacore.com/download
        :alt: Gnatcommunity Download
        :width: 100pc

**Try Ada and SPARK now with GNAT Community edition.**

GNAT Community includes the Ada compiler and toolchain, the SPARK verifier and provers, and the GNAT Programming Studio IDE.
