.. meta::
  :author: AdaCore

:nextprev_state: False

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
       :maxdepth: 2

       About <about>
       Courses <courses/courses>
       Books <books/books>


-------------

.. container:: download-button

    .. image:: images/GNAT-Community-download.png
        :target: https://www.adacore.com/download
        :alt: Gnatcommunity Download
        :width: 100pc

**Try Ada and SPARK now with GNAT Community edition.**

GNAT Community includes the Ada compiler and toolchain, the SPARK verifier and provers, and the GNAT Programming Studio IDE.

--------------

.. container:: mwac-banner

    .. image:: http://blog.adacore.com/uploads/_1800xAUTO_crop_center-center/MWAC-banner.png
        :target: https://www.makewithada.org
        :width: 100pc
