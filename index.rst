.. meta::
  :author: AdaCore

:nextprev_state: False

.. container:: github-tag

  .. image:: https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png
    :target: https://github.com/AdaCore/learn
    :alt: "Fork me on GitHub"
    :align: right

Learn.adacore.com
===================

**Learn.adacore.com is an interactive learning platform designed to teach the Ada and SPARK programming languages.**
With courses featuring hands-on labs and easy to understand code snippets, you will have the opportunity to see, understand and experiment with the language capabilities.

.. code:: ada

    with Ada.Text_IO;

    procedure Hello is
    begin
       --  Print "Hello, World!" to the screen
       Ada.Text_IO.Put_Line ("Hello, World!");
    end Hello;

.. container:: content-blocks

    .. toctree::
       :maxdepth: 2

       Courses <courses/courses>
       Labs <labs/labs>
       Books <books/books>


About Ada/SPARK
-----------------

The Ada programming language was designed from its inception to be used in applications where safety and security are of the utmost importance. Its feature set and programming paradigms, by design, allow software developers to develop applications more effectively and efficiently. It encourages a “think first, code later” principle which produces more readable, reliable, and maintainable software.

.. container:: download-button

    .. image:: http://blog.adacore.com/uploads/_1800xAUTO_crop_center-center/GNAT-Community-2018-download.png
        :target: https://www.adacore.com/download
        :alt: Gnatcommunity Download
        :width: 100pc

The SPARK programming language is a formally verifiable subset of the Ada language which allows developers to mathematically prove program correctness through static means. This is accomplished by exploiting the strengths of the Ada syntax while eliminating the features of the language that introduce ambiguity and non-deterministic behavior. The language put together with a verification toolset and a design methodology ensures the development and deployment of low-defect software for high reliability applications.

About AdaCore
---------------

Founded in 1994, AdaCore is the leading provider of commercial and open-source software solutions for Ada, a state-of-the-art programming language designed for large, long-lived applications where safety, security, and reliability are critical. AdaCore is committed to being an active member of the software development community providing the GNAT Ada compiler and SPARK formal methods technologies as open-source projects to the world to advocate their use in the future of safe and reliable computing.

.. container:: mwac-banner

    .. image:: http://blog.adacore.com/uploads/_1800xAUTO_crop_center-center/MWAC-banner.png
        :target: https://www.makewithada.org
        :width: 100pc
