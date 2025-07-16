.. include:: ./common_defs.rst

:prev_state: False
:next_state: False

.. _AdaCore_Technologies_Airborne_Software_Index:

AdaCore Technologies for Airborne Software
==========================================

.. subtitle for outputs other than PDF that has it on front page
.. only:: builder_html or builder_epub

	  Supporting certification and tool qualification for DO-178C:ED-12C

.. include:: ../../courses/global.txt

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

        Copyright © 2017 |ndash| 2025, AdaCore

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

.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/booklets/adacore-technologies-for-airborn-software.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/booklets/adacore-technologies-for-airborn-software.epub">
                Download EPUB
            </a>

.. only:: builder_latex or builder_epub

    .. raw:: latex

        \clearpage

.. rubric:: **About the Authors**

Frédéric Pothon

During his professional career dating back to the 1980s,
Frédéric Pothon has been a recognized expert in the area of
software aspects of certification (most notably |do-178|,
Levels A, B, and C). He was a member of the EUROCAE/RTCA
group that produced |do-248b|, which provides supporting
information for the |do-178b| standard. Mr. Pothon has
led projects at Turboméca (now Safran Helicopter Engines)
and Airbus, where he was responsible for software methodologies
and quality engineering processes. He founded the company
ACG-Solutions in 2007 and worked as an independent consulting
engineer, providing training, audits, and support, and he was
involved in several research projects. Mr. Pothon
is an expert in the qualification and utilization of automatic
code generation tools for model-based development, and he
served as co-chair of the Tool Qualification subgroup during
the |do-178c| project.

Quentin Ochem

Quentin Ochem is the Chief Product and Revenue Officer at AdaCore,
where he oversees marketing, sales, and product management while
steering the company's strategic initiatives. He joined
AdaCore in 2005 to work on the company's Integrated Development
Environments and cross-language bindings.
With an extensive background in software engineering in high-integrity
domains such as avionics and defense, he has served leading roles in technical
sales, customer training, and product development. Notably, he has
conducted training on the Ada language, AdaCore tools, and the
|do-178b| and |do-178c| software certification standards. In 2021
he stepped into his current role, directing the company's strategic
initiatives.

.. rubric:: **Foreword**

The guidance in the |do-178c| standard and its associated
technology-specific supplements helps achieve confidence that airborne
software meets its requirements. Certifying that a system complies with
this guidance is a challenging task, especially for the verification
activities, but appropriate usage of qualified tools and specialized run-time
libraries can significantly simplify the effort. This document explains
how a number of technologies offered by AdaCore --- tools, libraries, and
supplemental services --- can help. It covers not only the "core" |do-178c|
standard but also the technology supplements: Object-Oriented
Technology and Related Techniques |do-332|, and Formal
Methods (|do-333|). The content is based on the authors' many
years of practical experience with the certification of airborne software,
with the Ada and SPARK programming languages, and with the
technologies addressed by the |do-178c| supplements.

We gratefully acknowledge the assistance of Ben Brosgol
(AdaCore) for his review of and contributions to the material presented in
this document.

| Frédéric Pothon, ACG Solutions
| Montpellier, France
| March 2017

| Quentin Ochem, AdaCore
| New York, NY
| March 2017

.. rubric:: Foreword to V2.1

This revised booklet reflects the evolution of and enhancements to
AdaCore's products since the earlier edition.
Among other updates, the static analysis tools supplementing the
GNAT Pro development environment have been integrated
into a cohesive toolset (the *GNAT Static Analysis Suite*).
The dynamic analysis tools have likewise been consolidated, and
the resulting *GNAT Dynamic Analysis Suite* has introduced
a fuzzing tool --- *GNATfuzz* --- which exercises the software
with invalid input and checks for failsafe behavior.

I would like to express my appreciation to Olivier Appere (AdaCore) for
his detailed and helpful review of the content for the revised booklet.

| Ben Brosgol, AdaCore
| Bedford, Massachusetts
| July 2025

	    
.. toctree::
   :maxdepth: 4
   :numbered:    

   Introduction<introduction>
   The DO-178C/ED-12C Standards Suite<standards>
   AdaCore Tools and Technologies Overview<tools>
   Compliance with DO-178C / ED-12C Guidance: Analysis<analysis>
   Summary of contributions to DO-178C/ED-12C objectives<summary>
   
.. toctree::
   :maxdepth: 2
           
   References<references>
