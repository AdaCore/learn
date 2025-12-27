:prev_state: False
:next_state: False

.. _AdaCore_Technologies_Railway_Software_Index:

AdaCore Technologies for Railway Software
=========================================

.. subtitle for outputs other than PDF that has it on front page
.. only:: builder_html or builder_epub

          Supporting certification for CENELEC 50128

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

        Copyright © 2015 |ndash| 2025, AdaCore

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

            <a class="ebook-download-button" href="/pdf_books/booklets/adacore-technologies-for-railway-software.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/booklets/adacore-technologies-for-railway-software.epub">
                Download EPUB
            </a>

.. only:: builder_latex or builder_epub

    .. raw:: latex

        \clearpage

.. rubric:: **About the Authors**

Jean-Louis Boulanger

Since the late 1990s Jean-Louis Boulanger has been an independent
safety assessor for the CERTIFER certification authority in France,
for safety-critical software in railway systems. He is an experienced
safety expert in both the railway industries with the CENELEC standard
and the automotive domain with ISO 26262. He has published a number of
books on the industrial use of formal methods, as well as a book on
safety for hardware architectures and a recent book on the application
of the CENELEC |en-50128| and IEC 62279 standards. He has also served
as a professor and researcher at the University of Technology of
Compiègne.

Quentin Ochem

Quentin Ochem is the Chief Product and Revenue Officer at AdaCore,
where he oversees marketing, sales, and product management while
steering the company's strategic initiatives. He joined AdaCore in
2005 to work on the company's Integrated Development Environments and
cross-language bindings.  With an extensive background in software
engineering in high-integrity domains such as avionics and defense, he
has served leading roles in technical sales, customer training, and
product development. Notably, he has conducted training on the Ada
language, AdaCore tools, and the |do-178b| and |do-178c| software
certification standards. In 2021 he stepped into his current role,
directing the company's strategic initiatives.

.. rubric:: **Foreword**

The guidance in the CENELEC standard |en-50128|:2011 helps achieve
confidence that railway control and protection software meets its
safety requirements. Certifying compliance with this standard is a
challenging task, especially for the testing and verification
activities, but appropriate usage of qualified tools and specialized
run-time libraries can significantly simplify the effort.  This
document explains how a number of technologies offered by AdaCore
|mdash| tools, libraries, and supplemental services |mdash| can help.
The content is based on the authors' many years of practical
experience with the certification of railway software and with the Ada
and SPARK programming languages.

| Jean-Louis Boulanger
| October 2015

| Quentin Ochem, AdaCore
| October 2015

.. rubric:: Foreword to V2.1

In the years since the initial version of this document was published,
the |en-50128|:2011 standard has been amended twice, and AdaCore's products
have evolved to meet the growing demands for, and challenges to, high
assurance in mission-critical real-time software. This revised edition
reflects the current (2025) versions of |en-50128| and AdaCore's offerings.
Among other updates and enhancements to the company's products, the static
analysis tools supplementing the |gnatpro| development environment have been
integrated into a cohesive toolset (the *GNAT Static Analysis Suite*).
The dynamic analysis tools have likewise been consolidated, and the resulting
*GNAT Dynamic Analysis Suite* has introduced a fuzzing tool |mdash| *GNATfuzz*
|mdash| which exercises the software with invalid input and checks for
failsafe behavior.

As editor of this revised edition, I would like to thank Vasiliy Fofanov
(AdaCore) for his detailed and helpful review and suggestions.

For up-to-date information on AdaCore support for developers of rail
software, please visit :footcite:`Railway_SW_AdaCore_Web_Rail`.

| Ben Brosgol, AdaCore
| September 2025

.. toctree::
   :maxdepth: 4
   :numbered:

   Introduction<chapters/introduction>
   CENELEC EN 50128<chapters/cenelec>
   Tools and Technologies Overview<chapters/tools>
   AdaCore Contributions to The Software Quality Assurance Plan<chapters/contribution>
   Technology Usage Guide<chapters/technology>
   Technology Annex<chapters/annex>
