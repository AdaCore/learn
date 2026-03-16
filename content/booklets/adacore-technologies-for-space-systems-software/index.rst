:prev_state: False
:next_state: False

.. _AdaCore_Technologies_Space_Systems_Software_Index:

AdaCore Technologies for Space Systems Software
===============================================

.. subtitle for outputs other than PDF that has it on front page
.. only:: builder_html or builder_epub

	  Supporting Qualification for ECSS-E-ST-40C and ECSS-Q-ST-80C

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

        Copyright © 2015 |ndash| 2026, AdaCore

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

            <a class="ebook-download-button" href="/pdf_books/booklets/adacore-technologies-for-space-systems-software.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/booklets/adacore-technologies-for-space-systems-software.epub">
                Download EPUB
            </a>

.. only:: builder_latex or builder_epub

    .. raw:: latex

        \clearpage

.. rubric:: **About the Authors**

Benjamin M. Brosgol

Dr. Brosgol is a senior member of the technical staff at AdaCore. He has been
involved with programming language design and implementation throughout his
career, concentrating on languages and technologies for high-assurance
systems. He was a Distinguished Reviewer during the original Ada development,
a member of the language design team in the |ada-95| revision, and a member of
the Expert Group for the Real-Time Specification for Java under the Java
Community Process. He has published dozens of journal articles and delivered
conference presentations on topics including the avionics software standard
|do-178c|, the Ada and SPARK languages, real-time software technologies,
object-oriented methodologies, and the FACE\ |reg| (Future Airborne
Capabilities Environment) approach to software portability.

Jean-Paul Blanquart

Dr. Blanquart is a recognized authority on computer-based systems safety and
dependability, with a decades-long career that spans academic research
(LAAS-CNRS, Toulouse, France) and the space industry (Airbus Defence and
Space). He was a member of the ECSS Working Groups in charge of revision 1
of ECSS-Q-ST-80C and ECSS-Q-HB-80-03A (and also the dependability and safety
standards ECSS-Q-ST-30C and ECSS-Q-ST-40C). He has been an active member of a
French cross-domain Working Group on safety and safety standards since its
creation in 2010. This Working Group gathers industrial safety experts and
related tool providers from domains that include automotive, aviation,
defense, nuclear, industrial processes, railway and space.

.. rubric:: **Foreword**

| **"Failure is not an option"**
| Gene Kranz (NASA) in the film *Apollo 13*

Software development presents daunting challenges when the resulting system
needs to operate reliably, safely, and securely while meeting hard real-time
deadlines on a memory-limited target platform. Correct program execution can
literally be a matter of life and death, but such is the reality facing
developers of space software systems. A project's ability to produce
high-assurance software in a cost-effective manner depends on two factors:

* Effective processes for managing the software and system life cycles,
  with well-defined activities for planning, controlling, and monitoring
  the work; and

* Effective technologies (programming languages, development and verification
  tools, version control systems, etc.) to support the software life-cycle
  processes.

For safety-critical application domains, the first factor is typically
anticipated by a regulatory authority in the form of certification /
qualification standards such as are found in the civil aviation and
nuclear industries. In the space domain the ECSS software-related standards
play a similar role, with the current set of documents based on decades of
experience with space system development. In particular, the software
engineering standard |E-ST-40C| and the software product assurance standard
|Q-ST-80C| provide a framework in which software suppliers and customers
can interact, with a clear statement and understanding of processes and
responsibilities.

Technologies, and more specifically the choice of programming language(s)
and supporting toolsuites, directly affect the ease or difficulty of
developing, verifying, and maintaining quality software. The state of
the art in software engineering has made large strides over the years,
with programming language / methodology advances in areas such as
modularization and encapsulation. Nevertheless, the key messages have
stayed constant:

* The programming language should help prevent errors from being
  introduced in the first place; and

* If errors are present, the language rules should detect them early in
  the software life cycle, when defects are easiest and least expensive
  to correct.

These messages come through clearly in the Ada programming language,
which was designed from the start to enforce sound software engineering
principles, catching errors early and avoiding pitfalls such as buffer
overrun that arise in other languages. Ada has evolved considerably
since it first emerged in the mid-1980s, for example adding
Object-Oriented Programming support in Ada 95, but each new version
has kept true to the original design philosophy.

AdaCore's Ada-based tools have been helping developers design, develop
and maintain high-assurance software since the mid 1990s, in domains
that include space systems, commercial and military avionics, air
traffic control, train systems, automotive, and medical devices.
This document summarizes AdaCore's language and tool technologies
and shows how they can help space software suppliers meet the requirements
in |E-ST-40C| and |Q-ST-80C|. With effective processes as established by
these standards, and effective technologies as supplied by AdaCore,
software suppliers will be well equipped to meet the challenges of
space software development.

| Benjamin M. Brosgol
| AdaCore
| Bedford, Massachusetts USA
| November 2021

| Jean-Paul Blanquart
| Airbus Defence and Space
| Toulouse, France
| November 2021

.. rubric:: Foreword to V2.1

In the years since the initial version of this document was published,
both |E-ST-40C| and |Q-ST-80C| have been revised, and AdaCore's products
have evolved to meet the growing demands for, and challenges to, high
assurance in mission-critical real-time software. This new edition
reflects the current (2025) versions of |E-ST-40C|, |Q-ST-80C|, and AdaCore's
offerings.
Among other updates and enhancements to the company's products, the static
analysis tools supplementing the |gnatpro| development environment have been
integrated into a cohesive toolset (the *GNAT Static Analysis Suite*).
The dynamic analysis tools have likewise been consolidated, and the resulting
*GNAT Dynamic Analysis Suite* has introduced a fuzzing tool |mdash| *GNATfuzz*
|mdash| which exercises the software with invalid input and checks for
failsafe behavior.

As editor of this revised edition, I would like to thank my AdaCore colleagues
Olivier Appéré and Vasiliy Fofanov, as well as co-author Jean-Paul Blanquart,
for their detailed and helpful reviews and suggestions.

For up-to-date information on AdaCore support for developing space
software, please visit :footcite:p:`Space_SW_AdaCore_Web_Space`.

| Benjamin M. Brosgol, AdaCore
| Bedford, Massachusetts USA
| December 2025

.. toctree::
   :maxdepth: 4
   :numbered:

   Introduction<chapters/introduction>
   Programming Languages for Space Software<chapters/programming>
   Tools for Space Software Development<chapters/tools>
   Compliance with ECSS-E-ST-40C<chapters/compliance-e40c>
   Compliance with ECSS-Q-ST-80C<chapters/compliance-q80c>
   Abbreviations<chapters/abbreviations>


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
