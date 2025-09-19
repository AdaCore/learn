:prev_state: False

Preface
---------

.. include:: ../../../global.txt

Nowadays it seems like talking about programming languages is a bit passé. The technical wars of the past decade have subsided and today we see a variety of high-level and well-established languages offering functionality that can meet the needs of any programmer.

Python, Java, C++, C#, and Visual Basic are recent examples. Indeed, these languages make it easier to write code very quickly, are very flexible, offer features with highly dynamic behavior, and some even allow compilers to deduce the developer's probable intent.

Why, then, talk about yet another language? Well, by addressing the general programming market, the aforementioned languages have become poorly suited for working within the domain of high-integrity systems. In highly reliable, secure and safe applications such as those found in and around airplanes, rockets, satellites, trains, and in any device whose failure could jeopardize human life or critical assets, the programming languages used must support the high standard of software engineering necessary to maintain the integrity of the system.

The concept of verification |mdash| the practice of showing that the system behaves and performs as intended |mdash| is key in such environments. Verification can be accomplished by some combination of review, testing, static analysis, and formal proof techniques. The increasing reliance on software and increasing complexity of today's systems has made this task more difficult. Technologies and practices that might have been perfectly acceptable ten or fifteen years ago are insufficient today. Thankfully, the state of the art in analysis and proof tools and techniques has also advanced.

The latest revisions of the Ada language, Ada 2005 and Ada 2012, make enhanced software integrity possible. From its inception in the 1980s, Ada was designed to meet the requirements of high-integrity systems, and continues to be well-suited for the implementation of critical embedded or native applications. And it has been receiving increased attention recently. Every language revision has enhanced expressiveness in many areas. Ada 2012, in particular, has introduced new features for contract-based programming that are valuable to any project where verification is part of the engineering lifecycle. Along with these language enhancements, Ada compiler and tool technology has also kept pace with general computing developments over the past few years. Ada development environments are available on a wide range of platforms and are being used for the most demanding applications.

It is no secret that we at AdaCore are very enthusiastic about Ada, but we will not claim that Ada is always the solution; Ada is no more a silver bullet than any other language. In some domains other languages make sense because of the availability of particular libraries or development frameworks. For example, C++ and Java are considered good choices for desktop programs or applications where a shortened time to market is a major objective. Other areas, such as website programming or system administration, tend to rely on different formalisms such as scripting and interpreted languages. The key is to select the proper technical approach, in terms of the language and tools, to meet the requirements. Ada's strength is in areas where reliability is paramount.

Learning a new language shouldn't be complicated. Programming paradigms have not evolved much since object oriented programming gained a foothold, and the same paradigms are present one way or another in many widely used languages. This document will thus give you an overview of the Ada language using analogies to C++ and Java |mdash| these are the languages you're already likely to know. No prior knowledge of Ada is assumed. If you are working on an Ada project now and need more background, if you are interested in learning to program in Ada, or if you need to perform an assessment of possible languages to be used for a new development, this guide is for you.

..
  .. This chapter should be unindented when it is ready
  How to Run the Examples
  ------------------------

  Learning any language is best done by using it and seeing it in action. Therefore, each section of this document will include plenty of examples, available from [URL]. To run these examples, you will need a recent version of the GNAT compiler. The latest GNAT GPL distributions for OS X, Windows, and Linux are freely available from [URL] and are suitable to use with this guide. GNAT Pro, which is the commercial version for those developing professional applications, may also be used.

  In the directory for each example you'll find a *.gpr* file (that is, a "GNAT Project File"). This file contains information on where to find source code, where to put object and executable files, and compilation and build settings. We've made all the *.gpr* files in each example directory specify the same layout: source files are located at the top level alongside the *.gpr* file, and object and executable files are to be written to an ``obj``/ sub-directory.

  From the command line, compilation can be performed with a call to :program:`gprbuild`:

  .. code-block:: script

   $> gprbuild -P project.gpr

  You can run the freshly compiled code in the ``obj``/ directory the same way as you would any other executable on your platform. Invoke the example with:

  .. code-block:: script

   $> ./obj/main

  Source code for the examples is stored in *.ads* and *.adb* files. To view the contents of these files you can use your favorite programmer's editor or use GPS, the GNAT Programming Studio. To open GPS you can double-click on the *.gpr* project file or invoke GPS on the command line:

  .. code-block:: script

   $> gps -P project.gpr

  To compile your project using GPS, use the top-level menu to invoke Build -> Project -> main.adb (or use the keyboard shortcut, F4). To run the main program, use Build -> Run -> main (the keyboard shortcut for this is Shift + F2).
