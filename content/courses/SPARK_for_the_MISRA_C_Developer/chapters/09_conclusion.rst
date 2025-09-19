Conclusion
----------

.. include:: ../../../global.txt

The C programming language is "close to the metal" and has
emerged as a *lingua franca* for the majority of embedded platforms of all
sizes. However, its software engineering deficiencies (such as the absence of
data encapsulation) and its many traps and pitfalls present major obstacles
to those developing critical applications. To some extent, it
is possible to put the blame for programming errors on programmers themselves,
as Linus Torvalds admonished:

   `"Learn C, instead of just stringing random characters
   together until it compiles (with warnings)."`

But programmers are human, and even the best would be hard pressed to be 100%
correct about the myriad of semantic details such as those discussed
in this document. Programming language abstractions
have been invented precisely to help developers focus on the "big picture"
(thinking in terms of problem-oriented concepts) rather than low-level
machine-oriented details, but C lacks these abstractions.
As Kees Cook from the Kernel Self Protection
Project puts it (during the Linux Security Summit North America 2018):

   `"Talking about C as a language, and how it's really just a fancy
   assembler"`

Even experts sometimes have problems with
the C programming language rules, as illustrated by Microsoft
expert David LeBlanc (see :ref:`SPARK_For_MISRA_C_Dev_Enforcing_Strong_Typing_For_Scalars`) or the
MISRA C Committee itself (see the :ref:`SPARK_For_MISRA_C_Dev_Preface`).

The rules in MISRA C represent an impressive collective effort to improve the
reliability of C code in critical applications, with a focus on avoiding
error-prone features rather than enforcing a
particular programming style. The Rationale provided with each rule is a clear
and unobjectionable justification of the rule's benefit.

At a fundamental level, however, MISRA C is still built on a base language
that was not really designed with the goal of supporting large high-assurance
applications. As shown in this document, there are limits to what static
analysis can enforce with respect to the MISRA C rules. It's hard to
retrofit reliability, safety and security into a language that did not
have these as goals from the start.

The SPARK language took a different approach, starting from a base language
(Ada) that was designed from the outset to support solid software engineering,
and eliminating features that were implementation dependent or otherwise
hard to formally analyze. In this document we have shown how the SPARK
programming language and its associated formal verification tools can
contribute usefully to the goal of producing error-free
software, going beyond the guarantees that can be achieved in MISRA C.
