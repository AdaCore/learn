.. _SPARK_For_MISRA_C_Dev_Preface:

Preface
-------

.. include:: ../../../global.txt

MISRA C appeared in 1998 as a coding standard for C; it focused on avoiding
error-prone programming features of the C programming language rather than on
enforcing a particular programming style. A study of coding standards for C by
`Les Hatton <https://www.leshatton.org/Documents/MISRAC.pdf>`_ found that,
compared to ten typical coding standards for C, MISRA C was the only one to
focus exclusively on error avoidance rather than style enforcement, and by a
very large margin.

The popularity of the C programming language, as well as its many traps and
pitfalls, have led to the huge success of MISRA C
in domains where C is used for high-integrity sofware. This success has driven
tool vendors to propose many competing implementations of
:wikipedia:`MISRA C <MISRA_C>` checkers. Tools compete in particular on the
coverage of MISRA C guidelines that they help enforce, as it is
impossible to enforce the 16 directives and 143 rules (collectively referred to
as guidelines) of MISRA C.

The 16 directives are broad guidelines, and it is not possible to define
compliance in a unique and automated way. For example, `"all code should be
traceable to documented requirements"` (Directive 3.1). Thus no tool is
expected to enforce directives, as the MISRA C:2012 states in introduction to
the guidelines: `"different tools may place widely different interpretations on
what constitutes a non-compliance."`

The 143 rules on the contrary are completely and precisely defined, and
`"static analysis tools should be capable of checking compliance with
rules"`. But the same sentence continues with `"subject to the limitations
described in Section 6.5"`, which addresses "decidability of
rules". It turns out that 27 rules out of 143 are not decidable, so no tool can
always detect all violations of these rules without at the same time reporting
"false alarms" on code that does not constitute a violation.

An example of an undecidable rule is rule 1.3: `"There shall be no occurrence
of undefined or critical unspecified behaviour."` Appendix H of MISRA:C 2012
lists hundreds of cases of undefined and critical unspecified
behavior in the C programming language standard, a majority of which are not
individually decidable. For the most part, MISRA C checkers ignore undecidable
rules such as rule 1.3 and instead focus on the 116 rules for which detection
of violations can be automated. It is telling in that respect that the
MISRA C:2012 document and its accompanying set of examples (which can be
downloaded from the `MISRA website <https://www.misra.org.uk>`_) does not
provide any example for rule 1.3.

However, violations of undecidable rules such as rule 1.3 are known to have
dramatic impact on software quality. Violations of rule 1.3 in particular are
commonly amplified by compilers using the permission in the C standard to optimize
aggressively without looking at the consequences for programs with undefined or
critical unspecified behavior. It would be valid to ignore these rules if
violations did not occur in practice, but on the contrary even experienced
programmers write C code with undefined or critical unspecified
behavior. An example comes from the MISRA C Committee itself in its
"Appendix I: Example deviation record" of the MISRA C:2012 document, repeated
in "Appendix A: Example deviation record" of the `MISRA C: Compliance 2016
document
<https://www.misra.org.uk/LinkClick.aspx?fileticket=w_Syhpkf7xA%3d&tabid=57>`_,
where the following code is proposed as a deviation of rule 10.6 `"The value of
a composite expression shall not be assigned to an object with wider essential
type"`:

.. code-block:: c

   uint32_t prod = qty * time_step;

Here, the multiplication of two unsigned 16-bit values and assignment of the
result to an unsigned 32-bit variable constitutes a violation of the
aforementioned rule, which gets justified for efficiency reasons. What the
authors seem to have missed is that the multiplication is then performed with
the signed integer type :c:`int` instead of the target unsigned type
:c:`uint32_t`. Thus the multiplication of two unsigned 16-bit values may lead to
an overflow of the 32-bit intermediate signed result, which is an occurrence of
an undefined behavior. In such a case, a compiler is free to assume that the
value of :c:`prod` cannot exceed 2\ :sup:`31` - 1 (the maximal value of a
signed 32-bit integer) as otherwise an undefined behavior would have been
triggered. For example, the undefined behavior with values 65535 for :c:`qty`
and :c:`time_step` is reported when running the code compiled by either the GCC
or LLVM compiler with option ``-fsanitize=undefined``.

The MISRA C checkers that detect violations of
undecidable rules are either unsound tools that can detect only some of
the violations, or sound tools that guarantee to detect all such violations at
the cost of possibly many false reports of violations. This is a direct
consequence of undecidability. However, static analysis technology is available
that can achieve soundness without inundating users with false
alarms. One example is the SPARK toolset developed by AdaCore, Altran and Inria,
which is based on four principles:

- The base language Ada provides a solid foundation for static analysis through
  a well-defined language standard, strong typing and rich specification features.

- The SPARK subset of Ada restricts the base language in essential ways to support
  static analysis, by controlling sources of ambiguity such as side-effects and
  aliasing.

- The static analysis tools work mostly at the granularity of an individual
  function, making the analysis more precise and minimizing the
  possibility of false alarms.

- The static analysis tools are interactive, allowing users to guide
  the analysis if necessary or desired.

In this document, we show how SPARK can be used to achieve
high code quality with guarantees that go beyond what would be feasible
with MISRA C.

.. only:: builder_latex

   An on-line and interactive version of this document is available at
   `AdaCore's learn.adacore.com site
   <https://learn.adacore.com/courses/SPARK_for_the_MISRA_C_Developer>`_.
