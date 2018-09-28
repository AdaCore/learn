:code-config:`run_button=False;prove_button=True;accumulate_code=False`

.. _Preface:

Preface
-------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

MISRA-C appeared in 1998 as a coding standard for C code focused on avoiding
error-prone programming features of the C programming language rather than on
enforcing a particular programming style. A study of coding standards for C by
Les Hatton `found that <https://www.leshatton.org/Documents/MISRAC.pdf>`_,
compared to 10 typical coding standards for C, MISRA-C was the only one to
focus exclusively on error avoidance rather than style enforcement, and by a
very large margin.

The popularity of the C programming language, as well as its propensity to
trick programmers into making mistakes, have led to the huge success of MISRA-C
in domains where C is used for high integrity sofware. This success has driven
tool vendors to propose `many competing implementations of MISRA-C checkers
<https://en.wikipedia.org/wiki/MISRA_C>`_. Tools compete in particular on the
coverage of MISRA-C guidelines that they help enforce, as it is theoretically
impossible to enforce the 16 directives and 143 rules (collectively referred to
as guidelines) of MISRA-C.

The 16 directives are broad guidelines from which it is not possible to define
compliance in a unique and automated way. For example, `"all code should be
traceable to documented requirements"` (Directive 3.1). Thus no tool is
expected to enforce directives, as the MISRA-C:2012 states in introduction to
the guidelines: `"different tools may place widely different interpretations on
what constitutes a non-compliance."`

The 143 rules on the contrary are completely and precisely defined, and
`"static analysis tools should be capable of checking compliance with
rules"`. But the same sentence continues with `"subject to the limitations
described in Section 6.5"` referencing here a section on the "decidability of
rules". It turns out that 27 rules out of 143 are not decidable, so no tool can
always detect all violations of these rules without at the same time issuing
violations on code that does not constitute a violation.

An example of an undecidable rule is rule 1.3: `"There shall be no occurrence
of undefined or critical unspecified behaviour."` Appendix H of MISRA:C 2012
lists over 10 pages the hundreds of cases of undefined and critical unspecified
behavior in the C programming language standard, a majority of which are not
individually decidable. MISRA-C checkers ignore for the most part undecidable
rules such as rule 1.3 to focus on the 116 rules for which detection of
violations can be automated. It is telling in that respect that the
MISRA-C:2012 document and its accompanying set of examples (which can be
downloaded from the `MISRA website <https://www.misra.org.uk>`_) does not
provide any example for rule 1.3.

However, violations of undecidable rules such as rule 1.3 are known to have
dramatic impact on software quality. Violations of rule 1.3 in particular are
commonly amplified by compilers using the permission in C standard to optimize
aggressively without looking at consequences for programs with undefined or
critical unspecified behavior. It would be valid to ignore these rules if
violations did not occur in practice, but on the contrary even experienced
programmers are likely to write C code with undefined or critical unspecified
behavior. An example of that comes from the MISRA-C Committee itself in its
"Appendix I: Example deviation record" of the MISRA-C:2012 document, repeated
in the "Appendix A: Example deviation record" of the `MISRA-C:Compliance 2016
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
signed integer type ``int`` instead of the target unsigned type
``uint32_t``. Thus the multiplication of two unsigned 16-bit values may lead to
an overflow of the 32-bit intermediate signed result, which is an occurrence of
an undefined behavior. In such a case, a compiler is free to assume that the
value of prod cannot exceed 2^31-1 (the maximal value of a signed 32-bit
integer) as otherwise an undefined behavior would have been triggered. For
example, the undefined behavior with values 65535 for ``qty`` and ``time_step``
is reported when running the code compiled by either GCC or LLVM compiler with
option ``-fsanitize=undefined``.

For the MISRA-C checkers that provide the ability to detect violations of
undecidable rules, they are either unsound tools that can detect only some of
the violations, or sound tools that guarantee to detect all such violations at
the cost of possibly many false reports of violations. This is a direct
consequence of undecidability. Yet, there are ways to provide static analysis
tools that held up to the standard of soundness without burying users in false
alarms. The SPARK toolset developed by AdaCore, Altran and Inria is an example
of such an approach, based on four basic principles:

- The base language Ada provides a solid basis for static analysis throught
  unambiguous syntax, strong typing and rich specification features.

- The subset SPARK Ada restricts the base language in essential ways to support
  static analysis, by controlling sources of ambiguity like side-effects and
  aliasing.

- The static analysis tools work mostly at the granularity of an individual
  function to be able to deploy complex analyses, thus minimizing the
  possibility of false alarms.

- The previous constraint on static analysis means that the static analysis
  tools need to provide a rich set of interactive features for users to frame
  the analysis at function level.

In this document, we show how SPARK can be used to achieve the same objectives
of code quality as MISRA-C. Looking at the main areas of focus in MISRA-C, we
show how SPARK can provide guarantees that go beyond what would be feasible
with MISRA-C.
