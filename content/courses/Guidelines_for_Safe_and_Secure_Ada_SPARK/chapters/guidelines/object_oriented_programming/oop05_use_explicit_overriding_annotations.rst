---------------------------------------------
Use Explicit Overriding Annotations (OOP05)
---------------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Required

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security:

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` GNATcheck rule:
:rule:`Style_Checks:O` (builtin rule)

+++++++++++
Reference
+++++++++++

[AdaOOP2016]_ section 4.3

+++++++++++++
Description
+++++++++++++

The declaration of a primitive operation that overrides an inherited operation
must include an explicit :ada:`overriding` annotation.

The semantics of inheritance in mainstream object-oriented languages may result
in two kinds of programming errors: 1) intending, but failing, to override an
inherited subprogram, and 2) intending not to override an inherited subprogram,
but doing so anyway. Because an overridden  subprogram may perform
subclass-specific safety or security checks, the invocation of the parent
subprogram on a subclass instance can introduce a vulnerability.

The first issue (intending but failing to override) typically occurs when the
subprogram name is misspelled. In this case  a new or overloaded subprogram is
actually declared.

The second issue (unintended overriding) can arise when a new subprogram is
added to a parent type in an existing inheritance hierarchy. The new subprogram
happens to cause one or more inherited subprograms below it to override the new
superclass version. This mistake typically happens during program maintenance.

In Ada, much like other modern languages, one can annotate a subprogram
declaration (and body) with an indication that the subprogram is an overriding
of an inherited version. This is done with the :ada:`overriding` reserved word
preceding the subprogram specification.

Similarly, in Ada one can explicitly indicate that a subprogram is not an
overriding. To do so, the programmer includes the reserved words :ada:`not
overriding` immediately prior to the subprogram specification.

Of course, incorrect marking errors are flagged by the compiler. If a
subprogram is explicitly marked as overriding but is not actually overriding,
the compiler will reject the code.  Likewise, if a primitive subprogram is
explicitly marked as not overriding, but actually is overriding, the compiler
will reject the code.

However, most subprograms are not overriding so it would be a heavy burden on
the programmer to make them explicitly indicate that fact. That's not to
mention the relatively heavy syntax required.

In addition, both annotations are optional for the sake of compatibility with
prior versions of the language. Therefore, a subprogram without either
annotation might or might not be overriding. A legal program could contain some
explicitly annotated subprograms and some that are not annotated at all. But
because the compiler will reject explicit annotations that are incorrect, all
we require is that one of the two cases be explicitly indicated for all such
subprograms. Any unannotated subprograms not flagged as errors are then
necessarily not that case, they must be the other one.

Since overriding is less common and involves slightly less syntax to annotate,
the guideline requires explicit annotations only for overriding subprograms. It
follows that any subprograms not flagged as errors by the compiler are not
overriding, so they need not be marked explicitly as such.

This guideline is implemented by compiler switches, or  alternatively, by a
GNATcheck rule (specified below the table). With this guideline applied and
enforced, the two inheritance errors described in the introduction cannot
happen.

Note that the compiler switches will also require the explicit overriding
indicator when overriding a language-defined operator. The switches also apply
to inherited primitive subprograms for non-tagged types.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.34 Subprogram signature mismatch [OTR]
* 6.41 Inheritance [RIP]

++++++++++++++++++++++++++++++++++++++++
Applicable Common Weakness Enumeration
++++++++++++++++++++++++++++++++++++++++

* :cwe:`CWE-685 - Function Call With Incorrect Number of Arguments <685>`

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. literalinclude:: examples/oop05.ads
  :language: Ada
  :lines: 3-5
  :dedent: 3

.. literalinclude:: examples/oop05.ads
  :language: Ada
  :lines: 6-8
  :dedent: 3

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. literalinclude:: examples/oop05.ads
  :language: Ada
  :lines: 8-11
  :dedent: 3

+++++++
Notes
+++++++

This rule requires the GNAT compiler switches :switch:`-gnatyO` and
:switch:`-gnatwe` in order for the compiler to flag missing overriding
annotations as errors. The first causes the compiler to generate the
warnings, and the second causes those warnings to be treated as errors.
