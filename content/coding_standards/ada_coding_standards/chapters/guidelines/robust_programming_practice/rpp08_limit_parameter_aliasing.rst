-----------------------------------
Limit Parameter Aliasing  (RPP08)
-----------------------------------

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

**Remediation** :math:`\rightarrow` High

**Verification Method** :math:`\rightarrow` Code inspection

+++++++++++
Reference
+++++++++++

`Ada RM 6.2 - Formal Parameter Modes
<http://www.ada-auth.org/standards/2xrm/html/RM-6-2.html>`_,

`SPARK RM 6.4.2 - Anti-Aliasing
<https://docs.adacore.com/spark2014-docs/html/lrm/subprograms.html#anti-aliasing>`_

+++++++++++++
Description
+++++++++++++

In software, an alias is a name which refers to the same object as another
name. In some cases, it is an error in Ada to reference an object through a
name while updating it through another name in the same subprogram. Most of
these cases cannot be detected by a compiler. Even when not an error, the
presence of aliasing makes it more difficult to understand the code for both
humans and analysis tools, and thus it may lead to errors being introduced
during maintenance.

This rule is meant to detect problematic cases of aliasing that are introduced
through the actual parameters and between actual parameters and global
variables in a subprogram call. It is a simplified version of the SPARK rule
for anti-aliasing defined in *SPARK Reference Manual* section 6.4.2.

A formal parameter is said to be immutable when the subprogram cannot modify
its value or modify the value of an object by dereferencing a part of the
parameter of access type (at any depth in the case of SPARK). In Ada and SPARK,
this corresponds to either an anonymous access-to-constant parameter or a
parameter of mode :ada:`in` and not of an access type. Otherwise, the formal
parameter is said to be mutable.

A procedure call shall not pass two actual parameters which potentially
introduce aliasing via parameter passing unless either:

   * both of the corresponding formal parameters are immutable; or
   * at least one of the corresponding formal parameters is immutable and is
     of a by-copy type that is not an access type.

If an actual parameter in a procedure call and a global variable referenced by
the called procedure potentially introduce aliasing via parameter passing,
then:

   * the corresponding formal parameter shall be immutable; and
   * if the global variable is written in the subprogram, then the corresponding
     formal parameter shall be of a by-copy type that is not an access type.

Where one of the rules above prohibits the occurrence of an object or any of
its subcomponents as an actual parameter, the following constructs are also
prohibited in this context:

   * A type conversion whose operand is a prohibited construct;
   * A call to an instance of Unchecked_Conversion whose operand is a prohibited
     construct;
   * A qualified expression whose operand is a prohibited construct;
   * A prohibited construct enclosed in parentheses.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.32 Passing parameters and return values [CSJ]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

      type R is record
        Data : Integer := 0;
      end record;

      procedure Detect_Aliasing (Val_1 : in out R;
                                 Val_2 : in R)
      is
      begin
         null;
      end Detect_Aliasing;

      Obj : R;

   begin
      Detect_Aliasing (Obj, Obj);

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

Do not pass :ada:`Obj` as the actual parameter to both formal parameters.

+++++++
Notes
+++++++

All violations are detected by SPARK. The GNAT compiler switch
:switch:`-gnateA[1]` enables detection of some cases, but not all.
