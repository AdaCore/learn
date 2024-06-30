------------------------------------------------------
Use Precondition and Postcondition Contracts (RPP09)
------------------------------------------------------

.. include:: ../../../../global.txt

**Level** :math:`\rightarrow` Advisory

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability: :math:`\checkmark`
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` Low

**Verification Method** :math:`\rightarrow` Code inspection

+++++++++++
Reference
+++++++++++

Power of Ten rule 5: "The assertion density of the code should average to a
minimum of two assertions per function."

+++++++++++++
Description
+++++++++++++

Subprograms should declare :ada:`Pre` and/or :ada:`Post` contracts.  Developers should
consider specifying the Global contract as well, when the default does not
apply.

Subprogram contracts complete the Ada notion of a specification, enabling
clients to know what the subprogram does without having to know how it is
implemented.

Preconditions define those logical (Boolean) conditions required for the body
to be able to provide the specified behavior. As such, they are obligations on
the callers. These conditions are checked at run-time in Ada, prior to each
call, and verified statically in SPARK.

Postconditions define those logical (Boolean) conditions that will hold after
the call returns normally. As such, they express obligations on the
implementer, i.e., the subprogram body. The implementation must be such that
the postcondition holds, either at run-time for Ada, or statically in SPARK.

Not all subprograms will have both a precondition and a postcondition, some
will have neither.

The :ada:`Global` contract specifies interactions with those objects not local to the
corresponding subprogram body. As such, they help complete the specification
because, otherwise, one would need to examine the body of the subprogram itself
and all those it calls, directly or indirectly, to know whether any global
objects were accessed.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable Vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.42 Violations of the Liskov substitution principle or the contract model
  [BLP]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   type Stack is private;
   procedure Push (This : in out Stack;  Item : Element);

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   type Stack is private;
   procedure Push (This : in out Stack;  Item : Element) with
      Pre  => not Full (This),
      Post => not Empty (This)
              and Top_Element (This) = Item
              and Extent (This) = Extent (This)'Old + 1
              and Unchanged (This'Old, Within => This),
      Global => null;

+++++++
Notes
+++++++

This rule must be enforced by manual inspection.

Moreover, the program must be compiled with enabled assertions
(GNAT :switch:`-gnata` switch) to ensure that the contracts are executed, or
a sound static analysis tool such as CodePeer or SPARK toolset should be used
to prove that the contracts are always true.
