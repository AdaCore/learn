-------------------------------------------------------------
Do Not Re-Verify Preconditions In Subprogram Bodies (RPP10)
-------------------------------------------------------------

**Level** :math:`\rightarrow` Advisory

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

**Verification Method** :math:`\rightarrow` Static analysis tools

+++++++++++
Reference
+++++++++++

N/A

+++++++++++++
Description
+++++++++++++

Do not re-verify preconditions in the corresponding subprogram bodies. It is a
waste of cycles and confuses the reader as well.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

N/A

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   type Stack is private;
   procedure Push (This : in out Stack;  Item : Element) with
      Pre  => not Full (This),
      Post => ...
   ...
   procedure Push (This : in out Stack;  Item : Element) is
   begin
      if Full (This) then  -- redundant check
         raise Overflow;
      end if;
      This.Top := This.Top + 1;
      This.Values (This.Top) := Item;
   end Push;

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   type Stack is private;
   procedure Push (This : in out Stack;  Item : Element) with
      Pre  => not Full (This),
      Post => ...
   ...
   procedure Push (This : in out Stack;  Item : Element) is
   begin
      This.Top := This.Top + 1;
      This.Values (This.Top) := Item;
   end Push;

+++++++
Notes
+++++++

This rule can be enforced by CodePeer or SPARK, via detection of dead code.
