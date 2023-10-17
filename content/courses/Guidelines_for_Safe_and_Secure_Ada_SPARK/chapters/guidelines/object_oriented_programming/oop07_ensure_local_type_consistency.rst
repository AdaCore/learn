---------------------------------------
Ensure Local Type Consistency (OOP07)
---------------------------------------

**Level** :math:`\rightarrow` Required

Category
   :Safety: :math:`\checkmark`
   :Cyber: :math:`\checkmark`

Goal
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security: :math:`\checkmark`

**Remediation** :math:`\rightarrow` N/A

**Verification Method** :math:`\rightarrow` Software test

+++++++++++
Reference
+++++++++++

[AdaOOP2016]_ See section 4.2.

[GNATUG]_ See section 5.10.11.

+++++++++++++
Description
+++++++++++++

Either:

* Formally verify local type consistency, or
* Ensure that each tagged type passes all the tests of all the parent types
  which it can replace.

Rationale:

One of the fundamental benefits of OOP is the ability to manipulate objects in
a class inheritance hierarchy without "knowing" at compile-time the specific
classes of the objects being manipulated. By *manipulate* we mean invoking the
primitive operations, the *methods* defined by the classes.

We will use the words *class* and *type* interchangeably, because classes are
composed in Ada and SPARK using a combination of building blocks, especially
type declarations. However, we will use the term *subclass* rather than
*subtype* because the latter has a specific meaning in Ada and SPARK that is
unrelated to OOP.

The objects whose operations are being invoked can be of types anywhere in the
inheritance tree, from the root down to the bottom. The location, i.e., the
specific type, is transparent to the manipulating code. This type transparency
is possible because the invoked operations are dynamically dispatched at
run-time, rather than statically dispatched at compile-time.

Typically, the code manipulating the objects does so in terms of superclasses
closer to the root of the inheritance tree. Doing so increases generality
because it increases the number of potential subclasses that can be
manipulated. The actual superclass chosen will depend on the operations
required by the manipulation.  In Ada and SPARK, subclasses can add operations
but can never remove them, so more operations are found as we move down from
the root. That is the nature of specialization. Note that the guarantee of an
invoked operations' existence is essential for languages used in this domain.

However, for this transparent manipulation to be functionally correct -- to
accomplish what the caller intends -- the primitive operations of subclasses
must be functionally indistinguishable from those of the superclasses. That
doesn't mean the subclasses cannot make changes. Indeed, the entire point of
subclasses is to make changes. In particular, functional changes can be either
introduction of new operations, or overridings of inherited operations. It is
these overridings that must be functionally transparent to the manipulating
code. (Of course, for an inherited operation that is not overridden, the
functionality is inherited as-is, and is thus transparent trivially.)

The concept of functional transparency was introduced, albeit with different
terminology, by Liskov and Wing in 1994 [LiskovWing1994]_  and is, therefore,
known as the Liskov Substitution Principle, or LSP.  The *substitution* in LSP
means that a subclass must be substitutable for its superclass, i.e., a
subclass instance should be usable whenever a superclass instance is required.

Unfortunately, requirements-based testing will not detect violations of LSP
because unit-level requirements do not concern themselves with superclass
substitutability.

However, the OO supplement of DO-178C [DO178C]_ offers solutions, two of which
are in fact the options recommended by this guideline.

Specifically, the supplement suggests adding a specific verification activity
it defines as Local Type Consistency Verification. This activity ensures LSP is
respected and, in so doing, addresses the vulnerability.

Verification can be accomplished statically with formal methods in SPARK, or
dynamically via a modified form of testing.

For the static approach, type consistency is verified by examining the
properties of the overriding operation's preconditions and postconditions.
These are the properties required by Design by Contract, as codified by
Bertrand Meyer [Meyer1997]_. Specifically, an overridden primitive may only
replace the precondition with one weaker than that of the parent version, and
may only replace the postcondition with one stronger. In essence, relative to
the parent version, an overridden operation can only require the same or less,
and provide the same or more. Satisfying that requirement is sufficient to
ensure functional transparency because the manipulating code only "knows" the
contracts of the class it manipulates, i.e., the view presented by the type,
which may very well be a superclass of the one actually invoked.

In Ada and SPARK, the class-wide form of preconditions and postconditions are
inherited by overridden primitive operations of tagged types. The inherited
precondition and that of the overriding (if any) are combined into a
conjunction. All must hold, otherwise the precondition fails. Likewise, the
inherited postcondition is combined with the overriding postcondition into a
disjunction. At least one of them must hold. In Ada these are tested at
run-time. In SPARK, they are verified statically (or not, in which case proof
fails and an error is indicated).

To verify substitutability via testing, all the tests for all superclass types
are applied to objects of the given subclass type. If all the parent tests
pass, this provides a high degree of confidence that objects of the new tagged
type can properly substitute for parent type objects. Note that static proof of
consistency provides an even higher degree of confidence.

++++++++++++++++++++++++++++++++++++++++++++++++
Applicable vulnerability within ISO TR 24772-2
++++++++++++++++++++++++++++++++++++++++++++++++

* 6.42 Violations of the Liskov substitution principle of the contract model
  [BLP]
* 6.43 Redispatching [PPH]
* 6.44 Polymorphic variables [BKK]

+++++++++++++++++++++++++++
Noncompliant Code Example
+++++++++++++++++++++++++++

.. code-block:: Ada

   package P is
      pragma Elaborate_Body;
      type Rectangle is tagged private;
      procedure Set_Width (This  : in out Rectangle;
                           Value : Positive)
      with
         Post => Width (This) = Value and
                 Height (This) = Height (This'Old);

      function Width (This : Rectangle) return Positive;

      procedure Set_Height (This  : in out Rectangle;
                            Value : Positive)
      with
         Post => Height (This) = Value and
                 Width (This) = Width (This'Old);

      function Height (This : Rectangle) return Positive;

   private
      ...
   end P;

The postcondition for :ada:`Set_Width` states that the :ada:`Height`
is not changed.
Likewise, for :ada:`Set_Height`, the postcondition asserts that the :ada:`Width`
is not
changed. However, these postconditions are not class-wide so they are not
inherited by subclasses.

Now, in a subclass Square, the operations are overridden so that setting the
width also sets the height to the same value, and vice versa. Thus the
overridden operations do not maintain type consistency, but this fact is
neither detected at run-time, nor could SPARK verify it statically (and SPARK
is not used at all in these versions of the packages).

.. code-block:: Ada

   with P; use P;
   package Q is
      pragma Elaborate_Body;
      type Square is new Rectangle with private;

      overriding
      procedure Set_Width (This  : in out Square;
                         Value : Positive)
      with
     Post => Width (This) = Height (This);

      overriding
      procedure Set_Height (This  : in out Square;
                          Value : Positive)
      with
     Post  => Width (This) = Height (This);

   private
      ...
   end Q;

++++++++++++++++++++++++
Compliant Code Example
++++++++++++++++++++++++

.. code-block:: Ada

   package P with SPARK_Mode is
      pragma Elaborate_Body;
      type Rectangle is tagged private;

      procedure Set_Width (This  : in out Rectangle;
                           Value : Positive)
      with
         Post'Class => Width (This) = Value and
                       Height (This) = Height (This'Old);

      function Width (This : Rectangle) return Positive;

      procedure Set_Height (This  : in out Rectangle;
                            Value : Positive)
      with
         Post'Class => Height (This) = Value and
                       Width (This) = Width (This'Old);

      function Height (This : Rectangle) return Positive;

   private
      ...
   end P;

Now the postconditions are class-wide so they are inherited by subclasses. In
the subclass Square, the postconditions will not hold at run-time. Likewise,
SPARK can now prove that type consistency is not verified because the
postconditions are weaker than those inherited:

.. code-block:: Ada

   with P; use P;
   package Q with SPARK_Mode is
      pragma Elaborate_Body;
      type Square is new Rectangle with private;

      overriding
      procedure Set_Width (This  : in out Square;
                           Value : Positive)
      with
     Post'Class => Width (This) = Height (This);

      overriding
      procedure Set_Height (This  : in out Square;
                            Value : Positive)
      with
     Post'Class => Width (This) = Height (This);

   private
      type Square is new Rectangle with null record;
   end Q;

+++++++
Notes
+++++++

Verification can be achieved dynamically with the GNATtest tool, using the
:switch:`--validate-type-extensions` switch. SPARK enforces this rule.
