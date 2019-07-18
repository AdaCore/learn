:code-config:`run_button=True;prove_button=False;accumulate_code=False`

Enforcing Strong Typing
-----------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Annex C of MISRA C:2012 summarizes the problem succinctly:

  `"ISO C may be considered to exhibit poor type safety as it permits a wide
  range of implicit type conversions to take place. These type conversions can
  compromise safety as their implementation-defined aspects can cause developer
  confusion."`

The most severe consequences come from inappropriate conversions involving
pointer types, as they can cause memory safety violations. Two
sections of MISRA C are dedicated to these issues: "Pointer type
conversions" (9 rules) and "Pointers and arrays" (8 rules).

Inappropriate conversions between scalar types are only slightly less severe, as
they may introduce arbitrary violations of the intended functionality. MISRA C
has gone to great lengths to improve the situation, by defining a stricter
type system on top of the C language. This is described in Appendix D of
MISRA C:2012 and in the dedicated section on "The essential type model" (8
rules).

Enforcing Strong Typing for Pointers
************************************

Pointers in C provide a low-level view of the addressable memory as a set of
integer addresses. To write at address 42, just go through a pointer:

.. code:: c

   !main.c
   int main() {
      int *p = 42;
      *p = 0;
      return 0;
   }

Running this program is likely to hit a segmentation fault on an operating
system, or to cause havoc in an embedded system, both because address 42 will
not be correctly aligned on a 32-bit or 64-bit machine and because this address
is unlikely to correspond to valid addressable data for the application. The
compiler might issue a helpful warning on the above code (with option
``-Wint-conversion`` implied by ``-Wall`` in GCC or LLVM), but note that the
warning disappears when explicitly converting value 42 to the target pointer
type, although the problem is still present.

Beyond their ability to denote memory addresses, pointers are also used in C to
pass references as inputs or outputs to function calls, to construct complex
data structures with indirection or sharing, and to denote arrays of
elements. Pointers are thus at once pervasive, powerful and fragile.

Pointers Are Not Addresses
^^^^^^^^^^^^^^^^^^^^^^^^^^

In an attempt to rule out issues that come from direct addressing of memory
with pointers, MISRA C states in Rule 11.4 that `"A conversion should not be
performed between a pointer to object and an integer type"`. As this rule is
classified as only Advisory, MISRA C completes it with two Required rules:

* Rule 11.6: `"A cast shall not be performed between pointer to void and an
  arithmetic type"`

* Rule 11.7: `"A cast shall not be performed between pointer to object and
  a non-integer arithmetic type"`

In Ada, pointers are not addresses, and addresses are not integers. An opaque
standard type ``System.Address`` is used for addresses, and conversions to/from
integers are provided in a standard package ``System.Storage_Elements``. The
previous C code can be written as follows in Ada:

.. code:: ada

    with System;
    with System.Storage_Elements;

    procedure Pointer is
       A : constant System.Address := System.Storage_Elements.To_Address (42);
       M : aliased Integer with Address => A;
       P : constant access Integer := M'Access;
    begin
       P.all := 0;
    end Pointer;

The integer value 42 is converted to a memory address ``A`` by calling
``System.Storage_Elements.To_Address``, which is then used as the address of
integer variable ``M``. The pointer variable ``P`` is set to point to ``M``
(which is allowed because ``M`` is declared as ``aliased``).

Ada requires more verbiage than C:

* The integer value ``42`` must be explicitly converted to type ``Address``

* To get a pointer to a declared variable such as ``M``, the declaration
  must be marked as ``aliased``

The added syntax helps first in making clear what is happening and, second,
in ensuring that a potentially dangerous feature (assigning to a value at a
specific machine address) is not used inadvertently.

The above example is legal Ada but not SPARK, since SPARK does not support
pointers (they significantly complicate formal analysis). SPARK does allow
addresses, however.

Pointers Are Not References
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Passing parameters by reference is critical for efficient programs, but the
absence of references distinct from pointers in C incurs a
serious risk. Any parameter of a pointer type can be copied freely to a
variable whose lifetime is longer than the object pointed to, a problem known
as "dangling pointers". MISRA C forbids such uses in Rule 18.6: `"The address of
an object with automatic storage shall not be copied to another object that
persists after the first object has ceased to exist"`. Unfortunately, enforcing
this rule is difficult, as it is undecidable.

In SPARK, parameters can be passed by reference, but no pointer to the
parameter can be stored, which completely solves this issue. In fact, the
decision to pass a parameter by copy or by reference rests in many cases with
the compiler, but such compiler dependency has no effect on the functional
behavior of a SPARK program. In the example below, the compiler may decide to pass
parameter ``P`` of procedure ``Rotate_X`` either by copy or by reference, but
regardless of the choice the postcondition of ``Rotate_X`` will hold:
the final value of ``P`` will be modified by rotation around the ``X`` axis.

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

.. code:: ada prove_report_all_button

    package Geometry is

       type Point_3D is record
          X, Y, Z : Float;
       end record;

       procedure Rotate_X (P : in out Point_3D) with
         Post => P = P'Old'Update (Y => P.Z'Old, Z => -P.Y'Old);

    end Geometry;

    package body Geometry is

       procedure Rotate_X (P : in out Point_3D) is
          Tmp : constant Float := P.Y;
       begin
          P.Y := P.Z;
          P.Z := -Tmp;
       end Rotate_X;

    end Geometry;

:code-config:`run_button=True;prove_button=False;accumulate_code=False`

SPARK's analysis tool can mathematically prove that the postcondition is true.

Pointers Are Not Arrays
^^^^^^^^^^^^^^^^^^^^^^^

The greatest source of vulnerabilities regarding pointers is their use as
substitutes for arrays. Although the C language has a syntax for declaring and
accessing arrays, this is just a thin syntactic layer on top of pointers. Thus:

*  Array access is just pointer arithmetic;
*  If a function is to manipulate an array
   then the array's length must be separately passed as a parameter; and
*  The program is susceptible to the various vulnerabilities
   originating from the confusion of pointers and arrays, such as buffer overflow.

Consider a function that counts the number of times a value is present in an
array. In C, this could be written:

.. code:: c

   !main.c
   #include <stdio.h>

   int count(int *p, int len, int v) {
      int count = 0;
      while (len--) {
         if (*p++ == v) {
            count++;
         }
      }
      return count;
   }

   int main() {
      int p[5] = {0, 3, 9, 3, 3};
      int c = count(p, 5, 3);
      printf("value 3 is seen %d times in p\n", c);
      return 0;
   }

Function ``count`` has no control over the range of addresses accessed from
pointer ``p``. The critical property that the ``len`` parameter is a valid length
for an array of integers pointed to by parameter ``p`` rests completely with
the caller of ``count``, and ``count`` has no way to check that this is
true.

To mitigate the risks associated with pointers being used for arrays, MISRA C
contains eight rules in a section on "Pointers and arrays". These rules
forbid pointer arithmetic (Rule 18.4) or, if this Advisory rule is not
followed, require pointer arithmetic to stay within bounds (Rule 18.1). But,
even if we rewrite the loop in ``count`` to respect all decidable MISRA C
rules, the program's correctness still depends on the caller of ``count``
passing a correct value of ``len``:

.. code:: c

   !main.c
   #include <stdio.h>

   int count(int *p, int len, int v) {
      int count = 0;
      for (int i = 0; i < len; i++) {
         if (p[i] == v) {
            count++;
         }
      }
      return count;
   }

   int main() {
      int p[5] = {0, 3, 9, 3, 3};
      int c = count(p, 5, 3);
      printf("value 3 is seen %d times in p\n", c);
      return 0;
   }

The resulting code is more readable, but still vulnerable to incorrect values
of parameter ``len`` passed by the caller of ``count``, which violates
undecidable MISRA C Rules 18.1 (pointer arithmetic should stay within bounds)
and 1.3 (no undefined behavior). Contrast this with the same function in SPARK
(and Ada):

.. code:: ada

    package Types is
       type Int_Array is array (Positive range <>) of Integer;
    end Types;

    with Types; use Types;

    function Count (P : Int_Array; V : Integer) return Natural is
       Count : Natural := 0;
    begin
       for I in P'Range loop
          if P (I) = V then
             Count := Count + 1;
          end if;
       end loop;
       return Count;
    end Count;

    with Ada.Text_IO; use Ada.Text_IO;
    with Types; use Types;
    with Count;

    procedure Test_Count is
       P : constant Int_Array := (0, 3, 9, 3, 3);
       C : constant Integer := Count (P, 3);
    begin
       Put_Line ("value 3 is seen" & C'Img & " times in p");
    end Test_Count;

The array parameter ``P`` is not simply a homogeneous sequence of Integer
values. The compiler must represent ``P`` so that its lower and upper bounds
(P'First and P'Last) and thus also its length (P'Length) can be retrieved.
Function ``Count`` can
simply loop over the range of valid array indexes ``P'First .. P'Last`` (or
``P'Range`` for short). As a result, function ``Count`` can be verified in
isolation to be free of vulnerabilities such as buffer overflow, as it does
not depend on the values of parameters passed by its callers. In fact, we can
go further in SPARK and show that the value returned by ``Count`` is no greater
than the length of parameter ``P`` by stating this property in the postcondition of
``Count`` and asking the SPARK analysis tool to prove it:

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

.. code:: ada prove_report_all_button

    package Types is
       type Int_Array is array (Positive range <>) of Integer;
    end Types;

    with Types; use Types;

    function Count (P : Int_Array; V : Integer) return Natural with
      Post => Count'Result <= P'Length
    is
       Count : Natural := 0;
    begin
       for I in P'Range loop
          pragma Loop_Invariant (Count <= I - P'First);
          if P (I) = V then
             Count := Count + 1;
          end if;
       end loop;
       return Count;
    end Count;

:code-config:`run_button=True;prove_button=False;accumulate_code=False`

The only help that SPARK analysis required from the programmer, in order to prove the
postcondition, is a loop invariant (a special kind of assertion) that reflects
the value of ``Count`` at each iteration.

Pointers Should Be Typed
^^^^^^^^^^^^^^^^^^^^^^^^

The C language defines a special pointer type ``void*`` that corresponds to an
untyped pointer. It is legal to convert any pointer type to and from ``void*``,
which makes it a convenient way to simulate C++ style templates. Consider the following
code which indirectly applies ``assign_int`` to integer ``i`` and
``assign_float`` to floating-point ``f`` by calling ``assign`` on both:

.. code:: c

   !main.c
   #include <stdio.h>

   void assign_int (int *p) {
      *p = 42;
   }

   void assign_float (float *p) {
      *p = 42.0;
   }

   typedef void (*assign_fun)(void *p);

   void assign(assign_fun fun, void *p) {
      fun(p);
   }

   int main() {
      int i;
      float f;
      assign((assign_fun)&assign_int, &i);
      assign((assign_fun)&assign_float, &f);
      printf("i = %d; f = %f\n", i, f);
   }

The references to the variables ``i`` and ``f`` are implicitly converted to
the ``void*`` type as a way
to apply ``assign`` to any second parameter ``p`` whose type matches the
argument type of its first argument ``fun``. The use of an untyped argument
means that the responsibility for the correct typing rests completely
with the programmer. Swap ``i`` and ``f`` in the calls to ``assign``
and you still get a compilable program without warnings, that runs and produces
completely bogus output::

  i = 1109917696; f = 0.000000

instead of the expected::

  i = 42; f = 42.000000

Generics in SPARK (and Ada) can implement the desired functionality in a fully
typed way, with any errors caught at compile time, where procedure ``Assign``
applies its parameter procedure ``Initialize`` to its parameter ``V``:

.. code:: ada

    generic
       type T is private;
       with procedure Initialize (V : out T);
    procedure Assign (V : out T);

    procedure Assign (V : out T) is
    begin
       Initialize (V);
    end Assign;

    with Ada.Text_IO; use Ada.Text_IO;
    with Assign;

    procedure Apply_Assign is
       procedure Assign_Int (V : out Integer) is
       begin
          V := 42;
       end Assign_Int;

       procedure Assign_Float (V : out Float) is
       begin
          V := 42.0;
       end Assign_Float;

       procedure Assign_I is new Assign (Integer, Assign_Int);
       procedure Assign_F is new Assign (Float, Assign_Float);

       I : Integer;
       F : Float;
    begin
       Assign_I (I);
       Assign_F (F);
       Put_Line ("I =" & I'Img & "; F =" & F'Img);
    end Apply_Assign;

The generic procedure ``Assign`` must be instantiated with a specific
type for ``T`` and a specific procedure (taking a single ``out`` parameter
of this type) for ``Initialize``. The procedure resulting from the
instantiation applies to a variable of this type. So switching ``I`` and
``F`` above would result in an error detected by the compiler.
Likewise, an instantiation such as the following would also be
a compile-time error:

.. code-block:: ada

   procedure Assign_I is new Assign (Integer, Assign_Float);

.. _Enforcing Strong Typing for Scalars:

Enforcing Strong Typing for Scalars
***********************************

In C, all scalar types can be converted both implicitly and explicitly to any
other scalar type. The semantics is defined by rules of
`promotion` and `conversion`, which can confuse even experts.
One example was noted earlier, in the :ref:`Preface`.
Another example appears in
`an article introducing a safe library for manipulating scalars
<https://msdn.microsoft.com/en-us/library/ms972705.aspx>`_ by Microsoft expert
David LeBlanc. In its conclusion, the author acknowledges the inherent
difficulty in understanding scalar type conversions in C, by showing an early
buggy version of the code to produce the minimum signed integer:

.. code-block:: c

   return (T)(1 << (BitCount()-1));

The issue here is that the literal ``1`` on the left-hand side of the shift is an
``int``, so on a 64-bit machine with 32-bit ``int`` and 64-bit type ``T``, the
above is shifting 32-bit value ``1`` by 63 bits. This is a case of undefined behavior,
producing an unexpected output with the Microsoft compiler. The correction is to convert
the first literal ``1`` to ``T`` before the shift:

.. code-block:: c

   return (T)((T)1 << (BitCount()-1));

Although he'd asked some expert programmers to review the code, no one found
this problem.

To avoid these issues as much as possible, MISRA C defines its own type system
on top of C types, in the section on "The essential type model" (eight
rules). These can be seen as additional typing rules, since all rules in this section
are decidable, and can be enforced at the level of a single translation
unit. These rules forbid in particular the confusing cases
mentioned above. They can be divided into three sets of rules:

* restricting operations on types

* restricting explicit conversions

* restricting implicit conversions

Restricting Operations on Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Apart from the application of some operations to floating-point arguments (the
bitwise, mod and array access operations) which are invalid and reported by the
compiler, all operations apply to all scalar types in C. MISRA C Rule 10.1
constrains the types on which each operation is possible as follows.

Arithmetic Operations on Arithmetic Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Adding two Boolean values, or an Apple and an Orange, might sound like a
bad idea, but it is easily done in C:

.. code:: c

   !main.c
   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      bool b1 = true;
      bool b2 = false;
      bool b3 = b1 + b2;

      typedef enum {Apple, Orange} fruit;
      fruit f1 = Apple;
      fruit f2 = Orange;
      fruit f3 = f1 + f2;

      printf("b3 = %d; f3 = %d\n", b3, f3);

      return 0;
   }

No error from the compiler here. In fact, there is no undefined behavior in the
above code. Variables ``b3`` and ``f3`` both end up with value 1. Of course it
makes no sense to add Boolean or enumerated values, and thus MISRA C
Rule 18.1 forbids the use of all arithmetic operations on Boolean and
enumerated values, while also forbidding most arithmetic operations on
characters. That leaves the use of arithmetic operations for signed or unsigned
integers as well as floating-point types and the use of modulo operation ``%``
for signed or unsigned integers.

Here's an attempt to simulate the above C code in SPARK (and Ada):

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Arith is

       B1 : constant Boolean := True;
       B2 : constant Boolean := False;
       B3 : constant Boolean := B1 + B2;

       type Fruit is (Apple, Orange);
       F1 : constant Fruit := Apple;
       F2 : constant Fruit := Orange;
       F3 : constant Fruit := F1 + F2;

    end Bad_Arith;

Here is the output from AdaCore's GNAT compiler:

::

     1.     package Bad_Arith is
     2.
     3.        B1 : constant Boolean := True;
     4.        B2 : constant Boolean := False;
     5.        B3 : constant Boolean := B1 + B2;
                                           |
        >>> there is no applicable operator "+" for type "Standard.Boolean"

     6.
     7.        type Fruit is (Apple, Orange);
     8.        F1 : constant Fruit := Apple;
     9.        F2 : constant Fruit := Orange;
    10.        F3 : constant Fruit := F1 + F2;
                                         |
        >>> there is no applicable operator "+" for type "Fruit" defined at line 7

    11.
    12.     end Bad_Arith;

It is possible, however, to get the predecessor of a Boolean or enumerated
value with ``Value'Pred`` and its successor with ``Value'Succ``, as well as
to iterate over all values of the type:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Ok_Arith is

       B1 : constant Boolean := False;
       B2 : constant Boolean := Boolean'Succ (B1);
       B3 : constant Boolean := Boolean'Pred (B2);

       type Fruit is (Apple, Orange);
       F1 : constant Fruit := Apple;
       F2 : constant Fruit := Fruit'Succ (F1);
       F3 : constant Fruit := Fruit'Pred (F2);

    begin
       pragma Assert (B1 = B3);
       pragma Assert (F1 = F3);

       for B in Boolean loop
          Put_Line (B'Img);
       end loop;

       for F in Fruit loop
          Put_Line (F'Img);
       end loop;
    end Ok_Arith;

.. _Boolean Operations on Boolean:

Boolean Operations on Boolean
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"Two bee or not two bee? Let's C":

.. code:: c

   !main.c
   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      typedef enum {Ape, Bee, Cat} Animal;
      bool answer = (2 * Bee) || ! (2 * Bee);
      printf("two bee or not two bee? %d\n", answer);
      return 0;
   }

The answer to the question posed by Shakespeare's Hamlet is 1, since it
reduces to ``A or not A`` and this is true in classical logic.

As previously noted, MISRA C forbids the use of the multiplication operator
with an operand of an enumerated type. Rule 18.1 also forbids
the use of Boolean operations "and", "or", and "not" (``&&``, ``||``, ``!``,
respectively, in C) on anything other than Boolean operands. It would
thus prohibit the Shakespearian code above.

Below is an attempt to express the same code in SPARK (and Ada), where the Boolean operators are
``and``, ``or``, and ``not``. The ``and`` and ``or`` operators evaluate both
operands, and the language also supplies short-circuit forms that evaluate
the left operand and only evaluate the right operand when its value may affect
the result.

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Hamlet is
       type Animal is (Ape, Bee, Cat);
       Answer : Boolean := 2 * Bee or not 2 * Bee; -- Illegal
    end Bad_Hamlet;

As expected, the compiler rejects this code. There is no available ``*`` operation
that works on an enumeration type, and likewise no available ``or`` or ``not``
operation.

Bitwise Operations on Unsigned Integers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here's a genetic engineering example that combines a Bee with a Dog to produce
a Cat, by manipulating the atomic structure (the bits in its representation):

.. code:: c

   !main.c
   #include <stdbool.h>
   #include <assert.h>

   int main() {
      typedef enum {Ape, Bee, Cat, Dog} Animal;
      Animal mutant = Bee ^ Dog;
      assert (mutant == Cat);
      return 0;
   }

This algorithm works by accessing the underlying bitwise representation
of ``Bee`` and ``Dog`` (0x01 and 0x03, respectively) and, by applying the
exclusive-or operator ``^``, transforming it into the underlying bitwise
representation of a ``Cat`` (0x02). While powerful, manipulating the bits
in the representation of values is best reserved for unsigned integers as
illustrated in the book `Hacker's Delight <http://www.hackersdelight.org/>`_.
MISRA C Rule 18.1 thus forbids the use of all bitwise operations on anything
but unsigned integers.

Below is an attempt to do the same in SPARK (and Ada). The bitwise operators are
``and``, ``or``, ``xor``, and ``not``, and the related bitwise functions are
``Shift_Left``, ``Shift_Right``, ``Shift_Right_Arithmetic``, ``Rotate_Left``
and ``Rotate_Right``:

.. code:: ada
   :class: ada-expect-compile-error

   package Bad_Genetics is
      type Animal is (Ape, Bee, Cat, Dog);
      Mutant : Animal := Bee xor Dog;  --  ERROR
      pragma Assert (Mutant = Cat);
   end Bad_Genetics;

The declaration of ``Mutant`` is illegal, since the ``xor`` operator is only
available for Boolean and unsigned integer (modular) values; it is not available
for ``Animal``.  The same restriction applies to the other bitwise operators
listed above.  If we really wanted to achieve the effect of the above code
in legal SPARK (or Ada), then the following approach will work (the type ``Unsigned_8``
is an 8-bit modular type declared in the predefined package ``Interfaces``).

:code-config:`run_button=False;prove_button=False;accumulate_code=False`

.. code:: ada prove_flow_report_all_button

    with Interfaces; use Interfaces;
    package Unethical_Genetics is
       type Animal is (Ape, Bee, Cat, Dog);
       A      : constant array (Animal) of Unsigned_8 :=
                  (Animal'Pos (Ape), Animal'Pos (Bee),
                   Animal'Pos (Cat), Animal'Pos (Dog));
       Mutant : Animal := Animal'Val (A (Bee) xor A (Dog));
       pragma Assert (Mutant = Cat);
    end Unethical_Genetics;

:code-config:`run_button=True;prove_button=False;accumulate_code=False`

Note that ``and``, ``or``, ``not`` and ``xor`` are used both as logical operators
and as bitwise operators, but there is no possible confusion between these two uses.
Indeed the use of such operators on values from modular types is a natural
generalization of their uses on Boolean, since values from modular types are often
interpreted as arrays of Booleans.

Restricting Explicit Conversions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A simple way to bypass the restrictions of Rule 10.1 is to explicitly
convert the arguments of an operation to a type that the rule allows. While
it can often be useful to cast a value from one type to another, many
casts that are allowed in C are either downright errors or poor replacements
for clearer syntax.

One example is to cast from a scalar type to Boolean. A better way to
express ``(bool)x`` is to compare ``x`` to the zero value of its type: ``x != 0``
for integers, ``x != 0.0`` for floats, ``x != `\0``` for characters, ``x !=Enum``
where ``Enum`` is the first enumerated value of the type. Thus, MISRA C
Rule 10.5 advises avoiding casting non-Boolean values to Boolean.

Rule 10.5 also advises avoiding other casts that are, at best, obscure:

- from a Boolean to any other scalar type

- from a floating-point value to an enumeration or a character

- from any scalar type to an enumeration

The rules are not symmetric, so although a float should not be cast to
an enum, casting an enum to a float is allowed. Similarly, although it is
advised to not cast a character to an enum, casting an enum
to a character is allowed.

The rules in SPARK are simpler. There are no conversions between numeric types
(integers, fixed-point and floating-point) and non-numeric types (such as Boolean,
Character, and other enumeration types). Conversions between different
non-numeric types are limited to those that make semantic sense, for example
between a derived type and its parent type. Any numeric type can be converted to
any other numeric type, with precise
rules for rounding/truncating values when needed and run-time checking that the
converted value is in the range associated with the target type.

Restricting Implicit Conversions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Rules 10.1 and 10.5 restrict operations on types and explicit
conversions. That's not enough to avoid problematic C programs; a program
violating one of these rules can be expressed using only implicit type
conversions. For example, the Shakespearian code in section
:ref:`Boolean Operations on Boolean` can be reformulated to
satisfy both Rules 10.1 and 10.5:

.. code:: c

   !main.c
   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      typedef enum {Ape, Bee, Cat} Animal;
      int b = Bee;
      bool t = 2 * b;
      bool answer = t || ! t;
      printf("two bee or not two bee? %d\n", answer);
      return 0;
   }

Here, we're implicitly converting the enumerated value ``Bee`` to an int,
and then implicitly converting the integer value ``2 * b`` to a Boolean.
This does not violate 10.1 or 10.5, but it is prohibited by
MISRA C Rule 10.3:  `"The value of an
expression shall not be assigned to an object with a narrower essential type or
of a different essential type category"`.

Rule 10.1 also does not prevent arguments of an operation from being
inconsistent, for example comparing a floating-point value and an enumerated
value. But MISRA C Rule 10.4 handles this situation:
`"Both operands of an operator in which the usual arithmetic
conversions are performed shall have the same essential type category"`.

In addition, three rules in the "Composite operators and
expressions" section avoid common mistakes related to the combination of
explicit/implicit conversions and operations.

The rules in SPARK (and Ada) are far simpler: there are no implicit conversions! This
applies both between types of a different `essential type category` as MISRA C
puts it, as well as between types that are structurally the same but declared as
different types.

.. code:: ada
    :class: ada-expect-compile-error

    procedure Bad_Conversions is
       pragma Warnings (Off);
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       type My_Animal is new Animal; -- derived type
       A : Animal := Cat;
       M : My_Animal := Bee;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := I;  --  ERROR
       I := A;  --  ERROR
       A := B;  --  ERROR
       M := A;  --  ERROR
       B := C;  --  ERROR
       C := F;  --  ERROR
    end Bad_Conversions;

The compiler reports a mismatch on every statement in the above procedure
(the declarations are all legal).

Adding explicit conversions makes the assignments to F and M valid,
since SPARK (and Ada) allow conversions between numeric types and between a derived
type and its parent typ, but all other conversions are illegal:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Bad_Conversions is
       pragma Warnings (Off);
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       type My_Animal is new Animal; -- derived type
       A : Animal := Cat;
       M : My_Animal := Bee;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := Float (I);       --  OK
       I := Integer (A);     --  ERROR
       A := Animal (B);      --  ERROR
       M := My_Animal (A);   --  OK
       B := Boolean (C);     --  ERROR
       C := Character (F);   --  ERROR
    end Bad_Conversions;

Although an enumeration value cannot be converted to an integer (or *vice
versa*) either implicitly or explicitly, SPARK (and Ada) provide functions
to obtain the effect of a type conversion. For any enumeration type ``T``,
the function ``T'Pos(e)`` takes an enumeration value from type ``T``
and returns its relative position as an integer, starting at ``0``.
For example, ``Animal'Pos(Bee)`` is ``1``, and ``Boolean'Pos(False)``
is ``0``. In the other direction, ``T'Val(n)``, where ``n`` is an integer,
returns the enumeration value in type ``T`` at relative position ``n``.
If ``n`` is negative or greater then ``T'Pos(T'Last)`` then a run-time
exception is raised.

Hence, the following is valid SPARK (and Ada) code; ``Character`` is defined as
an enumeration type:

.. code:: ada

    procedure Ok_Conversions is
       pragma Warnings (Off);
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       type My_Animal is new Animal;
       A : Animal := Cat;
       M : My_Animal := Bee;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := Float (I);
       I := Animal'Pos (A);
       I := My_Animal'Pos (M);
       I := Boolean'Pos (B);
       I := Character'Pos (C);
       I := Integer (F);
       A := Animal'Val(2);
    end Ok_Conversions;
