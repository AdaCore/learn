:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Retrofitting Strong Typing Rules
--------------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

The Annex C of MISRA-C:2012 document puts it in the clearest terms:

  `"ISO C may be considered to exhibit poor type safety as it permits a wide
  range of implicit type conversions to take place. These type conversions can
  compromise safety as their implementation-defined aspects can cause developer
  confusion."`

The most severe consequences come from inappropriate conversions involving
pointer types, as they result in general in memory safety violations. Two
sections of MISRA-C are dedicated to these issues: a section on "Pointer type
conversions" (9 rules) and a section on "Pointers and arrays" (8 rules).

Inappropriate conversions between scalar type are only slightly less severe, as
they may introduce arbitrary violations of the intended functionality. MISRA-C
has gone to great length to improve the situation here, by defining a stricter
type system on top of the C language, which is described in Appendix D of
MISRA-C:2012 and in the dedicated section on "The essential type model" (8
rules).

Enforcing Strong Typing for Pointers
************************************

Pointers in C provide a low-level view of the addressable memory as a set of
integer addresses. Want to write at address 42? Just say so:

.. code-block:: c

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
warning disappears when converting explicitly value 42 to the target pointer
type, although the problem is still present.

Beyond their ability to denote memory addresses, pointers are also used in C to
pass references to inputs and outputs to function calls, to construct complex
data structures with indirection or sharing, and to denote arrays of
elements. Pointers are thus at once pervasive, powerful and fragile.

Pointers Are Not Addresses
^^^^^^^^^^^^^^^^^^^^^^^^^^

In an attempt to rule out issues that come from direct addressing of memory
with pointers, MISRA-C states in Rule 11.4 that `"A conversion should not be
performed between a pointer to object and an integer type"`. As this rule is
classified as only Advisory, MISRA-C completes it with two Required rules that
state: `"A cast shall not be performed between pointer to void and an
arithmetic type"` (Rule 11.6) and `"A cast shall not be performed between
pointer to object and a non-integer arithmetic type"` (Rule 11.7).

In Ada, pointers are not addresses, and addresses are not integers. An opaque
standard type ``System.Address`` defines addresses, and conversions to/from
integers are provided by standard unit ``System.Storage_Elements``. The
previous wrong C code can be written as follows in Ada:

.. code:: ada

    with System;
    with System.Storage_Elements;

    procedure Pointer is
       A : constant System.Address := System.Storage_Elements.To_Address (42);
       M : aliased Integer with Address => A;
       P : access Integer := M'Access;
    begin
       P.all := 0;
    end Pointer;

The integer value 42 is converted into a memory address ``A`` by calling
``System.Storage_Elements.To_Address``, which is then used as the address of
integer variable ``M``, and pointer variable ``P`` is set to point to ``M``
(which is only allowed because ``M`` is declared as ``aliased``).

Note that it requires far more typing to convince the compiler in Ada that this
is a valid program, which hopefully gives more time for programmers to rethink
their typing (pun intended) if it's incorrect.

We only mentioned Ada above because SPARK does not support pointers. But SPARK
allows addresses.

Pointers Are Not References
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Passing parameters by reference is critical for efficient programs, but the
absence of references distinct from pointers in C means that this incurs a
serious risk in C. Any parameter of pointer type can be copied freely to a
variable whose lifetime is larger than the memory pointed to, a problem known
as dangling pointers. MISRA-C forbids such uses in Rule 18.6: `"The address of
an object with automatic storage shall not be copied to another object that
persists after the first object has ceased to exist"`. Unfortunately, enforcing
this rule is difficult, as it is one of the undecidable ones.

In SPARK, parameters can be passed by reference, but no pointer to the
parameter can be stored, which completely solves this issue. In fact, the
decision to pass a parameter by copy or by reference lays in many cases with
the compiler, but such compiler dependency has no effect on the functional
behavior of a SPARK program. For example, the compiler may decide to pass
parameter ``P`` of procedure ``Rotate_X`` by copy or by parameter, but in any
case the postcondition of ``Rotate_X`` will hold: the resulting value of ``P``
will be modified by rotation around the ``X`` axis.

.. code:: ada spark-prove

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

SPARK analysis can mathematically prove this fact, and it issues a message that
the postcondition is proved here.

Pointers Are Not Arrays
^^^^^^^^^^^^^^^^^^^^^^^

The greatest source of vulnerabilities regarding pointers is their use as
substitutes for arrays. Although the C language has a syntax for declaring and
accessing arrays, this is just a thin syntactic layer on top of pointers. From
that follows that array access is just pointer arithmetic, array length must be
separately passed around by the programmer, and all the vulnerabilities
originating in that confusion of pointers and arrays, like buffer overflows.

Consider a function that counts the number of times a value is present in an
array. In C, this could be written:

.. code-block:: c

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
pointer ``p``. The critical property that ``len`` parameter is a valid length
for an array of integers pointed to by parameter ``p`` rests completely with
the caller of ``count``, and ``count`` has no way to check that this is
true.

To mitigate the risks associated with pointers being used for arrays, MISRA-C
contains 8 rules in a section on "Pointers and arrays", which in particular
forbid pointer arithmetic (Rule 18.4) or, if this Advisory rule is not
followed, require pointer arithmetic to stay within bounds (Rule 18.1). But,
even if we rewrite the loop in ``count`` to respect all decidable MISRA-C
rules, the complete dependency of ``count`` on its caller for passing a correct
value of ``len`` remains:

.. code-block:: c

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
undecidable MISRA-C Rules 18.1 (pointer arithmetic should stay within bounds)
and 1.3 (no undefined behavior). Contrast this with the same function in SPARK:

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
       P : Int_Array := (0, 3, 9, 3, 3);
       C : Integer := Count (P, 3);
    begin
       Put_Line ("value 3 is seen" & C'Img & "times in p");
    end Test_Count;

Here, array parameter ``P`` contains its own length ``P'Length`` as well as its
first index ``P'First`` and last index ``P'Last``, so function ``Count`` can
simply loop over the range of valid array indexes ``P'First .. P'Last`` (or
``P'Range`` for short). As a result, function ``Count`` can be verified in
isolation to be free of vulnerabilities such as buffer overflows, as it does
not depend on the values of parameters passed by its callers. In fact, we can
go further in SPARK and show that the value returned by ``Count`` is no greater
than the length of parameter ``P`` by stating this property in postcondition of
``Count`` and asking SPARK analysis to prove it:

.. code:: ada spark-prove

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

The only help that SPARK analysis required from us is to state how ``Count``
evolves at each iteration in a loop invariant (a special kind of assertion).

Pointers Should Be Typed
^^^^^^^^^^^^^^^^^^^^^^^^

The C language defines a special pointer type ``void*`` that corresponds to an
untyped pointer. It is legal to convert any pointer type to and from ``void*``,
which makes it a convenient replacement for templates. Consider the following
code which indirectly applies ``assign_int`` to integer ``i`` and
``assign_float`` to floating-point ``f`` by calling ``assign`` on both:

.. code-block:: c

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

Variables ``i`` and ``f`` are implicitly converted to ``void*`` type as a way
to apply ``assign`` to any second parameter ``p`` whose type matches the
argument type of its first argument ``fun``. The use of an untyped argument
means that the responsibility for the correction of typing rests completely
with programmers. Switch variables ``i`` and ``f`` in the calls to ``assign``
and you still get a compilable program without warnings, that runs and produces
completely bogus output::

  i = 1109917696; f = 0.000000

instead of the expected::

  i = 42; f = 42.000000

It is possible to use generics in SPARK to obtain the same result in a fully
typed way, where procedure ``Assign`` applies its parameter procedure
``Initialize`` to its parameter ``V``:

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

The generic procedure ``Assign`` must be instantiated with specific values of
type ``T`` and parameter procedure ``Initialize``, and the resulting
instantiated procedures apply to a unique possible type. So switching ``I`` and
``F`` here results in a compiler error.

.. _Enforcing Strong Typing for Scalars:

Enforcing Strong Typing for Scalars
***********************************

In C, all scalar types can be converted both implicitly and explicitly to any
other scalar type. The process for doing that is defined by the rules of
`promotion` and `conversion`, which trick even experts. See the :ref:`Preface`
for an example which tricked the MISRA-C committee. For another example, see
`this article introducing a safe library for manipulating scalars
<https://msdn.microsoft.com/en-us/library/ms972705.aspx>`_ by Microsoft expert
David LeBlanc. In its conclusion, the author acknowledges the inherent
difficulty in understanding scalar type conversions in C, by showing an early
buggy version of the code he wrote to produce the minimum signed integer:

.. code-block:: c

   return (T)(1 << (BitCount()-1));

The issue here is that the literal 1 on the left-hand side of the shift is an
``int``, so on a 64-bit machine with 32-bit ``int`` and 64-bit type ``T``, the
above is shifting 32-bit value 1 by 63 bits, a case of undefined behavior
producing an unexpected output with Microsoft compiler. The fix is to convert
first literal 1 to ``T`` before the shift:

.. code-block:: c

   return (T)((T)1 << (BitCount()-1));

Although he'd asked some excellent programmers to review the code, no one found
this problem. Did you?

To avoid as much as possible these issues, MISRA-C defines its own type system
on top of C types, presented in the section on "The essential type model" (8
rules). It can be seen as additional typing rules, as all rules in this section
are decidable, and can be decided at the scope of a single translation
unit. These rules forbid in particular the two tricky cases of confusion that
we mentioned above. They can be divided in three blocks of rules for:

* restricting operations on types

* restricting explicit conversions

* restricting implicit conversions

Restricting Operations on Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Apart from the application of some operations to floating-point arguments (the
bitwise, mod and array access operations) which are invalid and reported by the
compiler, all operations apply to all scalar types in C. MISRA-C Rule 10.1
constrains the types on which each operation is possible as follows.

Arithmetic Operations on Arithmetic Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ever tried to add two Booleans or an Apple and an Orange? Let's do it in C:

.. code-block:: c

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
above code. Variable ``b3`` and ``f3`` both end up with value 1. Of course it
makes no sense to add up Boolean or enumerated values, which is why MISRA-C
Rule 18.1 forbids the use of all arithmetic operations on Boolean and
enumerated values, while forbidding most arithmetic operations on
characters. That leaves the use of arithmetic operations for signed or unsigned
integers as well as floating-point types.

Let's try to do the same in SPARK:

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Arith is

       B1 : Boolean := True;
       B2 : Boolean := False;
       B3 : Boolean := B1 + B2;

       type Fruit is (Apple, Orange);
       F1 : Fruit := Apple;
       F2 : Fruit := Orange;
       F3 : Fruit := F1 + F2;

    end Bad_Arith;

The compiler reports that there is no applicable operator in both cases. It is
possible however to get the predecessor of a Boolean or enumerated value with
``Value'Pred`` and its successor with ``Value'Succ``, as well as to iterate
over all values of the type:

.. code:: ada spark-prove

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Ok_Arith is

       B1 : Boolean := False;
       B2 : Boolean := Boolean'Succ (B1);
       B3 : Boolean := Boolean'Pred (B2);

       type Fruit is (Apple, Orange);
       F1 : Fruit := Apple;
       F2 : Fruit := Fruit'Succ (F1);
       F3 : Fruit := Fruit'Pred (F2);

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

Modular Operation on Integers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Do you wonder if a false statement is a round number of true ones, or if a
pineapple can be divided evenly in oranges? Again, we can ask the question in
C:

.. code-block:: c

   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      bool b1 = true;
      bool b2 = false;
      bool b3 = b2 % b1;

      typedef enum {Apple, Orange, Pineapple} fruit;
      fruit f1 = Orange;
      fruit f2 = Pineapple;
      fruit f3 = f2 % f1;

      printf("b3 = %d; f3 = %d\n", b3, f3);

      return 0;
   }

There are no compiler errors and no undefined behavior in the above
code. Variable ``b3`` and ``f3`` both end up with value 0, showing that a false
statement is indeed a round number of true ones and that a pineapple can be
divided evenly in oranges. Like before, both the questions and the answers make
no sense, which is why MISRA-C Rule 18.1 forbids the use of modulo operation on
Boolean, character and enumerated values. That leaves the use of modulo
operation for signed or unsigned integers.

Let's try to do the same in SPARK, where the modulo operator is called ``mod``:

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Modulo is

       B1 : Boolean := True;
       B2 : Boolean := False;
       B3 : Boolean := B2 mod B1;

       type Fruit is (Apple, Orange, Pineapple);
       F1 : Fruit := Orange;
       F2 : Fruit := Pineapple;
       F3 : Fruit := F2 mod F1;

    end Bad_Modulo;

The compiler reports that there is no applicable operator in both cases.

No Comparison Operation on Boolean
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Is truth greater than falsity? Probably in moral, but not necessarily in
programs, where these values have symmetric roles. Yet, the C language imposes
an ordering on Boolean values inherited from their identification with
integers, ``false`` being the same as integer 0 and ``true`` being the same as
integer 1:

.. code-block:: c

   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      bool ff = false <= false;
      bool ft = false <= true;
      bool tf = true <= false;
      bool tt = true <= true;

      printf("false implies false? %d\n", ff);
      printf("false implies true? %d\n", ft);
      printf("true implies false? %d\n", tf);
      printf("true implies true? %d\n", tt);

      return 0;
   }

The above code shows the so-called truth table of the logical implication
operator, which is paradoxically identified with the less-than-or-equal (which
unfortunately resembles graphically a reverse implication arrow) on Boolean
values. This is rather obscure, which is why MISRA-C Rule 18.1 forbids the use
of ordering operations on Boolean values. That leaves the use of ordering
operations for all other scalar types.

This is one case where SPARK adopts the same convention as C of ordering the
false and true values, so the above code can be also be expressed in SPARK:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Truth_Table is
       FF : Boolean := False <= False;
       FT : Boolean := False <= True;
       TF : Boolean := True <= False;
       TT : Boolean := True <= True;
    begin
       Put_Line ("false implies false? " & FF'Img);
       Put_Line ("false implies true? " & FT'Img);
       Put_Line ("true implies false? " & TF'Img);
       Put_Line ("true implies true? " & TT'Img);
    end Truth_Table;

.. _Boolean Operations on Boolean:

Boolean Operations on Boolean
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Two bee or not two bee? Let's try to C:

.. code-block:: c

   #include <stdbool.h>
   #include <stdio.h>

   int main() {
      typedef enum {Ape, Bee, Cat} Animal;
      bool answer = (2 * Bee) || ! (2 * Bee);
      printf("two bee or not two bee? %d\n", answer);
      return 0;
   }

Did you guess the answer? It's 1 of course! Which is the correct logical answer
to Shakespeare's existential questioning, as it reduces to ``A or not A`` which
is true in classical logic.

We saw previously that MISRA-C forbids the use of the multiplication operator
with an operand of enumerated type like ``Bee``. It also forbids in Rule 18.1
the use of Boolean operations and/or/not (in C: ``&&``, ``||``, ``!``) on
anything else than Boolean operands, as misused in our Shakespearian code
above.

Let's try to do the same in SPARK, where the Boolean operators are called
``and``, ``or``, ``not`` (for the strict operators that always evaluate both
operands); ``and then``, ``or else`` (for the shortcut operators):

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Hamlet is
       type Animal is (Ape, Bee, Cat);
       Answer : Boolean := Bee or not Bee;
    end Bad_Hamlet;

As expected, the compiler reports that there is no applicable operator.

Bitwise Operations on Unsigned Integers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fancy genetic engineering? Look at how one can transform a Bee into a Cat by
manipulating individual genes (really, bits) of their matrix:

.. code-block:: c

   #include <stdbool.h>
   #include <assert.h>

   int main() {
      typedef enum {Ape, Bee, Cat} Animal;
      Animal mutant = Bee << 1;
      assert (mutant == Cat);
      return 0;
   }

This genetic algorithm works by accessing the underlying bitwise representation
of a ``Bee`` and transforming it into the underlying bitwise representation of
a ``Cat``. While very powerful, fiddling with the bitwise representation of
values is best left to computations on the so-called unsigned integers as
beautifully done in the well-known book `Hacker's Delight
<http://www.hackersdelight.org/>`_. For that reason, MISRA-C Rule 18.1 forbids
the use of all bitwise operations on anything else than unsigned integers.

Let's try to do the same in SPARK, where the bitwise operators are called
``and``, ``or``, ``xor``, ``not``, ``Shift_Left``, ``Shift_Right``,
``Shift_Right_Arithmetic``, ``Rotate_Left`` and ``Rotate_Right``:

.. code:: ada
    :class: ada-expect-compile-error

    package Bad_Genetics is
       type Animal is (Ape, Bee, Cat);
       function Shift_Left (A : Animal; V : Natural) return Animal
         with Import, Convention => Intrinsic;
       Mutant : Animal := Shift_Left (Bee, 1);
    end Bad_Genetics;

Operator ``Shift_Left`` must be declared explicitly for it to be available for
a type. This is to no use here, as the compiler reports that ``Shift_Left``
cannot be used on an enumerated type like ``Animal``. All the previously
mentioned operators are available for unsigned integers only in SPARK, also
called `modular` types.

Note that ``and``, ``or``, ``not`` are used both as logical operators and as
bitwise operators, but there is no possible confusion between these two uses in
SPARK, as logical operators apply only to Boolean, bitwise operators apply only
to modular types, and there are no implicit conversions between these two
types.

Restricting Explicit Conversions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A simple way to bypass the restrictions of Rule 10.1 consists in explicitly
converting the arguments of an operation to a type allowed by Rule 10.1. While
it can make sense sometimes to cast a value from one type to another, many
casts that are allowed in C make no sense in general, or are poor replacements
for clearer syntax.

Consider the case of casting from any scalar type to Boolean. A better way to
express ``(bool)x`` is to compare ``x`` to the null value of its type: ``x !=
0`` for integers, ``x != 0.0`` for floats, ``x != `\0``` for characters, ``x !=
Enum`` where ``Enum`` is the first enumerated value of the type. Thus, MISRA-C
Rule 10.5 advises to avoid casting non-Boolean values to Boolean.

Rule 10.5 also advises to avoid nonsensical casts:

- from a Boolean to any other scalar type

- from a floating-point value to an enumeration or a character

- from any scalar type to an enumeration

The rules are not symmetric, so although it is advised not to cast a float into
an enum, it is allowed to cast an enum into a float. Similarly, it is advised
not to cast a character into an enum, it is allowed to cast an enum into a
character.

The rules in SPARK are simpler. There are no conversions between numeric types
(integers, fixed-point and floating-point) and non-numeric types (Boolean,
characters, enumerations). There are no conversions between different
non-numeric types. Any numeric type can be converted to any other numeric
type. So the rules are symmetric and restricted to numeric types, with precise
rules for rounding/truncating values when needed and runtime checks that the
conversion is meaningful for the given value.

Restricting Implicit Conversions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We have seen that Rules 10.1 and 10.5 restrict operations on types and explicit
conversions. That's not enough to avoid problematic C programs, as any program
violating one of these rules can be reexpressed using only implicit type
conversions. Take for example the Shakespearian code we saw in section
:ref:`Boolean Operations on Boolean`. It can be reexpressed in a way that
satisfies both Rules 10.1 and 10.5:

.. code-block:: c

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

Here, we're implicitly converting the enumerated value ``Bee`` to an integer,
and then implicitly converting the integer value ``2 * b`` to a Boolean. Both
of these are forbidden by MISRA-C Rule 10.3 stating that `"The value of an
expression shall not be assigned to an object with a narrower essential type or
of a different essential type category"`.

Rule 10.1 also does not prevent arguments of an operation to be somwhat at
odds, for example comparing a floating-point value and an enumerated
value. Hence MISRA-C Rule 10.4 forces some type consistency between arguments,
stating that `"Both operands of an operator in which the usual arithmetic
conversions are performed shall have the same essential type category"`.

In addition, MISRA-C contains 3 rules in a section on "Composite operators and
expressions" to avoid common mistakes related to the combination of
explicit/implicit conversions and operations.

The rules in SPARK are far simpler: there are no implicit conversions! This
applies both between types of a different `essential type category` as MISRA-C
puts it, as well as between types that are essentially the same but defined as
different.

.. code:: ada
    :class: ada-expect-compile-error

    procedure Bad_Conversions is
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       E : Animal := Cat;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := I;
       I := E;
       E := B;
       B := C;
       C := F;
    end Bad_Conversions;

The compiler reports a mismatch on every line in the above procedure. Adding
explicit conversions only makes the first line valid, as SPARK only allows
converting between numeric types:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Bad_Conversions is
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       E : Animal := Cat;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := Float (I);
       I := Integer (E);
       E := Animal (B);
       B := Boolean (C);
       C := Character (F);
    end Bad_Conversions;

However, it is possible to get the position of an enumerated value in the
enumeration with attribute ``Pos`` which starts from value 0. This applies to
user-defined enumerations like ``Animal`` above, as well as Boolean (defined as
an enumeration with values ``False`` and ``True``) and characters (defined as
an enumeration with character values). Hence, the following is valid SPARK
code:

.. code:: ada

    procedure Ok_Conversions is
       F : Float := 0.0;
       I : Integer := 0;
       type Animal is (Ape, Bee, Cat);
       E : Animal := Cat;
       B : Boolean := True;
       C : Character := 'a';
    begin
       F := Float (I);
       I := Animal'Pos (E);
       I := Boolean'Pos (B);
       I := Character'Pos (C);
       I := Integer (F);
    end Ok_Conversions;
