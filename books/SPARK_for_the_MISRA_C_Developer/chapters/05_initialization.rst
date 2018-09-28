:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Initializing Data Before Use
----------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

As in many other languages, data is not always initialized at declaration in C,
which makes it possible to unintentionally read uninitialized data. This is a
case of undefined behavior, which can sometimes be used to attack the program
in various ways.

.. _Detecting Read of Uninitialized Data:

Detecting Read of Uninitialized Data
************************************

MISRA-C attempts to prevent reads of uninitialized data in a specific section
on "Initialization" containing 5 rules. The most important is Rule 9.1 stating
that "`The value of an object with automatic storage duration shall not be read
before it has been set`". The first example in the rule is interesting, as it
shows a non-trivial (and common) case of conditional initialization, where a
function ``f`` initializes an output parameter ``p`` only in come cases, and
the caller ``g`` of ``f`` ends up reading the value of the variable ``u``
passed in argument to ``f`` in cases where it has not been initialized:

.. code-block:: c

   static void f ( bool_t b, uint16_t *p )
   {
     if ( b )
     {
        *p = 3U;
     }
   }

   static void g (void)
   {
      uint16_t u;

      f ( false, &u );

      if ( u == 3U )
      {
         /* Non-compliant use - "u" has not been assigned a value. */
      }
   }

Detecting the violation of Rule 9.1 can be arbitrarily complex, as the program
points corresponding to initialization and read can be separated by many calls
and conditions. This rule is one of the undecidable ones, for which most
MISRA-C checkers won't detect all violations.

In SPARK, the guarantee that all reads are to initialized data is provided by a
part of the tool called `flow analysis`. Every subprogram is analyzed
separately to check that it cannot read uninitialized data. To make this
modular analysis possible, SPARK programs should respect the following
constraints:

- all inputs of a subprogram should be initialized on subprogram entry

- all outputs of a subprogram should be initialized on subprogram exit

Hence, the SPARK analysis tool called GNATprove reports on the following code
translated from C that function ``F`` might not always initialize output
parameter ``P``:

.. code:: ada spark-flow

    with Interfaces; use Interfaces;

    package Init is
       procedure F (B : Boolean; P : out Unsigned_16);
       procedure G;
    end Init;

    package body Init is

       procedure F (B : Boolean; P : out Unsigned_16) is
       begin
          if B then
             P := 3;
          end if;
       end F;

       procedure G is
          U : Unsigned_16;
       begin
          F (False, U);

          if U = 3 then
             null;
          end if;
       end G;

    end Init;

Let's fix the program by initializing ``P`` to value 0 when condition ``B`` is
not satisfied:

.. code:: ada spark-flow

    with Interfaces; use Interfaces;

    package Init is
       procedure F (B : Boolean; P : out Unsigned_16);
       procedure G;
    end Init;

    package body Init is

       procedure F (B : Boolean; P : out Unsigned_16) is
       begin
          if B then
             P := 3;
          else
             P := 0;
          end if;
       end F;

       procedure G is
          U : Unsigned_16;
       begin
          F (False, U);

          if U = 3 then
             null;
          end if;
       end G;

    end Init;

GNATprove does not report any more check messages for possible reads of
uninitialized data. On the contrary it confirms that all reads are made to
initialized data.

Contrary to C, SPARK does not guarantee that global data (called
`library-level` data in SPARK) is zero-initialized at program startup. Instead,
GNATprove checks that all global data is explicitly initialized (at declaration
or elsewhere) before it is read. Hence it goes beyond MISRA-C Rule 9.1 which
considers global data as always initialized, even if the default value of
all-zeros might not be valid data for the application! Consider a variant of
the above code where variable ``U`` is now global:

.. code:: ada spark-flow

    with Interfaces; use Interfaces;

    package Init is
       U : Unsigned_16;
       procedure F (B : Boolean);
       procedure G;
    end Init;

    package body Init is

       procedure F (B : Boolean) is
       begin
          if B then
             U := 3;
          end if;
       end F;

       procedure G is
       begin
          F (False);

          if U = 3 then
             null;
          end if;
       end G;

    end Init;

    with Init;

    procedure Call_Init is
    begin
       Init.G;
    end Call_Init;

GNATprove reports here that variable ``U`` might not be initialized at program
startup, which is indeed the case here. It reports this issue on the main
program ``Call_Init`` because its analysis showed that ``F`` needs to take
``U`` as an initialized input (since ``F`` is not initializing ``U`` on all
paths, ``U`` keeps its value on the other path, which needs to be an
initialized value), which means that ``G`` which calls ``F`` also needs to take
``U`` as an initialized input, which in turn means that ``Call_Init`` which
calls ``G`` also needs to take ``U`` as an initialized input. At this point,
we've reached the main program, so the initialization phase called
`elaboration` in SPARK should have taken care of initializing ``U``, which is
not the case here, hence the message from GNATprove.

It is possible in SPARK to specify that ``G`` should initialize variable ``U``
with a `data dependency` contract introduced with aspect ``Global`` following
the declaration of procedure ``G``:

.. code:: ada spark-flow

    with Interfaces; use Interfaces;

    package Init is
       U : Unsigned_16;
       procedure F (B : Boolean);
       procedure G with Global => (Output => U);
    end Init;

    package body Init is

       procedure F (B : Boolean) is
       begin
          if B then
             U := 3;
          end if;
       end F;

       procedure G is
       begin
          F (False);

          if U = 3 then
             null;
          end if;
       end G;

    end Init;

    with Init;

    procedure Call_Init is
    begin
       Init.G;
    end Call_Init;

In that case, GNATprove reports the error on the call to ``F`` in ``G``, as it
knows at this point that ``F`` needs ``U`` to be initialized but the calling
context in ``G`` cannot provide that guarantee. If we provide the same data
dependency contract for ``F``, then GNATprove reports the error on ``F``
itself, similarly to what we saw for an output parameter ``U``.

Detecting Partial or Redundant Initialization of Arrays and Structures
**********************************************************************

The other rules in the section on "Initialization" deal with common errors with
initializing aggregates and `designated initializers` in C99 to initialize a
structure or array at declaration. These rules attempt to patch holes created
by the lax syntax and rules in C standard. For example, here are five valid
initializations of an array of 10 elements in C:

.. code-block:: c

   int main() {
      int a[10] = {0};
      int b[10] = {0, 0};
      int c[10] = {0, [8] = 0};
      int d[10] = {0, [8] = 0, 0};
      int e[10] = {0, [8] = 0, 0, [8] = 1};
      return 0;
   }

Only ``a`` is fully initialized to all-zeros in the above code snippet. MISRA-C
Rule 9.3 thus forbids all other declarations by stating that `"Arrays shall not
be partially initialized"`. In addition, MISRA-C Rule 9.4 forbids the
declaration of ``e`` by stating that `"An element of an object shall not be
initialised more than once"` (in ``e``'s declaration, the element at index 8 is
initialized twice).

The same holds for initialization of structures. Here is an equivalent set of
declarations with the same potential issues:

.. code-block:: c

   int main() {
     typedef struct { int x; int y; int z; } rec;
      rec a = {0};
      rec b = {0, 0};
      rec c = {0, .y = 0};
      rec d = {0, .y = 0, 0};
      rec e = {0, .y = 0, 0, .y = 1};
      return 0;
   }

Here only ``a``, ``d`` and ``e`` are fully initialized. MISRA-C Rule 9.3 thus
forbids the declarations of ``b`` and ``c``. In addition, MISRA-C Rule 9.4
forbids the declaration of ``e``.

In SPARK, the aggregate used to initialize an array or a record should fully
match the components of the array or record. Violations lead to compilation
errors, both for records:

.. code:: ada
    :class: ada-expect-compile-error

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1);
    end Init_Record;

and for arrays:

.. code:: ada

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 => 1);
    end Init_Array;

Similarly, redundant initialization leads to compilation errors for records:

.. code:: ada
    :class: ada-expect-compile-error

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1, Y => 1, Z => 1, X => 2);
    end Init_Record;

and for arrays:

.. code:: ada
    :class: ada-expect-compile-error

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 .. 8 => 1, 9 .. 10 => 2, 7 => 3);
    end Init_Array;

Finally, while it is legal in Ada to leave uninitialized parts in a record or
array aggregate by using the box notation (meaning that the default
initialization of the type is used, which may be no initialization at all),
SPARK analysis rejects such use when it leads to components not being
initialized, both for records:

.. code:: ada

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1, others => <>);
    end Init_Record;

and for arrays:

.. code:: ada

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 .. 8 => 1, 9 .. 10 => <>);
    end Init_Array;
