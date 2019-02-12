:code-config:`run_button=False;prove_button=False;accumulate_code=False`

Initializing Data Before Use
----------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

As with most programming languages, C does not require that variables be initialized at
their declaration, which makes it possible to unintentionally read
uninitialized data. This is a case of undefined behavior, which can sometimes
be used to attack the program.

.. _Detecting Read of Uninitialized Data:

Detecting Reads of Uninitialized Data
*************************************

MISRA C attempts to prevent reads of uninitialized data in a specific section
on "Initialization", containing five rules. The most important is Rule 9.1:
"`The value of an object with automatic storage duration shall not be read
before it has been set`". The first example in the rule is interesting, as it
shows a non-trivial (and common) case of conditional initialization, where a
function ``f`` initializes an output parameter ``p`` only in some cases, and
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
points corresponding to a variable's initialization and read can be separated
by many calls and conditions. This is one of the undecidable rules, for which most
MISRA C checkers won't detect all violations.

In SPARK, the guarantee that all reads are to initialized data is enforced by
the SPARK analysis tool, GNATprove, through what is referred to as
`flow analysis`. Every subprogram is analyzed
separately to check that it cannot read uninitialized data. To make this
modular analysis possible, SPARK programs need to respect the following
constraints:

- all inputs of a subprogram should be initialized on subprogram entry

- all outputs of a subprogram should be initialized on subprogram return

Hence, given the following code translated from C, GNATprove reports that
function ``F`` might not always initialize output parameter ``P``:

.. code:: ada prove_flow_button

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

We can correct the program by initializing ``P`` to value 0 when condition ``B`` is
not satisfied:

.. code:: ada prove_flow_button

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

GNATprove now does not report any possible reads of uninitialized data.
On the contrary, it confirms that all reads are made from initialized data.

In contrast with C, SPARK does not guarantee that global data (called
`library-level` data in SPARK and Ada) is zero-initialized at program startup. Instead,
GNATprove checks that all global data is explicitly initialized (at declaration
or elsewhere) before it is read. Hence it goes beyond the MISRA C Rule 9.1, which
considers global data as always initialized even if the default value of
all-zeros might not be valid data for the application. Here's a variation of
the above code where variable ``U`` is now global:

.. code:: ada prove_flow_button

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
we've reached the main program, so the initialization phase (referred to as
`elaboration` in SPARK and Ada) should have taken care of initializing ``U``.
This is not the case here, hence the message from GNATprove.

It is possible in SPARK to specify that ``G`` should initialize variable ``U``;
this is done with a `data dependency` contract introduced with aspect ``Global``
following the declaration of procedure ``G``:

.. code:: ada prove_flow_button

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

GNATprove reports the error on the call to ``F`` in ``G``, as it
knows at this point that ``F`` needs ``U`` to be initialized but the calling
context in ``G`` cannot provide that guarantee. If we provide the same data
dependency contract for ``F``, then GNATprove reports the error on ``F``
itself, similarly to what we saw for an output parameter ``U``.

Detecting Partial or Redundant Initialization of Arrays and Structures
**********************************************************************

The other rules in the section on "Initialization" deal with common errors in
initializing aggregates and `designated initializers` in C99 to initialize a
structure or array at declaration. These rules attempt to patch holes created
by the lax syntax and rules in C standard. For example, here are five valid
initializations of an array of 10 elements in C:

.. code:: c run_button

   !main.c
   int main() {
      int a[10] = {0};
      int b[10] = {0, 0};
      int c[10] = {0, [8] = 0};
      int d[10] = {0, [8] = 0, 0};
      int e[10] = {0, [8] = 0, 0, [8] = 1};
      return 0;
   }

Only ``a`` is fully initialized to all-zeros in the above code snippet. MISRA C
Rule 9.3 thus forbids all other declarations by stating that `"Arrays shall not
be partially initialized"`. In addition, MISRA C Rule 9.4 forbids the
declaration of ``e`` by stating that `"An element of an object shall not be
initialised more than once"` (in ``e``'s declaration, the element at index 8 is
initialized twice).

The same holds for initialization of structures. Here is an equivalent set of
declarations with the same potential issues:

.. code:: c run_button

   !main.c
   int main() {
      typedef struct { int x; int y; int z; } rec;
      rec a = {0};
      rec b = {0, 0};
      rec c = {0, .y = 0};
      rec d = {0, .y = 0, 0};
      rec e = {0, .y = 0, 0, .y = 1};
      return 0;
   }

Here only ``a``, ``d`` and ``e`` are fully initialized. MISRA C Rule 9.3 thus
forbids the declarations of ``b`` and ``c``. In addition, MISRA C Rule 9.4
forbids the declaration of ``e``.

In SPARK and Ada, the aggregate used to initialize an array or a record must fully
cover the components of the array or record. Violations lead to compilation
errors, both for records:

.. code:: ada run_button
    :class: ada-expect-compile-error

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1); -- Error, Y and Z not specified
    end Init_Record;

and for arrays:

.. code:: ada run_button

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 => 1); -- Error, elements 2..10 not specified
    end Init_Array;

Similarly, redundant initialization leads to compilation errors for records:

.. code:: ada run_button
    :class: ada-expect-compile-error

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1, Y => 1, Z => 1, X => 2); -- Error, X duplicated
    end Init_Record;

and for arrays:

.. code:: ada run_button
    :class: ada-expect-compile-error

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 .. 8 => 1, 9 .. 10 => 2, 7 => 3); -- error, A(7) duplicated
    end Init_Array;

Finally, while it is legal in Ada to leave uninitialized parts in a record or
array aggregate by using the box notation (meaning that the default
initialization of the type is used, which may be no initialization at all),
SPARK analysis rejects such use when it leads to components not being
initialized, both for records:

.. code:: ada prove_flow_button

    package Init_Record is
       type Rec is record
          X, Y, Z : Integer;
       end record;
       R : Rec := (X => 1, others => <>); -- Error, Y and Z not specified
    end Init_Record;

and for arrays:

.. code:: ada prove_flow_button

    package Init_Array is
       type Arr is array (1 .. 10) of Integer;
       A : Arr := (1 .. 8 => 1, 9 .. 10 => <>); -- Error, A(9..10) not specified
    end Init_Array;
