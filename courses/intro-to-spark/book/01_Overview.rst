:prev_state: False

:code-config:`run_button=False;prove_button=True;accumulate_code=False`

.. _SPARK Overview:

SPARK Overview
=====================================================================

.. role:: ada(code)
   :language: ada

This tutorial is an interactive introduction to the SPARK programming
language and its formal verification tools. You need not know any specific
programming language (although going over the
:doc:`../../intro-to-ada/index` first may help) or have experience in
formal verification.

For some of the sample code presented, you'll be able to compile and run
the program and/or run the formal verification tool on the program. These
are available through the buttons labelled:

- `Run`: compile the code with assertions enabled and run the executable
  produced.
- `Examine`: perform the `flow analysis` stage of formal verification
- `Prove`: perform the `proof` stage of formal verification (which includes
  `flow analysis`)

You can edit the sample code, so you can modify it and rerun the tools
to see the effect of your changes on the compilation or analysis. Use
the `Reset` button to restore the example to its initial version.


What is it?
---------------------------------------------------------------------

SPARK refers to two different things:

- a programming language targeted at functional specification and static
  verification, and
- a set of development and verification tools for that language.

The SPARK language is based on a subset of the Ada language.  Ada is
particularly well suited to formal verification since it was designed
for critical software development. SPARK builds on that foundation.

.. image:: 01_spark_ada.png
   :align: center

Version 2012 of Ada introduced the use of `aspects`, which can be used
for subprogram contracts, and version 2014 of SPARK added its own
aspects to further aid static analysis.


What do the tools do?
---------------------------------------------------------------------

We start by reviewing static verification of programs, which is
verification of the source code performed without compiling or executing
it.  Verification uses tools that perform static analysis.  These can take
various forms. They include tools that check types and enforce visibility
rules, such as the compiler, in addition to those that perform more complex
reasoning, such as abstract interpretation, as done by a tool like
`CodePeer <https://www.adacore.com/codepeer>`_ from AdaCore. The tools that
come with SPARK perform two different forms of static analysis:

- `flow analysis` is the fastest form of analysis. It checks
  initializations of variables and looks at data dependencies between
  inputs and outputs of subprograms. It can also find unused assignments
  and unmodified variables.

- `proof` checks for the absence of runtime errors as well as the
  conformance of the program with its specifications.


Key Tools
---------------------------------------------------------------------

The tool for formal verification of the SPARK language is called
`GNATprove`. It checks for conformance with the SPARK subset and performs
flow analysis and proof of the source code. Several other tools support the
SPARK language, including both the `GNAT compiler
<https://www.adacore.com/gnatpro>`_ and the `GPS integrated development
environment <https://www.adacore.com/gnatpro/toolsuite/gps>`_.


A trivial example
---------------------------------------------------------------------

We start with a simple example of a subprogram in Ada that uses SPARK
aspects to specify verifiable subprogram contracts. The subprogram, called
``Increment``, adds 1 to the value of its parameter ``X``:

.. code:: ada prove_report_all_button

   procedure Increment
     (X : in out Integer)
   with
     Global  => null,
     Depends => (X => X),
     Pre     => X < Integer'Last,
     Post    => X = X'Old + 1;

   procedure Increment
     (X : in out Integer)
   is
   begin
     X := X + 1;
   end Increment;

The contracts are written using the Ada `aspect` feature and those shown
specify several properties of this subprogram:

- The SPARK Global aspect says that ``Increment`` does not read or write
  any global variables.

- The SPARK Depend aspect is especially interesting for security: it says
  that the value of the parameter ``X`` after the call depends only on the
  (previous) value of ``X``.

- The :ada:`Pre` and :ada:`Post` aspects of Ada specify functional
  properties of ``Increment``:

   - ``Increment`` is only allowed to be called if the value of ``X`` prior
     to the call is less than :ada:`Integer'Last`. This ensures that the
     addition operation performed in the subprogram body doesn't overflow.

   - ``Increment`` does indeed perform an increment of ``X``: the value of
     ``X`` after a call is one greater than its value before the call.

GNATprove can verify all of these contracts.  In addition, it verifies
that no error can be raised at runtime when executing ``Increment``'s
body.


The Programming Language
---------------------------------------------------------------------

It's important to understand why there are differences between the SPARK
and Ada languages. The aim when designing the SPARK subset of Ada was to
create the largest possible subset of Ada that was still amenable to simple
specification and sound verification.

The most notable exclusions from Ada include exceptions and access types
(including related features such as allocators), both of which are known to
considerably increase the amount of user-written annotations required. Goto
statements and controlled types are also not supported since they introduce
non-trivial control flow. The two remaining restrictions relate to
side-effects in expressions and aliasing of names, which we now cover in
more detail.


Limitations
---------------------------------------------------------------------

No side-effects in expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SPARK language doesn't allow side-effects in expressions.  In other
words, evaluating a SPARK expression must not update any object. This
limitation is necessary to avoid unpredictable behavior that depends on
order of evaluation, parameter passing mechanisms, or compiler
optimizations. The expression for ``G`` below is non-deterministic due to
the order in which the two calls to F are evaluated.  It's therefore not
legal SPARK.

.. code:: ada prove_flow_button run_button
    :class: ada-expect-compile-error

    procedure Show_Illegal_Ada_Code is

       function F (X : in out Integer) return Integer is
          Tmp : constant Integer := X;
       begin
          X := X + 1;
          return Tmp;
       end F;

       G : Integer := 0;

    begin
       G := F (G) - F (G); -- ??
    end Show_Illegal_Ada_Code;

In fact, the code above is not even legal Ada, so the same error is
generated by the GNAT compiler. But SPARK goes further and GNATprove also
produces an error for the following equivalent code that is accepted by the
Ada compiler:

.. code:: ada prove_flow_button run_button

    procedure Show_Illegal_SPARK_Code is

       G : Integer := 0;

       function F return Integer is
          Tmp : constant Integer := G;
       begin
          G := G + 1;
          return Tmp;
       end F;

    begin
       G := F - F; -- ??
    end Show_Illegal_SPARK_Code;

The SPARK languages enforces the lack of side-effects in expressions by
forbidding side-effects in functions, which include modifications to either
parameters or global variables. As a consequence, SPARK forbids functions
with :ada:`out` or :ada:`in out` parameters in addition to functions
modifying a global variable. Function ``F`` below is illegal in
SPARK, while Function ``Incr`` might be legal if it doesn't modify any
global variables and function ``Incr_And_Log`` might be illegal if it
modifies global variables to perform logging.

.. code-block:: ada

    function F (X : in out Integer) return Integer;     -- Illegal

    function Incr (X : Integer) return Integer;         -- OK?

    function Incr_And_Log (X : Integer) return Integer; -- OK?

In most cases, you can easily replace these functions by procedures with an
:ada:`out` parameter that returns the computed value.

When it has access to function bodies, GNATprove verifies that those
functions are indeed free from side-effects. Here for example, the two
functions ``Incr`` and ``Incr_And_Log`` have the same signature, but only
``Incr`` is legal in SPARK. ``Incr_And_Log`` isn't: it attempts to update
the global variable ``Call_Count``.

.. code:: ada prove_flow_button

    package Side_Effects is

       function Incr (X : Integer) return Integer;         -- OK?

       function Incr_And_Log (X : Integer) return Integer; -- OK?

    end Side_Effects;

    package body Side_Effects is

       function Incr (X : Integer) return Integer
       is (X + 1); -- OK

       Call_Count : Natural := 0;

       function Incr_And_Log (X : Integer) return Integer is
       begin
          Call_Count := Call_Count + 1; -- Illegal
          return X + 1;
       end Incr_And_Log;

    end Side_Effects;

No aliasing of names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Another restriction imposed by the SPARK subset concerns `aliasing
<https://en.wikipedia.org/wiki/Aliasing_(computing)>`_. We say that two
names are `aliased` if they refer to the same object. There are two reasons
why aliasing is forbidden in SPARK:

- It makes verification more difficult because it requires taking into
  account the fact that modifications to variables with different names may
  actually update the same object.

- Results may seem unexpected from a user point of view. The results of a
  subprogram call may depend on compiler-specific attributes, such as
  parameter passing mechanisms, when its parameters are aliased.

Since access types (`pointers
<https://en.m.wikipedia.org/wiki/Pointer_(computer_programming)>`_ in Ada)
are not allowed in SPARK, aliasing can only occur as part of the parameter
passing that occurs in a subprogram call. Functions have no side-effects in
SPARK, so aliasing of parameters in function calls isn't problematic; we
need only consider procedure calls. When a procedure is called, SPARK
verifies that no :ada:`out` or :ada:`in out` parameter is aliased with
either another parameter of the procedure or a global variable modified in
the procedure's body.

Procedure ``Move_To_Total`` is an example where the possibility of aliasing
wasn't taken into account by the programmer:

.. code:: ada run_button prove_flow_button
    :class: ada-run-expect-failure

    procedure No_Aliasing is

       Total : Natural := 0;

       procedure Move_To_Total (Source : in out Natural)
         with Post => Total = Total'Old + Source'Old and Source = 0
       is
       begin
          Total  := Total + Source;
          Source := 0;
       end Move_To_Total;

       X : Natural := 3;

    begin
       Move_To_Total (X);         -- OK
       pragma Assert (Total = 3); -- OK
       Move_To_Total (Total);     -- flow analysis error
       pragma Assert (Total = 6); -- runtime error
    end No_Aliasing;

``Move_To_Total`` adds the value of its input parameter ``Source`` to
the global variable ``Total`` and then resets ``Source`` to 0.  The
programmer has clearly not taken into account the possibility of an
aliasing between ``Total`` and ``Source``.  (This sort of error is
quite common.)

This procedure itself is valid SPARK. When doing verification,
GNATprove assumes, like the programmer did, that there's no aliasing
between ``Total`` and ``Source``. To ensure this assumption is valid,
GNATprove checks for possible aliasing on every call to
``Move_To_Total``.  Its final call in procedure ``No_Aliasing``
violates this assumption, which produces both a message from GNATprove
and a runtime error (an assertion violation corresponding to the
expected change in ``Total`` from calling ``Move_To_Total``). Note
that the postcondition of ``Move_To_Total`` is not violated on this
second call since integer parameters are passed by copy and the
postcondition is checked before the copy-back from the formal
parameters to the actual arguments.

Designating SPARK Code
---------------------------------------------------------------------

Since the SPARK language is restricted to only allow easily specifiable and
verifiable constructs, there are times when you can't or don't want to
abide by these limitations over your entire code base. Therefore, the SPARK
tools only check conformance to the SPARK subset on code which you identify
as being in SPARK.

You do this by using an aspect named :ada:`SPARK_Mode`. If you don't
explicitly specify otherwise, :ada:`SPARK_Mode` is `Off`, meaning you can
use the complete set of Ada features in that code and that it should not be
analyzed by GNATprove. You can change this default either selectively (on
some units or subprograms or packages inside units) or globally (using a
configuration pragma, which is what we're doing in this tutorial). To allow
simple reuse of existing Ada libraries, entities declared in imported units
with no explicit :ada:`SPARK_Mode` can still be used from SPARK code. The
tool only checks for SPARK conformance on the declaration of those entities
which are actually used within the SPARK code.

Here's a common case of using the :ada:`SPARK_Mode` aspect:

.. code-block:: ada

    package P
      with SPARK_Mode => On
    is
       -- package spec is IN SPARK, so can be used by SPARK clients
    end P;

    package body P
      with SPARK_Mode => Off
    is
       -- body is NOT IN SPARK, so is ignored by GNATprove
    end P;

The package ``P`` only defines entities whose specifications are in the
SPARK subset. However, it wants to use all Ada features in its body.
Therefore the body should not be analyzed and has its :ada:`SPARK_Mode`
aspect set to `Off`.

You can specify :ada:`SPARK_Mode` in a fine-grained manner on a per-unit
basis.  An Ada package has four different components: the visible and
private parts of its specification and the declarative and statement parts
of its body.  You can specify :ada:`SPARK_Mode` as being either `On` or
`Off` on any of those parts. Likewise, a subprogram has two parts: its
specification and its body.

A general rule in SPARK is that once :ada:`SPARK_Mode` has been set to
`Off`, it can never be switched `On` again in the same part of a package or
subprogram. This prevents setting :ada:`SPARK_Mode` to `On` for subunits of
a unit with :ada:`SPARK_Mode` `Off` and switching back to :ada:`SPARK_Mode`
`On` for a part of a given unit where it was set fo `Off` in a previous
part.


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

Here's a package defining an abstract stack type (defined as a private type
in SPARK) of ``Element`` objects along with some subprograms providing the
usual functionalities of stacks. It's marked as being in the SPARK subset.

.. code:: ada prove_flow_button

    package Stack_Package
      with SPARK_Mode => On
    is
       type Element is new Natural;
       type Stack is private;

       function Empty return Stack;
       procedure Push (S : in out Stack; E : Element);
       function Pop (S : in out Stack) return Element;

    private
       type Stack is record
          Top : Integer;
          --  ...
       end record;

    end Stack_Package;

Side-effects in expressions are not allowed in SPARK. Therefore, ``Pop``
is not allowed to modify its parameter ``S``.


Example #2
~~~~~~~~~~

Let's turn to an abstract state machine version of a stack, where the unit
provides a single instance of a stack. The content of the stack (global
variables ``Content`` and ``Top``) is not directly visible to clients. In
this stripped-down version, only the function ``Pop`` is available to
clients. The package spec and body are marked as being in the SPARK subset.

.. code:: ada prove_flow_button

    package Global_Stack
      with SPARK_Mode => On
    is
       type Element is new Integer;

       function Pop return Element;

    end Global_Stack;

    package body Global_Stack
      with SPARK_Mode => On
    is
       Max : constant Natural := 100;
       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array;
       Top     : Natural;

       function Pop return Element is
          E : constant Element := Content (Top);
       begin
          Top := Top - 1;
          return E;
       end Pop;

    end Global_Stack;

As above, functions should be free from side-effects. Here, ``Pop`` updates
the global variable ``Top``, which is not allowed in SPARK.


Example #3
~~~~~~~~~~

We now consider two procedures: ``Permute`` and ``Swap``. ``Permute``
applies a circular permutation to the value of its three parameters.
``Swap`` then uses ``Permute`` to swap the value of ``X`` and ``Y``.

.. code:: ada run_button prove_flow_button
    :class: ada-expect-compile-error

    package P
      with SPARK_Mode => On
    is
       procedure Permute (X, Y, Z : in out Positive);
       procedure Swap (X, Y : in out Positive);
    end P;

    package body P
      with SPARK_Mode => On
    is
       procedure Permute (X, Y, Z : in out Positive) is
          Tmp : constant Positive := X;
       begin
          X := Y;
          Y := Z;
          Z := Tmp;
       end Permute;

       procedure Swap (X, Y : in out Positive) is
       begin
          Permute (X, Y, Y);
       end Swap;
    end P;

    with P; use P;

    procedure Test_Swap
      with SPARK_Mode => On
    is
       A : Integer := 1;
       B : Integer := 2;
    begin
       Swap (A, B);
    end Test_Swap;

Here, the values for parameters ``Y`` and ``Z`` are aliased in the call to
``Permute``, which is not allowed in SPARK. In fact, in this particular
case, this is even a violation of Ada rules so the same error is issued by
the Ada compiler.

In this example, we see the reason why aliasing is not allowed in SPARK:
since ``Y`` and ``Z`` are :ada:`Positive`, they are passed by copy and the
result of the call to ``Permute`` depends on the order in which they're
copied back after the call.


Example #4
~~~~~~~~~~

Here, the ``Swap`` procedure is used to swap the value of the two record
components of ``R``.

.. code:: ada prove_flow_button

    package P
      with SPARK_Mode => On
    is
       type Rec is record
          F1 : Positive;
          F2 : Positive;
       end record;

       procedure Swap_Fields (R : in out Rec);
       procedure Swap (X, Y : in out Positive);
    end P;

    package body P
      with SPARK_Mode => On
    is
       procedure Swap (X, Y : in out Positive) is
          Tmp : constant Positive := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap;

       procedure Swap_Fields (R : in out Rec) is
       begin
          Swap (R.F1, R.F2);
       end Swap_Fields;

    end P;

This code is correct. The call to ``Swap`` is safe: two different
components of the same record can't refer to the same object.


Example #5
~~~~~~~~~~

Here's a slight modification of the previous example using an array instead
of a record: ``Swap_Indexes`` calls ``Swap`` on values stored in the array
``A``.

.. code:: ada prove_flow_button

    package P
      with SPARK_Mode => On
    is
       type P_Array is array (Natural range <>) of Positive;

       procedure Swap_Indexes (A : in out P_Array; I, J : Natural);
       procedure Swap (X, Y : in out Positive);
    end P;

    package body P
      with SPARK_Mode => On
    is
       procedure Swap (X, Y : in out Positive) is
          Tmp : constant Positive := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap;

       procedure Swap_Indexes (A : in out P_Array; I, J : Natural) is
       begin
          Swap (A (I), A (J));
       end Swap_Indexes;

    end P;

GNATprove detects a possible case of aliasing. Unlike the previous example,
it has no way of knowing that the two elements ``A (I)`` and ``A (J)`` are
actually distinct when we call ``Swap``.  GNATprove issues a check message
here instead of an error, giving you the possibility of justifying the
message after review (meaning that you've verified manually that this
can't, in fact, occur).


Example #6
~~~~~~~~~~

We now consider a package declaring a type ``Dictionary``, an array
containing a word per letter. The procedure ``Store`` allows us to insert a
word at the correct index in a dictionary.

.. code:: ada prove_flow_button

    package P
      with SPARK_Mode => On
    is
       subtype Letter is Character range 'a' .. 'z';
       type String_Access is access String;
       type Dictionary is array (Letter) of String_Access;

       procedure Store (D : in out Dictionary; W : String);
    end P;

    package body P
      with SPARK_Mode => On
    is
       procedure Store (D : in out Dictionary; W : String) is
          First_Letter : constant Letter := W (W'First);
       begin
          D (First_Letter) := new String'(W);
       end Store;
    end P;

This code is not correct: access types are not part of the SPARK subset. In
this case, they are very useful because without them we can't store
arbitrarily long strings in an array. One solution here is to use
:ada:`SPARK_Mode` to separate parts of the access type from the rest of the
code in a fine grained manner.


Example #7
~~~~~~~~~~

Here's a new version of the previous example, which we've modified to hide
the access type inside the private part of package ``P``, using ``pragma
SPARK_Mode (Off)`` at the start of the private part.

.. code:: ada prove_flow_button

    package P
      with SPARK_Mode => On
    is
       subtype Letter is Character range 'a' .. 'z';
       type String_Access is private;
       type Dictionary is array (Letter) of String_Access;

       function New_String_Access (W : String) return String_Access;

       procedure Store (D : in out Dictionary; W : String);

    private
       pragma SPARK_Mode (Off);

       type String_Access is access String;

       function New_String_Access (W : String) return String_Access is
         (new String'(W));
    end P;

Since the access type is defined and used inside of a part of the code
ignored by GNATprove, this code is correct.


Example #8
~~~~~~~~~~

Let's put together the new spec for package ``P`` with the body of ``P`` seen
previously.

.. code:: ada prove_flow_button
    :class: ada-expect-compile-error

    package P
      with SPARK_Mode => On
    is
       subtype Letter is Character range 'a' .. 'z';
       type String_Access is private;
       type Dictionary is array (Letter) of String_Access;

       function New_String_Access (W : String) return String_Access;

       procedure Store (D : in out Dictionary; W : String);

    private
       pragma SPARK_Mode (Off);

       type String_Access is access String;

       function New_String_Access (W : String) return String_Access is
         (new String'(W));
    end P;

    package body P
      with SPARK_Mode => On
    is
       procedure Store (D : in out Dictionary; W : String) is
          First_Letter : constant Letter := W (W'First);
       begin
          D (First_Letter) := New_String_Access (W);
       end Store;
    end P;

The body of ``Store`` doesn't actually use any construct that's not in the
SPARK subset, but we nevertheless can't set :ada:`SPARK_Mode` to ``On`` for
``P``'s body because it has visibility to ``P``'s private part, which is
not in SPARK, even if we don't use it.


Example #9
~~~~~~~~~~

Next, we moved the declaration and the body of the procedure ``Store`` to
another package named ``Q``.

.. code:: ada prove_flow_button

    package P
      with SPARK_Mode => On
    is
       subtype Letter is Character range 'a' .. 'z';
       type String_Access is private;
       type Dictionary is array (Letter) of String_Access;

       function New_String_Access (W : String) return String_Access;

    private
       pragma SPARK_Mode (Off);

       type String_Access is access String;

       function New_String_Access (W : String) return String_Access is
         (new String'(W));
    end P;

    with P; use P;
    package Q
      with SPARK_Mode => On
    is
       procedure Store (D : in out Dictionary; W : String);
    end Q;

    package body Q
      with SPARK_Mode => On
    is
       procedure Store (D : in out Dictionary; W : String)  is
          First_Letter : constant Letter := W (W'First);
       begin
          D (First_Letter) := New_String_Access (W);
       end Store;
    end Q;

And now everything is fine: we've managed to retain the use of the access
type while having most of our code in the SPARK subset so GNATprove is able
to analyze it.


Example #10
~~~~~~~~~~~

Our final example is a package with two functions to search for the value 0
inside an array ``A``. The first raises an exception if 0 isn't found in
``A`` while the other simply returns 0 in that case.

.. code:: ada

    package P
      with SPARK_Mode => On
    is
       type N_Array is array (Positive range <>) of Natural;
       Not_Found : exception;

       function Search_Zero_P (A : N_Array) return Positive;

       function Search_Zero_N (A : N_Array) return Natural;
    end P;

    package body P
      with SPARK_Mode => On
    is
       function Search_Zero_P (A : N_Array) return Positive is
       begin
          for I in A'Range loop
             if A (I) = 0 then
                return I;
             end if;
          end loop;
          raise Not_Found;
       end Search_Zero_P;

       function Search_Zero_N (A : N_Array) return Natural
         with SPARK_Mode => Off is
       begin
          return Search_Zero_P (A);
       exception
          when Not_Found => return 0;
       end Search_Zero_N;
    end P;

This code is perfectly correct, despite the use of exception handling,
because we've carefully isolated this non-SPARK feature in a function body
marked with a ``SPARK_Mode`` of ``Off`` so it's ignored by GNATprove.
However, GNATprove tries to show that ``Not_Found`` is never raised in
``Search_Zero_P``, producing a message about a possible exception being
raised.  Looking at ``Search_Zero_N``, it's indeed likely that an exception
is meant to be raised in some cases, which means you need to verify that
``Not_Found`` is only raised when appropriate using other methods such as
peer review or testing.
