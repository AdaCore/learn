.. _Intro_SPARK_Flow_Analysis:

Flow Analysis
=====================================================================

.. include:: ../../../global.txt

In this section we present the flow analysis capability provided by the
GNATprove tool, a critical tool for using SPARK.


What does flow analysis do?
---------------------------------------------------------------------

Flow analysis concentrates primarily on variables. It models how
information flows through them during a subprogram's execution, connecting
the final values of variables to their initial values. It analyzes global
variables declared at library level, local variables, and formal parameters
of subprograms.

Nesting of subprograms creates what we call *scope variables*: variables
declared locally to an enclosing unit. From the perspective of a nested
subprogram, scope variables look very much like global variables

Flow analysis is usually fast, roughly as fast as compilation. It detects
various types of errors and finds violations of some SPARK legality rules,
such as the absence of aliasing and freedom of expressions from
side-effects.  We discussed these rules in the :ref:`Intro_SPARK_Overview`.

Flow analysis is *sound*: if it doesn't detect any errors of a type it's
supposed to detect, we know for sure there are no such errors.


Errors Detected
---------------------------------------------------------------------

Uninitialized Variables
~~~~~~~~~~~~~~~~~~~~~~~

We now present each class of errors detected by flow analysis.  The first
is the reading of an uninitialized variable.  This is nearly always an
error: it introduces non-determinism and breaks the type system because the
value of an uninitialized variable may be outside the range of its subtype.
For these reasons, SPARK requires every variable to be initialized before
being read.

Flow analysis is responsible for ensuring that SPARK code always fulfills
this requirement. For example, in the function :ada:`Max_Array` shown below,
we've neglected to initialize the value of :ada:`Max` prior to entering the
loop. As a consequence, the value read by the condition of the if statement
may be uninitialized. Flow analysis detects and reports this error.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Uninitialized
    :class: ada-expect-prove-error

    package Show_Uninitialized is

       type Array_Of_Naturals is array (Integer range <>) of Natural;

       function Max_Array (A : Array_Of_Naturals) return Natural;

    end Show_Uninitialized;

    package body Show_Uninitialized is

       function Max_Array (A : Array_Of_Naturals) return Natural is
          Max : Natural;
       begin
          for I in A'Range loop
             if A (I) > Max then -- Here Max may not be initialized
                Max := A (I);
             end if;
          end loop;
          return Max;
       end Max_Array;

    end Show_Uninitialized;

.. note::

   For more details on how flow analysis verifies data initialization, see the
   :spark_ugs:`SPARK User's Guide <language_restrictions.html#data-initialization-policy>`.

Ineffective Statements
~~~~~~~~~~~~~~~~~~~~~~

Ineffective statements are different than dead code: they're executed, and
often even modify the value of variables, but have no effect on any of the
subprogram's visible outputs: parameters, global variables or the function
result. Ineffective statements should be avoided because they make the code
less readable and more difficult to maintain.

More importantly, they're often caused by errors in the program: the
statement may have been written for some purpose, but isn't accomplishing
that purpose.  These kinds of errors can be difficult to detect in other
ways.

For example, the subprograms :ada:`Swap1` and :ada:`Swap2` shown below don't
properly swap their two parameters :ada:`X` and :ada:`Y`.  This error caused a
statement to be ineffective.  That ineffective statement is not an error in
itself, but flow analysis produces a warning since it can be indicative of
an error, as it is here.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Ineffective_Statements

    package Show_Ineffective_Statements is

       type T is new Integer;

       procedure Swap1 (X, Y : in out T);
       procedure Swap2 (X, Y : in out T);

    end Show_Ineffective_Statements;

    package body Show_Ineffective_Statements is

       procedure Swap1 (X, Y : in out T) is
          Tmp : T;
       begin
          Tmp := X; -- This statement is ineffective
          X   := Y;
          Y   := X;
       end Swap1;

       Tmp : T := 0;

       procedure Swap2 (X, Y : in out T) is
          Temp : T := X; -- This variable is unused
       begin
          X := Y;
          Y := Tmp;
       end Swap2;

    end Show_Ineffective_Statements;

So far, we've seen examples where flow analysis warns about ineffective
statements and unused variables.


Incorrect Parameter Mode
~~~~~~~~~~~~~~~~~~~~~~~~

Parameter modes are an important part of documenting the usage of a
subprogram and affect the code generated for that subprogram. Flow analysis
checks that each specified parameter mode corresponds to the usage of that
parameter in the subprogram's body.  It checks that an :ada:`in` parameter
is never modified, either directly or through a subprogram call, checks
that the initial value of an :ada:`out` parameter is never read in the
subprogram (since it may not be defined on subprogram entry), and warns
when an :ada:`in out` parameter isn't modified or when its initial value
isn't used.  All of these may be signs of an error.

We see an example below. The subprogram :ada:`Swap` is incorrect and GNATprove
warns about an input which isn't read:

.. code:: ada prove_button project=Courses.Intro_To_Spark.Flow_Analysis.Incorrect_Param_Mode

    package Show_Incorrect_Param_Mode is

       type T is new Integer;

       procedure Swap (X, Y : in out T);

    end Show_Incorrect_Param_Mode;

    package body Show_Incorrect_Param_Mode is

       procedure Swap (X, Y : in out T) is
          Tmp : T := X;
       begin
          Y := X;   -- The initial value of Y is not used
          X := Tmp; -- Y is computed to be an out parameter
       end Swap;

    end Show_Incorrect_Param_Mode;

In SPARK, unlike Ada, you should declare an :ada:`out` parameter to be
:ada:`in out` if it's not modified on every path, in which case its value
may depend on its initial value. SPARK is stricter than Ada to allow more
static detection of errors. This table summarizes SPARK's valid parameter
modes as a function of whether reads and writes are done to the parameter.

+---------------+------------+------------+----------------+
| Initial value | Written on | Written on | Parameter mode |
| read          | some path  | every path |                |
+===============+============+============+================+
| X             |            |            | :ada:`in`      |
+---------------+------------+------------+----------------+
| X             | X          |            | :ada:`in out`  |
+---------------+------------+------------+----------------+
| X             |            | X          | :ada:`in out`  |
+---------------+------------+------------+----------------+
|               | X          |            | :ada:`in out`  |
+---------------+------------+------------+----------------+
|               |            | X          | :ada:`out`     |
+---------------+------------+------------+----------------+


Additional Verifications
---------------------------------------------------------------------

Global Contracts
~~~~~~~~~~~~~~~~

So far, none of the verifications we've seen require you to write any
additional annotations. However, flow analysis also checks flow annotations
that you write. In SPARK, you can specify the set of global and scoped
variables accessed or modified by a subprogram.  You do this using a
contract named :ada:`Global`.

When you specify a :ada:`Global` contract for a subprogram, flow analysis
checks that it's both correct and complete, meaning that no variables other
than those stated in the contract are accessed or modified, either directly
or through a subprogram call, and that all those listed are accessed or
modified. For example, we may want to specify that the function
:ada:`Get_Value_Of_X` reads the value of the global variable :ada:`X` and doesn't
access any other global variable. If we do this through a comment, as is
usually done in other languages, GNATprove can't verify that the code
complies with this specification:

.. code-block:: ada

    package Show_Global_Contracts is

       X : Natural := 0;

       function Get_Value_Of_X return Natural;
       --  Get_Value_Of_X reads the value of the global variable X

    end Show_Global_Contracts;

You write global contracts as part of the subprogram specification.  In
addition to their value in flow analysis, they also provide useful
information to users of a subprogram. The value you specify for the
:ada:`Global` aspect is an aggregate-like list of global variable names,
grouped together according to their mode.

In the example below, the procedure :ada:`Set_X_To_Y_Plus_Z` reads both :ada:`Y`
and :ada:`Z`.  We indicate this by specifying them as the value for
:ada:`Input`.  It also writes :ada:`X`, which we specify using
:ada:`Output`. Since :ada:`Set_X_To_X_Plus_Y` both writes :ada:`X` and reads its
initial value, :ada:`X`'s mode is :ada:`In_Out`. Like parameters, if no mode
is specified in a :ada:`Global` aspect, the default is :ada:`Input`.  We
see this in the case of the declaration of :ada:`Get_Value_Of_X`. Finally, if
a subprogram, such as :ada:`Incr_Parameter_X`, doesn't reference any global
variables, you set the value of the global contract to :ada:`null`.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Global_Contracts

    package Show_Global_Contracts is

       X, Y, Z : Natural := 0;

       procedure Set_X_To_Y_Plus_Z with
         Global => (Input  => (Y, Z), --  reads values of Y and Z
                    Output => X);     --  modifies value of X

       procedure Set_X_To_X_Plus_Y with
         Global => (Input  => Y,  --  reads value of Y
                    In_Out => X); --  modifies value of X and
                                  --  also reads its initial value

       function Get_Value_Of_X return Natural with
         Global => X;  -- reads the value of the global variable X

       procedure Incr_Parameter_X (X : in out Natural) with
         Global => null; -- do not reference any global variable

    end Show_Global_Contracts;

.. note::

   For more details on global contracts, see the
   :spark_ugs:`SPARK User's Guide <subprogram_contracts.html#data-dependencies>`.

Depends Contracts
~~~~~~~~~~~~~~~~~

You may also supply a :ada:`Depends` contract for a subprogram to specify
dependencies between its inputs and outputs. These dependencies include not
only global variables but also parameters and the function's result.  When
you supply a :ada:`Depends` contract for a subprogram, flow analysis checks
that it's correct and complete, that is, for each dependency you list, the
variable depends on those listed and on no others.

For example, you may want to say that the new value of each parameter of
:ada:`Swap`, shown below, depends only on the initial value of the other
parameter and that the value of :ada:`X` after the return of :ada:`Set_X_To_Zero`
doesn't depend on any global variables. If you indicate this through a
comment, as you often do in other languages, GNATprove can't verify that
this is actually the case.

.. code-block:: ada

    package Show_Depends_Contracts is

       type T is new Integer;

       procedure Swap (X, Y : in out T);
       --  The value of X (resp. Y) after the call depends only
       --  on the value of Y (resp. X) before the call

       X : Natural;
       procedure Set_X_To_Zero;
       --  The value of X after the call depends on no input

    end Show_Depends_Contracts;

Like :ada:`Global` contracts, you specify a :ada:`Depends` contract in
subprogram declarations using an aspect. Its value is a list of one or more
dependency relations between the outputs and inputs of the subprogram. Each
relation is represented as two lists of variable names separated by an
arrow. On the left of each arrow are variables whose final value
depends on the initial value of the variables you list on the right.

For example, here we indicate that the final value of each parameter of
:ada:`Swap` depends only on the initial value of the other parameter. If the
subprogram is a function, we list its result as an output, using the
:ada:`Result` attribute, as we do for :ada:`Get_Value_Of_X` below.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Depends_Contracts

    package Show_Depends_Contracts is

       type T is new Integer;

       X, Y, Z : T := 0;

       procedure Swap (X, Y : in out T) with
         Depends => (X => Y,
                     --  X depends on the initial value of Y
                     Y => X);
                     --  Y depends on the initial value of X

       function Get_Value_Of_X return T with
         Depends => (Get_Value_Of_X'Result => X);
                     --  result depends on the initial value of X

       procedure Set_X_To_Y_Plus_Z with
         Depends => (X => (Y, Z));
                     --  X depends on the initial values of Y and Z

       procedure Set_X_To_X_Plus_Y with
         Depends => (X =>+ Y);
                 --  X depends on Y and X's initial value

       procedure Do_Nothing (X : T) with
         Depends => (null => X);
                     --  no output is affected by X

       procedure Set_X_To_Zero with
         Depends => (X => null);
                     --  X depends on no input

    end Show_Depends_Contracts;

Often, the final value of a variable depends on its own initial value.  You
can specify this in a concise way using the :ada:`+` character, as we did
in the specification of :ada:`Set_X_To_X_Plus_Y` above. If there's more than
one variable on the left of the arrow, a :ada:`+` means each variables
depends on itself, not that they all depend on each other.  You can write
the corresponding dependency with (:ada:`=> +`) or without (:ada:`=>+`)
whitespace.

If you have a program where an input isn't used to compute the final value
of any output, you express that by writting :ada:`null` on the left of the
dependency relation, as we did for the :ada:`Do_Nothing` subprogram above.
You can only write one such dependency relation, which lists all unused
inputs of the subprogram, and it must be written last.  Such an annotation
also silences flow analysis' warning about unused parameters. You can also
write :ada:`null` on the right of a dependency relation to indicate that an
output doesn't depend on any input. We do that above for the procedure
:ada:`Set_X_To_Zero`.

.. note::

   For more details on depends contracts, see the
   :spark_ugs:`SPARK User's Guide <subprogram_contracts.html#flow-dependencies>`.

Shortcomings
---------------------------------------------------------------------

Modularity
~~~~~~~~~~

Flow analysis is sound, meaning that if it doesn't output a message on some
analyzed SPARK code, you can be assured that none of the errors it tests
for can occur in that code. On the other hand, flow analysis often issues
messages when there are, in fact, no errors. The first, and probably most
common reason for this relates to modularity.

To scale flow analysis to large projects, verifications are usually done on
a per-subprogram basis, including detection of uninitialized variables.  To
analyze this modularly, flow analysis needs to assume the initialization of
inputs on subprogram entry and modification of outputs during subprogram
execution. Therefore, each time a subprogram is called, flow analysis
checks that global and parameter inputs are initialized and each time a
subprogram returns, it checks that global and parameter outputs were
modified.

This can produce error messages on perfectly correct subprograms.  An
example is :ada:`Set_X_To_Y_Plus_Z` below, which only sets its :ada:`out`
parameter :ada:`X` when :ada:`Overflow` is :ada:`False`.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Set_X_To_Y_Plus_Z
    :class: ada-expect-prove-error

    procedure Set_X_To_Y_Plus_Z
      (Y, Z     :     Natural;
       X        : out Natural;
       Overflow : out Boolean)
    is
    begin
       if Natural'Last - Z < Y then
          Overflow := True; -- X should be initialized on every path
       else
          Overflow := False;
          X := Y + Z;
       end if;
    end Set_X_To_Y_Plus_Z;

The message means that flow analysis wasn't able to verify that the program
didn't read an uninitialized variable. To solve this problem, you can
either set :ada:`X` to a dummy value when there's an overflow or manually
verify that :ada:`X` is never used after a call to :ada:`Set_X_To_Y_Plus_Z` that
returned :ada:`True` as the value of :ada:`Overflow`.


Composite Types
~~~~~~~~~~~~~~~

Another common cause of false alarms is caused by the way flow analysis
handles composite types. Let's start with arrays.

Flow analysis treats an entire array as single object instead of one object
per element, so it considers modifying a single element to be a
modification of the array as a whole.  Obviously, this makes reasoning
about which global variables are accessed less precise and hence the
dependencies of those variables are also less precise. This also affects
the ability to accurately detect reads of uninitialized data.

It's sometimes impossible for flow analysis to determine if an entire array
object has been initialized. For example, after we write code to initialize
every element of an unconstrained array :ada:`A` in chunks, we may still receive a
message from flow analysis claiming that the array isn't initialized. To
resolve this issue, you can either use a simpler loop over the full range of
the array, or (even better) an aggregate assignment, or, if that's not possible,
verify initialization of the object manually.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Composite_Types_Shortcoming
    :class: ada-expect-prove-error

    package Show_Composite_Types_Shortcoming is

       type T is array (Natural range <>) of Integer;

       procedure Init_Chunks (A : out T);
       procedure Init_Loop (A : out T);
       procedure Init_Aggregate (A : out T);

    end Show_Composite_Types_Shortcoming;

    package body Show_Composite_Types_Shortcoming is

       procedure Init_Chunks (A : out T) is
       begin
          A (A'First) := 0;
          for I in A'First + 1 .. A'Last loop
             A (I) := 0;
          end loop;
          --  flow analysis doesn't know that A is initialized
       end Init_Chunks;

       procedure Init_Loop (A : out T) is
       begin
          for I in A'Range loop
             A (I) := 0;
          end loop;
          --  flow analysis knows that A is initialized
       end Init_Loop;

       procedure Init_Aggregate (A : out T) is
       begin
          A := (others => 0);
          --  flow analysis knows that A is initialized
       end Init_Aggregate;

    end Show_Composite_Types_Shortcoming;

Flow analysis is more precise on record objects because it tracks the value
of each component of a record separately within a single subprogram.  So
when a record object is initialized by successive assignments of its
components, flow analysis knows that the entire object is initialized.
However, record objects are still treated as single objects when analyzed
as an input or output of a subprogram.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.Flow_Analysis.Record_Flow_Analysis_1

    package Show_Record_Flow_Analysis is

       type Rec is record
          F1 : Natural;
          F2 : Natural;
       end record;

       procedure Init (R : out Rec);

    end Show_Record_Flow_Analysis;

    package body Show_Record_Flow_Analysis is

       procedure Init (R : out Rec) is
       begin
          R.F1 := 0;
          R.F2 := 0;
          --  R is initialized
       end Init;

    end Show_Record_Flow_Analysis;

Flow analysis complains when a procedure call initializes only some
components of a record object.  It'll notify you of uninitialized
components, as we see in subprogram :ada:`Init_F2` below.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Record_Flow_Analysis_2
    :class: ada-expect-prove-error

    package Show_Record_Flow_Analysis is

       type Rec is record
          F1 : Natural;
          F2 : Natural;
       end record;

       procedure Init (R : out Rec);
       procedure Init_F2 (R : in out Rec);

    end Show_Record_Flow_Analysis;

    package body Show_Record_Flow_Analysis is

       procedure Init_F2
         (R : in out Rec) is
       begin
          R.F2 := 0;
       end Init_F2;

       procedure Init (R : out Rec) is
       begin
          R.F1 := 0;
          Init_F2 (R); -- R should be initialized before this call
       end Init;

    end Show_Record_Flow_Analysis;

Value Dependency
~~~~~~~~~~~~~~~~

Flow analysis is not value-dependent: it never reasons about the values of
expressions, only whether they have been set to some value or not. As a
consequence, if some execution path in a subprogram is impossible, but the
impossibility can only be determined by looking at the values of
expressions, flow analysis still considers that path feasible and may emit
messages based on it believing that execution along such a path is
possible.

For example, in the version of :ada:`Absolute_Value` below, flow analysis
computes that :ada:`R` is uninitialized on a path that enters neither of the
two conditional statements. Because it doesn't consider values of
expressions, it can't know that such a path is impossible.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.Flow_Analysis.Absolute_Value_1
    :class: ada-expect-prove-error

    procedure Absolute_Value
      (X :     Integer;
       R : out Natural)
    is
    begin
       if X < 0 then
          R := -X;
       end if;
       if X >= 0 then
          R := X;
       end if;
       --  flow analysis doesn't know that R is initialized
    end Absolute_Value;

To avoid this problem, you should make the control flow explicit, as in
this second version of :ada:`Absolute_Value`:

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Absolute_Value_2

    procedure Absolute_Value
      (X :     Integer;
       R : out Natural)
    is
    begin
       if X < 0 then
          R := -X;
       else
          R := X;
       end if;
       --  flow analysis knows that R is initialized
    end Absolute_Value;

Contract Computation
~~~~~~~~~~~~~~~~~~~~

The final cause of unexpected flow messages that we'll discuss also comes
from inaccuracy in computations of contracts. As we explained earlier, both
:ada:`Global` and :ada:`Depends` contracts are optional, but GNATprove uses
their data for some of its analysis.

For example, flow analysis can't detect reads from uninitialized variables
without knowing the set of variables accessed. It needs to analyze and
check both the :ada:`Depends` contracts you wrote for a subprogram and
those you wrote for callers of that subprogram. Since each flow contract on
a subprogram depends on the flow contracts of all the subprograms called
inside its body, this computation can often be quite
time-consuming. Therefore, flow analysis sometimes trades-off the precision
of this computation against the time a more precise computation would take.

This is the case for :ada:`Depends` contracts, where flow analysis simply
assumes the worst, that each subprogram's output depends on all of that
subprogram's inputs. To avoid this assumption, all you have to do is supply
contracts when default ones are not precise enough.  You may also want to
supply :ada:`Global` contracts to further speed up flow analysis on larger
programs.


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

The procedure :ada:`Search_Array` searches for an occurrence of element :ada:`E`
in an array :ada:`A`. If it finds one, it stores the index of the element in
:ada:`Result`.  Otherwise, it sets :ada:`Found` to :ada:`False`.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_01
    :class: ada-expect-prove-error

    package Show_Search_Array is

       type Array_Of_Positives is array (Natural range <>) of Positive;

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Integer;
          Found  : out Boolean);

    end Show_Search_Array;

    package body Show_Search_Array is

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Integer;
          Found  : out Boolean) is
       begin
          for I in A'Range loop
             if A (I) = E then
                Result := I;
                Found  := True;
                return;
             end if;
          end loop;
          Found := False;
       end Search_Array;

    end Show_Search_Array;

GNATprove produces a message saying that :ada:`Result` is possibly
uninitialized on return.  There are perfectly legal uses of the function
:ada:`Search_Array`, but flow analysis detects that :ada:`Result` is not
initialized on the path that falls through from the loop. Even though this
program is correct, you shouldn't ignore the message: it means flow
analysis cannot guarantee that :ada:`Result` is always initialized at the call
site and so assumes any read of :ada:`Result` at the call site will read
initialized data.  Therefore, you should either initialize :ada:`Result` when
:ada:`Found` is false, which silences flow analysis, or verify this assumption
at each call site by other means.


Example #2
~~~~~~~~~~

To avoid the message previously issued by GNATprove, we modify
:ada:`Search_Array` to raise an exception when :ada:`E` isn't found in :ada:`A`:

.. code:: ada prove_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_02
    :class: ada-expect-prove-error

    package Show_Search_Array is

       type Array_Of_Positives is array (Natural range <>) of Positive;

       Not_Found : exception;

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Integer);
    end Show_Search_Array;

    package body Show_Search_Array is

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Integer) is
       begin
          for I in A'Range loop
             if A (I) = E then
                Result := I;
                return;
             end if;
          end loop;
          raise Not_Found;
       end Search_Array;

    end Show_Search_Array;

Flow analysis doesn't emit any messages in this case, meaning it can verify
that :ada:`Result` can't be read in SPARK code while uninitialized. But why is
that, since :ada:`Result` is still not initialized when :ada:`E` is not in :ada:`A`?
This is because the exception, :ada:`Not_Found`, can never be caught within
SPARK code (SPAK doesn't allow exception handlers).  However, the GNATprove
tool also tries to ensure the absence of runtime errors in SPARK code, so
tries to prove that :ada:`Not_Found` is never raised.  When it can't do that
here, it produces a different message.

Example #3
~~~~~~~~~~

In this example, we're using a discriminated record for the result of
:ada:`Search_Array` instead of conditionally raising an exception.  By using
such a structure, the place to store the index at which :ada:`E` was found
exists only when :ada:`E` was indeed found.  So if it wasn't found, there's
nothing to be initialized.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_03

    package Show_Search_Array is

       type Array_Of_Positives is array (Natural range <>) of Positive;

       type Search_Result (Found : Boolean := False) is record
          case Found is
             when True =>
                Content : Integer;
             when False => null;
          end case;
       end record;

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Search_Result)
       with Pre => not Result'Constrained;

    end Show_Search_Array;

    package body Show_Search_Array is

       procedure Search_Array
         (A      :     Array_Of_Positives;
          E      :     Positive;
          Result : out Search_Result) is
       begin
          for I in A'Range loop
             if A (I) = E then
                Result := (Found   => True,
                           Content => I);
                return;
             end if;
          end loop;
          Result := (Found => False);
       end Search_Array;

    end Show_Search_Array;

This example is correct and flow analysis doesn't issue any message: it can
verify both that no uninitialized variables are read in :ada:`Search_Array`'s
body, and that all its outputs are set on return.  We've used the attribute
:ada:`Constrained` in the precondition of :ada:`Search_Array` to indicate that
the value of the :ada:`Result` in argument can be set to any variant of the
record type :ada:`Search_Result`, specifically to either the variant where
:ada:`E` was found and where it wasn't.

.. _Intro_SPARK_Flow_Analysis_Example_4:

Example #4
~~~~~~~~~~

The function :ada:`Size_Of_Biggest_Increasing_Sequence` is supposed to find
all sequences within its parameter :ada:`A` that contain elements with
increasing values and returns the length of the longest one. To do this, it
calls a nested procedure :ada:`Test_Index` iteratively on all the elements of
:ada:`A`.  :ada:`Test_Index` checks if the sequence is still increasing. If so,
it updates the largest value seen so far in this sequence.  If not, it
means it's found the end of a sequence, so it computes the size of that
sequence and stores it in :ada:`Size_Of_Seq`.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_04
    :class: ada-expect-prove-error

    package Show_Biggest_Increasing_Sequence is

       type Array_Of_Positives is array (Integer range <>) of Positive;

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural;

    end Show_Biggest_Increasing_Sequence;

    package body Show_Biggest_Increasing_Sequence is

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural
       is
          Max         : Natural;
          End_Of_Seq  : Boolean;
          Size_Of_Seq : Natural;
          Beginning   : Integer;

          procedure Test_Index (Current_Index : Integer) is
          begin
             if A (Current_Index) >= Max then
                Max := A (Current_Index);
                End_Of_Seq := False;
             else
                Max         := 0;
                End_Of_Seq  := True;
                Size_Of_Seq := Current_Index - Beginning;
                Beginning   := Current_Index;
             end if;
          end Test_Index;

          Biggest_Seq : Natural := 0;

       begin
          for I in A'Range loop
             Test_Index (I);
             if End_Of_Seq then
                Biggest_Seq := Natural'Max (Size_Of_Seq, Biggest_Seq);
             end if;
          end loop;
          return Biggest_Seq;
       end Size_Of_Biggest_Increasing_Sequence;

    end Show_Biggest_Increasing_Sequence;

However, this example is not correct. Flow analysis emits messages for
:ada:`Test_Index` stating that :ada:`Max`, :ada:`Beginning`, and :ada:`Size_Of_Seq`
should be initialized before being read. Indeed, when you look carefully,
you see that both :ada:`Max` and :ada:`Beginning` are missing initializations
because they are read in :ada:`Test_Index` before being written. As for
:ada:`Size_Of_Seq`, we only read its value when :ada:`End_Of_Seq` is true, so it
actually can't be read before being written, but flow analysis isn't able
to verify its initialization by using just flow information.

The call to :ada:`Test_Index` is automatically inlined by GNATprove, which
leads to another messages above. If GNATprove couldn't inline the call to
:ada:`Test_Index`, for example if it was defined in another unit, the same
messages would be issued on the call to :ada:`Test_Index`.


Example #5
~~~~~~~~~~

In the following example, we model permutations as arrays where the element
at index :ada:`I` is the position of the :ada:`I`'th element in the
permutation. The procedure :ada:`Init` initializes a permutation to the
identity, where the :ada:`I`'th elements is at the :ada:`I`'th
position. :ada:`Cyclic_Permutation` calls :ada:`Init` and then swaps elements to
construct a cyclic permutation.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_05
    :class: ada-expect-prove-error

    package Show_Permutation is

       type Permutation is array (Positive range <>) of Positive;

       procedure Swap (A    : in out Permutation;
                       I, J : Positive);

       procedure Init (A : out Permutation);

       function Cyclic_Permutation (N : Natural) return Permutation;

    end Show_Permutation;

    package body Show_Permutation is

       procedure Swap (A    : in out Permutation;
                       I, J : Positive)
       is
          Tmp : Positive := A (I);
       begin
          A (I) := A (J);
          A (J) := Tmp;
       end Swap;

       procedure Init (A : out Permutation) is
       begin
          A (A'First) := A'First;
          for I in A'First + 1 .. A'Last loop
             A (I) := I;
          end loop;
       end Init;

       function Cyclic_Permutation (N : Natural) return Permutation is
          A : Permutation (1 .. N);
       begin
          Init (A);
          for I in A'First .. A'Last - 1 loop
             Swap (A, I, I + 1);
          end loop;
          return A;
       end Cyclic_Permutation;

    end Show_Permutation;

This program is correct. However, flow analysis will nevertheless still
emit messages because it can't verify that every element of :ada:`A` is
initialized by the loop in :ada:`Init`. This message is a false alarm.  You
can either ignore it or justify it safely.


Example #6
~~~~~~~~~~

This program is the same as the previous one except that we've changed the
mode of :ada:`A` in the specification of :ada:`Init` to :ada:`in out` to avoid
the message from flow analysis on array assignment.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_06
    :class: ada-expect-prove-error

    package Show_Permutation is

       type Permutation is array (Positive range <>) of Positive;

       procedure Swap (A    : in out Permutation;
                       I, J : Positive);

       procedure Init (A : in out Permutation);

       function Cyclic_Permutation (N : Natural) return Permutation;

    end Show_Permutation;

    package body Show_Permutation is

       procedure Swap (A    : in out Permutation;
                       I, J : Positive)
       is
          Tmp : Positive := A (I);
       begin
          A (I) := A (J);
          A (J) := Tmp;
       end Swap;

       procedure Init (A : in out Permutation) is
       begin
          A (A'First) := A'First;
          for I in A'First + 1 .. A'Last loop
             A (I) := I;
          end loop;
       end Init;

       function Cyclic_Permutation (N : Natural) return Permutation is
          A : Permutation (1 .. N);
       begin
          Init (A);
          for I in A'First .. A'Last - 1 loop
             Swap (A, I, I + 1);
          end loop;
          return A;
       end Cyclic_Permutation;

    end Show_Permutation;

This program is not correct. Changing the mode of a parameter that should
really be :ada:`out` to :ada:`in out` to silence a false alarm is not a
good idea. Not only does this obfuscate the specification of :ada:`Init`, but
flow analysis emits a message on the procedure where :ada:`A` is not
initialized, as shown by the message in :ada:`Cyclic_Permutation`.


Example #7
~~~~~~~~~~

:ada:`Incr_Step_Function` takes an array :ada:`A` as an argument and iterates
through :ada:`A` to increment every element by the value of :ada:`Increment`,
saturating at a specified threshold value. We specified a :ada:`Global`
contract for :ada:`Incr_Until_Threshold`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_07

    package Show_Increments is

       type Array_Of_Positives is array (Natural range <>) of Positive;

       Increment : constant Natural := 10;

       procedure Incr_Step_Function (A : in out Array_Of_Positives);

    end Show_Increments;

    package body Show_Increments is

       procedure Incr_Step_Function (A : in out Array_Of_Positives) is

          Threshold : Positive := Positive'Last;

          procedure Incr_Until_Threshold (I : Integer) with
            Global => (Input  => Threshold,
                       In_Out => A);

          procedure Incr_Until_Threshold (I : Integer) is
          begin
             if Threshold - Increment <= A (I) then
                A (I) := Threshold;
             else
                A (I) := A (I) + Increment;
             end if;
          end Incr_Until_Threshold;

       begin
          for I in A'Range loop
             if I > A'First then
                Threshold := A (I - 1);
             end if;
             Incr_Until_Threshold (I);
          end loop;
       end Incr_Step_Function;

    end Show_Increments;

Everything is fine here.  Specifically, the :ada:`Global` contract is
correct. It mentions both :ada:`Threshold`, which is read but not written in
the procedure, and :ada:`A`, which is both read and written.  The fact that
:ada:`A` is a parameter of an enclosing unit doesn't prevent us from using it
inside the :ada:`Global` contract; it really is global to
:ada:`Incr_Until_Threshold`. We didn't mention :ada:`Increment` since it's a
static constant.


Example #8
~~~~~~~~~~

We now go back to the procedure :ada:`Test_Index` from
:ref:`Intro_SPARK_Flow_Analysis_Example_4` and
correct the missing initializations.  We want to know if the :ada:`Global`
contract of :ada:`Test_Index` is correct.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_08
    :class: ada-expect-prove-error

    package Show_Biggest_Increasing_Sequence is

       type Array_Of_Positives is array (Integer range <>) of Positive;

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural;

    end Show_Biggest_Increasing_Sequence;

    package body Show_Biggest_Increasing_Sequence is

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural
       is
          Max         : Natural := 0;
          End_Of_Seq  : Boolean;
          Size_Of_Seq : Natural := 0;
          Beginning   : Integer := A'First - 1;

          procedure Test_Index (Current_Index : Integer) with
            Global => (In_Out => (Beginning, Max, Size_Of_Seq),
                       Output => End_Of_Seq,
                       Input  => Current_Index)
          is
          begin
             if A (Current_Index) >= Max then
                Max := A (Current_Index);
                End_Of_Seq := False;
             else
                Max         := 0;
                End_Of_Seq  := True;
                Size_Of_Seq := Current_Index - Beginning;
                Beginning   := Current_Index;
             end if;
          end Test_Index;

          Biggest_Seq : Natural := 0;

       begin
          for I in A'Range loop
             Test_Index (I);
             if End_Of_Seq then
                Biggest_Seq := Natural'Max (Size_Of_Seq, Biggest_Seq);
             end if;
          end loop;
          return Biggest_Seq;
       end Size_Of_Biggest_Increasing_Sequence;

    end Show_Biggest_Increasing_Sequence;

The contract in this example is not correct: :ada:`Current_Index` is a
parameter of :ada:`Test_Index`, so we shouldn't reference it as a global
variable. Also, we should have listed variable :ada:`A` from the outer scope
as an :ada:`Input` in the :ada:`Global` contract.


Example #9
~~~~~~~~~~

Next, we change the :ada:`Global` contract of :ada:`Test_Index` into a
:ada:`Depends` contract. In general, we don't need both contracts because
the set of global variables accessed can be deduced from the :ada:`Depends`
contract.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_09

    package Show_Biggest_Increasing_Sequence is

       type Array_Of_Positives is array (Integer range <>) of Positive;

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural;

    end Show_Biggest_Increasing_Sequence;

    package body Show_Biggest_Increasing_Sequence is

       function Size_Of_Biggest_Increasing_Sequence (A : Array_Of_Positives)
          return Natural
       is
          Max         : Natural := 0;
          End_Of_Seq  : Boolean;
          Size_Of_Seq : Natural := 0;
          Beginning   : Integer := A'First - 1;

          procedure Test_Index (Current_Index : Integer) with
            Depends => ((Max, End_Of_Seq)        => (A, Current_Index, Max),
                        (Size_Of_Seq, Beginning) =>
                           + (A, Current_Index, Max, Beginning))
          is
          begin
             if A (Current_Index) >= Max then
                Max := A (Current_Index);
                End_Of_Seq := False;
             else
                Max         := 0;
                End_Of_Seq  := True;
                Size_Of_Seq := Current_Index - Beginning;
                Beginning   := Current_Index;
             end if;
          end Test_Index;

          Biggest_Seq : Natural := 0;

       begin
          for I in A'Range loop
             Test_Index (I);
             if End_Of_Seq then
                Biggest_Seq := Natural'Max (Size_Of_Seq, Biggest_Seq);
             end if;
          end loop;
          return Biggest_Seq;
       end Size_Of_Biggest_Increasing_Sequence;

    end Show_Biggest_Increasing_Sequence;

This example is correct. Some of the dependencies, such as :ada:`Size_Of_Seq`
depending on :ada:`Beginning`, come directly from the assignments in the
subprogram. Since the control flow influences the final value of all of the
outputs, the variables that are being read, :ada:`A`, :ada:`Current_Index`, and
:ada:`Max`, are present in every dependency relation.  Finally, the
dependencies of :ada:`Size_Of_Eq` and :ada:`Beginning` on themselves are because
they may not be modified by the subprogram execution.


Example #10
~~~~~~~~~~~

The subprogram :ada:`Identity` swaps the value of its parameter two times. Its
:ada:`Depends` contract says that the final value of :ada:`X` only depends on
its initial value and likewise for :ada:`Y`.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.Flow_Analysis.Example_10
    :class: ada-expect-prove-error

    package Show_Swap is

       procedure Swap (X, Y : in out Positive);

       procedure Identity (X, Y : in out Positive) with
         Depends => (X => X,
                     Y => Y);

    end Show_Swap;

    package body Show_Swap is

       procedure Swap (X, Y : in out Positive) is
          Tmp : constant Positive := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap;

       procedure Identity (X, Y : in out Positive) is
       begin
          Swap (X, Y);
          Swap (Y, X);
       end Identity;

    end Show_Swap;

This code is correct, but flow analysis can't verify the :ada:`Depends`
contract of :ada:`Identity` because we didn't supply a :ada:`Depends` contract
for :ada:`Swap`. Therefore, flow analysis assumes that all outputs of
:ada:`Swap`, :ada:`X` and :ada:`Y`, depend on all its inputs, both :ada:`X` and
:ada:`Y`'s initial values. To prevent this, we should manually specify a
:ada:`Depends` contract for :ada:`Swap`.
