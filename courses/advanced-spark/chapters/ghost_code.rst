:code-config:'run_button=False;prove_button=True;accumulate_code=False'

Ghost Code
=====================================================================

.. role:: ada(code)
   :language: ada


What is ghost code?
---------------------------------------------------------------------

    ghost code is part of the program that
    is added for the purpose of specification

    Why3 team, “The Spirit of Ghost Code”

    ... or verification

    addition by SPARK team

- Examples of ghost code:

    - contracts (:ada:`Pre`, :ada:`Post`, :ada:`Contract_Cases`, etc.)

    - assertions (:ada:`pragma Assert`, loop (in)variants, etc.)

    - special values :ada:`Func'Result`, :ada:`Var'Old`, :ada:`Var'Loop_Entry`

- Is it enough?


Ghost code – A trivial example
---------------------------------------------------------------------

- how to express it?

.. code:: ada

    Data : Data_Array;
    Free : Natural;

    procedure Alloc is
    begin
       -- some computations here
       assert that Free “increases”
    end Alloc;


Ghost variables – aka auxiliary variables
---------------------------------------------------------------------

- Variables declared with aspect :ada:`Ghost`

    - declaration is discarded by compiler when ghost code ignored

- Ghost assignments to ghost variables

    - assignment is discarded by compiler when ghost code ignored

.. code:: ada

    Data : Data_Array;
    Free : Natural;

    procedure Alloc is
       Free_Init : Natural with Ghost;
    begin
       Free_Init := Free;
       -- some computations here
       pragma Assert (Free > Free_Init);
    end Alloc;



Ghost variables – non-interference rules
---------------------------------------------------------------------

- Ghost variable cannot be assigned to non-ghost one

    Free := Free_Init;

- Ghost variable cannot indirectly influence assignment to non-ghost one

.. code:: ada

    if Free_Init < Max then
       Free := Free + 1;
    end if;

.. code:: ada

    procedure Assign (From : Natural; To : out Natural) is
    begin
       To := From;
    end Assign;

    Assign (From => Free_Init, To => Free);


Ghost statements
---------------------------------------------------------------------

- Ghost variables can only appear in ghost statements

    - assignments to ghost variables

    - assertions and contracts

    - calls to ghost procedures

.. code:: ada

    procedure Assign (From : Natural; To : out Natural)
       with Ghost
    is
    begin
       To := From;
    end Assign;

    Assign (From => Free, To => Free_Init);

    Assign (From => Free_Init, To => Free);


Ghost procedures
---------------------------------------------------------------------

- Ghost procedures cannot write into non-ghost variables

.. code:: ada

    procedure Assign (Value : Natural) with Ghost is
    begin
       Free := Value;
    end Assign;


- Used to group statements on ghost variables

    - in particular statements not allowed in non-ghost procedures

.. code:: ada

    procedure Assign_Cond (Value : Natural) with Ghost is
    begin
       if Condition then
          Free_Init := Value;
       end if;
    end Assign_Cond;

- Can have :ada:`Global` (including :ada:`Proof_In`) & :ada:`Depends` contracts


Ghost functions
---------------------------------------------------------------------

- Functions for queries used only in contracts

.. code:: ada

    procedure Alloc with
       Pre  => Free_Memory > 0,
       Post => Free_Memory < Free_Memory'Old;

    function Free_Memory return Natural with Ghost;

- Typically implemented as expression functions

    - in private part – proof of client code can use expression

    - or in body – only proof of unit can use expression

.. code:: ada

    function Free_Memory return Natural is (...);
    -- if completion of ghost function declaration

    function Free_Memory return Natural is (...) with Ghost;
    -- if function body as declaration


Imported ghost functions
---------------------------------------------------------------------

- Ghost functions without a body

    - cannot be executed

.. code:: ada

    function Free_Memory return Natural with Ghost, Import;

- Typically used with abstract ghost private types

    - definition in :ada:`SPARK_Mode(Off)`  type is abstract for GNATprove

.. code:: ada

       type Memory_Chunks is private;
       function Free_Memory return Memory_Chunks
         with Ghost, Import;
    private
       pragma SPARK_Mode (Off);
       type Memory_Chunks is null record;

- Definition of ghost types/functions given in proof

    - either in Why3 using :ada:`External_Axiomatization`

    - or in an interactive prover (Coq, Isabelle, etc.)


Ghost packages and ghost abstract state
---------------------------------------------------------------------

- Every entity in a ghost package is ghost

    - local ghost package can group all ghost entities

    - library-level ghost package can be withed/used in regular units

- Ghost abstract state can only represent ghost variables

.. code:: ada

    package Mem with
       Abstract_State => (State with Ghost)
    is

    package body Mem with
       Refined_State => (State => (Data, Free, Free_Init))
    is

- Non-ghost abstract state can contain both ghost and non-ghost variables


Executing ghost code
---------------------------------------------------------------------

- Ghost code can be enabled globally

    - using compilation switch ``-gnata`` (for all assertions)

- Ghost code can be enabled selectively

    - using :ada:`pragma Assertion_Policy (Ghost => Check)`

    - SPARK rules enforce consistency – in particular no write disabled

.. code:: ada

    pragma Assertion_Policy (Ghost => Ignore, Pre => Check);

    procedure Alloc with
       Pre => Free_Memory > 0;

    function Free_Memory return Natural with Ghost;

- GNATprove analyzes all ghost code and assertions


Example of use – encoding a state automaton
---------------------------------------------------------------------

- Tetris in SPARK

    - at `Tetris <http://blog.adacore.com/tetris-in-spark-on-arm-cortex-m4>`_

- Global state encoded in global ghost variable

    - updated at the end of procedures of the API

.. code:: ada

    type State is (Piece_Falling, ...) with Ghost;
    Cur_State : State with Ghost;

- Properties encoded in ghost functions

.. code:: ada

    function Valid_Configuration return Boolean is
       (case Cur_State is
          when Piece_Falling => ...,
          when ...)
    with Ghost;


Example of use – expressing useful lemmas
---------------------------------------------------------------------

- GCD in SPARK

    - at `GCD <http://www.spark-2014.org/entries/detail/gnatprove-tips-and-tricks-proving-the-ghost-common-denominator-gcd>`_

- Lemmas expressed as ghost procedures

.. code:: ada

    procedure Lemma_Not_Divisor (Arg1, Arg2 : Positive) with
       Ghost,
       Global => null,
       Pre  => Arg1 in Arg2 / 2 + 1 .. Arg2 - 1,
       Post => not Divides (Arg1, Arg2);

- Most complex lemmas further refined into other lemmas

    - code in procedure body used to guide proof (e.g. for induction)


Example of use – specifying an API through a model
---------------------------------------------------------------------

- Red black trees in SPARK

    - at `Red black trees <http://www.spark-2014.org/entries/detail/research-corner-auto-active-verification-in-spark>`_

- Invariants of data structures expressed as ghost functions

    - using :ada:`Type_Invariant` on private types

- Model of data structures expressed as ghost functions

    - called from :ada:`Pre` / :ada:`Post` of subprograms from the API

- Lemmas expressed as ghost procedures

    - sometimes without contracts to benefit from inlining in proof


Extreme proving with ghost code – red black trees in SPARK
---------------------------------------------------------------------

.. image:: ghost_code_red_black.png
   :align: center


Positioning ghost code in proof techniques
---------------------------------------------------------------------

.. image:: ghost_code_degree_of_automation.png
   :align: center


Code Examples / Pitfalls
---------------------------------------------------------------------

Example #1
~~~~~~~~~~

.. code:: ada

    Data : Data_Array;
    Free : Natural;

    procedure Alloc is
       Free_Init : Natural with Ghost;
    begin
       Free_Init := Free;
       -- some computations here
       if Free <= Free_Init then
          raise Program_Error;
       end if;
    end Alloc;

This code is not correct.

ghost entity cannot appear in this context


Example #2
~~~~~~~~~~

.. code:: ada

    Data : Data_Array;
    Free : Natural;

    procedure Alloc is
       Free_Init : Natural with Ghost;

       procedure Check with Ghost is
       begin
          if Free <= Free_Init then
             raise Program_Error;
          end if;
       end Check;
    begin
       Free_Init := Free;
       -- some computations here
       Check;
    end Alloc;

This code is correct.

Note that procedure ``Check`` is inlined for proof (no contract).


Example #3
~~~~~~~~~~

.. code:: ada

    pragma Assertion_Policy (Pre => Check);

    procedure Alloc with
       Pre => Free_Memory > 0;

    function Free_Memory return Natural with Ghost;

This code is not correct.

Incompatible ghost policies in effect during compilation, as ghost code is ignored by default.

Note that GNATprove accepts this code as it enables all ghost code and assertions.


Example #4
~~~~~~~~~~

.. code:: ada

    procedure Alloc with
       Post => Free_Memory < Free_Memory'Old;

    function Free_Memory return Natural with Ghost;

    Max : constant := 1000;

    function Free_Memory return Natural is
    begin
       return Max - Free + 1;
    end Free_Memory;

    procedure Alloc is
    begin
       Free := Free + 10;
    end Alloc;

This code is not correct.

No postcondition on ``Free_Memory`` that would allow proving the postcondition on ``Alloc``.


Example #5
~~~~~~~~~~

.. code:: ada

    procedure Alloc with
       Post => Free_Memory < Free_Memory'Old;

    function Free_Memory return Natural with Ghost;

    Max : constant := 1000;

    function Free_Memory return Natural is (Max - Free + 1);

    procedure Alloc is
    begin
       Free := Free + 10;
    end Alloc;

This code is correct.

``Free_Memory`` has an implicit postcondition as an expression function.


Example #6
~~~~~~~~~~

.. code:: ada

    subtype Resource is Natural range 0 .. 1000;
    subtype Num is Natural range 0 .. 6;
    subtype Index is Num range 1 .. 6;
    type Data is array (Index) of Resource;

    function Sum (D : Data; To : Num) return Natural is
      (if To = 0 then 0 else D(To) + Sum(D,To-1))
    with Ghost;

    procedure Create (D : out Data) with
       Post => Sum (D, D'Last) < 42
    is
    begin
       for J in D'Range loop
          D(J) := J;
          pragma Loop_Invariant (2 * Sum(D,J) <= J * (J+1));
       end loop;
    end Create;

This code is not correct.

info: expression function body not available for proof
(``Sum`` may not return)


Example #7
~~~~~~~~~~

.. code:: ada

    subtype Resource is Natural range 0 .. 1000;
    subtype Num is Natural range 0 .. 6;
    subtype Index is Num range 1 .. 6;
    type Data is array (Index) of Resource;

    function Sum (D : Data; To : Num) return Natural is
      (if To = 0 then 0 else D(To) + Sum(D,To-1))
    with Ghost, Annotate => (GNATprove, Terminating);

    procedure Create (D : out Data) with
       Post => Sum (D, D'Last) < 42
    is
    begin
       for J in D'Range loop
          D(J) := J;
          pragma Loop_Invariant (2 * Sum(D,J) <= J * (J+1));
       end loop;
    end Create;

This code is correct.

Note that GNATprove does not prove the termination of ``Sum`` here.


Example #8
~~~~~~~~~~

.. code:: ada

    subtype Resource is Natural range 0 .. 1000;
    subtype Num is Natural range 0 .. 6;
    subtype Index is Num range 1 .. 6;
    type Data is array (Index) of Resource;

    function Sum (D : Data; To : Num) return Natural is
      (if To = 0 then 0 else D(To) + Sum(D,To-1))
    with Ghost, Annotate => (GNATprove, Terminating);

    procedure Create (D : out Data) with
       Post => Sum (D, D'Last) < 42
    is
    begin
       for J in D'Range loop
          D(J) := J;
       end loop;
    end Create;


This code is correct.

The loop is unrolled by GNATprove here, as :ada:`D'Range` is :ada:`0 .. 6`.
The automatic prover unrolls the recursive definition of ``Sum``.


Example #9
~~~~~~~~~~

.. code:: ada

    subtype Resource is Natural range 0 .. 1000;
    subtype Index is Natural range 1 .. 42;

    package Seqs is new
      Ada.Containers.Functional_Vectors (Index, Resource);   use Seqs;

    function Create return Sequence with
       Post => (for all K in 1 .. Last (Create'Result) =>
                  Get (Create'Result, K) = K)
    is
       S : Sequence;
    begin
       for K in 1 .. 42 loop
          S := Add (S, K);
       end loop;
       return S;
    end Create;

This code is not correct.

Loop requires a loop invariant to prove the postcondition.


Example #10
~~~~~~~~~~~

.. code:: ada

    subtype Resource is Natural range 0 .. 1000;
    subtype Index is Natural range 1 .. 42;

    package Seqs is new
      Ada.Containers.Functional_Vectors (Index, Resource);   use Seqs;

    function Create return Sequence with
       Post => (for all K in 1 .. Last (Create'Result) =>
                  Get (Create'Result, K) = K)
    is
       S : Sequence;
    begin
       for K in 1 .. 42 loop
          S := Add (S, K);
          pragma Loop_Invariant (Integer (Length (S)) = K);
          pragma Loop_Invariant
            (for all J in 1 .. K => Get (S, J) = J);
       end loop;
       return S;
    end Create;

This code is correct.
