State Abstraction
=====================================================================

.. include:: ../../../global.txt

Abstraction is a key concept in programming that can drastically simplify
both the implementation and maintenance of code. It's particularly well
suited to SPARK and its modular analysis. This section explains what state
abstraction is and how you use it in SPARK. We explain how it impacts
GNATprove's analysis both in terms of information flow and proof of program
properties.

State abstraction allows us to:

- express dependencies that wouldn't otherwise be expressible because some
  data that's read or written isn't visible at the point where a subprogram
  is declared |mdash| examples are dependencies on data, for which we use the
  :ada:`Global` contract, and on flow, for which we use the :ada:`Depends`
  contract.

- reduce the number of variables that need to be considered in flow
  analysis and proof, a reduction which may be critical in order to scale
  the analysis to programs with thousands of global variables.


What's an Abstraction?
---------------------------------------------------------------------

Abstraction is an important part of programming language design. It
provides two views of the same object: an abstract one and a refined
one. The abstract one |mdash| usually called *specification* |mdash| describes
what the object does in a coarse way. A subprogram's specification usually
describes how it should be called (e.g., parameter information such as how
many and of what types) as well as what it does (e.g., returns a result or
modifies one or more of its parameters).

Contract-based programming, as supported in Ada, allows contracts to be
added to a subprogram's specification. You use contracts to describe the
subprogram's behavior in a more fine-grained manner, but all the details of
how the subprogram actually works are left to its refined view, its
implementation.

Take a look at the example code shown below.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.No_Abstraction

    procedure Increase (X : in out Integer) with
      Global => null,
      Pre    => X <= 100,
      Post   => X'Old < X;

    procedure Increase (X : in out Integer) is
    begin
       X := X + 1;
    end Increase;

We've written a specification of the subprogram :ada:`Increase` to say that it's
called with a single argument, a variable of type :ada:`Integer` whose
initial value is less than 100. Our contract says that the only effect of
the subprogram is to increase the value of its argument.


Why is Abstraction Useful?
---------------------------------------------------------------------

A good abstraction of a subprogram's implementation is one whose
specification precisely and completely summarizes what its callers can rely
on. In other words, a caller of that subprogram shouldn't rely on any
behavior of its implementation if that behavior isn't documented in its
specification.

For example, callers of the subprogram :ada:`Increase` can assume that it
always strictly increases the value of its argument. In the code snippet
shown below, this means the loop must terminate.

.. code:: ada prove_button project=Courses.Intro_To_Spark.State_Abstraction.Using_Abstraction
    :class: ada-expect-prove-error

    procedure Increase (X : in out Integer) with
      Global => null,
      Pre    => X <= 100,
      Post   => X'Old < X;

    with Increase;
    procedure Client is
       X : Integer := 0;
    begin
       while X <= 100 loop      --  The loop will terminate
          Increase (X);         --  Increase can be called safely
       end loop;
       pragma Assert (X = 101); --  Will this hold?
    end Client;

Callers can also assume that the implementation of :ada:`Increase` won't cause
any runtime errors when called in the loop. On the other hand, nothing in
the specification guarantees that the assertion show above is correct: it
may fail if :ada:`Increase`'s implementation is changed.

If you follow this basic principle, abstraction can bring you significant
benefits. It simplifies both your program's implementation and
verification. It also makes maintenance and code reuse much easier since
changes to the implementation of an object shouldn't affect the code using
this object.  Your goal in using it is that it should be enough to
understand the specification of an object in order to use that object,
since understanding the specification is usually much simpler than
understanding the implementation.

GNATprove relies on the abstraction defined by subprogram contracts and
therefore doesn't prove the assertion after the loop in :ada:`Client` above.

Abstraction of a Package's State
---------------------------------------------------------------------

Subprograms aren't the only objects that benefit from abstraction.  The
state of a package |mdash| the set of persistent variables defined in it |mdash|
can also be hidden from external users. You achieve this form of
abstraction |mdash| called *state abstraction* |mdash| by defining variables in
the body or private part of a package so they can only be accessed through
subprogram calls. For example, our :ada:`Stack` package shown below provides
an abstraction for a :ada:`Stack` object which can only be modified using the
:ada:`Pop` and :ada:`Push` procedures.

.. code-block:: ada

    package Stack is
       procedure Pop  (E : out Element);
       procedure Push (E : in  Element);
    end Stack;

    package body Stack is
       Content : Element_Array (1 .. Max);
       Top     : Natural;
       ...
    end Stack;

The fact that we implemented it using an array is irrelevant to the caller.
We could change that without impacting our callers' code.


Declaring a State Abstraction
---------------------------------------------------------------------

Hidden state influences a program's behavior, so SPARK allows that state to
be declared.  You can use the :ada:`Abstract_State` aspect, an abstraction
that names a state, to do this, but you aren't required to use it even for
a package with hidden state.  You can use several state abstractions to
declare the hidden state of a single package or you can use it for a
package with no hidden state at all. However, since SPARK doesn't allow
aliasing, different state abstractions must always refer to disjoint sets
of variables.  A state abstraction isn't a variable: it doesn't have a type
and can't be used inside expressions, either those in bodies or contracts.

As an example of the use of this aspect, we can optionally define a state
abstraction for the entire hidden state of the :ada:`Stack` package like this:

.. code-block:: ada

    package Stack with
      Abstract_State => The_Stack
    is
      ...

Alternatively, we can define a state abstraction for each hidden variable:

.. code-block:: ada

    package Stack with
      Abstract_State => (Top_State, Content_State)
    is
      ...

Remember: a state abstraction isn't a variable (it has no type) and can't
be used inside expressions. For example:

.. code-block:: ada

    pragma Assert (Stack.Top_State = ...);
    -- compilation error: Top_State is not a variable


Refining an Abstract State
---------------------------------------------------------------------

Once you've declared an abstract state in a package, you must refine it
into its constituents using a :ada:`Refined_State` aspect. You must place
the :ada:`Refined_State` aspect on the package body even if the package
wouldn't otherwise have required a body. For each state abstraction you've
declared for the package, you list the set of variables represented by that
state abstraction in its refined state.

If you specify an abstract state for a package, it must be complete,
meaning you must have listed every hidden variable as part of some state
abstraction. For example, we must add a :ada:`Refined_State` aspect on our
:ada:`Stack` package's body linking the state abstraction (:ada:`The_Stack`) to
the entire hidden state of the package, which consists of both :ada:`Content`
and :ada:`Top`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Refined_State

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element);
       procedure Push (E : Element);

    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;
       --  Both Content and Top must be listed in the list of
       --  constituents of The_Stack

       procedure Pop (E : out Element) is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;

    end Stack;

Representing Private Variables
---------------------------------------------------------------------

You can refine state abstractions in the package body, where all the
variables are visible. When only the package's specification is available,
you need a way to specify which state abstraction each private variable
belongs to. You do this by adding the :ada:`Part_Of` aspect to the
variable's declaration.

:ada:`Part_Of` annotations are mandatory: if you gave a package an abstract
state annotation, you must link all the hidden variables defined in its
private part to a state abstraction. For example:

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Private_Variables

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element);
       procedure Push (E : Element);

    private

       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array          with Part_Of => The_Stack;
       Top     : Natural range 0 .. Max with Part_Of => The_Stack;

    end Stack;

Since we chose to define :ada:`Content` and :ada:`Top` in :ada:`Stack`'s private
part instead of its body, we had to add a :ada:`Part_Of` aspect to both of
their declarations, associating them with the state abstraction
:ada:`The_Stack`, even though it's the only state abstraction. However, we
still need to list them in the :ada:`Refined_State` aspect in :ada:`Stack`'s
body.

.. code-block:: ada

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))


Additional State
---------------------------------------------------------------------

Nested Packages
~~~~~~~~~~~~~~~

So far, we've only discussed hidden variables. But variables aren't the
only component of a package's state. If a package :ada:`P` contains a nested
package, the nested package's state is also part of :ada:`P`'s state.  If the
nested package is hidden, its state is part of :ada:`P`'s hidden state and
must be listed in :ada:`P`'s state refinement.

We see this in the example below, where the package :ada:`Hidden_Nested`'s
hidden state is part of :ada:`P`'s hidden state.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Nested_Packages

    package P with
       Abstract_State => State
    is
       package Visible_Nested with
          Abstract_State => Visible_State
       is
          procedure Get (E : out Integer);
       end Visible_Nested;
    end P;

    package body P with
       Refined_State => (State => Hidden_Nested.Hidden_State)
    is
       package Hidden_Nested with
          Abstract_State => Hidden_State,
          Initializes    => Hidden_State
       is
          function Get return Integer;
       end Hidden_Nested;

       package body Hidden_Nested with
          Refined_State => (Hidden_State => Cnt)
       is
          Cnt : Integer := 0;

          function Get return Integer is (Cnt);
       end Hidden_Nested;

       package body Visible_Nested with
          Refined_State => (Visible_State => Checked)
       is
          Checked : Boolean := False;

          procedure Get (E : out Integer) is
          begin
             Checked := True;
             E := Hidden_Nested.Get;
          end Get;
       end Visible_Nested;
    end P;

Any visible state of :ada:`Hidden_Nested` would also have been part of :ada:`P`'s
hidden state.  However, if :ada:`P` contains a visible nested package, that
nested package's state isn't part of :ada:`P`'s hidden state.  Instead, you
should declare that package's hidden state in a separate state abstraction
on its own declaration, like we did above for :ada:`Visible_Nested`.


Constants that Depend on Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some constants are also possible components of a state abstraction. These
are constants whose value depends either on a variable or a subprogram
parameter.  They're handled as variables during flow analysis because they
participate in the flow of information between variables throughout the
program. Therefore, GNATprove considers these constants to be part of a
package's state just like it does for variables.

If you've specified a state abstraction for a package, you must list such
hidden constants declared in that package in the state abstraction
refinement. However, constants that don't depend on variables don't
participate in the flow of information and must not appear in a state
refinement.

Let's look at this example.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Constants_And_Variables

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element);
       procedure Push (E : Element);
    end Stack;

    package Configuration with
      Initializes => External_Variable
    is
       External_Variable : Positive with Volatile;
    end Configuration;

    with Configuration;
    pragma Elaborate (Configuration);

    package body Stack with
      Refined_State => (The_Stack => (Content, Top, Max))
      --  Max has variable inputs. It must appear as a
      --  constituent of The_Stack
    is
       Max : constant Positive := Configuration.External_Variable;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;

       procedure Pop (E : out Element) is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;

    end Stack;

Here, :ada:`Max` |mdash| the maximum number of elements that can be stored in
the stack |mdash| is initialized from a variable in an external package.
Because of this, we must include :ada:`Max` as part of the state abstraction
:ada:`The_Stack`.

.. note::

   For more details on state abstractions, see the
   :spark_ugs:`SPARK User's Guide <package_contracts.html#state-abstraction>`.


Subprogram Contracts
---------------------------------------------------------------------

Global and Depends
~~~~~~~~~~~~~~~~~~

Hidden variables can only be accessed through subprogram calls, so you
document how state abstractions are modified during the program's execution
via the contracts of those subprograms.  You use :ada:`Global` and
:ada:`Depends` contracts to specify which of the state abstractions are
used by a subprogram and how values flow through the different variables.
The :ada:`Global` and :ada:`Depends` contracts that you write when
referring to state abstractions are often less precise than contracts
referring to visible variables since the possibly different dependencies of
the hidden variables contained within a state abstraction are collapsed
into a single dependency.

Let's add :ada:`Global` and :ada:`Depends` contracts to the :ada:`Pop`
procedure in our stack.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Global_Depends

    package Stack with
       Abstract_State => (Top_State, Content_State)
    is
       type Element is new Integer;

       procedure Pop (E : out Element) with
         Global  => (Input  => Content_State,
                     In_Out => Top_State),
         Depends => (Top_State => Top_State,
                     E         => (Content_State, Top_State));

    end Stack;

In this example, the :ada:`Pop` procedure only modifies the value of the
hidden variable :ada:`Top`, while :ada:`Content` is unchanged. By using distinct
state abstractions for the two variables, we're able to preserve this
semantic in the contract.

Let's contrast this example with a different representation of
:ada:`Global` and :ada:`Depends` contracts, this time using a single
abstract state.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Global_Single_Abstract_State

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element) with
         Global  => (In_Out => The_Stack),
         Depends => ((The_Stack, E) => The_Stack);

    end Stack;

Here, :ada:`Top_State` and :ada:`Content_State` are merged into a single state
abstraction, :ada:`The_Stack`. By doing so, we've hidden the fact that
:ada:`Content` isn't modified (though we're still showing that :ada:`Top` may be
modified).  This loss in precision is reasonable here, since it's the whole
point of the abstraction. However, you must be careful not to aggregate
unrelated hidden state because this risks their annotations becoming
meaningless.

Even though imprecise contracts that consider state abstractions as a whole
are perfectly reasonable for users of a package, you should write
:ada:`Global` and :ada:`Depends` contracts that are as precise as possible
within the package body. To allow this, SPARK introduces the notion of
*refined contracts*, which are precise contracts specified on the bodies of
subprograms where state refinements are visible. These contracts are the
same as normal :ada:`Global` and :ada:`Depends` contracts except they refer
directly to the hidden state of the package.

When a subprogram is called inside the package body, you should write
refined contracts instead of the general ones so that the verification can
be as precise as possible. However, refined :ada:`Global` and
:ada:`Depends` are optional: if you don't specify them, GNATprove will
compute them to check the package's implementation.

For our :ada:`Stack` example, we could add refined contracts as shown below.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Global_Refined

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element) with
         Global  => (In_Out => The_Stack),
         Depends => ((The_Stack, E) => The_Stack);

       procedure Push (E : Element) with
         Global  => (In_Out    => The_Stack),
         Depends => (The_Stack => (The_Stack, E));

    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;

       procedure Pop (E : out Element) with
         Refined_Global  => (Input  => Content,
                             In_Out => Top),
         Refined_Depends => (Top => Top,
                             E   => (Content, Top))
       is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

       procedure Push (E : Element) with
         Refined_Global  => (In_Out => (Content, Top)),
         Refined_Depends => (Content =>+ (Content, Top, E),
                             Top     => Top) is
       begin
         Top := Top + 1;
         Content (Top) := E;
       end Push;

    end Stack;

Preconditions and Postconditions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We mostly express functional properties of subprograms using preconditions
and postconditions.  These are standard Boolean expressions, so they can't
directly refer to state abstractions. To work around this restriction, we
can define functions to query the value of hidden variables. We then use
these functions in place of the state abstraction in the contract of other
subprograms.

For example, we can query the state of the stack with functions
:ada:`Is_Empty` and :ada:`Is_Full` and call these in the contracts of procedures
:ada:`Pop` and :ada:`Push`:

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Pre_Postconditions_1

    package Stack is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Pop (E : out Element) with
         Pre  => not Is_Empty,
         Post => not Is_Full;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;

    end Stack;

    package body Stack is

       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;

       function Is_Empty return Boolean is (Top = 0);
       function Is_Full  return Boolean is (Top = Max);

       procedure Pop (E : out Element) is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;

    end Stack;

Just like we saw for :ada:`Global` and :ada:`Depends` contracts, you may
often find it useful to have a more precise view of functional contracts in
the context where the hidden variables are visible. You do this using
expression functions in the same way we did for the functions :ada:`Is_Empty`
and :ada:`Is_Full` above. As expression function, bodies act as contracts for
GNATprove, so they automatically give a more precise version of the
contracts when their implementation is visible.

You may often need a more constraining contract to verify the package's
implementation but want to be less strict outside the abstraction.  You do
this using the :ada:`Refined_Post` aspect. This aspect, when placed on a
subprogram's body, provides stronger guarantees to internal callers of a
subprogram. If you provide one, the refined postcondition must imply the
subprogram's postcondition. This is checked by GNATprove, which reports a
failing postcondition if the refined postcondition is too weak, even if
it's actually implied by the subprogram's body. SPARK doesn't peform a
similar verification for normal preconditions.

For example, we can refine the postconditions in the bodies of :ada:`Pop` and
:ada:`Push` to be more detailed than what we wrote for them in their
specification.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Pre_Postconditions_2

    package Stack is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Pop (E : out Element) with
         Pre  => not Is_Empty,
         Post => not Is_Full;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;

    end Stack;

    package body Stack is

       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;

       function Is_Empty return Boolean is (Top = 0);
       function Is_Full  return Boolean is (Top = Max);

       procedure Pop (E : out Element) with
         Refined_Post => not Is_Full and E = Content (Top)'Old
       is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

       procedure Push (E : Element) with
         Refined_Post => not Is_Empty and E = Content (Top)
       is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;

    end Stack;

.. note::

   For more details on refinement in contracts, see the
   :spark_ugs:`SPARK User's Guide <subprogram_contracts.html#state-abstraction-and-contracts>`.

Initialization of Local Variables
---------------------------------------------------------------------

As part of flow analysis, GNATprove checks for the proper initialization of
variables. Therefore, flow analysis needs to know which variables are
initialized during the package's elaboration.

.. todo::

   What is the target audience of this book? If its people who have no
   Ada experience then they likely have no idea what elaboration is.
   One option is to add the prerequisite knowledge for this book in the intro,
   and link to the Introduction to Ada (which also does not discuss elaboration).

You can use the :ada:`Initializes` aspect to specify the set of visible
variables and state abstractions that are initialized during the
elaboration of a package.  An :ada:`Initializes` aspect can't refer to a
variable that isn't defined in the unit since, in SPARK, a package can only
initialize variables declared immediately within the package.

:ada:`Initializes` aspects are optional. If you don't supply any, they'll
be derived by GNATprove.

For our :ada:`Stack` example, we could add an :ada:`Initializes` aspect.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Local_Init

    package Stack with
      Abstract_State => The_Stack,
      Initializes    => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element);

    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       Max : constant := 100;

       type Element_Array is array (1 .. Max) of Element;

       Content : Element_Array := (others => 0);
       Top     : Natural range 0 .. Max := 0;

       procedure Pop (E : out Element) is
       begin
          E   := Content (Top);
          Top := Top - 1;
       end Pop;

    end Stack;

Flow analysis also checks for dependencies between variables, so it must be
aware of how information flows through the code that performs the
initialization of states.  We discussed one use of the :ada:`Initializes`
aspect above.  But you also can use it to provide flow information. If the
initial value of a variable or state abstraction is dependent on the value
of another visible variable or state abstraction from another package, you
must list this dependency in the :ada:`Initializes` contract. You specify
the list of entities on which a variable's initial value depends using an
arrow following that variable's name.

Let's look at this example:

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Initializes

    package Q is
       External_Variable : Integer := 2;
    end Q;

    with Q;
    package P with
      Initializes => (V1, V2 => Q.External_Variable)
    is
       V1 : Integer := 0;
       V2 : Integer := Q.External_Variable;
    end P;

Here we indicated that :ada:`V2`'s initial value depends on the value of
:ada:`Q.External_Variable` by including that dependency in the
:ada:`Initializes` aspect of :ada:`P`.  We didn't list any dependency for
:ada:`V1` because its initial value doesn't depend on any external
variable. We could also have stated that lack of dependency explicitly by
writing :ada:`V1 => null`.

GNATprove computes dependencies of initial values if you don't supply an
:ada:`Initializes` aspect.  However, if you do provide an
:ada:`Initializes` aspect for a package, it must be complete: you must list
every initialized state of the package, along with all its external
dependencies.

.. note::

   For more details on :ada:`Initializes`, see the
   :spark_ugs:`SPARK User's Guide <package_contracts.html#package-initialization>`.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples to illustrate potential pitfalls.

Example #1
~~~~~~~~~~

Package :ada:`Communication` defines a hidden local package, :ada:`Ring_Buffer`,
whose capacity is initialized from an external configuration during
elaboration.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Example_01
    :class: ada-expect-prove-error

    package Configuration is

       External_Variable : Natural := 1;

    end Configuration;

    with Configuration;

    package Communication with
      Abstract_State => State,
      Initializes    => (State => Configuration.External_Variable)
    is
       function Get_Capacity return Natural;

    private

       package Ring_Buffer with
         Initializes => (Capacity => Configuration.External_Variable)
       is
          Capacity : constant Natural := Configuration.External_Variable;
       end Ring_Buffer;

    end Communication;

    package body Communication with
      Refined_State => (State => Ring_Buffer.Capacity)
    is

       function Get_Capacity return Natural is
       begin
          return Ring_Buffer.Capacity;
       end Get_Capacity;

    end Communication;

This example isn't correct. :ada:`Capacity` is declared in the private part
of :ada:`Communication`. Therefore, we should have linked it to :ada:`State` by
using the :ada:`Part_Of` aspect in its declaration.


Example #2
~~~~~~~~~~

Let's add :ada:`Part_Of` to the state of hidden local package :ada:`Ring_Buffer`,
but this time we hide variable :ada:`Capacity` inside the private part of
:ada:`Ring_Buffer`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Example_02

    package Configuration is

       External_Variable : Natural := 1;

    end Configuration;

    with Configuration;

    package Communication with
      Abstract_State => State
    is
    private

       package Ring_Buffer with
         Abstract_State => (B_State with Part_Of => State),
         Initializes    => (B_State => Configuration.External_Variable)
       is
          function Get_Capacity return Natural;
       private
          Capacity : constant Natural := Configuration.External_Variable
            with Part_Of => B_State;
       end Ring_Buffer;

    end Communication;

    package body Communication with
      Refined_State => (State => Ring_Buffer.B_State)
    is

       package body Ring_Buffer with
          Refined_State => (B_State => Capacity)
       is
          function Get_Capacity return Natural is (Capacity);
       end Ring_Buffer;

    end Communication;

This program is correct and GNATprove is able to verify it.


Example #3
~~~~~~~~~~

Package :ada:`Counting` defines two counters: :ada:`Black_Counter` and
:ada:`Red_Counter`. It provides separate initialization procedures for each,
both called from the main procedure.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Example_03
    :class: ada-expect-prove-error

    package Counting with
      Abstract_State => State
    is
       procedure Reset_Black_Count;
       procedure Reset_Red_Count;
    end Counting;

    package body Counting with
      Refined_State => (State => (Black_Counter, Red_Counter))
    is
       Black_Counter, Red_Counter : Natural;

       procedure Reset_Black_Count is
       begin
          Black_Counter := 0;
       end Reset_Black_Count;

       procedure Reset_Red_Count is
       begin
          Red_Counter := 0;
       end Reset_Red_Count;
    end Counting;

    with Counting; use Counting;

    procedure Main is
    begin
       Reset_Black_Count;
       Reset_Red_Count;
    end Main;

This program doesn't read any uninitialized data, but GNATprove fails to
verify that. This is because we provided a state abstraction for package
:ada:`Counting`, so flow analysis computes the effects of subprograms in terms
of this state abstraction and thus considers :ada:`State` to be an in-out
global consisting of both :ada:`Black_Counter` and
:ada:`Red_Counter`. So it issues the message requiring that :ada:`State` be
initialized after elaboration as well as the warning that no procedure in
package :ada:`Counting` can initialize its state.


Example #4
~~~~~~~~~~

Let's remove the abstract state on package :ada:`Counting`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Example_04

    package Counting is
       procedure Reset_Black_Count;
       procedure Reset_Red_Count;
    end Counting;

    package body Counting is
       Black_Counter, Red_Counter : Natural;

       procedure Reset_Black_Count is
       begin
          Black_Counter := 0;
       end Reset_Black_Count;

       procedure Reset_Red_Count is
       begin
          Red_Counter := 0;
       end Reset_Red_Count;
    end Counting;

    with Counting; use Counting;

    procedure Main is
    begin
       Reset_Black_Count;
       Reset_Red_Count;
    end Main;

This example is correct. Because we didn't provide a state abstraction,
GNATprove reasons in terms of variables, instead of states, and proves data
initialization without any problem.


Example #5
~~~~~~~~~~

Let's restore the abstract state to package :ada:`Counting`, but this time
provide a procedure :ada:`Reset_All` that calls the initialization procedures
:ada:`Reset_Black_Counter` and :ada:`Reset_Red_Counter`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Example_05

    package Counting with
      Abstract_State => State
    is
       procedure Reset_Black_Count with Global => (In_Out => State);
       procedure Reset_Red_Count   with Global => (In_Out => State);
       procedure Reset_All         with Global => (Output => State);
    end Counting;

    package body Counting with
      Refined_State => (State => (Black_Counter, Red_Counter))
    is
       Black_Counter, Red_Counter : Natural;

       procedure Reset_Black_Count is
       begin
          Black_Counter := 0;
       end Reset_Black_Count;

       procedure Reset_Red_Count is
       begin
          Red_Counter := 0;
       end Reset_Red_Count;

       procedure Reset_All is
       begin
          Reset_Black_Count;
          Reset_Red_Count;
       end Reset_All;
    end Counting;

This example is correct.  Flow analysis computes refined versions of
:ada:`Global` contracts for internal calls and uses these to verify that
:ada:`Reset_All` indeed properly initializes :ada:`State`. The
:ada:`Refined_Global` and :ada:`Global` annotations are not mandatory and
can be computed by GNATprove.

Example #6
~~~~~~~~~~

Let's consider yet another version of our abstract stack unit.

.. code:: ada no_button gnat=12.2.0-1 gnatprove=12.1.0-1 project=Courses.Intro_To_Spark.State_Abstraction.Example_06
    :class: ada-compile, ada-prove-flow, ada-expect-compile-error, ada-expect-prove-error

    package Stack with
      Abstract_State => The_Stack
    is
       pragma Unevaluated_Use_Of_Old (Allow);

       type Element is new Integer;

       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;
       subtype Length_Type is Natural range 0 .. Max;

       procedure Push (E : Element) with
         Post =>
           not Is_Empty and
           (if Is_Full'Old then The_Stack = The_Stack'Old else Peek = E);

       function Peek     return Element with Pre => not Is_Empty;
       function Is_Full  return Boolean;
       function Is_Empty return Boolean;
    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Top, Content))
    is
       Top     : Length_Type := 0;
       Content : Element_Array (1 .. Max) := (others => 0);

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;

       function  Peek     return Element is (Content (Top));
       function  Is_Full  return Boolean is (Top >= Max);
       function  Is_Empty return Boolean is (Top = 0);
    end Stack;

This example isn't correct. There's a compilation error in :ada:`Push`'s
postcondition: :ada:`The_Stack` is a state abstraction, not a variable, and
therefore can't be used in an expression.


Example #7
~~~~~~~~~~

In this version of our abstract stack unit, a copy of the stack is returned
by function :ada:`Get_Stack`, which we call in the postcondition of :ada:`Push`
to specify that the stack shouldn't be modified if it's full.  We also
assert that after we push an element on the stack, either the stack is
unchanged (if it was already full) or its top element is equal to the
element just pushed.

.. code:: ada prove_button project=Courses.Intro_To_Spark.State_Abstraction.Example_07
    :class: ada-expect-prove-error

    package Stack with
      Abstract_State => The_Stack
    is
       pragma Unevaluated_Use_Of_Old (Allow);

       type Stack_Model is private;

       type Element is new Integer;
       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;
       subtype Length_Type is Natural range 0 .. Max;

       function Peek      return Element with Pre => not Is_Empty;
       function Is_Full   return Boolean;
       function Is_Empty  return Boolean;
       function Get_Stack return Stack_Model;

       procedure Push (E : Element) with
         Post => not Is_Empty and
           (if Is_Full'Old then Get_Stack = Get_Stack'Old else Peek = E);

    private

       type Stack_Model is record
          Top     : Length_Type := 0;
          Content : Element_Array (1 .. Max) := (others => 0);
       end record;

    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Top, Content))
    is
       Top     : Length_Type := 0;
       Content : Element_Array (1 .. Max) := (others => 0);

       procedure Push (E : Element) is
       begin
          if Top >= Max then
             return;
          end if;
          Top             := Top + 1;
          Content (Top) := E;
       end Push;

       function Peek     return Element is (Content (Top));
       function Is_Full  return Boolean is (Top >= Max);
       function Is_Empty return Boolean is (Top = 0);

       function Get_Stack return Stack_Model is (Stack_Model'(Top, Content));

    end Stack;

    with Stack; use Stack;

    procedure Use_Stack (E : Element) with
      Pre => not Is_Empty
    is
       F : Element := Peek;
    begin
       Push (E);
       pragma Assert (Peek = E or Peek = F);
    end Use_Stack;

This program is correct, but GNATprove can't prove the assertion in
:ada:`Use_Stack`. Indeed, even if :ada:`Get_Stack` is an expression function, its
body isn't visible outside of :ada:`Stack`'s body, where it's defined.


Example #8
~~~~~~~~~~

Let's move the definition of :ada:`Get_Stack` and other expression functions
inside the private part of the spec of :ada:`Stack`.

.. code:: ada prove_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Example_08

    package Stack with
      Abstract_State => The_Stack
    is
       pragma Unevaluated_Use_Of_Old (Allow);

       type Stack_Model is private;

       type Element is new Integer;
       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;
       subtype Length_Type is Natural range 0 .. Max;

       function Peek      return Element with Pre => not Is_Empty;
       function Is_Full   return Boolean;
       function Is_Empty  return Boolean;
       function Get_Stack return Stack_Model;

       procedure Push (E : Element) with
         Post => not Is_Empty and
           (if Is_Full'Old then Get_Stack = Get_Stack'Old else Peek = E);

    private

       Top     : Length_Type              := 0 with Part_Of => The_Stack;
       Content : Element_Array (1 .. Max) := (others => 0) with
         Part_Of => The_Stack;

       type Stack_Model is record
          Top     : Length_Type := 0;
          Content : Element_Array (1 .. Max) := (others => 0);
       end record;

       function Peek      return Element     is (Content (Top));
       function Is_Full   return Boolean     is (Top >= Max);
       function Is_Empty  return Boolean     is (Top = 0);

       function Get_Stack return Stack_Model is (Stack_Model'(Top, Content));

    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Top, Content))
    is

       procedure Push (E : Element) is
       begin
          if Top >= Max then
             return;
          end if;
          Top             := Top + 1;
          Content (Top) := E;
       end Push;

    end Stack;

    with Stack; use Stack;

    procedure Use_Stack (E : Element) with
      Pre => not Is_Empty
    is
       F : Element := Peek;
    begin
       Push (E);
       pragma Assert (Peek = E or Peek = F);
    end Use_Stack;

This example is correct. GNATprove can verify the assertion in
:ada:`Use_Stack` because it has visibility to :ada:`Get_Stack`'s body.


Example #9
~~~~~~~~~~

Package :ada:`Data` defines three variables, :ada:`Data_1`, :ada:`Data_2` and
:ada:`Data_3`, that are initialized at elaboration (in :ada:`Data`'s package
body) from an external interface that reads the file system.

.. code:: ada prove_flow_button project=Courses.Intro_To_Spark.State_Abstraction.Example_09
    :class: ada-expect-prove-error

    package External_Interface with
      Abstract_State => File_System,
      Initializes    => File_System
    is
       type Data_Type_1 is new Integer;
       type Data_Type_2 is new Integer;
       type Data_Type_3 is new Integer;

       type Data_Record is record
          Field_1 : Data_Type_1;
          Field_2 : Data_Type_2;
          Field_3 : Data_Type_3;
       end record;

       procedure Read_Data (File_Name : String; Data : out Data_Record)
         with Global => File_System;
    end External_Interface;

    with External_Interface; use External_Interface;

    package Data with
      Initializes => (Data_1, Data_2, Data_3)
    is
       pragma Elaborate_Body;

       Data_1 : Data_Type_1;
       Data_2 : Data_Type_2;
       Data_3 : Data_Type_3;

    end Data;

    with External_Interface;
    pragma Elaborate_All (External_Interface);

    package body Data is
    begin
       declare
          Data_Read : Data_Record;
       begin
          Read_Data ("data_file_name", Data_Read);
          Data_1 := Data_Read.Field_1;
          Data_2 := Data_Read.Field_2;
          Data_3 := Data_Read.Field_3;
       end;
    end Data;

This example isn't correct. The dependency between :ada:`Data_1`'s, :ada:`Data_2`'s, and
:ada:`Data_3`'s initial values and :ada:`File_System` must be listed in
:ada:`Data`'s :ada:`Initializes` aspect.


Example #10
~~~~~~~~~~~

Let's remove the :ada:`Initializes` contract on package :ada:`Data`.

.. code:: ada prove_flow_report_all_button project=Courses.Intro_To_Spark.State_Abstraction.Example_10

    package External_Interface with
      Abstract_State => File_System,
      Initializes    => File_System
    is
       type Data_Type_1 is new Integer;
       type Data_Type_2 is new Integer;
       type Data_Type_3 is new Integer;

       type Data_Record is record
          Field_1 : Data_Type_1;
          Field_2 : Data_Type_2;
          Field_3 : Data_Type_3;
       end record;

       procedure Read_Data (File_Name : String; Data : out Data_Record)
         with Global => File_System;
    end External_Interface;

    with External_Interface; use External_Interface;

    package Data is
       pragma Elaborate_Body;

       Data_1 : Data_Type_1;
       Data_2 : Data_Type_2;
       Data_3 : Data_Type_3;

    end Data;

    with External_Interface;
    pragma Elaborate_All (External_Interface);

    package body Data is
    begin
       declare
          Data_Read : Data_Record;
       begin
          Read_Data ("data_file_name", Data_Read);
          Data_1 := Data_Read.Field_1;
          Data_2 := Data_Read.Field_2;
          Data_3 := Data_Read.Field_3;
       end;
    end Data;

This example is correct. Since :ada:`Data` has no :ada:`Initializes` aspect,
GNATprove computes the set of variables initialized during its elaboration
as well as their dependencies.
