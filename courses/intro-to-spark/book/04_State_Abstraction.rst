Lesson 4: State Abstraction
=====================================================================

.. role:: ada(code)
   :language: ada

Abstraction is a key concept in programming as it can drastically simplify
both implementation and code maintenance. It is particularly well suited
to SPARK and its modular analysis. This course explains what state
abstraction is and how it can be specified in SPARK. We will provide
details on how it impacts GNATprove's analysis both in terms of
information flow and in terms of proof of program properties.


What is an Abstraction?
---------------------------------------------------------------------

The abstraction principle is commonly used in programming languages. It
consists in having two views of the same object: an abstract one and a
refined one. The abstract one --- usually called specification --- only
describes what the object does in a coarse way. A subprogram's
specification usually explains how it should be called, how many
parameters it has, of which type, etc, as well as what it will do, return
a result, modify one of its parameters, etc.

Contract based programming, supported in Ada 2012, allows contracts to be
added to a subprogram's specification. Contracts can be used to describe
the subprogram's behavior in a more fine-grained manner. All the details
of how the subprogram actually works are left to its refined view, that
is, its implementation.

Take a look at the example code shown below:

.. code:: ada

    procedure Show_Abstraction is
       procedure Increase (X : in out Integer) with
         Global => null,
         Pre    => X <= 100,
         Post   => X'Old < X;

       procedure Increase (X : in out Integer) is
       begin
          X := X + 1;
       end Increase;
    begin
       null;
    end Show_Abstraction;


The specification of the subprogram ``Increase`` states that it should be
called on a unique argument, which should be a variable of type
:ada:`Integer` smaller than 100. Via this contract, it ensures that its
only effect will be to strictly increase the value of its argument.


Why is Abstraction Useful?
---------------------------------------------------------------------

To obtain a good abstraction of a subprogram's implementation, its
specification should summarize exactly what users of an object can rely
on. In other words, user code should not rely on a behavior of an object's
implementation if it is not documented in its specification.

For example, callers of the subprogram ``Increase`` can assume that it
will always strictly increase the value of its argument. On our user code
snippet shown below, it means that the loop is bound to terminate.

.. code:: ada

    procedure Increase (X : in out Integer) with
      Global => null,
      Pre    => X <= 100,
      Post   => X'Old < X;

    with Increase;
    procedure Show_Abstraction_2 is
       X : Integer := 0;
    begin
       while X <= 100 loop      --  The loop will terminate
          Increase (X);          --  Increase can be called safely
       end loop;
       pragma Assert (X = 101); --  Will this hold?
    end Show_Abstraction_2;


They can also assume that the implementation of ``Increase`` won't cause
any runtime error when called in the loop. However, on the other hand, the
assertion may fail if ``Increase``'s implementation is changed.

If this basic principle is followed, abstraction can bring significant
advantages. First, it simplifies both the program's implementation and its
verification. Often, it is enough to understand the specification of an
object to use it, which is in general simpler than trying to understand
its actual implementation. It also makes maintenance and code reuse that
much easier, as changes to the implementation of an object won't affect
the code using this object.


Abstraction of a Package's State
---------------------------------------------------------------------

Subprograms are not the only objects that can benefit from abstraction.
The state of a package --- that is, the set of persistent variables
defined in it --- can also be hidden from external users. This form of
abstraction --- called state abstraction --- is usually achieved by
defining variables in the body or private part of a package, so that they
can only be accessed through subprogram calls. For example, our ``Stack``
package shown below provides an abstraction for a unique ``Stack`` object
which can be modified using the ``Pop`` and ``Push`` procedures.

.. code:: ada
    :class: ada-nocheck

    package Stack is
      procedure Pop  (E : out Element);
      procedure Push (E : in  Element);
    end Stack;

    package body Stack is
      Content : Element_Array (1 .. Max);
      Top     : Natural;

The fact that it is implemented using an array is irrelevant to the user
and could be changed without impacting user code.


Declaring a State Abstraction
---------------------------------------------------------------------

As the hidden state influences the program's behavior, SPARK allows it to
be declared. For this, a named state abstraction can be introduced using
the :ada:`Abstract_State` aspect. This is not mandatory even for a package
which has hidden state. Several state abstractions can also be introduced
for the hidden state of a single package or for a package with no hidden
state at all. Note however that, as SPARK does not allow aliasing,
different state abstractions must always refer to disjoint sets of
concrete variables. Note also that a state abstraction is not a variable,
it does not have a type and cannot be used inside expressions, be it in
bodies or in contracts.

For example of the ``Stack`` package, we can, optionally, define either a
state abstraction for the whole hidden state of the package like this:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;
    end Stack;

Alternative, we can define a state abstraction for each hidden variable:

.. code:: ada
    :class: ada-nocheck

    package Stack with
      Abstract_State => (Top_State, Content_State)
    is
      ...

Note that a state abstraction is not a variable (it has no type), and
cannot be used inside expressions. For example:

.. code:: ada
    :class: ada-nocheck

    pragma Assert (Stack.Top_State = ...);
    --  Compilation error: Top_State is not a variable



Refining an Abstract State
---------------------------------------------------------------------

Once an abstract state has been declared in a package, it must be refined
into its constituents using a :ada:`Refined_State` aspect. The
:ada:`Refined_State` aspect must be placed on the package's body even if
the package previously did not require a body. For each state abstraction
declared for the package, the refined state lists the set of variables
which are represented by this state abstraction.

If an abstract state is specified for a package, then it must be complete,
in the sense that every hidden variable must be part of a state
abstraction. For example, on our ``Stack`` package's body, we must add a
:ada:`Refined_State` aspect linking the state abstraction ``The_Stack``
that we have introduced to the whole hidden state of the package,
including both ``Content`` and ``Top``.

.. code:: ada

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       type Element_Array is array (Natural range <>) of Element;

       Max : constant := 100;

       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;
       --  Both Content and Top must be listed in the list of
       --  constituents of The_Stack

       function Is_Empty return Boolean is
         (Top < Content'First);

       function Is_Full return Boolean is
         (Top >= Content'Last);

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
    end Stack;


Representing Private Variables
---------------------------------------------------------------------

State abstractions are always refined in the package's body, where all the
variables are visible. When only the package's specification is available,
we need a way to specify to which state abstraction private variables
belong. This is done using the :ada:`Part_Of` aspect on the variable's
declarations.

:ada:`Part_Of` annotations are mandatory: if a package has an abstract
state annotation, then all the hidden states defined in its private part
must be linked to a state abstraction. For example:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Pop  (E : out Element) with
         Pre  => not Is_Empty;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;

    private
       type Element_Array is array (Natural range <>) of Element;

       Max : constant := 100;

       Content : Element_Array (1 .. Max) with Part_Of => The_Stack;
       Top     : Natural                  with Part_Of => The_Stack;
    end Stack;

If we choose to define ``Content`` and ``Top`` in ``Stack``'s private part
instead of its body, then we must add a :ada:`Part_Of` aspect to both
their declarations, associating them with the state abstraction
``The_Stack``, even though it is the only state abstraction defined in
``Stack``. Note that they still need to be listed in the
:ada:`Refined_State` aspect in the ``Stack``'s body:

.. code:: ada
    :class: ada-nocheck

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))


Additional State
---------------------------------------------------------------------

Nested Packages
~~~~~~~~~~~~~~~

Until now, we have only spoken of hidden variables. But variables are not
the only constituents of a package's state. If a package ``P`` contains a
nested package, then the nested package's state is part of ``P``'s state.
As a consequence, if the nested package is hidden, its state is part of
``P``'s hidden state and must be listed in ``P``'s state refinement.

This is the case in our example shown below, where the package
``Hidden_Nested``'s hidden state is part of ``P``'s hidden state:

.. code:: ada

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

Note that a visible state of ``Hidden_Nested`` would also have been part
of ``P``'s hidden state. Also note that, if ``P`` contains a visible
nested package, then the nested package's state is not part of ``P``'s
hidden state. In particular, its hidden state should be declared in a
separate state abstraction on its own declaration, like it is done on our
example for ``Visible_Nested``.


Constants with Variable Inputs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Other possible constituents of a state abstraction are constants with
variable inputs. We call constants with variable inputs constants whose
value depends on either a variable or a subprogram parameter. Those are
usually handled as variables in flow analysis, as they participate to the
flow of information between variables throughout the program. Thus,
constants with variable inputs, just like variables, are considered to be
part of a package's state.

If a state abstraction is specified for a package, then hidden constants
with variable inputs declared in this package must be listed in the state
abstraction refinement. Note that, on the other hand, constants without
variable inputs do not participate to the flow of information and
therefore cannot appear in a state refinement.

Let's look at this example:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;
    end Stack;


    package External_Configuration
    is
       Max : Positive := 100;
    end External_Configuration;


    with External_Configuration;

    package body Stack with
      Refined_State => (The_Stack => (Content, Top, Max))
      --  Max has variable inputs. It must appear as a
      --  constituent of The_Stack
    is
       type Element_Array is array (Natural range <>) of Element;

       Max : constant Positive := External_Configuration.Max;

       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;
       --  Both Content and Top must be listed in the list of
       --  constituents of The_Stack

       function Is_Empty return Boolean is (Top = 0);
       function Is_Full  return Boolean is (Top >= Max);

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
    end Stack;

Here, ``Max`` --- the maximal number of elements that can be stored in the
stack --- is initialized with a variable from an external package. Since
it now has variable inputs, ``Max`` must be a part of the state
abstraction ``The_Stack``.


Subprogram Contracts
---------------------------------------------------------------------

Global and Depends
~~~~~~~~~~~~~~~~~~

As hidden variables can only be accessed through subprogram calls,
subprogram's contract are the proper way of documenting how state
abstractions can be modified during the program's execution. First off,
:ada:`Global` and :ada:`Depends` contracts can be used to specify which of
the state abstractions are accessed by a subprogram and how their values
flow through the different variables. Note that :ada:`Global` and
:ada:`Depends` contracts referring to state abstractions may be less
precise than contracts referring to visible variables, as the different
modes of the hidden variables aggregated in a state abstraction are
collapsed into a single mode.

Let's look at this example:

.. code:: ada

    package Stack with
      Abstract_State => (Top_State, Content_State)
    is
       type Element is new Integer;

       procedure Pop  (E : out Element) with
         Global  => (Input  => Content_State,
                     In_Out => Top_State),
         Depends => (Top_State => Top_State,
                     E         => (Content_State, Top_State));
    end Stack;

In this example, the ``Pop`` procedure only modifies the value of the
hidden variable ``Top`` and keeps ``Content`` unchanged. If two distinct
state abstractions are used for the two variables, then this contract is
preserved.

Let's contrast this example with another example:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack
    is
       type Element is new Integer;

       procedure Pop  (E : out Element) with
         Global  => (In_Out => The_Stack),
         Depends => ((The_Stack, E) => The_Stack);

    end Stack;

Here, ``Top_State`` and ``Content_State`` are collapsed into one single
state abstraction. In this case, we lose the fact that ``Content`` is
preserved, only keeping the fact that ``The_Stack`` is modified. This loss
in precision is reasonable here, it is the whole point of abstraction. But
users must be careful not to aggregate unrelated hidden state lest their
annotations become meaningless.

If imprecise contracts dealing with state abstractions as a whole are
perfectly reasonable for users of a package, :ada:`Global` and
:ada:`Depends` contracts should remain as precise as possible inside the
package's body itself. For this reason, SPARK introduces the notion of
refined contracts. Those are precise contracts, specified on the bodies of
subprograms, where state refinements are visible. These contracts are
exactly like normal :ada:`Global` and :ada:`Depends` contracts, except
they refer directly to the hidden state of the package.

When a subprogram is called inside the package's body, these refined
contracts are used instead of the general ones, so that the verification
can be as precise as possible. Note that refined :ada:`Global` and
:ada:`Depends` are optional: if they are not specified by the user, the
tool will compute them to check the package's implementation.

For our ``Stack`` example, we could add refined contracts like this:

.. code:: ada

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       type Element_Array is array (Natural range <>) of Element;

       Max     : constant := 100;
       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;

       procedure Pop (E : out Element) with
         Refined_Global  => (Input  => Content,
                             In_Out => Top),
         Refined_Depends => (Top => Top,
                             E   => (Content, Top))
       is
       begin
          E     := Content (Top);
          Top   := Top - 1;
       end Pop;

    end Stack;


Pre and Postconditions
~~~~~~~~~~~~~~~~~~~~~~

Functional properties of subprogram's are usually expressed using pre and
postconditions. As these contracts are standard Boolean expressions, they
cannot refer directly to state abstractions. To work around this
restriction, functions can be defined to query the value of hidden
variables. These functions can then be used in place of the state
abstraction in other subprogram's contracts.

That is what is done in this example:

.. code:: ada

    package Stack is
       type Element is new Integer;

       function Is_Empty return Boolean;
       function Is_Full  return Boolean;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post => not Is_Empty;
    end Stack;

    package body Stack is
       type Element_Array is array (Natural range <>) of Element;

       Max     : constant := 100;
       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;

       function Is_Empty return Boolean is (Top = 0);
       function Is_Full  return Boolean is (Top >= Max);

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
    end Stack;

Here, we define two functions accessing the state of the stack:
``Is_Empty`` and ``Is_Full``. We use them to specify the procedure
``Push``.

As for :ada:`Global` and :ada:`Depends` contracts, it is often useful to
have a more precise view of functional contracts when the hidden variables
are visible. This can be achieved using expression functions. As
expression function bodies act as contracts for GNATprove, they
automatically give a more precise version of the contracts when their
implementation is visible.

It may be the case that we need a more constraining contract to verify the
package's implementation than we want to ensure outside the abstraction.
This can be achieved using the :ada:`Refined_Post` aspect. This aspect,
when placed on a subprogram's body, is used to provide stronger guaranties
to internal callers of a subprogram. If provided, the refined post
condition must imply the subprogram's postcondition. This is checked by
GNATprove, who will report a failing postcondition if the refined
postcondition is too weak, even if it is actually implied by the
subprogram's body. Note that SPARK does not supply a similar notation for
preconditions.

For our ``Stack`` example, we could add a refined post condition like
this:

.. code:: ada

    package body Stack is
       type Element_Array is array (Natural range <>) of Element;

       Max     : constant := 100;
       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;

       function Is_Empty return Boolean is (Top = 0);
       function Is_Full  return Boolean is (Top >= Max);

       procedure Push (E : Element) with
         Refined_Post => not Is_Empty and E = Content (Top)
         --  Using refined postcondition
       is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
    end Stack;


Initialization of Local Variables
---------------------------------------------------------------------

As part of flow analysis, GNATprove checks for proper initialization of
variables. Therefore, flow analysis needs to know which are the variables
initialized during the package's elaboration.

The :ada:`Initializes` aspect can be used to specify the set of visible
variables and state abstractions of a package that are initialized during
its elaboration. Note that an :ada:`Initializes` aspect cannot refer to a
variable that is not defined in the unit as, in SPARK 2014, a package
shall only initialize variables declared immediately within the package.

:ada:`Initializes` aspects are optional. If they are not supplied by the
user, they will be computed by GNATprove.

For our ``Stack`` example, we could add an :ada:`Initializes` aspect like
this:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack,
      Initializes    => The_Stack
    is
       type Element is new Integer;

       function Is_Empty return Boolean;

       --  Flow analysis will make sure both Top and Content are
       --  initialized at package elaboration
    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Content, Top))
    is
       type Element_Array is array (Natural range <>) of Element;

       Max     : constant := 100;
       Content : Element_Array (1 .. Max) := (others => 0);
       Top     : Natural := 0;

       function Is_Empty return Boolean is (Top = 0);
    end Stack;

As flow analysis can also check for dependencies between variables, it
must be aware of information flowing through initialization of states. The
:ada:`Initializes` aspect also serves this purpose. If the initial value
of a variable or state abstraction is dependent on the value of a visible
variable or state abstraction from another package, then this dependency
must be listed in the :ada:`Initializes` contract. The list of entities on
which a variable's initial value depends are associated to the variable
using an arrow.

Let's look at this example:

.. code:: ada

    package Q is
       External_Variable : Integer := 2;
    end Q;

    with Q;
    package P with
      Initializes => (V1, V2 => Q.External_Variable)
    is
       V1 : Integer := 0;
       V2 : Integer := Q.External_Variable;

       --  The association for V1 is omitted, it does not depend
       --  on any external state.
    end P;

In our example, we stated in the :ada:`Initializes` aspect of ``P`` that
``V2``'s initial value depends on the value of ``Q.External_Variable``.
Note that we omitted the dependency for ``V1``, as its initial value does
not depend on any external variable. This dependency could also have been
stated explicitly, writing :ada:`V1 => null`.

Dependencies of initial values can be computed by the tool if no
:ada:`Initializes` aspect is supplied. On the other hand, if an
:ada:`Initializes` aspect is provided for a package, then it should be
complete, that is, every initialized state of the package should be
listed, along with all its external dependencies.


Code Examples / Pitfalls
---------------------------------------------------------------------

This section contains some code examples and pitfalls.

Example #1
~~~~~~~~~~

Let's review this code:

.. code:: ada

    package Configuration
    is
       External_Variable : Natural := 1;
    end Configuration;

    with Configuration;
    package Communication with
      Abstract_State => State,
      Initializes    => (State => Configuration.External_Variable)
    is
       function Get_Capacity return Natural;
    private
       package Ring_Buffer is
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


This example is not correct. Here, ``Capacity`` is declared in the private
part of ``Communication``. Therefore, it should be linked to ``State`` at
declaration using the :ada:`Part_Of` aspect.


Example #2
~~~~~~~~~~

Let's review this code:

.. code:: ada

    package Configuration
    is
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
          Capacity : constant Natural :=
            Configuration.External_Variable
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

This program is correct and GNATprove will be able to verify it.


Example #3
~~~~~~~~~~

Let's review this code:

.. code:: ada

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

    procedure Example_3 is
    begin
       Reset_Black_Count;
       Reset_Red_Count;
    end Example_3;

This example is correct. This program does not read uninitialized data,
but GNATprove will fail to verify this fact. As we have provided a state
abstraction, flow analysis will compute subprogram's effects in terms of
this state abstraction, and thus, will count the call to
``Reset_Black_Count`` as a read of ``State``.


Example #4
~~~~~~~~~~

Let's review this code:

.. code:: ada

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

    procedure Example_4 is
    begin
       Reset_Black_Count;
       Reset_Red_Count;
    end Example_4;

This example is correct. Here, no state abstraction is provided. GNATprove
will reason in terms of variables and will prove data initialization
without any problem.


Example #5
~~~~~~~~~~

Let's review this code:

.. code:: ada

    package Counting with Abstract_State => State is
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

This example is correct. Flow analysis uses the refined version of
:ada:`Global` contracts for internal calls and thus can verify that
``Reset_All`` indeed properly initializes ``State``. Note that
:ada:`Refined_Global` and :ada:`Global` annotations are not mandatory,
they can also be computed by the tool.


Example #6
~~~~~~~~~~

Let's review this code:

.. code:: ada
    :class: ada-expect-compile-error

    package Stack with Abstract_State => The_Stack is
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
       Content : Element_Array (1 .. Max);

       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
       function  Peek     return Element is (Content (Top));
       function  Is_Full  return Boolean is (Top >= Max);
       function  Is_Empty return Boolean is (Top = 0);
    end Stack;

This example is not correct. There is a compilation error in ``Push``'s
postcondition. Indeed, ``The_Stack`` is a state abstraction and not a
variable and cannot be mentioned in an expression.


Example #7
~~~~~~~~~~

Let's review this code:

.. code:: ada

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
      Refined_State => (The_Stack => S)
    is
       S : Stack_Model;

       procedure Push (E : Element) is
       begin
          S.Top             := S.Top + 1;
          S.Content (S.Top) := E;
       end Push;

       function  Peek     return Element is (S.Content (S.Top));
       function  Is_Full  return Boolean is (S.Top >= Max);
       function  Is_Empty return Boolean is (S.Top = 0);

       function Get_Stack return Stack_Model is (S);
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

This program is correct, but GNATprove won't be able to verify the
assertion in ``Use_Stack``. Indeed, even if ``Get_Stack`` is an expression
function, its body is not visible outside of ``Stack``'s body.


Example #8
~~~~~~~~~~

Let's review this code:

.. code:: ada

    package Stack with
      Abstract_State => The_Stack
    is
       pragma Elaborate_Body;
       pragma Unevaluated_Use_Of_Old (Allow);

       type Element is new Integer;

       type Element_Array is array (Positive range <>) of Element;
       Max : constant Natural := 100;
       subtype Length_Type is Natural range 0 .. Max;

       type Stack_Model is record
          Top     : Length_Type;
          Content : Element_Array (1 .. Max);
       end record;

       function Peek      return Element with Pre => not Is_Empty;
       function Is_Full   return Boolean;
       function Is_Empty  return Boolean;
       function Get_Stack return Stack_Model;

       procedure Push (E : Element) with
         Pre  => not Is_Full,
         Post =>
           not Is_Empty and
           (if Is_Full'Old then Get_Stack = Get_Stack'Old else Peek = E);

    private
       Top     : Length_Type              with Part_Of => The_Stack;
       Content : Element_Array (1 .. Max) with Part_Of => The_Stack;

       function Peek      return Element     is (Content (Top));
       function Is_Full   return Boolean     is (Top >= Max);
       function Is_Empty  return Boolean     is (Top = 0);
       function Get_Stack return Stack_Model is ((Top, Content));
    end Stack;

    package body Stack with
      Refined_State => (The_Stack => (Top, Content))
    is
       procedure Push (E : Element) is
       begin
          Top           := Top + 1;
          Content (Top) := E;
       end Push;
    begin
       Top     := 0;
       Content := (others => 0);
    end Stack;

    with Stack; use Stack;

    procedure Use_Stack (E : Element) with
      Pre => not Is_Empty and not Is_Full
    is
       F : Element := Peek;
    begin
       Push (E);
       pragma Assert (Peek = E or Peek = F);
    end Use_Stack;

This example is correct. GNATprove will be able to verify the assertion in
``Use_Stack`` since it has visibility of ``Get_Stack``'s body.


Example #9
~~~~~~~~~~

Let's review this code:

.. code:: ada

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

This example is not correct. The dependency between ``Data_1``'s initial
value and ``File_System`` must be listed in ``Data``'s :ada:`Initializes`
aspect.


Example #10
~~~~~~~~~~~

Let's review this code:

.. code:: ada

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

    with Data;               use Data;
    with External_Interface; use External_Interface;

    procedure Use_Data is
       X : Data_Type_1 := Data_1;
    begin
       null;
    end Use_Data;

This example is correct. Since ``Data`` has no :ada:`Initializes` aspect,
GNATprove will compute the set of variables initialized during its
elaboration. Thereby, it can ensure that ``Data_1`` is always initialized
in ``Use_Data``.

