.. _Ada_Idioms_Controlling_Object_Initialization_And_Creation:

Controlling Object Initialization and Creation
==============================================

.. include:: ../../global.txt


Motivation
----------

Developers are responsible for ensuring that no uninitialized objects are read
in Ada programs. Default initialization is a good way to meet this requirement
because
it is guaranteed to happen and requires no actions on the part of the client
code. But of the many kinds of types provided by Ada, only access types have a
language-defined default initial value. Fortunately, Ada supports user-defined
default initialization for user-defined types.

Default initialization is conveniently expressed, especially because components
of record types can have default initial values. Record types are perhaps the
most commonly used non-numeric type in the language. Sometimes a given type was
*wrapped* inside a record type purely for the sake of default component
initialization, e.g., numeric types. That wrapping approach is less common than
in earlier versions of the language, given the comparatively more recent aspect
:ada:`Default_Value` for scalar types, and :ada:`Default_Component_Value` for
scalar array components.

These facilities are often sufficient to express an abstraction's initial
state. For example, we can expect that container objects will be initially
empty. Consider a bounded stack ADT. The representation is likely a
record type containing an array component and a :ada:`Top` component indicating
the index of the last array component used. We can default initialize objects
to the empty state simply by setting :ada:`Top` to zero in the record
component's declaration:

.. code-block:: ada

    type Content is array (Positive range <>) of Element;
    type Stack (Capacity : Positive) is record
       Values : Content (1 .. Capacity);
       Top    : Natural := 0;
    end record;

For an unbounded container such as a simple binary tree, if the representation
is an access type, the automatic default value :ada:`null` initializes
:ada:`Tree` objects to the empty state.

.. code-block:: ada

    package Binary_Trees is
       type Tree is limited private;
       ...
    private
       type Leaf_and_Branch is record ...
       type Tree is access Leaf_and_Branch;
       ...
    end Binary_Trees;

In both cases, simply declaring an object in the client code is sufficient to
ensure it is initially empty.

However, not all abstractions have a meaningful default initial state. Default
initialization will not suffice to fully initialize objects in these cases, so
explicit initialization is required.

An explicit procedure call could be used to set the initial state of an object
(passed to a mode-out parameter), but there is no guarantee that the call will
occur and no way to force a client to make it.

In contrast, the declaration of the object is guaranteed to occur, and as part
of the declaration the object can be given an explicit initial value. The
initial value can be specified by a literal for the type, by the value of
another object of that type, or by the value of that type returned from a
function call.

.. code-block:: ada

    declare
       X : Integer := Some_Existing_Integer_Object;
       Prompt : constant String := "Name? ";
       Reply : constant String := Response (Prompt);
    begin
       ...
    end;

The initial value can also specify constraints, if required.
In the code above, the object
:ada:`Prompt` has a lower bound of :ada:`Positive'First` and an upper bound set
to the length of the literal. The specific bounds of :ada:`Reply` are
determined by the function, and need not start at :ada:`Positive'First`.

An object cannot be used before it is declared. Since this explicit initial
value is part of the declaration, the object cannot be read before it is
initialized. That fact is the key to the solutions.

However, although the object declaration is guaranteed to occur, explicit
initialization is optional. But unlike a procedure call, we can force the
initial value to be given. There are two ways to force it, so there are two
solutions presented.

In addition, a specific form of explicit initialization may be required because
not all forms of initialization are necessarily appropriate for a given
abstraction. Imagine a type representing a thread lock, implemented in such a
way that default initialization isn't an option. Unless we prevent it,
initialization by some other existing object will be possible:

.. code-block:: ada

    declare
       X : Thread_Lock := Y;    --  Y is some other Thread_Lock object
    begin
       --  ...
    end;

This would amount to a copy, which might not make sense. Imagine the lock type
contains a queue of pending callers...

More generally, if a type's representation includes access type components,
initialization by another object will create a shallow copy of the designated
objects. That is typically inappropriate.

Using an existing object for the initial value amounts to a complete copy of
that other object, perhaps more of a copy than required. For example, consider
a bounded container type, e.g., another stack, backed by an array and an index
component named :ada:`Top`. At any time, for any stack, the contained content
is in the slice of the array from 1 up to :ada:`Top`. Any array component at an
index greater than :ada:`Top` has a junk value. Those components may never even
have been assigned during use. Now consider the declaration of a :ada:`Stack`
object, :ada:`A`, whose initial value is that of another existing :ada:`Stack`
named :ada:`B`.

.. code-block:: ada

    A : Stack := B;

The entire value of :ada:`B` is copied into :ada:`A`, so :ada:`B.Top` is copied
to :ada:`A.Top`, which makes sense. But likewise, the entire array in :ada:`B`
will be copied to the array in :ada:`A`. For a stack with a large backing array
that might take a significant amount of time. If :ada:`B` is logically full
then the time required for the full array copy is unavoidable. But if only a
few values are contained by :ada:`B`, the hit could be avoided by only copying
up to :ada:`Top`.

And of course, the initial value might require client-specific information.

Calling a
:ref:`constructor function <Ada_Idioms_Constructor_Functions_For_Abstract_Data_Types>`
for the initial value would be the right approach in these cases, returning an
object of the type. The function might even take an existing object as a
parameter, creating a new object with only the necessary parts copied.

Therefore, for some abstractions, not only do we need to guarantee explicit
object initialization, we may also need to restrict the form of initial value
to a function call.

The other purpose of the idiom is controlling, for some type, whether object
creation itself is to be allowed by clients. As you will see, controlling
object initialization can be used to control object creation.

Preventing object creation is not typical but is not unknown. The
:wikipedia:`singleton design pattern <Singleton_pattern>`
is an example, in which a type is defined but corresponding object creation by
clients is not intended. Instead, the abstraction implementation creates a
single object of the type. The abstraction is a type, rather than an
:ref:`ADM <Ada_Idioms_Abstract_Data_Machines>`, for the sake of potential
extension via inheritance. We will illustrate this design pattern and solution
using a real-world hardware device.


Requiring Initialization by Clients
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two ways to force an explicit initial value as part of an object
declaration. One is a matter of legality at compile-time so it is enforced by
the compiler. The other is enforced by a run-time check.

Note that both solutions are type-specific, so when we say *objects* we mean
objects of a type that has been designed with one of these two idiom solutions.
Neither solution applies to every object of every type used in the client code.
(SPARK, a formal language based closely on Ada, statically ensures all objects
are initialized before read.)

.. todo::

    Add link to SPARK ref for data initialization

The :ref:`ADT idiom <Ada_Idioms_Abstract_Data_Types>` describes Ada *building
blocks* that developers can use to compose types with semantics that we
require. We can declare a type to be private, for example, so that the
implementation is not compile-time visible to clients.

In addition to private types, we can decorate a type declaration with the
reserved word :ada:`limited` so that assignment is not allowed (among other
things) for client objects of the type. We can combine the two building blocks,
creating a type that is both private and limited.

Throughout this discussion we will assume that these designs are based on
:ref:`Abstract Data Types <Ada_Idioms_Abstract_Data_Types>`, hence we assume
the use of private types. That's a general, initial design assumption but in
this case private types are required by the two idiom solutions. The types are
not necessarily limited as well, but in one situation they will be limited too.
But in both solutions the primary types will be private types.


Solution 1: Compile-Time Legality
---------------------------------

We can combine the private type and limited type building blocks with another,
known as *unknown discriminants*, to force explicit object initialization by
clients, to control the form of explicit initialization, and, when required,
to control client object creation itself. Limited and private types are fairly
common building blocks, but *unknown discriminants* are less common so we will
first explain them, and then show how to utilize the combinations for this
idiom.

Discriminants are useful for our purpose because types with discriminants are
*indefinite types* (under certain circumstances). Indefinite types do not allow
object declarations without also specifying some sort of constraints for those
objects. Unconstrained array types, such as :ada:`String`, are good examples.
We cannot simply declare an object of type :ada:`String` without also
specifying the array bounds, one way or another:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Initialization_Demo is
       S1 : String (1 .. 11) := (others => ' ');
       S2 : String := "Hello World";
       S3 : String := S1;
    begin
       Put_Line ('"' & S1 & '"');
       Put_Line ('"' & S2 & '"');
       Put_Line ('"' & S3 & '"');
    end Initialization_Demo;

In the code above, :ada:`String` objects :ada:`S1`, :ada:`S2`, and :ada:`S3`
all have the same constraints: a lower bound of :ada:`Positive'First` and an
upper bound of 11. :ada:`S1` gives the bounds directly, whereas :ada:`S2` and
:ada:`S3` take their constraints from their initial values. A function that
returned a :ada:`String` value would suffice for the initial value too and
would thus serve to specify the array bounds. There are other ways to specify a
constraint as well, but we can ignore them in this idiom because the building
blocks we'll use preclude them.

Types with discriminants are indefinite types unless the discriminants have
default values. That fact will not apply in this idiom because of the
characteristics of the building blocks. You will see why in a moment. The
important idea is that we can leverage the object constraint requirements of
indefinite types to force explicit initialization on declarations.

Discriminants come in two flavors. So-called *known* discriminants are the most
common. These discriminants are known in the sense that they are compile-time
visible to client code. Clients then have everything needed for declaring
objects of the corresponding type. For example, here is the type declaration
for a bounded stack ADT:

.. code-block:: ada

    type Stack (Capacity : Positive) is private;

In the above, :ada:`Capacity` is the physical number of components in the array
backing the bounded implementation. Clients can, therefore, have different
objects of the type with different capacities:

.. code-block:: ada

    Trays : Stack (Capacity => 10);
    Operands : Stack (100);

The existence of :ada:`Capacity` is known to clients via the partial view, so
the requirement for the constraint is visible and can be expressed.

In contrast, types may have *unknown discriminants* in the client's view. The
syntax reflects their confidential nature:

.. code-block:: ada

    type Foo (<>) is private;

The parentheses are required as usual, but the *box* symbol appears inside,
instead of one or more discriminant declarations. The box symbol always
indicates *not specified here* so in this case no discriminants are included in
the view. There may or may not be discriminants in the full view, but client's
don't have compile-time visibility to that information because the type is
private.

Unknown discriminants can be specified for various kinds of types, not only
private types. See the
:ref:`Notes section <Ada_Idioms_Controlling_Object_Initialization_And_Creation_Notes>`
for the full list. That said, combining them with private type declarations, or
private type extension declarations, is the most common usage when composing
abstraction definitions. For example:

.. code-block:: ada

    package P is
       type Q (<>) is private;
    private
       type Q is range 0 .. 100;
    end P;

Clients of package :ada:`P` must use type :ada:`Q` as if :ada:`Q` requires
discriminant constraints, even though clients don't have compile-time
visibility to whatever constraints are actually required, if any. In the above,
:ada:`Q` is just an integer type in the full view. No constraint is required to
create objects of type :ada:`Q`, but clients cannot take advantage of that fact
because they only have the partial view. Only the package private part, the
package body, and child units have the visibility required to treat :ada:`Q` as
an integer type.

:ada:`Q` might actually be completed as an indefinite type, but the constraint
required need not be a discriminant constraint. In the following, objects of
type :ada:`Q` require an array bounds constraint:

.. code-block:: ada

    package P is
       type Q (<>) is private;
    private
       type Q is array  (Positive range <>) of Integer;
    end P;

Code with the full view must respect the index bounds requirement, but the
semantics of the partial view remain the same.

As illustrated, the consequence of combining indefinite types with private
types is that, when declaring objects, clients must express a constraint but
cannot do so directly. The constraints must instead be provided by the initial
value. Hence, for these types, the initial value is now a requirement that the
compiler enforces on client object declarations.

But because the type is private, the initial value cannot be specified by a
literal. Instead, the initial value must be either an existing object of the
type, or the result of a call to a function that returns an object of the type.

Consider the following:

.. code-block:: ada

    package P is
       type Q (<>) is private;
       function F return Q;
    private
       type Q is range 0 .. 100;
    end P;

    package body P is
       function F return Q is (42);
       --  since that is the answer to everything...
    end P;

    with P;
    procedure Demo is
       Obj1 : P.Q;  --  not legal, requires initial value for constraint
       Obj2 : P.Q := 42;  --  not legal, per client's partial view
       Obj3 : P.Q := P.F;
       Obj4 : P.Q := Obj3;
    begin
       null;
    end Demo;

The declaration for :ada:`Obj1` is illegal because no constraint is provided.
Because :ada:`P.Q` is also private, the declaration of :ada:`Obj2` is illegal
because clients don't have the full view supporting integer usage. But the
initial value can be provided by a function result (:ada:`Obj3`), thereby also
specifying the required constraint. And an existing object can be used to give
the constraints to other objects during their declarations (:ada:`Obj4`).
Explicit client initialization in these two ways is required by the compiler
for indefinite private types.

But as illustrated by the spin-lock example, initialization by an existing
object is not always appropriate. We can restrict the initial value to a
function call result by making the type limited as well as private and
indefinite. Then only constructor functions can be used legally for the initial
values, and the compiler will require them to be called during object
declarations (e.g., :ada:`Obj3` above). That's what we'd do for the spin-lock
type. We'd make the type limited in the completion too, to prevent copying in
any form, including the function result. (The function result would then be
built in place instead of copied.)

To recap, the primary purpose of the idiom, for a given type, is to ensure that
clients initialize objects of that type as part of the object declarations. In
this first solution we meet the requirement by composing the type via building
blocks that:

    #. require a constraint to be given when declaring any object of the type,
       and

    #. require an initial value to give that constraint, and

    #. allow only objects and function call results as the initial values, and

    #. when necessary, allow only function call results to be used for the
       initial values.

The compiler will reject declarations that do not adhere to these rules.
Explicit initialization in the client code is thus guaranteed.

For a concrete example, consider a closed loop process controller, specifically
a
:wikipedia:`proportional–integral–derivative (PID) controller <Proportional–integral–derivative_controller>`.
A PID controller examines the difference between an intended value, such as the
desired speed of your automobile, and the current value (the actual speed). In
response to that difference the controller increases or decreases the throttle
setting. This measurement and resulting control output response happens
iteratively at some rate. This is a sophisticated ADT, and explaining how a PID
controller actually works is beyond the scope of this document. There are
numerous web sites available that describe them in detail. What you should know
for our purpose is that they are used to control physical processes, such as
your car's cruise control system, that affect our lives directly. Ensuring
proper initialization is part of ensuring correct use.

The PID controller must be explicitly initialized because there is no default
initial state that would allow subsequent safe use. Only a partial meaningful
state can be defined by default. Specifically, a PID controller can be enabled
and disabled by the user (the external process control engineer) at arbitrary
times. We can define default initialization such that the objects are initially
in the disabled state. When disabled, the output computation actually affects
nothing, so starting from that state would be safe. However, there is nothing
to prevent the user from enabling the controller object without first
configuring it. Configuring the various parameters is essential for safe and
predictable behavior.

To address that problem, we could add operation preconditions requiring the
object to be in some *configured* state, but that isn't always appropriate.
Such a precondition would just raise an exception, which isn't in the
use-cases. (Statically proving prior configuration in the client code would be
a viable alternative, but that's also beyond the scope of this document.)

Therefore, default initialization doesn't really suffice for this ADT. We need
to force initialization (configuration) during object creation so that enabling
the ADT output will always be safe. This idiom solution does exactly that.

The following is a cut-down version of the package declaration using this idiom
solution, with some operations and record components elided for the sake of
simplicity. In the full version the unit is a generic package for the sake of
not hard-coding the floating point types. We use a regular package and type
:ada:`Float` here for convenience. The full version is here:

- `AdaCore/Robotics_with_Ada/src/control_systems (GitHub) <https://github.com/AdaCore/Robotics_with_Ada/blob/master/src/control_systems/>`_

.. code-block:: ada

    package Process_Control is

       type PID_Controller (<>) is tagged limited private;

       type Bounds is record
          Min, Max : Float;
       end record with
         Predicate => Min < Max;

       type Controller_Directions is (Direct, Reversed);

       type Millisecond_Units is mod 2**32;

       subtype Positive_Milliseconds is
         Millisecond_Units range 1 .. Millisecond_Units'Last;

       function Configured_Controller
         (Proportional_Gain : Float;
          Integral_Gain     : Float;
          Derivative_Gain   : Float;
          Invocation_Period : Positive_Milliseconds;
          Output_Limits     : Bounds;
          Direction         : Controller_Directions := Direct)
       return PID_Controller;

       procedure Enable
         (This             : in out PID_Controller;
          Process_Variable : Float;   --  current input value from the process
          Control_Variable : Float);  --  current output value

       procedure Disable (This : in out PID_Controller);

       procedure Compute_Output
         (This             : in out PID_Controller;
          Process_Variable : Float;          --  the input, Measured Value/Variable
          Setpoint         : Float;
          Control_Variable : in out Float);  --  the output, Manipulated Variable

       --  ...

       function Enabled (This : PID_Controller) return Boolean;

    private

       type PID_Controller is tagged limited record
          --  ...
          Enabled : Boolean;
       end record;

    end Process_Control;

As you can see, the PID controller type is indefinite limited private:

.. code-block:: ada

    type PID_Controller (<>) is tagged limited private;

It is also tagged, primarily for the sake of the distinguished receiver call
syntax. We don't really expect type extensions in this specific ADT, although
nothing prevents them.

Therefore, the language requires an initial value when creating objects of the
type, and because the type is limited, a function must be used for that initial
value. The compiler will not compile the code containing the declaration
otherwise. The only constructor function provided is
:ada:`Configured_Controller` so it is guaranteed to be called. (A later child
package could add another
:ref:`constructor function <Ada_Idioms_Constructor_Functions_For_Abstract_Data_Types>`.
For that matter, we probably should have declared this one in a child package.
In any case one of them is guaranteed to be called.)

Here is an example declaration taken from the steering control module for an
`RC car written in Ada <https://blog.adacore.com/making-an-rc-car-with-ada-and-spark>`_.

The PID controller, named :ada:`Steering_Computer`, is declared within the body
of a task :ada:`Servo` that controls a motor, :ada:`Steering_Motor`, in
response to requested directions from the remote control.
:ada:`Steering_Motor` is an instance of an ADT named :ada:`Basic_Motors`, and
is declared elsewhere. The :ada:`Servo` task is declared within the body of a
package that contains various values referenced within the task, such as the
various PID gain parameters, that are not shown.

.. code-block:: ada

       task body Servo is
          Next_Release       : Time;
          Target_Angle       : Float;
          Current_Angle      : Float := 0.0;
          --  zero for call to Steering_Computer.Enable
          Steering_Power     : Float := 0.0;
          --  zero for call to Steering_Computer.Enable
          Motor_Power        : NXT.Motors.Power_Level;
          Rotation_Direction : NXT.Motors.Directions;
          Steering_Offset    : Float;
          Steering_Computer  : PID_Controller :=
            Configured_Controller
              (Proportional_Gain => Kp,
               Integral_Gain     => Ki,
               Derivative_Gain   => Kd,
               Invocation_Period => System_Configuration.Steering_Control_Period,
               Output_Limits     => Power_Level_Limits,
               Direction         => Closed_Loop.Direct);
       begin
          Global_Initialization.Critical_Instant.Wait (Epoch => Next_Release);
          Initialize_Steering_Mechanism (Steering_Offset);
          Steering_Computer.Enable (Process_Variable => Current_Angle,
                                    Control_Variable => Steering_Power);
          loop
             Current_Angle := Current_Motor_Angle (Steering_Motor) -
                              Steering_Offset;
             Target_Angle := Float (Remote_Control.Requested_Steering_Angle);
             Limit (Target_Angle, -Steering_Offset, +Steering_Offset);
             Steering_Computer.Compute_Output
               (Process_Variable => Current_Angle,
                Setpoint         => Target_Angle,
                Control_Variable => Steering_Power);
             Convert_To_Motor_Values (Steering_Power,
                                      Motor_Power,
                                      Rotation_Direction);
             Steering_Motor.Engage (Rotation_Direction, Motor_Power);

             Next_Release := Next_Release + Period;
             delay until Next_Release;
          end loop;
       end Servo;

Because :ada:`Steering_Computer` must be declared before it can be passed as a
parameter, the call to configure the object's state necessarily precedes any
other operation (e.g., :ada:`Enable`).


Solution 2: Run-Time Checks
----------------------------------------------------------

Ada 2022 adds another building block, :ada:`Default_Initial_Condition` (DIC),
that can be used as an alternative to the unknown discriminants used above. We
must still have a private type or private type extension, and the type may or
may not be limited, but unknown discriminants will not be involved. The
compiler would not allow the combination, in fact.

DIC is an aspect applied to a private type or private extension declaration.
Developers use it to specify a developer-defined Boolean condition that will be
true at run-time after the default initialization of an object of the type.
Specifically, if :ada:`Default_Initial_Condition` is specified for a type, a
run-time check is emitted for each object declaration of that type that uses
default initialization. The check consists of the evaluation of the DIC
expression. The exception :ada:`Assertion_Error` is raised if the check fails.
You can think of this aspect as specifying the effects of default
initialization for the type, with a verification at run-time when needed. No
check is emitted for those declarations that use explicit initialization.

For example, the following is a partial definition of a :ada:`Stack` ADT. It is
only a partial definition primarily because :ada:`Pop` is not provided, but
other operations would be included as well. Moreover, a fully realistic version
would be a generic package. We have used a subtype named :ada:`Element` as a
substitute for the generic formal type what would have had that name. Note that
there is a :ada:`Default_Initial_Condition` aspect specifying that any object
of type :ada:`Stack` is initially empty as a result of default initialization.
The *argument* to the function call is the corresponding type name,
representing the current instance object, thus any object of the type.

.. code-block:: ada

    package Bounded_Stacks is

       subtype Element is Integer;
       --  arbitrary substitute for generic formal type

       type Stack (Capacity : Positive) is limited private with
         Default_Initial_Condition => Empty (Stack);

       procedure Push (This : in out Stack; Value : Element) with
         Pre  => not Full (This),
         Post => not Empty (This);

       function Full (This : Stack) return Boolean;

       function Empty (This : Stack) return Boolean;

    private

       type Contents is array (Positive range <>) of Element;

       type Stack (Capacity : Positive) is limited record
          Content : Contents (1 .. Capacity);
          Top     : Natural :=0;
       end record;

       function Full (This : Stack) return Boolean is
         (This.Top = This.Capacity);

       function Empty (This : Stack) return Boolean is
         (This.Top = 0);

    end Bounded_Stacks;

    package body Bounded_Stacks is

       procedure Push (This : in out Stack; Value : Element) is
       begin
          This.Top := This.Top + 1;
          This.Content (This.Top) := Value;
       end Push;

    end Bounded_Stacks;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Bounded_Stacks; use Bounded_Stacks;

    procedure Demo is
       S : Stack (Capacity => 10);
    begin
       Push (S, 42);
       Put_Line ("Done");
    end Demo;

The function :ada:`Empty` returns :ada:`True` when :ada:`Top` is zero, and zero
is assigned to :ada:`Top` during default initialization. Consequently,
:ada:`Assertion_Error` is not raised when :ada:`Demo` executes because the
object :ada:`S` was indeed default initialized to the empty state.

We said that when DIC is applied to a type, the run-time check is emitted for
all object declarations of that type that rely on default initialization. But
suppose the type does not define any default initialization. We can detect
these uninitialized objects at run-time if we set the DIC Boolean expression
to indicate that there is no default initialization defined for this type. The
checks will then fail for those objects. That's the second solution to the
initialization requirement.

Specifically, we can express the lack of default initialization by a DIC
condition that is hard-coded to the literal :ada:`False`. The evaluation during
the check will then necessarily fail, raising :ada:`Assertion_Error`. Hence,
for this type, explicit initialization is guaranteed in a program that does not
raise :ada:`Assertion_Error` for this cause.

The following is an example of the DIC set to :ada:`False`:

.. code-block:: ada

    package P is

       type Q is limited private with
         Default_Initial_Condition => False;

       function F return Q;

    private

       type Q is range -1 .. 100;

    end P;

    package body P is

       function F return Q is (42);

    end P;

    with Ada.Text_IO;  use Ada.Text_IO;

    with P; use P;

    procedure Main is
       Obj1 : constant Q := F;
       Obj2 : Q;  --  triggers Assertion_Error
    begin
       Put_Line (Obj1'Image);
       Put_Line (Obj2'Image);
       Put_Line ("Done");
    end Main;

In the above, :ada:`Assertion_Error` is raised by the elaboration of
:ada:`Obj2` because the DIC check necessarily fails. There is no check on the
declaration of :ada:`Obj1` because it is initialized, explicitly.

To recap, we can ensure initialization for objects of the type by detecting,
during elaboration at run-time, any objects not explicitly initialized.

This approach is sufficient because when elaboration of an object declaration
raises an exception, no use of that object is possible. That's guaranteed
because the frame containing that declarative part is immediately abandoned and
the exception is propagated up to the previous level. A local handler never can
apply. But even if there is a matching handler in the previous level, there's
really nothing much to be done. Re-entering the frame containing the
declaration will raise the exception all over again, necessarily. Thus the code
will have to be changed and recompiled, meeting the goal of the idiom.

We can illustrate this assurance using :ada:`Storage_Error`. Consider the
following program, in which the main procedure calls an inner procedure
:ada:`P`:

.. code-block:: ada

    with Text_IO; use Text_IO;

    procedure Main is

       procedure P (Output : out Float) is
          N : array (Positive) of Float; -- Storage_Error is likely
       begin
          Put_Line ("P's body assigns N's components and uses them");
          -- The following indexes and component values are arbitrary
          -- and used purely for illustration...
          N := (others => 0.0);
          -- other computations and assignments to N ...
          Output := N (5);
       exception
          when Storage_Error =>
             Output := N (1);
       end P;

       X : Float;

    begin
       P (X);
       Put_Line (X'Image);
       Put_Line ("Done");
    exception
       when Storage_Error =>
          Put_Line ("Main completes abnormally");
    end Main;

When :ada:`Main` calls :ada:`P`, the elaboration of the declarative part of
:ada:`P` almost certainly fails because there is insufficient storage to
allocate to the object :ada:`P.N`, hence :ada:`Storage_Error` is raised. (If
your machine can handle the above, congratulations.) Even though procedure
:ada:`P` has a handler specifically for :ada:`Storage_Error`, that handler
never applies because the declarative part is immediately abandoned. Instead,
the exception is raised in the caller, where it can be caught. This behavior is
essential to ensure that problematic objects are not referenced in the local
handlers. In the above, the handler in :ada:`P` for :ada:`Storage_Error`
references the object :ada:`P.N` to assign the :ada:`P.Output` parameter. If
that assignment could happen |mdash| again, it cannot |mdash| what would it
mean, functionally? No one knows.

Handling :ada:`Storage_Error` is a little tricky anyway. Does the OS give the
program a chance to execute a handler? If so, is there sufficient storage
remaining to execute the exception handler's statements? In any case you can
see the problem that the declaration failure semantics preclude.

Therefore, although the DIC solution is not enforced at compile-time, it is
nevertheless sufficient to ensure no uninitialized object of the type can be
used.


Preventing Object Creation by Clients
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The other idiom requirement is the ability to control object creation itself.
The solution is trivially achieved using an indefinite limited private type:
we can prevent client object creation simply by not providing any constructor
functions. Doing so removes any means for initializing objects of the type, and
since the type is indefinite there is then no way for clients to declare
objects at all. The compiler again enforces this solution.

For a concrete example, we can apply the Singleton design pattern to represent
the *time stamp counter* (:wikipedia:`TSC <Time_Stamp_Counter>`) provided by
x86 architectures. The TSC is a 64-bit hardware register incremented once per
clock cycle, starting from zero at power-up. We can use it to make a timestamp
abstraction. As explained by
:wikipedia:`Wikipedia page <Time_Stamp_Counter>`, some care is required when
using the register for that purpose on modern hardware, but it will suffice to
illustrate the idiom solution. Note that the Singleton pattern is itself
somewhat controversial in the OOP community, but that's beyond the scope of
this document.

Why use the Singleton pattern in this case? Ordinarily, clients of some ADT
will reasonably expect that the states of distinct objects are independent of
each other. When using an ADT to represent a single piece of hardware, however,
this presumption of independence will not hold because the device is shared by
all the objects, unavoidably. The singleton idiom prevents the resulting
problems by precluding the existence of multiple objects in the first place.

In this specific case, the time stamp counter hardware is read-only, so the
lack of independence is not an issue. Multiple objects would not be a problem.
But many devices are not read-only, so the singleton pattern is worth knowing.

First we'll define a singleton ADT representing the TSC register itself, then
we will extend that type to add convenience operations for measuring elapsed
times. We'll use the design approach of indefinite limited private types
without any constructor functions in order to ensure clients cannot create
objects of the type. The type will also be tagged for the sake of allowing type
extensions. Adding the tagged characteristic doesn't change anything regarding
the idiom solution.

.. code-block:: ada

    with Interfaces;

    package Timestamp is

       type Cycle_Counter (<>) is tagged limited private;

       type Cycle_Counter_Reference is access all Cycle_Counter;

       function Counter return not null Cycle_Counter_Reference;

       type Cycle_Count is new Interfaces.Unsigned_64;

       function Sample (This : not null access Cycle_Counter) return Cycle_Count;

    private

       type Cycle_Counter is tagged limited null record;

       function Read_TimeStamp_Counter return Cycle_Count with
         Import,
         Convention    => Intrinsic,
         External_Name => "__rdtsc",
         Inline;
       --  This gcc builtin issues the machine instruction to read the time-stamp
       --  counter, i.e., RDTSC, which returns a 64-bit count of the number of
       --  system clock cycles since power-up.

       function Sample (This : not null access Cycle_Counter)
                        return Cycle_Count is
         (Read_TimeStamp_Counter);
         --  The formal parameter This is not referenced

    end Timestamp;

Note also that the primitive function named :ada:`Counter` is not a
constructor |mdash| it doesn't return an object of the :ada:`Cycle_Counter`
type. As such, it cannot be used as an initial value for a :ada:`Cycle_Counter`
object declaration. Clients cannot, therefore, create their own objects of
type :ada:`Cycle_Counter`.

Instead, function :ada:`Counter` returns an access value designating an object
of the type. Because clients cannot declare objects themselves the function is
the only way to get an object, albeit indirectly. Therefore, the function can
control how many objects are created. As you will see, the function only
creates a single object of the type.

The type :ada:`Cycle_Counter` is completed as a null record because the state
is maintained in the hardware register we're reading.

The function :ada:`Sample` reads the timestamp counter register by calling the
:ada:`Read_TimeStamp_Counter` function. That second function accesses the TSC
register by executing an assembly language instruction dedicated to that
purpose. We could have :ada:`Sample` issue that instruction instead, without
declaring a separate function, but there is no run-time cost (due to the
inlining) and separating them emphasizes that one is a member of the API and
the other is an implementation artifact. Note that :ada:`Sample` does not
actually reference the formal parameter :ada:`This`. The parameter exists just
to make :ada:`Sample` a primitive function. Assuming we don't have a use-clause
for :ada:`Timestamp`, to call :ada:`Sample` we could say:

.. code-block:: ada

    TimeStamp.Counter.Sample

for example:

.. code-block:: ada

    with Timestamp;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Demo_TimeStamp is
    begin
       for K in 1 .. 10 loop
          Put_Line (Timestamp.Counter.Sample'Image);
       end loop;
    end Demo_TimeStamp;

The above calls the :ada:`Timestamp.Counter` function and then implicitly
dereferences the resulting access value to call the :ada:`Sample` function
using the distinguished receiver syntax. The resulting number is then converted
to a :ada:`String` value and output to :ada:`Standard_Output`.

We could have instead used positional call notation for the call to
:ada:`Sample`:

.. code-block:: ada

    Timestamp.Sample (Timestamp.Counter)

In that case we need the package name on the references, or we'd add a
use-clause.

The package body is shown below. Only the function :ada:`Counter` has a body
because :ada:`Sample` is completed in the package declaration's private part
and :ada:`Read_TimeStamp_Counter` is an imported intrinsic, i.e., without a
body.

.. code-block:: ada

    package body Timestamp is

       The_Instance : Cycle_Counter_Reference;

       -------------
       -- Counter --
       -------------

       function Counter return not null Cycle_Counter_Reference is
       begin
          if The_Instance = null then
             The_Instance := new Cycle_Counter;
          end if;
          return The_Instance;
       end Counter;

    end Timestamp;

Function :ada:`Counter` creates the single object that this singleton
implementation creates. It does so by lazily allocating an object dynamically.
If :ada:`Counter` is never called (because some subclass is used instead) then
no object of type :ada:`Cycle_Counter` is created. At most one
:ada:`Cycle_Counter` object is ever created.

We could instead declare :ada:`The_Instance` as a :ada:`Cycle_Counter` object
in the package body, mark it as aliased, and return a corresponding access
value designating it. But when objects are large, declaring one that might
never be used is wasteful. The indirection avoids that wasted storage at the
cost of an access object, which is small. On the other hand, now the heap is
involved.

Note that we could have declared :ada:`The_Instance` in the private part of the
package declaration. Type extensions in child packages could then use it, if
needed. Presumably we'd make :ada:`The_Instance` be of some access to
class-wide type so that extensions could use it to allocate objects of their
specific type, otherwise extensions in child packages would have no need for
it. But that only saves the storage for an access object in the child packages,
so we leave the declaration in the parent package body. See the
:ref:`Programming by Extension idiom <Ada_Idioms_Programming_By_Extension>`
for a discussion of whether to declare an entity in the package private part or
the package body.

Next, we declare a type extension in a child package. The child package body
will contain its own object named :ada:`The_Instance`, returning an access
value designating the specific extension type. The client API in the package
declaration follows that of the parent type :ada:`Cycle_Counter`, but with
additional primitives for working with samples.

.. code-block:: ada

    package Timestamp.Sampling is

       type Timestamp_Sampler is new Cycle_Counter with private;

       type Timestamp_Sampler_Reference is access all Timestamp_Sampler;

       function Counter return not null Timestamp_Sampler_Reference with Inline;
       --  returns an access value designating the single instance

       procedure Take_First_Sample  (This : not null access Timestamp_Sampler)
         with Inline;
       procedure Take_Second_Sample (This : not null access Timestamp_Sampler)
         with Inline;

       function First_Sample  (This : not null access Timestamp_Sampler)
                               return Cycle_Count;
       function Second_Sample (This : not null access Timestamp_Sampler)
                               return Cycle_Count;
       function Elapsed       (This : not null access Timestamp_Sampler)
                               return Cycle_Count;

    private

       type Timestamp_Sampler is new Cycle_Counter with record
          First  : Cycle_Count := 0;
          Second : Cycle_Count := 0;
       end record;

    end Timestamp.Sampling;

    package body Timestamp.Sampling is

       The_Instance : Timestamp_Sampler_Reference;

       -------------
       -- Counter --
       -------------

       function Counter return not null Timestamp_Sampler_Reference is
       begin
          if The_Instance = null then
             The_Instance := new Timestamp_Sampler;
          end if;
          return The_Instance;
       end Counter;

       -----------------------
       -- Take_First_Sample --
       -----------------------

       procedure Take_First_Sample (This : not null access Timestamp_Sampler) is
       begin
          This.First := Sample (This);
       end Take_First_Sample;

       ------------------------
       -- Take_Second_Sample --
       ------------------------

       procedure Take_Second_Sample (This : not null access Timestamp_Sampler) is
       begin
          This.Second := Sample (This);
       end Take_Second_Sample;

       ------------------
       -- First_Sample --
       ------------------

       function First_Sample (This : not null access Timestamp_Sampler)
                              return Cycle_Count is
         (This.First);

       -------------------
       -- Second_Sample --
       -------------------

       function Second_Sample (This : not null access Timestamp_Sampler)
                               return Cycle_Count is
         (This.Second);

       -------------
       -- Elapsed --
       -------------

       function Elapsed (This : not null access Timestamp_Sampler)
                         return Cycle_Count is
         (This.Second - This.First + 1);

    end Timestamp.Sampling;

The inherited :ada:`Sample` function is called in the two procedures that take
the two samples of the timestamp register. The formal parameter :ada:`This` is
passed to the calls, but as mentioned earlier the argument is not referenced
within :ada:`Sample`. All the formal parameter does is participate in
dispatching the calls to :ada:`Sample`, in this case meaning that the inherited
version of :ada:`Sample` is the one called because :ada:`This` is of the
extended type.

But :ada:`Sample` is not overridden in this child package, therefore
effectively we are calling the parent version. Is :ada:`Sample` ever likely to
be overridden? Arguably not, because it is so directly dependent on the
underlying hardware. Of course, some future type extension may override
:ada:`Sample` for some unforeseen reason |mdash| that's the point of making it
possible, after all. Presumably the overridden version would also call the
parent version, otherwise the timestamp counter would not be accessed. Because
we can't say for certain that it will never need to be overridden, we have made
:ada:`Sample` a primitive function, thus overridable.

Suppose we came to the opposite conclusion, that :ada:`Timestamp.Sample` would
never need to be overridden. In that case we have some options worth exploring.

Clearly function :ada:`Sample` must be part of the client API, but that doesn't
force it to be a primitive function.

We could have declared :ada:`Sample` in :ada:`Timestamp` as a visible
non-primitive operation, i.e., without a formal parameter or function result of
the ADT type:

.. code-block:: ada

    function Sample return Cycle_Count with Inline;

As a non-primitive function it would be neither inherited nor overridable. But
we'd still be able to call it in client code.

Yet, as a non-primitive, this version looks like an implementation artifact,
hence out of place as part of the visible client API. It isn't illegal by any
means, it just *looks* wrong.

Furthermore, if we are going to make :ada:`Sample` a non-primitive function,
why not remove it and replace it with the other non-primitive function
:ada:`Read_Timestamp_Counter`? Or make the body of :ada:`Sample` call the
imported intrinsic, and do away with function :ada:`Read_Timestamp_Counter`?
There is no clear winner here.

An attractive alternative would be to make :ada:`Sample` be a class-wide
operation. To do so, we make the formal parameter class-wide instead of
removing it:

.. code-block:: ada

    function Sample (This : not null access Cycle_Counter'Class)
                     return Cycle_Count
      with Inline;

In the version above, the formal parameter type is now (anonymous) access to
:ada:`Cycle_Counter'Class`, i.e., class-wide, so in this version :ada:`Sample`
can be passed a value designating an object of type :ada:`Cycle_Counter` or any
type derived from it. We don't want to have a null access value passed so we
add that to the parameter specification.

In this version the function is again not a primitive operation and so is
neither inherited nor overridable, but because it mentions type
:ada:`Cycle_Counter` it looks like a reasonable part of an Abstract Data Type.
As it happens this version of :ada:`Sample` also doesn't actually reference the
formal parameter, so it is somewhat unusual. Ordinarily in the body we'd expect
the class-wide formal to be used in dynamic dispatching calls to primitive
operations, but that's not required by the language.

Ultimately whether to make :ada:`Sample` a primitive operation is a judgment
call. We don't know that :ada:`Sample` will never need to be overridden so we
declare it as a primitive op.

With all that said, here is an example program using the child type. Because
the timestamp register is updated once per clock cycle, if we know the system
clock frequency we can use the counter to measure elapsed time. In the demo
below we measure the accuracy of the delay statement by delaying for a known
time, with samples taken before and after the delay statement. We can then
compare the known delay time to the measured elapsed time, printing the
difference.

Note the constant :ada:`Machine_Cycles_Per_Second`. Before you run the demo you
will likely need to change it in the source code to your machine's clock
frequency.

.. code-block:: ada

    with Timestamp.Sampling;   use Timestamp.Sampling;
    with Ada.Text_IO;          use Ada.Text_IO;

    procedure Demo_Sampling_Cycle_Counter is

       Delay_Interval : constant Duration := 1.0;
       --  arbitrary, change if desired
       Elapsed_Time   : Duration;

       GHz : constant := 1_000_000_000;

       Machine_Cycles_Per_Second : constant := 1.9 * GHz;
       --  This is the system clock rate on the machine running this executable.
       --  It corresponds to the rate at which the time stamp counter hardware is
       --  incremented. Change it according to your target.

       use type Timestamp.Cycle_Count;  --  for "<"
    begin
       Put_Line ("Using" & Machine_Cycles_Per_Second'Image
                 & " Hertz for system clock");

       Put_Line ("Delaying for" & Delay_Interval'Image & " second(s) ...");

       Counter.Take_First_Sample;
       delay Delay_Interval;
       Counter.Take_Second_Sample;

       Put_Line ("First sample            :" & Counter.First_Sample'Image);
       Put_Line ("Second sample           :" & Counter.Second_Sample'Image);

       if Counter.Second_Sample < Counter.First_Sample then
          Put_Line ("RDTSC counter wrapped around!?");
          return;
       end if;

       Elapsed_Time := Duration (Elapsed (Counter)) / Machine_Cycles_Per_Second;

       Put_Line ("Elapsed count           :" & Elapsed (Counter)'Image);
       Put_Line ("Specified delay interval:" & Delay_Interval'Image);
       Put_Line ("Measured delay interval :" & Elapsed_Time'Image);
    end Demo_Sampling_Cycle_Counter;

In the above, :ada:`Delay_Interval` is set to 1.0 so the program will delay for
1 second, with samples taken from the TSC before and after. Delay statement
semantics are such that at least the amount of time requested is delayed, so
some value slightly greater than 1 second is expected. There will be overhead
too, so an elapsed time slightly larger than requested should be seen. The
value of :ada:`Delay_Interval` is arbitrary, change it to whatever you like.

If you have set the :ada:`Machine_Cycles_Per_Second` properly but still get
elapsed measurement values that are much larger than expected or don't make
sense at all, it may be that your machine does not support using the
:wikipedia:`TSC <Time_Stamp_Counter>` this way reliably.


Pros
----

Ensuring explicit initialization is easily achieved. The abstraction should
likely be a private type anyway, and the syntax for the required additional
building blocks is light: all are just additional decorations on the
declaration of the private type or private extension. The compiler does the
rest, either at compile-time itself or via a generated check verified at
run-time.

Likewise, ensuring that only the implementation can create objects of a type
is straightforward. We take the same approach for ensuring initialization via
function calls in object declarations, but then don't provide any such
functions. Only the implementation will have the required visibility to create
objects of the type, and can limit that number of objects to one (or any other
number). Client access to this hidden object must be indirect, but that is not
a heavy burden.


Cons
----

None.


Relationship With Other Idioms
------------------------------

The :ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` is assumed, in
the form of a private type.


.. _Ada_Idioms_Controlling_Object_Initialization_And_Creation_Notes:

Notes
----------------------------------------------------------

Only certain types can have unknown discriminants. For completeness here is the
list:

    - A private type
    - A private extension
    - An incomplete type
    - A generic formal private type
    - A generic formal private type extension
    - A generic formal derived type
    - Descendants of the above

The types above will either have a corresponding completion or a generic actual
parameter to either define the discriminants or specify that there are none.

As we mentioned, :ada:`Default_Initial_Condition` is new in Ada 2022. The other
solution, based on indefinite private types, is supported by Ada 2022 but also
by earlier versions of the language. However, if the type is also limited, Ada
2005 is the earliest version allowing that solution. Prior to that version an
object of a limited type could not be initialized in the object's declaration.
