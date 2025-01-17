.. _Ada_Idioms_Abstract_Data_Machines:

Abstract Data Machines
======================

.. include:: ../../global.txt

Motivation
----------

In some systems, only one logical "instance" of an abstraction should exist in 
the software. This requirement may stem from the functionality involved. 
For example, a subsystem-level software logging facility should be 
unique at that level. Likewise, the function of a hardware device may be 
such that only one instance should exist in both the system and the 
software. A security device that validates users would be an example. 
Another reason can be simple physical reality. There might be only one 
on-board or on-chip device of some sort. Execution on that board or chip 
entails there being only one such device present. 

How can the software representing the abstraction best implement this 
requirement? 

The Abstract Data Type (ADT) :ref:`Abstract Data Type 
<Ada_Idioms_Abstract_Data_Types>` idiom is the primary abstraction 
definition facility in Ada. Given an ADT that provides the required 
facility you could simply declare a single object of the type. But how 
could you ensure that some other client, perhaps in the future, doesn't 
declare another object of the type, either accidentally or maliciously? 

As a general statement about program design, if there is something that 
must not be allowed, the ideal approach is to use the language rules to make 
it impossible. That's far better than debugging. For example, we 
don't want clients to have compile-time access to internal 
representation artifacts, so we leverage the language visibility rules 
to make such access illegal. The compiler will then reject undesired 
references, rigorously.

The occasional need to control object creation is well-known, so much so 
that there is a design pattern for creating an ADT in which only one 
instance can ever exist. Known as the "singleton" pattern, the given 
programming language's rules are applied such that only the ADT 
implementation can create objects of the type. Clients cannot do so. The 
implementation only creates one such object, so multiple object 
declarations are precluded. 

Singletons can be expressed easily in Ada
:ref:`Controlling Object Initialization and Creation <Ada_Idioms_Controlling_Object_Initialization_And_Creation>`
but there is an alternative in this specific situation.

This idiom entry describes the alternative, known as the Abstract Data
Machine (ADM). The Abstract Data Machine was introduced by Grady Booch [1]_
as the Abstract State Machine, but that name, though appropriate,
encompasses more in computer science than we intend to evoke.

Implementation(s)
-----------------

The ADM is similar to the ADT idiom in that it presents an 
abstraction that doesn't already exist in the programming language. 
Furthermore, like the ADT, operations are provided to clients to 
manipulate the abstraction state, which is not otherwise compile-time
visible to client code. 

Unlike the ADT, however, the ADM does not define the abstraction as a 
type. To understand this difference, first recall that type declarations 
are descriptions for objects that will contain data (the state). For 
example, our earlier :ada:`Stack` ADT was represented as a record containing 
two components: an array to hold the values logically contained by the 
:ada:`Stack` and an integer indicating the logical top of that array 
(the stack pointer). No data actually exists, i.e., is allocated 
storage, until objects of the type are declared. Clients can declare as 
many objects of type :ada:`Stack` as they require and each object has a 
distinct, independent copy of the data. 

Continuing the :ada:`Stack` example, clients could choose to declare only
one object of the :ada:`Stack` type, in which case only one instance of 
the data described by the :ada:`Stack` type will exist:

.. code-block:: ada

       Integer_Stack : Stack;

But, other than convenience, there is no *functional* difference from the
client declaring individual variables of the representational
component types directly, one for the array and one for the stack pointer: 

.. code-block:: ada

       Capacity : constant := 100;
       type Content is array  (1 .. Capacity) of Integer;
       Values : Content;
       Top    : Integer range 0 .. Capacity := 0;

or even this, using an anonymously-typed array:

.. code-block:: ada

       Capacity : constant := 100;
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;

If there is to be only one logical stack, these two variables will
suffice.

That's what the ADM does. The state variables are declared directly 
within a package, rather than as components of a type. In that way the 
package, usually a library package, declares the necessary state for a 
single abstraction instance. But, as an abstraction, those data 
declarations must not be compile-time visible to clients. Therefore, the 
state is declared in either the package private part or the package 
body. Doing so requires that visible operations be made available to 
clients, as with the ADT. Hence the combination of a package, the 
encapsulated variables, and the operations is the one instance of the 
abstraction. That combination is the fundamental concept for the ADM idiom.

Therefore, the package declaration's visible section contains only the
following:

   - Constants (but almost certainly not variables)

   - Ancillary Types

   - Exceptions

   - Operations

The package declaration's private part and the package body may contain all
the above, but one or the other (or both) will contain variable declarations
representing the abstraction's state.

Consider the following ADM version of the package :ada:`Integer_Stacks`, now
renamed to :ada:`Integer_Stack` for reasons we will discuss shortly. In this
version we declare the state in the package body.

.. _Ada_Idioms_Abstract_Data_Machines_Code_Example:

.. code-block:: ada

    package Integer_Stack is
       procedure Push (Item : in Integer);
       procedure Pop (Item : out Integer);
       function Empty return Boolean;
       Capacity : constant := 100;
    end Integer_Stack;

    package body Integer_Stack is
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;
       procedure Push  (Item : in Integer) is
       begin
          --  ...
          Top := Top + 1;
          Values (Top) := Item;
       end Push;
       procedure Pop (Item : out Integer) is ... end Pop;
       function Empty return Boolean is (Top = 0);
    end Integer_Stack;

Note how the procedure and function bodies directly access the local
variables hidden in the package body.

For those readers familiar with programming languages that can declare 
entities to be "static," the effect is as if the two variables in the 
package body are static variables.

When using this idiom, there is only one stack (containing values of 
some type, in this case type Integer). That's why we changed the name of 
the package from :ada:`Integer_Stacks`, i.e., from the plural form to 
the singular. It may help to note that what is now the package name was 
the name of the client's variable name when there was a :ada:`Stack` 
type involved. 

As with the ADT idiom, clients of an ADM can only manipulate the 
encapsulated state via the visible operations. The difference is that 
the state to be manipulated is no longer an object passed as an argument 
to the operations. For illustration, consider the :ada:`Push` procedure. 
The ADT version requires the client to pass the :ada:`Stack` object intended to 
contain the new value (i.e., the actual parameter for the formal named :ada:`This`): 

.. code-block:: ada

       procedure Push (This : in out Stack; Item : in Integer);

In contrast, the ADM version has one less formal parameter, the value to be pushed:

.. code-block:: ada

       procedure Push (Item : in Integer);

Here is a call to the ADM version of Push:

.. code-block:: ada

    Integer_Stack.Push (42);

That call places the value 42 in the (hidden) array :ada:`Integer_Stack.Values` located
within the package body. Compare that to the ADT approach, in which objects of
type :ada:`Stack` are manipulated:

.. code-block:: ada

    Answers : Stack;
    --  ...
    Push (Answers, 42);

That call places the value 42 in the (hidden) array :ada:`Answers.Values`, i.e., within
the :ada:`Answers` variable. Clients can declare as many :ada:`Stack` objects
as they require, each containing a distinct copy of the state defined by the
type. In the ADM version, there is only one stack and therefore only one instance
of the state variables. Hence the :ada:`Stack` formal parameter is not required.

Rather than declare the abstraction state in the package body, we could just as
easily declare it in the package's private section:

.. code-block:: ada

    package Integer_Stack is
       procedure Push (Item : in Integer);
       procedure Pop (Item : out Integer);
       function Empty return Boolean;
       Capacity : constant := 100;
    private
       Values : array  (1 .. Capacity) of Integer;
       Top    : Integer range 0 .. Capacity := 0;
    end Integer_Stack;

Doing so doesn't change anything from the client code point of view; just as
clients have no compile-time visibility to declarations in the package body,
they have no compile-time visibility to the items in the package private part.
This placement also doesn't change the fact that there is only one instance of
the data. We've only changed where the data are declared. (We will ignore the
effect of child packages here.) 

Because the two variables are implementation artifacts we don't declare 
them in the package's visible part. 

Note that the private section wasn't otherwise required when we chose to 
declare the data in the package body. 

The ADM idiom applies information hiding to the internal state, like the 
ADT idiom, except that the state is not in an object declared by the 
client. Also, like the :ref:`Groups of Related Program Units 
<Ada_Idioms_Groups_Of_Related_Program_Units>`, the implementations of 
the visible subprograms are hidden in the package body, along with any 
non-visible entities required for their implementation. 

There are no constructor functions returning a value of the abstraction 
type because the abstraction is not represented as a type. However, 
there could be one or more initialization procedures, operating directly 
on the hidden state in the package private part or package body. In the 
:ada:`Stack` ADM there is no need for them because of the 
abstraction-appropriate default initial value, as is true of the ADT 
version. 

The considerations regarding selectors/accessors are the same for the ADM as
for the ADT idiom, so they are not provided by default. Also like the ADT,
so-called *getters* and *setters* are highly suspect and not provided by the
idiom by default.

As mentioned, the ADM idiom can be applied to hardware abstractions. For 
example, consider a target that has a single on-board rotary switch 
for arbitrary use by system designers. The switch value is available to 
the software via an 8-bit integer located at a dedicated memory address, 
mapped like so: 

.. code-block:: ada

       Switch : Unsigned_8 with
          Volatile,
          Address => System.Storage_Elements.To_Address (16#FFC0_0801#);

Reading the value of the memory-mapped :ada:`Switch` variable provides the
rotary switch's current value.

However, on this target the memory at that address is read-only, and 
rightly so because the only way to change the value is to physically 
rotate the switch. Writing to that address has no effect whatsoever. 
Although doing so is a logical error no indication is provided by the 
hardware, which is potentially confusing to developers. It certainly 
looks like a variable, after all. Declaring it as a constant wouldn't 
suffice because the user could rotate the switch during 
execution.

Furthermore, although mapped as a byte, the physical switch has only 16 total
positions, read as the values zero through fifteen. An unsigned byte has no
such constraints.

The compiler will enforce the read-only view and the accessor operation 
can handle the range constraint. The ADM is a reasonable choice because 
there is only one such physical switch; a type doesn't bring any 
advantages in this case. The following illustrates the 
approach:

.. code-block:: ada

    with Interfaces; use Interfaces;
    package Rotary_Switch is
       subtype Values is Unsigned_8 range 0 .. 15;
       function State return Values;
    end Rotary_Switch;

Clients can then call the function :ada:`Rotary_Switch.State` to get the
switch's current value, as a constrained subtype. The body will handle all the
details.

.. code-block:: ada

    with System.Storage_Elements;  use System.Storage_Elements;
    package body Rotary_Switch is
       Switch : Unsigned_8 with Volatile, Address => To_Address (16#FFC0_0801#);
       function State return Values is
       begin
          if Switch in Values then
             return Switch;
          else
             raise Program_Error;
          end if;
       end State;
    end Rotary_Switch;

The range check in the function body might be considered over-engineering
because the switch is a physical device that cannot have more than 16 values,
but physical devices have a habit of springing surprises. Note that
:ref:`attribute Valid <Adv_Ada_Valid_Attribute>` would not be useful here
because there are no invalid bit patterns for an unsigned integer. If, on the
other hand, we were working with an enumeration type, for example, then
:ada:`'Valid` would be useful.


Pros
----

In terms of abstraction and information hiding, the ADM idiom provides 
the same advantages as the ADT idiom: clients have no visibility to 
representation details and must use the operations declared in the 
package to manipulate the state. The compiler enforces this abstract 
view. The ADM also has the ADT benefit of knowing where any bugs could 
possibly be located. If there is a bug in the behavior, it must be in 
the one package defining the abstraction itself. No other code would 
have the compile-time visibility necessary.

In addition, less source code text is required to express the abstraction.

Cons
----

The disadvantage of the ADM is the lack of flexibility.

An ADM defines only one abstraction instance. If more than one becomes 
necessary, the developer must copy-and-paste the entire package and then 
change the new package's unit name. This approach doesn't scale well.

Furthermore, the ADM cannot be used to compose other types, e.g., as the
component type in an array or record type. The ADM cannot be used to 
define the formal parameter of a client-defined subprogram, cannot be 
dynamically allocated, and so on. 

But if one can know with certainty that only one thing is ever going to be
represented, as in the hardware rotary switch example, the ADM limitations are
irrelevant.

Bibliography
------------

.. [1] Booch, G. (1983). Software Engineering with Ada, Benjamin/Cummings
       Publishing Company.
