.. _Ada_Idioms_Abstract_Data_Machines:

Abstract Data Machines
======================

.. include:: ../../global.txt

Motivation
----------

.. todo::

    Complete section!


Solution
--------

The Abstract Data Machine (ADM) idiom is similar to the
:ref:`Abstract Data Type <Ada_Idioms_Abstract_Data_Types>` idiom in that it
presents an abstraction that doesn't already exist in the
programming language. Furthermore, like the ADT, operations are provided to
manipulate the abstraction state, which is not otherwise compile-time
visible to client code. These operations are thus enforced as the only
manipulation possible, as per the designer's intent.  (The Abstract Data
Machine was introduced by Grady Booch [1]_ as the Abstract State Machine, but that
name, though appropriate, encompasses more in computer science than we intend
to evoke.)

Unlike the ADT, however, the ADM does not define the abstraction as a type. To
understand this point, recall that type declarations are descriptions for
objects that will contain data (the state). For example,
our earlier :ada:`Stack` type was
defined as a record containing two components: an array to hold the values
logically contained by the :ada:`Stack` and an integer indicating the logical
top of that array. No data actually exists, i.e., is allocated storage, until
objects are declared. Clients can declare as many objects of type :ada:`Stack`
as they require and each object has a distinct, separate copy of those two
components.

Clients can, of course, choose to declare only one object of a given type, in
which case only one instance of the data described by the type will exist. But
in that case, other than convenience, there is no functional difference from
declaring objects of the component types directly, rather than indirectly via
some enclosing type. Instead of using the :ada:`Stack` type to declare a
single composite object, for example, the developer could have instead declared
two objects, one for the array and one for the stack pointer:

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

If there is only one *stack*, these two objects will suffice.

That's what the ADM does. The package, usually a library package, declares
the necessary state for a single abstraction instance. But, as an abstraction,
those data declarations must not be compile-time visible to clients. Therefore,
the state is declared in either the package private part or the package body.
Doing so requires that visible operations be made available to clients, like any
other abstraction. Hence the package is the one instance of the abstraction, as
opposed to defining one or more objects of a type.

Therefore, the package declaration's visible section contains only the
following:

   - Constants (but almost certainly not variables)

   - Ancillary Types

   - Exceptions

   - Operations

The package declaration's private part and the package body may contain all
the above, but one or the other (or both) will contain object declarations
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

Now there is no type presenting a :ada:`Stack` abstraction and the operations
do not take a stack parameter because the package and its data is the instance
of the abstraction. When using this idiom, there is only one stack of integers.
That's why we changed the name of the package from :ada:`Integer_Stacks`, i.e.,
from the plural form to the singular.

As with the ADT idiom, clients of an ADM can only manipulate the encapsulated
state indirectly, via the visible operations. The difference is that the state
to be manipulated is no longer a formal parameter. For example:

.. code-block:: ada

    Integer_Stack.Push (42);

That call places the value 42 in the array :ada:`Integer_Stack.Values` located
within the package body. Compare that to the ADT approach, in which objects of
type :ada:`Stack` are manipulated:

.. code-block:: ada

    Answers : Stack;
    --  ...
    Push (Answers, 42);

That call places the value 42 in the array :ada:`Answers.Values`, i.e., within
the :ada:`Answers` variable. Clients can declare as many :ada:`Stack` objects
as they require, each containing a distinct copy of the state defined by the
type. In the ADM version, there is only one stack and therefore only one instance
of the state.

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
the data. We've only changed where the data are declared. (We will discuss the
effect of child packages separately.)

The private section wasn't otherwise required when we chose to declare the data in
the package body.

The ADM idiom applies information hiding to the internal state, like the
ADT idiom, except that the state is not in objects. Also, like the
:ref:`Groups of Related Program Units <Ada_Idioms_Groups_Of_Related_Program_Units>`,
the implementations of the visible subprograms are hidden in the package body,
along with any non-visible entities required for their implementation.

There are no constructor functions returning a value of the abstraction
type because there is no such type with the ADM. However, there could be one or
more initialization procedures, operating directly on the hidden state in the
package private part or package body. In the :ada:`Stack` ADM there is no need
because of the reasonable initial state, as is true with the ADT version.

The considerations regarding selectors/accessors are the same for the ADM as
for the ADT idiom, so they are not provided by default. Also like the ADT,
so-called *getters* and *setters* are highly suspect and not provided by the
idiom by default.

Pros
----

In terms of abstraction and information hiding, the ADM idiom provides the same
advantages as the ADT idiom: clients have no visibility to
representation details and
must use the operations declared in the package to manipulate the state. The
compiler enforces this abstract view. The ADM also has the ADT benefit of
knowing where any bugs could possibly be located. If there is a bug in the
manipulation, it must be in the one package defining the abstraction itself. No
other code would have the compile-time visibility necessary.

This idiom can be applied to any situation requiring abstraction, including
hardware. For example, consider a microprocessor that has an on-board rotary
switch for arbitrary use by system designers. The switch value is
available to the software via an 8-bit integer located at a dedicated
memory address, mapped like so:

.. code-block:: ada

       Switch : Unsigned_8 with
          Volatile,
          Address => System.Storage_Elements.To_Address (16#FFC0_0801#);

Reading the value of the memory-mapped :ada:`Switch` variable provided the
current switch value.

However, the memory at that address was read-only, and rightly so because the
only way to change the value was to physically rotate the switch. Writing to
that address had no effect whatsoever. Although doing so was a logical error no
indication was provided by the hardware. That silence was potentially confusing
to developers. It certainly looked like a variable, after all. Declaring it as
a constant wouldn't suffice because the user could rotate the switch during
execution.

Furthermore, although mapped as a byte, the physical switch has only 16 total
positions, read as the values zero through fifteen. An unsigned byte has no
such constraints.

A good general rule is that if something shouldn't be done by clients, we
should use the compiler to make it impossible. That's better than debugging,
any day. Therefore, we will use the ADM idiom to represent the rotary switch.
The compiler will enforce the read-only view and the operation can handle the
range constraint. The ADM is a reasonable choice because there is only one such
physical switch; a type doesn't bring any advantages in this case. The
following illustrates the approach:

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

Cons
----

An ADM defines only one abstraction instance. If more than one is required, the
developer must copy-and-paste the entire package and then change the package
unit name.

Furthermore, the ADM cannot be used to compose other types, e.g., as the
component type in an array or record type. The ADM cannot be used to define the
formal parameter of a client-defined subprogram, cannot be dynamically
allocated, and so on.

But if one can know with certainty that only one thing is ever going to be
represented, as in the hardware switch example, the ADM limitations are
irrelevant. That said, certainty is usually not available |mdash| even the
hardware changes.

Bibliography
------------

.. [1] Booch, G. (1983). Software Engineering with Ada, Benjamin/Cummings
       Publishing Company.
