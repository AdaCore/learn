Providing Component Access to Enclosing Record Objects
======================================================

.. include:: ../../global.txt

Problem Statement
-----------------

In some design situations we want to have a record component that is of a task
or protected type. That in itself is trivially accomplished because task types
and protected types can be used to declare record components. But there's more
to this idiom.

We would want a task type or protected type record component when:

    a) a task or protected object (PO) is required to implement part |mdash|
       but not all |mdash| of the record type's functionality, and

    b) each such task or PO is intended to implement its functionality only for
       the object logically containing that specific task object or protected
       object. The record object and contained task/PO object pair is a
       functional unit, independent of all other such units.

This idiom applies to both enclosed task types and protected types, but for the
sake of concision let's assume the record component will be of a protected
type.

As part of a functional unit, the PO component will almost certainly be
required to reference the other record components in the enclosing record
object. That reference will allow the PO to read and/or update those other
components.  Note that these record components include discriminants, if any.

To be a functional unit, the record object referenced by a given PO in this
relationship must be the same record object at run-time that contains that
specific PO instance. That will allow the PO instance to implement the
functionality for the specific record object containing that PO instance.

Unless we arrange it, that back-reference from the protected object to the
record object isn't provided. Consider the following:

.. code-block:: ada

    package Q is
       protected type P is ... end P;
       type R is record
         ...
         Y : P;
       end record;
    end Q;

During execution, whenever an object of type :ada:`Q.R` is declared or
allocated, at run-time we will have two objects, instances of two distinct
types |mdash| the record object and the protected object. Let's say that a
client declares an object :ada:`Obj` of type :ada:`R`. There is only one
reference direction defined, from the record denoted by :ada:`Obj` to the
component protected object denoted by :ada:`Obj.Y`. This idiom, however,
requires a reference in the opposite direction, from :ada:`Oby.Y` to
:ada:`Obj`.

This may seem like an unrealistic situation, but it is not. An IO device type
that involves interrupt handling is just one real-world example, one that we
will show in detail.

The idiom context is a type because there will often be multiple real-world
entities being represented in software. Representing these entities as multiple
objects declared of a single type is by far the most reasonable approach.

We assume the functional unit will be implemented as an
:ref:`Abstract Data Type (ADT) <Ada_Idioms_Abstract_Data_Types>`. Strictly
speaking, the ADT idiom is not required here, but that is the best approach for
defining major types, for the good reasons given in that idiom entry. There's
no reason not to use an ADT in this case so we will.


Solution
-----------------

As mentioned, the solution applies to enclosed components of both task types
and protected types. We will continue the discussion in terms of protected
types.

The solution has two parts:

    1. An access discriminant on the PO type, designating the enclosing
       record's type. That part is straightforward.

    2. A value given to that discriminant that designates the object of the
       enclosing record type, i.e., the record object that contains that PO.
       That part requires a relatively obscure language construct.

Given those two parts, the PO can then dereference its access discriminant to
read or update the other components in the same enclosing record object.

Consider the following (very artificial) package declaration illustrating these
two parts:

.. code-block:: ada

    package P is
       type Enclosing is tagged limited private;
    private

       protected type Controller (Instance : not null access Enclosing) is
          -- Part 1

          procedure Increment_X;
       end Controller;

       type Enclosing is tagged limited record
          X : Integer;  -- arbitrary type

          C : Controller (Instance => ...);
          -- Part 2, not fully shown yet
       end record;

    end P;

The record type named :ada:`Enclosing` contains a component named :ada:`X`,
arbitrarily of type :ada:`Integer`, and another component :ada:`C` that is of
protected type :ada:`Controller`.  Part #1 of the solution is the access
discriminant on the declaration of the protected type :ada:`Controller`:

.. code-block:: ada

    protected type Controller (Instance : not null access Enclosing) is

Given a value for the discriminant :ada:`Instance`, the code within the spec
and body of type :ada:`Controller` can then reference some :ada:`Enclosing`
object via that discriminant.

But not just any object of type :ada:`Enclosing` will suffice. For Part #2, we
must give the :ada:`Instance` discriminant a value that refers to the current
instance of the record object containing this same PO object. In the package
declaration above, the value passed to :ada:`Instance` is elided. The following
is that code again, now showing just the declaration for :ada:`Enclosing`, but
also including the construct that is actually passed. This is where the
subtlety comes into play:

.. code-block:: ada

    type Enclosing is tagged limited record
          ...
          C : Controller (Instance => Enclosing'Access);
    end record;

The subtlety is the expression :ada:`Enclosing'Access`. Within a type
declaration, usage of the type name denotes the current instance of that type.
The current instance of a type is the object of the type that is associated
with the execution that evaluates the type name. For example, during execution,
when an object of type :ada:`Enclosing` is elaborated, the name
:ada:`Enclosing` refers to that object.

It isn't compiler-defined magic, the semantics are defined by the Ada standard
so it is completely portable. (There are other cases for expressing the current
instance of task types, protected types, and generics.)

Therefore, within the declaration of type :ada:`Enclosing`, the expression
:ada:`Enclosing'Access` provides an access value designating the current
instance of that type. This is exactly what we want and is the crux of the
idiom expression. With that discriminant value, the enclosed PO spec and body
can reference the other record components of the same object that contains the
PO.

To illustrate that, here is the package body for this trivial example. Note the
value referenced in the body of procedure :ada:`Increment_X`:

.. code-block:: ada

    package body P is

       protected body Controller is

          procedure Increment_X is
          begin
             Instance.X := Instance.X + 1;
          end Increment_X;

       end Controller;

    end P;

Specifically, the body of procedure :ada:`Increment_X` can use the access
discriminant :ada:`Instance` to get to the current instance's :ada:`X`
component. (We could express it as :ada:`Instance.all.X` but why bother.
Implicit dereferencing is a wonderful thing.)

That's the solution. Now for some necessary details.

Note that we declared type :ada:`Enclosing` as a limited type, first in the
visible part of the package:

.. code-block:: ada

    type Enclosing is tagged limited private;

and again in the type completion in the package private part:

.. code-block:: ada

    type Enclosing is tagged limited record ... end record;

The type need not be tagged for this idiom solution, but if you do make it
tagged, the partial and full views must always match. That is, a tagged type
must be limited in both views if it is limited in either view.

For the idiom solution to be legal, the type's completion in the private part
must always be *immutably limited*, meaning that it is always truly limited.
There are various ways to make that happen (see
:aarm22:`AARM22 7.5 (8.1/3) <7-5>` ) but the easiest way to is to include the
reserved word :ada:`limited` in the type definition within the full view, as we
did above. That is known as making the type *explicitly limited*.

Why does the compiler require the type to be immutably limited? 

Recall that a (non-tagged) private type need not be limited in both views. It
can be limited in the partial client view but non-limited in the private full
view:

.. code-block:: ada

    package Q is
       type T is limited private;
       -- the partial view for clients in package visible part
       ...
    private
       type T is record -- the full view in the package private part
          ...
       end record;
    end Q;

Clients must treat type :ada:`Q.T` as if it is limited, but :ada:`Q.T`
isn't really limited because the full view defines reality. Clients simply
have a more restricted view of the type than is really the case.

Types that are explicitly limited really are limited, and always have a view as
a limited type. That's important because the type given in :ada:`type_name'Access` must be
aliased for :ada:`'Access` to be meaningful and possible on the corresponding
objects. But if the type's view could change between limited and not limited,
it will be aliased in some contexts and not aliased in others. To prevent that
complexity, the language requires the type's view to be permanently limited so
that the type will be permanently aliased. An immutably limited type is
permanently aliased. In practice, we're working with record types and type
extensions, so just make the type definition explicitly limited and all will be
well.


Real-World Example
----------------------------------

For a concrete, real-world example, suppose we have a serial IO device on an
embedded target board. The device can be either a UART or
:wikipedia:`USART <Universal_synchronous_and_asynchronous_receiver-transmitter>`.
For the sake of brevity let's assume we have USARTs available.

Many boards have more than one USART resident, so it makes sense to represent
them in software as instances of an ADT. This example uses the USART ADT
supported in the
`Ada Drivers Library (ADL) <https://github.com/AdaCore/Ada_Drivers_Library>`_
that is named, imaginatively, USART. (We don't show package
:ada:`STM32.USARTs`, but you will see it referenced in the example's context
clauses.) Each of these USART devices can support either a polling
implementation or an interrupt-driven implementation. We will first define a
basic USART ADT, and then extend that to a new one that works with interrupts.

At the most basic level, to work with a given USART device we must combine it
with some other hardware, specifically the IO pins that connect it to the
outside world. That combination will be represented by a new ADT, the type
:ada:`Device` defined in package :ada:`Serial_IO`.

Any given :ada:`Serial_IO.Device` object will be associated permanently with
one USART. Therefore, type :ada:`Device` will have a discriminant named
:ada:`Transceiver` designating that :ada:`USART` object.

There are some low-level operations that a :ada:`Serial_IO.Device` will
implement, such as initializing the hardware and setting the baud rate and so
forth. We can also implement the hardware-oriented input and output routines in
this package because both are independent of the polling or interrupt-driven
designs.

Here's the resulting package declaration for the serial IO device ADT. Parts of
the package are elided for simplicity (the full code is
:ref:`at the end of this idiom entry <Ada_Idioms_Serial_IO_Complete_Example>`):

.. code-block:: ada

    with STM32;         use STM32;
    with STM32.GPIO;    use STM32.GPIO;
    with STM32.USARTs;  use STM32.USARTs;
    with HAL;  -- the ADL's Hardware Abstraction Layer

    package Serial_IO is

       type Device (Transceiver : not null access USART) is tagged limited private;

       procedure Initialize
         (This           : in out Device;
          Tx_Pin         : GPIO_Point;
          Rx_Pin         : GPIO_Point;
          ...);

       procedure Configure (This : in out Device;  Baud_Rate : Baud_Rates;  ...);
       ...
       procedure Put (This : in out Device;  Data : HAL.UInt8) with Inline;
       procedure Get (This : in out Device;  Data : out HAL.UInt8) with Inline;

    private

       type Device (Transceiver : not null access USART) is tagged limited record
          Tx_Pin : GPIO_Point;
          Rx_Pin : GPIO_Point;
          ...
       end record;

    end Serial_IO;

When called, procedure :ada:`Initialize` does the hardware setup required, such
as enabling power for the USART and pins. We can ignore those details for
this discussion.

Given this basic :ada:`Device` type we can then use inheritance (type
extension) to define distinct types that support the polling and
interrupt-driven facilities. These types will themselves be ADTs. Let's focus
on the new interrupt-driven ADT, named :ada:`Serial_Port`. This type will be
declared in the child package :ada:`Serial_IO.Interrupt_Driven`.

When interrupts are used, each USART raises a USART-specific interrupt for
sending and receiving. Each interrupt occurrence is specific to one device.
Therefore, the interrupt handler code is specific to each :ada:`Serial_Port`
object instance. We use protected objects as interrupt handlers in (canonical)
Ada, hence each :ada:`Serial_Port` object will contain a dedicated interrupt
handling PO as a record component.

As a controller and handler for a USART's interrupts, the PO will require a way
to access the USART and pins being driven. Our idiom design provides that
access.

Here is the client view of the ADT for the interrupt-driven implementation:

.. code-block:: ada

    with Ada.Interrupts;      use Ada.Interrupts;
    with HAL;
    with System; use System;

    package Serial_IO.Interrupt_Driven is

       type Serial_Port
         (Transceiver  : not null access USART;
          IRQ          : Interrupt_ID;
          IRQ_Priority : Interrupt_Priority)
       is new Serial_IO.Device with private;

       --  The procedures Initialize and Configure, among others, are
       --  implicitly declared here as operations inherited from
       --  Serial_IO.Device.

       overriding
       procedure Put (This : in out Serial_Port;  Data : HAL.UInt8) with Inline;

       overriding
       procedure Get (This : in out Serial_Port;  Data : out HAL.UInt8) with Inline;

    private
       ...
    end Serial_IO.Interrupt_Driven;

The declaration of type :ada:`Serial_Port` uses
:ref:`Interface Inheritance <Ada_Idioms_Inheritance_Idioms>` to extend
:ada:`Serial_IO.Device` with both visible and hidden components. The three
visible extension components are the discriminants :ada:`Transceiver`,
:ada:`IRQ`, and :ada:`IRQ_Priority`. :ada:`Transceiver` will designate the
:ada:`USART` to drive (discussed in a moment). :ada:`IRQ` is the
:ada:`Interrupt_ID` indicating the interrupt that the associated :ada:`USART`
raises. :ada:`IRQ_Priority` is the priority for that interrupt. (*IRQ* in a
common abbreviation for *Interrupt ReQuest*.) These two interrupt-oriented
discriminants are used within the PO declaration to configure it for interrupt
handling.

Clients will know which USART they are working with so they will be able to
determine which interrupt ID and priority to specify, presumably by consulting
the board documentation.

Now let's examine the :ada:`Serial_Port` type completion in the package's
private part.

We've said we will use a PO interrupt handler as a component of the
:ada:`Serial_Port` record type.  This PO type, named :ada:`IO_Manager`, will
include discriminants for the two interrupt-specific values it requires as an
interrupt handler. It will also have a discriminant providing access to the
enclosing :ada:`Serial_Port` record type.

.. code-block:: ada

    protected type IO_Manager
      (IRQ          : Interrupt_ID;
       IRQ_Priority : Interrupt_Priority;
       Port         : not null access Serial_Port)
    with
       Interrupt_Priority => IRQ_Priority
    is
       entry Put (Datum : HAL.UInt8);
       entry Get (Datum : out HAL.UInt8);
    private
       ...
       procedure IRQ_Handler with Attach_Handler => IRQ;
     end IO_Manager;

Note how the first two discriminants are used within the type declaration to
give the priority of the PO and to attach the interrupt handler procedure
:ada:`IRQ_Handler` to the interrupt indicated by :ada:`IRQ`.  The :ada:`Port`
discriminant will be the back-reference to the enclosing record object.

We can then, finally, provide the :ada:`Serial_Port` type completion, in which
the record object and protected object are connected whenever a
:ada:`Serial_Port` object is declared:

.. code-block:: ada

    type Serial_Port
      (Transceiver  : not null access USART;
       IRQ          : Interrupt_ID;
       IRQ_Priority : Interrupt_Priority)
    is new Serial_IO.Device (Transceiver) with record
       Controller : IO_Manager (IRQ, IRQ_Priority, Serial_Port'Access);
    end record;

The type completion repeats the declaration in the public part, up to the point
where the :ada:`Serial_Port.Transceiver` discriminant is passed to the
:ada:`Serial_IO.Device.Transceiver` discriminant. Type :ada:`Device` must be
constrained with a discriminant value here, so we just pass the discriminant
defined for :ada:`Serial_Port`.

Why does :ada:`Serial_Port` also have a :ada:`Transceiver` discriminant? Just
as :ada:`Serial_IO.Device` is a wrapper for the combination of a USART and IO
pins, :ada:`Serial_Port` is a wrapper for :ada:`Serial_IO.Device`. Hence
:ada:`Serial_Port` also needs a discriminant designating a :ada:`USART`.

The full definition of type :ada:`Serial_Port` contains the declaration of the
component named :ada:`Controller`, of the protected type :ada:`IO_Manager`. The
two interrupt-oriented discriminants from :ada:`Serial_Port` are passed to the
discriminants defined for this PO component. The third :ada:`IO_Manager`
discriminant value, :ada:`Serial_Port'Access`, denotes the current instance of
the :ada:`Serial_Port` type.  Thus the idiom requirements are achieved.

Let's see that back-reference in use within the protected body.

As mentioned, there is one interrupt used for both sending and receiving, per
:ada:`USART`.  Strictly speaking, the device itself does use two dedicated
interrupts, one indicating that a 9-bit value has been received, and one
indicating that transmission for a single 9-bit value has completed. But these
two are signaled to the software on one interrupt line, and that is the value
indicated by :ada:`IRQ`.

Therefore, there is one interrupt handling protected procedure, named
:ada:`IRQ_Handler`. In response to this interrupt, :ada:`IRQ_Handler`
determines which event has occurred by checking a status register. Here is the
body of that procedure:

.. code-block:: ada

    procedure IRQ_Handler is
    begin
       --  check for data arrival
       if Port.Transceiver.Status (Read_Data_Register_Not_Empty) and then
           Port.Transceiver.Interrupt_Enabled (Received_Data_Not_Empty)
       then  -- handle reception
          -- call the Serial_IO.Device version:
          Get (Serial_IO.Device (Port.all), Incoming);

          Await_Reception_Complete : loop
             exit when not Port.Transceiver.Status (Read_Data_Register_Not_Empty);
          end loop Await_Reception_Complete;
          Port.Transceiver.Disable_Interrupts (Received_Data_Not_Empty);
          Port.Transceiver.Clear_Status (Read_Data_Register_Not_Empty);
          Incoming_Data_Available := True;
       end if;

       --  check for transmission ready
       if Port.Transceiver.Status (Transmission_Complete_Indicated) and then
          Port.Transceiver.Interrupt_Enabled (Transmission_Complete)
       then  -- handle transmission
          -- call the Serial_IO.Device version:
          Put (Serial_IO.Device (Port.all), Outgoing);

          Port.Transceiver.Disable_Interrupts (Transmission_Complete);
          Port.Transceiver.Clear_Status (Transmission_Complete_Indicated);
          Transmission_Pending := False;
       end if;
    end IRQ_Handler;

Note how the handler references the :ada:`Transceiver` via the :ada:`Port`
discriminant. That's required, the PO cannot simply reference a
:ada:`Serial_Port`\'s components directly. That is, again, the point of the
idiom.

In this example, although the PO only accesses the :ada:`Transceiver`
component in the enclosing record object, additional functionality might need
to access more components, for this example perhaps using some of the
inherited IO pin components.


Pros
-----------------

The solution is directly expressed, requiring only an access discriminant and
the current instance semantics of :ada:`type_name'Access`.

Although the real-word example is complex |mdash| multiple discriminants are
involved, and a type extension |mdash| the idiom solution itself requires little
text. Interrupt handling is relatively complex in any language.


Cons
-----------------

The record type must be truly a limited type, but that is not the severe
limitation it was in earlier versions of Ada. Note that although access
discriminants are required, there is no dynamic allocation involved.


Relationship With Other Idioms
-------------------------------

This idiom is useful when we have a record type enclosing a PO or task object.
If the :ref:`Abstract Data Machine (ADM) <Ada_Idioms_Abstract_Data_Machines>`
would instead be appropriate, the necessary visibility can be achieved without
requiring this idiom solution because there would be no enclosing record type.
But as described in the ADM discussion, the
:ref:`ADT approach <Ada_Idioms_Abstract_Data_Types>` is usually superior.


Notes
-----------------

    1. As a wrapper abstraction for a USART, package :ada:`Serial_IO` is still
       more hardware-specific than absolutely necessary, as reflected in the
       parameters' types for procedure :ada:`Initialize` and the corresponding
       record component types. We could use the Hardware Abstraction Layer
       (HAL) to further isolate the hardware dependencies, but that doesn't
       affect the idiom expression itself.

    2. We use the
       `Ada Drivers Library (ADL) <https://github.com/AdaCore/Ada_Drivers_Library>`_
       extensively in the IO example.


Bibliography
-----------------

None.


.. _Ada_Idioms_Serial_IO_Complete_Example:


Full Source Code for Selected Units
---------------------------------------------------

We did not show some significant parts of the code discussed above, for the
sake of not obscuring the points being made. Doing so, however, means that the
interested reader cannot see how everything fits together and works, such as
the actual IO using interrupts. The code below shows the relevant packages in
their entirety. Note that the ADL STM32 hierarchy packages and the HAL
(Hardware Abstraction Layer) package are in the
`Ada Drivers Library on GitHub <https://github.com/AdaCore/Ada_Drivers_Library>`_.

First, the basic :ada:`Serial_IO` abstraction:

.. code-block:: ada

    with STM32;         use STM32;
    with STM32.GPIO;    use STM32.GPIO;
    with STM32.USARTs;  use STM32.USARTs;
    with HAL;

    package Serial_IO is

       type Device (Transceiver : not null access USART) is tagged limited private;

       procedure Initialize
         (This           : in out Device;
          Transceiver_AF : GPIO_Alternate_Function;
          Tx_Pin         : GPIO_Point;
          Rx_Pin         : GPIO_Point;
          CTS_Pin        : GPIO_Point;
          RTS_Pin        : GPIO_Point);
       --  must be called before Configure

       procedure Configure
         (This      : in out Device;
          Baud_Rate : Baud_Rates;
          Parity    : Parities     := No_Parity;
          Data_Bits : Word_Lengths := Word_Length_8;
          End_Bits  : Stop_Bits    := Stopbits_1;
          Control   : Flow_Control := No_Flow_Control);

       procedure Set_CTS (This : in out Device; Value : Boolean) with Inline;
       procedure Set_RTS (This : in out Device; Value : Boolean) with Inline;

       procedure Put (This : in out Device;  Data : HAL.UInt8)     with Inline;
       procedure Get (This : in out Device;  Data : out HAL.UInt8) with Inline;

    private

       type Device (Transceiver : not null access USART) is tagged limited record
          Tx_Pin  : GPIO_Point;
          Rx_Pin  : GPIO_Point;
          CTS_Pin : GPIO_Point;
          RTS_Pin : GPIO_Point;
       end record;

    end Serial_IO;


And the package body:

.. code-block:: ada

    with STM32.Device; use STM32.Device;

    package body Serial_IO is

       ----------------
       -- Initialize --
       ----------------

       procedure Initialize
         (This           : in out Device;
          Transceiver_AF : GPIO_Alternate_Function;
          Tx_Pin         : GPIO_Point;
          Rx_Pin         : GPIO_Point;
          CTS_Pin        : GPIO_Point;
          RTS_Pin        : GPIO_Point)
       is
          IO_Pins : constant GPIO_Points := Rx_Pin & Tx_Pin;
       begin
          This.Tx_Pin := Tx_Pin;
          This.Rx_Pin := Rx_Pin;
          This.CTS_Pin := CTS_Pin;
          This.RTS_Pin := RTS_Pin;

          Enable_Clock (This.Transceiver.all);

          Enable_Clock (IO_Pins);

          Configure_IO
            (IO_Pins,
             Config => (Mode_AF,
                        AF             => Transceiver_AF,
                        AF_Speed       => Speed_50MHz,
                        AF_Output_Type => Push_Pull,
                        Resistors      => Pull_Up));

          Enable_Clock (RTS_Pin & CTS_Pin);

          Configure_IO (RTS_Pin, Config => (Mode_In, Resistors => Pull_Up));

          Configure_IO
            (CTS_Pin,
             Config => (Mode_Out,
                        Speed       => Speed_50MHz,
                        Output_Type => Push_Pull,
                        Resistors   => Pull_Up));
       end Initialize;

       ---------------
       -- Configure --
       ---------------

       procedure Configure
         (This      : in out Device;
          Baud_Rate : Baud_Rates;
          Parity    : Parities     := No_Parity;
          Data_Bits : Word_Lengths := Word_Length_8;
          End_Bits  : Stop_Bits    := Stopbits_1;
          Control   : Flow_Control := No_Flow_Control)
       is
       begin
          This.Transceiver.Disable;

          This.Transceiver.Set_Baud_Rate    (Baud_Rate);
          This.Transceiver.Set_Mode         (Tx_Rx_Mode);
          This.Transceiver.Set_Stop_Bits    (End_Bits);
          This.Transceiver.Set_Word_Length  (Data_Bits);
          This.Transceiver.Set_Parity       (Parity);
          This.Transceiver.Set_Flow_Control (Control);

          This.Transceiver.Enable;
       end Configure;

       -------------
       -- Set_CTS --
       -------------

       procedure Set_CTS (This : in out Device; Value : Boolean) is
       begin
          This.CTS_Pin.Drive (Value);
       end Set_CTS;

       -------------
       -- Set_RTS --
       -------------

       procedure Set_RTS (This : in out Device; Value : Boolean) is
       begin
          This.RTS_Pin.Drive (Value);
       end Set_RTS;

       ---------
       -- Put --
       ---------

       procedure Put (This : in out Device;  Data : HAL.UInt8) is
       begin
          This.Transceiver.Transmit (HAL.UInt9 (Data));
       end Put;

       ---------
       -- Get --
       ---------

       procedure Get (This : in out Device;  Data : out HAL.UInt8) is
          Received : HAL.UInt9;
       begin
          This.Transceiver.Receive (Received);
          Data := HAL.UInt8 (Received);
       end Get;

    end Serial_IO;


Next, the interrupt-driven extension.

.. code-block:: ada

    with Ada.Interrupts;      use Ada.Interrupts;
    with HAL;
    with System; use System;

    package Serial_IO.Interrupt_Driven is
       pragma Elaborate_Body;

       type Serial_Port
         (Transceiver  : not null access USART;
          IRQ          : Interrupt_ID;
          IRQ_Priority : Interrupt_Priority)
       is new Serial_IO.Device with private;
       --  A serial port that uses interrupts for I/O. Extends the Device
       --  abstraction that is itself a wrapper for the USARTs hardware.

       --  The procedures Initialize and Configure, among others, are implicitly
       --  declared here, as operations inherited from Serial_IO.Device

       overriding
       procedure Put (This : in out Serial_Port;  Data : HAL.UInt8) with Inline;
       --  Non-blocking, ie the caller can return before the Data goes out,
       --  but does block until the underlying UART is not doing any other
       --  transmitting. Does no polling. Will not interfere with any other I/O
       --  on the same device.

       overriding
       procedure Get (This : in out Serial_Port;  Data : out HAL.UInt8) with Inline;
       --  Blocks the caller until a character is available! Does no polling.
       --  Will not interfere with any other I/O on the same device.

    private

       --  The protected type defining the interrupt-based I/O for sending and
       --  receiving via the USART attached to the serial port designated by
       --  Port. Each serial port object of the type defined by this package has
       --  a component of this protected type.
       protected type IO_Manager
         (IRQ          : Interrupt_ID;
          IRQ_Priority : Interrupt_Priority;
          Port         : not null access Serial_Port)
       --  with
          --  Interrupt_Priority => IRQ_Priority   -- compiler bug :-(
       is
          pragma Interrupt_Priority (IRQ_Priority);

          entry Put (Datum : HAL.UInt8);

          entry Get (Datum : out HAL.UInt8);

       private

          Outgoing : HAL.UInt8;
          Incoming : HAL.UInt8;

          Incoming_Data_Available : Boolean := False;
          Transmission_Pending    : Boolean := False;

          procedure IRQ_Handler with Attach_Handler => IRQ;

        end IO_Manager;

       type Serial_Port
         (Transceiver  : not null access USART;
          IRQ          : Interrupt_ID;
          IRQ_Priority : Interrupt_Priority)
       is
          new Serial_IO.Device (Transceiver) with
       record
          Controller : IO_Manager (IRQ, IRQ_Priority, Serial_Port'Access);
          --  Note that Serial_Port'Access provides the Controller with a view
          --  to the current instance's components, including the discriminant
          --  components
       end record;

    end Serial_IO.Interrupt_Driven;


And the package body:

.. code-block:: ada

    with STM32.Device; use STM32.Device;

    package body Serial_IO.Interrupt_Driven is

       ---------
       -- Put --
       ---------

       overriding
       procedure Put (This : in out Serial_Port;  Data : HAL.UInt8) is
       begin
          This.Controller.Put (Data);
       end Put;

       ---------
       -- Get --
       ---------

       overriding
       procedure Get (This : in out Serial_Port;  Data : out HAL.UInt8) is
       begin
          This.Transceiver.Enable_Interrupts (Received_Data_Not_Empty);
          This.Controller.Get (Data);
       end Get;

       ----------------
       -- IO_Manager --
       ----------------

       protected body IO_Manager is

          -----------------
          -- IRQ_Handler --
          -----------------

          procedure IRQ_Handler is
          begin
             --  check for data arrival
             if Port.Transceiver.Status (Read_Data_Register_Not_Empty) and then
                Port.Transceiver.Interrupt_Enabled (Received_Data_Not_Empty)
             then  -- handle reception
                -- call the Serial_IO.Device version:
                Get (Serial_IO.Device (Port.all), Incoming);

                Await_Reception_Complete : loop
                   exit when not Port.Transceiver.Status (Read_Data_Register_Not_Empty);
                end loop Await_Reception_Complete;
                Port.Transceiver.Disable_Interrupts (Received_Data_Not_Empty);
                Port.Transceiver.Clear_Status (Read_Data_Register_Not_Empty);
                Incoming_Data_Available := True;
             end if;

             --  check for transmission ready
             if Port.Transceiver.Status (Transmission_Complete_Indicated) and then
                Port.Transceiver.Interrupt_Enabled (Transmission_Complete)
             then  -- handle transmission
                -- call the Serial_IO.Device version:
                Put (Serial_IO.Device (Port.all), Outgoing);

                Port.Transceiver.Disable_Interrupts (Transmission_Complete);
                Port.Transceiver.Clear_Status (Transmission_Complete_Indicated);
                Transmission_Pending := False;
             end if;
          end IRQ_Handler;
          ---------
          -- Put --
          ---------

          entry Put (Datum : HAL.UInt8) when not Transmission_Pending is
          begin
             Transmission_Pending := True;
             Outgoing := Datum;
             Port.Transceiver.Enable_Interrupts (Transmission_Complete);
          end Put;

          ---------
          -- Get --
          ---------

          entry Get (Datum : out HAL.UInt8) when Incoming_Data_Available is
          begin
             Datum := Incoming;
             Incoming_Data_Available := False;
          end Get;

       end IO_Manager;

    end Serial_IO.Interrupt_Driven;

