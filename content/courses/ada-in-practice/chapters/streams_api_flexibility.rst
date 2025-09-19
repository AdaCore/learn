.. _Ada_In_Practice_Using_Streams_API_Flexibility:

Using Streams for API Flexibility
=================================

.. include:: ../../../global.txt

Motivation
----------

Software interfaces for hardware devices usually support only primitive data
types. Communications devices such as serial I/O ports and network adapters are
good examples. Their device drivers provide an API for sending and receiving
individual 8-bit or 9-bit numeric quantities, or sequences of these.
Software clients |mdash| either
the application, or higher-level layers of device interfaces |mdash| may want
to send and receive more complex data types. If so, how can the device driver
support them?

We certainly don't want the clients to do unchecked conversions everywhere.
That's error-prone, it prevents the compiler from checking the usage,
and makes clients responsible for what should be an internal implementation
detail.

This is a general issue, not specific to communications hardware, but we will
discuss it in that context because of the familiarity of such devices.

The traditional approach is for the device driver's I/O routines to have two
parameters for this purpose: an address and a length. The address indicates the
first byte of the client value to be sent or received, and the length indicates
how many bytes are involved. Values of any type can be sent or received using
this interface, but it's a very unsafe / unrobust approach. Developers could
pass the wrong starting address, or specify the wrong length, thus potentially
transmitting only part of the intended value or including part of some wholly
unrelated object. Moreover, developers could pass the address and length of
some object that is not of the type expected on the other end of the connection.
After all, Ada allows us to take the address of just about anything. The effect
on the receiver would be difficult to predict. These mistakes are very
expensive to locate, and the compiler cannot help.

We need a type-safe approach for sending and receiving higher-level types so
that the compiler can catch our mistakes. After all, preventing coding errors
is much cheaper than fixing them later.


Implementation
--------------

We will explore the possibilities using a concrete
:wikipedia:`USART (Universal Synchronous/Asynchronous Receiver Transmitter) <Universal_synchronous_and_asynchronous_receiver-transmitter>`
defined in the
`Ada Drivers Library (ADL) <https://github.com/AdaCore/Ada_Drivers_Library>`_.
A USART is the physical communications device underlying what is commonly
referred to as a *serial port*. That name reflects the fact that the device
transmits and receives data serially, as opposed to in parallel.

The ADL provides packages representing specific microcontrollers as well as
their on-chip peripherals, including USARTs, timers, DMA controllers, and so
forth. Each kind of peripheral is represented by a dedicated
:ref:`Abstract Data Type (ADT) <Ada_In_Practice_Abstract_Data_Types>`. The
following is the elided ADT declaration for USARTs on STM32 microcontrollers:

.. code-block:: ada

    --  ...

    package STM32.USARTs is

       type USART is ... private;

       --  ...
       procedure Receive  (This : USART;         Data : out UInt9);
       procedure Transmit (This : in out USART;  Data : UInt9);

       function Tx_Ready (This : USART) return Boolean;
       function Rx_Ready (This : USART) return Boolean;

       --  ...

    private
       --  ...
    end STM32.USARTs;

Note the formal parameter data type for both :ada:`Receive` and
:ada:`Transmit`, i.e., a single 9-bit unsigned numeric value. Although a client
might actually want to send and receive such values directly, that's probably
not the case.

Our implementation is structured as a package hierarchy rooted at package
:ada:`Serial_IO`. (This implementation is part of an example in the ADL |mdash| see
:ref:`Note #1 <Ada_In_Practice_Using Streams_Note_ADL_Links>` below.) This root
package declares a record type and routines that are common to any implementation.
The type, named :ada:`Peripheral_Descriptor`, contains a component named
:ada:`Transceiver` that will designate the actual on-chip USART device being
driven. The other record components are required for connecting that device to
the external world.

Type :ada:`Peripheral_Descriptor` is hardware-specific and is therefore not
defined as an ADT. Instead, the package uses the
:ref:`Groups of Related Program Units <Ada_In_Practice_Groups_Of_Related_Program_Units>`
idiom.

.. code-block:: ada

    with STM32;         use STM32;
    with STM32.GPIO;    use STM32.GPIO;
    with STM32.USARTs;  use STM32.USARTs;

    package Serial_IO is

       type Peripheral_Descriptor is record
          Transceiver    : not null access USART;
          Transceiver_AF : GPIO_Alternate_Function;
          Tx_Pin         : GPIO_Point;
          Rx_Pin         : GPIO_Point;
       end record;

       procedure Initialize_Hardware (Device : Peripheral_Descriptor);
       --  enable clocks, configure GPIO pins, etc.

       procedure Configure
         (Device    : access USART;
          Baud_Rate : Baud_Rates;
          Parity    : Parities     := No_Parity;
          Data_Bits : Word_Lengths := Word_Length_8;
          End_Bits  : Stop_Bits    := Stopbits_1;
          Control   : Flow_Control := No_Flow_Control);

       --  ...

    end Serial_IO;

Procedure :ada:`Configure` is a convenience routine, provided because a
specific sequence of driver calls is required in order to set the individual
parameters.

Clients will call these procedures directly to set up the STM32 on-chip USART
device.

Our implementation will consist of an ADT named :ada:`Serial_Port`, and a means for
sending and receiving values of higher-level types via :ada:`Serial_Port`
objects. Type :ada:`Serial_Port` will be a wrapper for the device driver's
USART type. Therefore, the type :ada:`Serial_Port` will have an access
discriminant designating a USART:

.. code-block:: ada

       type Serial_Port (Device : not null access USART) is ...

Using this :ada:`Device` discriminant a :ada:`Serial_Port` object can reference
the wrapped physical USART in order to send and receive values via that
hardware device.

With that introduction in place, we can consider the possible approaches to
sending and receiving values of higher-level types via :ada:`Serial_Port`
objects.

The canonical Ada approach consists of a generic package with a generic formal
parameter type. That formal type represents a client-specific type to be sent
or received. Clients instantiate the generic for every client-defined type
necessary.

The generic package would look like the following:

.. code-block:: ada

       generic
          type Client_Data (<>) is limited private;
       package Client_IO is

          procedure Send
            (This     : in out Serial_Port;
             Outgoing : Client_Data);

          procedure Receive
            (This     : in out Serial_Port;
             Incoming : out Client_Data);

       end Client_IO;

In the procedure bodies, the values of the :ada:`Incoming` or :ada:`Outgoing`
parameters would be converted to or from bytes as necessary and sent or
received via the USART designated by :ada:`This.Device`.

This approach supports as many client-level types as required, including
limited types. It is type-safe so the compiler can catch errors in the type(s)
being sent and received. In addition, the low-level implementation details,
such as unchecked conversions, are hidden inside the generic package body.

Moreover, the approach is independent of other design considerations, such as
whether callers wait for completion of the invoked I/O operation. The bodies of
the generic procedures can be implemented to provide the expected behavior.

However, this approach is somewhat heavy because the generic package must be
instantiated for every client type to be supported. If there are many such
types, there will be many instantiations. Not only is that more lines of source
code, but also more object code because most Ada implementations do not support
code-sharing for generic instantiations. But that said, the need for a given
client to send and receive values of many different types is not typical.

However, there is a more concise approach possible, for both the driver and
client source code. This alternative approach leverages the flexibility of
streams and stream attributes.
Using streams allows the wrapper to support an unlimited number of distinct
client types, with no additional source code required per type.

Recall that the stream attributes are callable routines whose first parameter
is an access value designating some stream object. The formal parameter type is
access-to-class-wide, so any stream object is allowed. For example, the
notional specification for the :ada:`'Output` attribute looks like this, for
some subtype :ada:`S` of type :ada:`T`:

.. code-block:: ada

    procedure S'Output
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in T);

Therefore, the fundamental approach will be to declare the :ada:`Serial_Port`
ADT as a stream type. Clients can then send and receive values simply by
invoking the stream attributes, passing (access to) :ada:`Serial_Port` objects
as the first parameter to the invocations.

The :ada:`Serial_Port` ADT will be defined in package
:ada:`Serial_IO.Streaming`. Given that, a client can declare a
:ada:`Serial_Port` object like so:

.. code-block:: ada

    with STM32.Device;
    with Serial_IO.Streaming; use Serial_IO;

    package Peripherals_Streaming is

       --  the USART selection is arbitrary but the AF number and the
       --  pins must be those required by that USART
       Peripheral : constant Serial_IO.Peripheral_Descriptor :=
                      (Transceiver    => STM32.Device.USART_1'Access,
                       Transceiver_AF => STM32.Device.GPIO_AF_USART1_7,
                       Tx_Pin         => STM32.Device.PB6,
                       Rx_Pin         => STM32.Device.PB7);

       COM : aliased Streaming.Serial_Port (Peripheral.Transceiver);

    end Peripherals_Streaming;

In the above, :ada:`Peripheral` is an object that describes a specific USART on
the SMT32 Discovery Board microcontroller, along with the values necessary to
connect that specific USART to the external environment. The :ada:`Peripheral`
variable will be passed to a call to :ada:`Initialize_Hardware`. Similarly, COM
is an object that wraps the USART designated by :ada:`Peripheral`.

Because :ada:`Serial_Port` will be a stream type (it will be in the derivation
class rooted at :ada:`Root_Stream_Type`), COM will be a streaming object that
we can pass to invocations of the stream attributes. For example, to send a
:ada:`String` value via COM we could write:

.. code-block:: ada

    String'Output (COM'Access, "Hello World");

To send an :ada:`Integer` value:

.. code-block:: ada

    Integer'Write (COM'Access, 42);

To receive an :ada:`Integer` value into the :ada:`Integer` object :ada:`X`:

.. code-block:: ada

    Integer'Read (COM'Access, X);

That's all clients must do to send and receive values via the USART wrapped by
COM. They could do the same for floating-point types, record types, and so on.
Objects of any type with the streaming attributes defined can be sent or
received, and in any order.

To make :ada:`Serial_Port` a stream type, the declaration visibly extends
:ada:`Ada.Streams.Root_Stream_Type`:

.. code-block:: ada

       type Serial_Port (Device : not null access USART) is
         new Ada.Streams.Root_Stream_Type with private;

This is :ref:`Interface Inheritance <Ada_In_Practice_Inheritance_Idioms>` so
that clients can treat :ada:`Serial_Port` as a stream type. The private
extension hides implementation details that we'll describe momentarily.

As a concrete extension of :ada:`Root_Stream_Type` we must declare overridings
for procedures :ada:`Read` and :ada:`Write`:

.. code-block:: ada

       overriding
       procedure Read
         (This   : in out Serial_Port;
          Buffer : out Ada.Streams.Stream_Element_Array;
          Last   : out Ada.Streams.Stream_Element_Offset);

       overriding
       procedure Write
         (This   : in out Serial_Port;
          Buffer : Ada.Streams.Stream_Element_Array);

:ada:`Stream_Element_Array` is an unconstrained array type with
:ada:`Stream_Element` as the array component. :ada:`Stream_Element` is an
unsigned numeric type corresponding to a machine storage element, e.g., a byte.

Procedure :ada:`Write` inserts these array components into the designated
stream. Procedure :ada:`Read` consumes the array components from the stream and
includes a parameter indicating the index of the last component assigned.

These two procedures are called by the various stream attributes'
implementations, not by clients. For example, consider again the call to
:ada:`'Output`:

.. code-block:: ada

    String'Output (COM'Access, "Hello World");

The call dispatches to our overriding of procedure :ada:`Write` because the
first parameter designates our specific stream object.

For both procedures the array components hold the serialized representation of
the value read from, or to be written to, the stream. For procedure
:ada:`Write`, the array contains the stream-oriented representation of the
client value, e.g., the "Hello World!" passed to :ada:`String'Output`. For
procedure :ada:`Read`, the array contains the value consumed from the stream
that will be converted into the client type, e.g., type :ada:`Integer` for a
call to :ada:`Integer'Read`, and loaded into the client variable.

Note that the two procedures are only responsible for reading or writing the
array components from/to the specified stream. Conversions between the types in
the clients' attribute invocations and type :ada:`Stream_Element_Array` are not
their responsibility. That metamorphosis is handled automatically by the
language-defined attributes' implementations. It's a nice separation of
concerns.

Here then is the full package declaration for the :ada:`Serial_Port` ADT:

.. code-block:: ada

    with Ada.Streams;
    with Ada.Real_Time; use Ada.Real_Time;

    package Serial_IO.Streaming is
       pragma Elaborate_Body;

       type Serial_Port (Device : not null access USART) is
         new Ada.Streams.Root_Stream_Type with private;

       procedure Set_Read_Timeout
         (This : in out Serial_Port;
          Wait : Time_Span);
       --  Stream attributes that call Read (below) can either wait
       --  indefinitely or can be set to return any current values
       --  received after a given interval. If the value Time_Span_Last
       --  is passed to Wait, the effect is essentially to wait forever,
       --  i.e., blocking. That is also the effect if this routine is
       --  never called.

       overriding
       procedure Read
         (This   : in out Serial_Port;
          Buffer : out Ada.Streams.Stream_Element_Array;
          Last   : out Ada.Streams.Stream_Element_Offset);

       overriding
       procedure Write
         (This   : in out Serial_Port;
          Buffer : Ada.Streams.Stream_Element_Array);

    private

       type Serial_Port (Device : access USART) is
         new Ada.Streams.Root_Stream_Type with record
           Timeout : Time_Span := Time_Span_Last;
         end record;

       procedure Await_Send_Ready (This : access USART) with Inline;

       procedure Await_Data_Available
         (This      : access USART;
          Timeout   : Time_Span := Time_Span_Last;
          Timed_Out : out Boolean)
       with Inline;

       use Ada.Streams;

       function Last_Index
         (First : Stream_Element_Offset;
          Count : Long_Integer)
          return Stream_Element_Offset
       with Inline;

    end Serial_IO.Streaming;

Prior to procedures :ada:`Read` and :ada:`Write`, the package declares a
procedure for controlling a timeout associated with a :ada:`Serial_Port`
stream. This timeout controls how long procedure :ada:`Read` should wait for
input to be available in the stream. The default is to wait for what amounts to
forever. Note that the timeout applies both to the case of some input received,
and none received.

In the package private part we see that the type extension contains the record
component named :ada:`Timeout`, with the initial value providing the default.
That's the only other record component required, besides the discriminant.

Additional implementation-oriented routines are also declared there, rather
than in the package body, for the sake of any child packages that might be
declared in the future. Note in particular the two that await I/O completion,
as this is a blocking implementation.

Here is the package body:

.. code-block:: ada

    with HAL;

    package body Serial_IO.Streaming is

       ----------------------
       -- Set_Read_Timeout --
       ----------------------

       procedure Set_Read_Timeout
         (This : in out Serial_Port;
          Wait : Time_Span)
       is
       begin
          This.Timeout := Wait;
       end Set_Read_Timeout;

       ----------------------
       -- Await_Send_Ready --
       ----------------------

       procedure Await_Send_Ready (This : access USART) is
       begin
          loop
             exit when This.Tx_Ready;
          end loop;
       end Await_Send_Ready;

       --------------------------
       -- Await_Data_Available --
       --------------------------

       procedure Await_Data_Available
         (This      : access USART;
          Timeout   : Time_Span := Time_Span_Last;
          Timed_Out : out Boolean)
       is
          Deadline : constant Time := Clock + Timeout;
       begin
          Timed_Out := True;
          while Clock < Deadline loop
             if This.Rx_Ready then
                Timed_Out := False;
                exit;
             end if;
          end loop;
       end Await_Data_Available;

       ----------------
       -- Last_Index --
       ----------------

       function Last_Index
         (First : Stream_Element_Offset;
          Count : Long_Integer)
          return Stream_Element_Offset
       is
       begin
          if First = Stream_Element_Offset'First and then Count = 0 then
             --  although we intend to return First - 1, we cannot
             raise Constraint_Error;  -- per AI95-227
          else
             return First + Stream_Element_Offset (Count) - 1;
          end if;
       end Last_Index;

       ----------
       -- Read --
       ----------

       overriding
       procedure Read
         (This   : in out Serial_Port;
          Buffer : out Ada.Streams.Stream_Element_Array;
          Last   : out Ada.Streams.Stream_Element_Offset)
       is
          Raw       : HAL.UInt9;
          Timed_Out : Boolean;
          Count     : Long_Integer := 0;
       begin
          Receiving : for K in Buffer'Range loop
             Await_Data_Available (This.Device, This.Timeout, Timed_Out);
             exit Receiving when Timed_Out;
             This.Device.Receive (Raw);
             Buffer (K) := Stream_Element (Raw);
             Count := Count + 1;
          end loop Receiving;
          Last := Last_Index (Buffer'First, Count);
       end Read;

       -----------
       -- Write --
       -----------

       overriding
       procedure Write
         (This   : in out Serial_Port;
          Buffer : Ada.Streams.Stream_Element_Array)
       is
       begin
          for Next of Buffer loop
             Await_Send_Ready (This.Device);
             This.Device.Transmit (HAL.UInt9 (Next));
          end loop;
       end Write;

    end Serial_IO.Streaming;

Procedure :ada:`Read` polls the wrapped USART device, continually, until a byte
becomes available or the timeout is reached. Procedure
:ada:`Await_Data_Available` performs this timed polling. Polling without
relinquishing the processor is extremely questionable on a main CPU, but in a
device driver on a dedicated microcontroller it is not necessarily a poor
choice. But if polling is a problem, there is nothing preventing a
non-blocking, interrupt-based implementation with a stream-based client API.

The function :ada:`Last_Index` is a convenience function called by procedure
:ada:`Read`. It is used to compute :ada:`Read.Last`, the index of the last
array component assigned in :ada:`Read.Buffer`. The function result is
:ada:`Buffer'First - 1` when no components are assigned, except when that
would be less than the lowest possible array index value.

Here is a demonstration procedure to be run on the STM32 F4 Discovery Board:

.. code-block:: ada

    with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
    with Serial_IO;
    with Peripherals_Streaming; use Peripherals_Streaming;

    procedure Demo_Serial_Port_Streaming is
    begin
       Serial_IO.Initialize_Hardware (Peripheral);
       Serial_IO.Configure (COM.Device, Baud_Rate => 115_200);
       --  This baud rate selection is entirely arbitrary. Note that you may
       --  have to alter the settings of your host serial port to match this
       --  baud rate, or just change the above to match whatever the host
       --  serial port has set already. An application such as TerraTerm
       --  or RealTerm is helpful.

       loop
          declare
             --  await the next msg from the serial port
             Incoming : constant String := String'Input (COM'Access);
          begin
             --  echo the received msg content
             String'Output (COM'Access, "You sent '" & Incoming & "'");
          end;
       end loop;
    end Demo_Serial_Port_Streaming;

The specific USART on the STM32 F4 Discovery Board must be connected to a
serial port on the host computer. With that connection in place the embedded
ARM board and the host computer can communicate over the two serial ports. This
demonstration iteratively receives a string sent from the host, prepends some
text, and sends that back, in effect echoing the host sender's text.

The stream attributes :ada:`String'Output` and :ada:`String'Input` write and
read the bounds as well as the characters. As a consequence, you will need to
use a program on the host that handles those bounds. A good way to do that is
to use a host program that also uses streams to send and receive :ada:`String`
values. Note that the ADL serial port examples include a host application that
you can build and run for this purpose.


Pros
----

The stream-based approach has all the advantages of the generic-based approach
without requiring generic instantiations. There is no limit to the number and
kinds of client types supported, including limited types if they have the
attributes defined. It is type-safe by default, because the compiler will
verify that the type used to invoke a stream attribute is the same type as the
value involved. In addition, the low-level implementation details are hidden
inside the package body.

Furthermore, the approach is independent of other design considerations, such
as whether callers wait for completion of the invoked I/O operation.

Because it is maximally flexible and concise, we consider it the best implementation
for this idiom. The generic-based approach remains a good one, however.


.. _Ada_In_Practice_Using Streams_Cons:

Cons
----

Limited types do not support the stream I/O attributes by default, but
developers can define them. Note that this is not a problem for the
generic-based approach, because we declared the generic formal type as
:ada:`limited` and wouldn't need to do anything within the generic that would
contradict that. The client's generic actual type can then be either a limited
type or not.

When multiple types are being sent and received, the sender and receiver must
be coordinated so that the next value consumed from the stream is of the type
expected by the receiver. For example, the next value in the stream might have
been written by the sender as a floating-point value, via
:ada:`Float'Write (...)`. The receiver must use :ada:`Float'Read(...)` to
consume that value from the stream. Arguably, this is not really a *con*
because it's true for any stream when multiple types are involved. Even if we
used the generic-based approach, developers could instantiate the generic multiple
times with different types and send their values via the same port. With streams this
approach is as type-safe as it can be. 
However, see
:ref:`Note #2 <Ada_In_Practice_Using Streams_Note_Generic_Dispatching_Constructor>`
below for a possible mitigation.  


Relationship With Other Idioms
------------------------------

As stated above, we use
:ref:`Interface Inheritance <Ada_In_Practice_Inheritance_Idioms>` to visibly
extend the root stream type.


Notes
-----

.. _Ada_In_Practice_Using Streams_Note_ADL_Links:

    1. You can find the streams-based approach, and others, in the
       *serial_ports* example in the ADL, for an STM32 F4 Discovery Board,
       found in
       `Ada_Drivers_Library/examples/STM32F4_DISCO/* <https://github.com/AdaCore/Ada_Drivers_Library/tree/master/examples/STM32F4_DISCO>`_.
       You can build and run them using the GNAT project file named
       `serial_ports_f4disco.gpr <https://github.com/AdaCore/Ada_Drivers_Library/blob/master/examples/STM32F4_DISCO/serial_ports_f4disco.gpr>`_
       located in that directory.  See the
       `Ada_Drivers_Library/examples/shared/serial_ports/README.md <https://github.com/AdaCore/Ada_Drivers_Library/tree/master/examples/shared>`_
       file for how to run them, including the special cable required for
       connecting the target board to the host computer. Note that a
       non-blocking, interrupt-drive approach is included there, although it is
       not stream-based.

.. _Ada_In_Practice_Using Streams_Note_Generic_Dispatching_Constructor:

    2. In the :ref:`Cons section <Ada_In_Practice_Using Streams_Cons>` above,
       we mentioned the coordination issue that arises when values of multiple
       types are inserted and retrieved from a given stream. A possible
       alternative would be to send and receive only tagged types in a given
       class hierarchy. The receiver could then use the language-defined
       :ada:`Generic_Dispatching_Constructor` to dynamically dispatch to
       constructors for the values received from the stream. Thus, the receiver
       would not need to know in advance what specific types of values are
       incoming.
