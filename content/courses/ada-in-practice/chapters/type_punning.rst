.. _Ada_In_Practice_Type_Punning:

Type Punning
============

.. include:: ../../../global.txt

Motivation
----------

When declaring an object, the type chosen by the developer is presumably one
that meets the operational requirements. Sometimes, however, the chosen type is
not sufficient for clients of that object. Normally that situation would
indicate a design error, but not necessarily.

Consider a device driver that receives external data, such as a network or
serial I/O driver. Typically the driver presents incoming data to clients as arrays of raw
bytes. That's how the data enter the receiving machine, so that's the most
natural representation at the lowest level of the device driver code. However,
that representation is not likely sufficient for the higher-level clients.
These clients are not necessarily at the application level, they may be lower
than that, but they are clients because they use the data presented.

For example, the next level of the network processing code must interpret the
byte arrays in order to implement the network protocol. Interpreting the data
requires reading headers that are logically contained within slices of those
bytes. These headers are naturally represented as record types containing
multiple fields. How, then, can the developer apply such record types? An array
of bytes contains bytes, not headers.

Stated generally, on occasion a developer needs to manipulate or access the
value of a given object in a manner not supported by the object's type. The
issue is that the compiler will enforce the type model defined by the language,
to some degree of rigor, potentially resulting in the rejection of the
alternative access and manipulation code. In such cases the developer must
circumvent this enforcement. That's the purpose of this idiom.

A common circumvention technique, across programming languages, is to apply
multiple distinct types to a single given object. Doing so makes available
additional operations or accesses not provided by the type used to declare the
object in the first place. The technique is known as
:wikipedia:`type punning <Type_punning>` in the programming community because
different types are used for the same object in much the same way that a pun in
natural languages uses different meanings for words that sound the same when
spoken.

Ada is accurately described as "a language with inherently reliable features,
only compromised via explicit escape hatches having well-defined and portable
semantics when used appropriately." :footcite:p:`1993:taft`
The foundation for this reliability is
static named typing with rigorous enforcement by the compiler.

Specifically, the Ada compiler checks that the operations and values applied to
an object are consistent with the one type specified when the object is
declared. Any usage not consistent with that type is rejected by the compiler.
Similarly, the Ada compiler also checks that any type conversions
involve only those types that the language defines as meaningfully
convertible.

By design, this strong typing model does not lend itself to circumvention
(thankfully). That's the point of Ada's *escape hatches* |mdash| they provide
standard ways to circumvent these and other checks. To maintain the integrity
of the type model not many escape hatches exist. The most commonly used of
these, *unchecked conversion*, allows type conversion between arbitrary types.
Unchecked conversions remain explicit, but the compiler does not limit them to
the types defined as reasonable by the language.


Implementation(s)
-----------------

There are two common approaches for expressing type punning in Ada. We show
both in the following subsections. The purpose in both approaches is to apply a
different type, thereby making available a different type-specific view of the
storage.

Overlays
~~~~~~~~

The first approach applies an alternative type to an existing object by
declaring another object at the same location in memory but with a different
type. Given an existing object, the developer:

#. declares (or reuses) an alternative type that provides the required
   operations and values not provided by the existing object's type, and

#. declares another object of this alternative type, and

#. as part of the new object's declaration, specifies that this new object
   shares some or all of the storage occupied by the existing object.

The result is a partial or complete storage overlay. Because there are now
multiple distinct types involved, there are multiple views of that shared
storage, each view providing different operations and values. Thus, the shared
storage can be legally manipulated in distinct ways. As usual, the Ada compiler
verifies that the usage corresponds to the type view presented by the object
name referenced.

For example, let's say that we have an existing object, and that a signed
integer is most appropriate for its type. On some occasions let's also say we
need to access individual bits within that existing object. Signed integer
types don't support bit-level operations, unlike unsigned integers, but we've
said that a signed type is the best fit for the bulk of the usage.

One of the ways to enable bit access, then, is to apply another type that does
have bit-level operations. We could overlay the existing object with an
unsigned integer type of the same size, but let's take a different approach for
the sake of illustration. Instead, we'll declare an array type with components
that can be represented as a single bit. The length of the array type will
reflect the number of bits used by the signed integer type so that the entire
object will be overlaid. (A record type would work too, with a component
allocated to each bit.)

The type :ada:`Boolean` will suffice for the array component type as long as
we force the single-bit representation. :ada:`Boolean` array components are
likely to be represented as individual bytes otherwise. Alternatively, we could
just make up an integer type with range :ada:`0 .. 1` but that seems
unnecessary, unless numeric values would make the code clearer.  (Maybe the
requirements specify ones and zeros for the bit values.) In any case we'll need
to force single bits for the components.

Assuming values of type :ada:`Integer` require exactly 32 bits, the following
code illustrates the approach:

.. code-block:: ada

   type Bits32 is array (0 .. 31) of Boolean with Component_Size => 1;

   X : aliased Integer;
   Y : Bits32 with Address => X'Address;

In the above, we use :ada:`X'Address` to query the starting address of object
:ada:`X`. We use that address to specify the location of the overlay object
:ada:`Y`. As a result, :ada:`X` and :ada:`Y` start at the same address.

We marked :ada:`X` as explicitly :ada:`aliased` because :ada:`Integer` is not
a by-reference type. The :ada:`Address` attribute is not required to provide a
useful result otherwise. (Maybe the compiler would have put :ada:`X` into a register, for
example.) The compiler, seeing :ada:`'Address` applied, would probably do the
right thing anyway, but this makes it certain.

Here is a simple main program that illustrates the approach.

.. code:: ada run_button project=Courses.Ada_In_Practice.Type_Punning.Solutions.Overlay

   with Ada.Text_IO;  use Ada.Text_IO;

   procedure Main is

      X : aliased Integer := 42;

      type Bits32 is array (0 .. 31) of Boolean
        with Component_Size => 1;

      Y : Bits32 with Address => X'Address;

   begin
      X := Integer'First;
      Put_Line (X'Image);
      for Bit in Bits32'Range loop
         Put (if Y (Bit) then '1' else '0');
      end loop;
      New_Line;
   end Main;

Object :ada:`Y` starts at the same address as :ada:`X` and has the same size,
so all references to :ada:`X` and :ada:`Y` refer to the same storage. The source
code can manipulate that memory as either a signed integer or as an array of
bits, including individual bit access, using the two object names. The compiler
will ensure that every reference via :ada:`X` is compatible with the integer
view, and every reference via :ada:`Y` is compatible with the array view.

In the above example, we've ignored the endianess issue. If you wanted to change the
sign bit, for example, or display the bits in the "correct" order, you'd need to
handle that detail.

This expression of type-punning does not use an escape hatch but does achieve
the effect. (We don't include address clauses as an escape hatch because address
clauses aren't dedicated to overlaying multiple objects of different types. On
the other hand, even one Ada object with an address specified overlays that object's view
with the machine storage view of that address...)


Unchecked Conversions on Address Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The common implementation of type punning, across multiple languages, involves
converting the address of a given object into a pointer designating the
alternative type to be applied. Dereferencing the resulting pointer provides a
different compile-time view of the object. Thus, the operations defined by the
alternative type are made applicable to the object.

Expressing this approach in Ada requires unchecked conversion because, in Ada,
address values are semantically distinct from pointer values (*access values*).
An access value might be represented by an address value, but because
architectures vary, that implementation in not guaranteed. Therefore, the
language does not define checked conversions between addresses and access
values. We need the escape hatch.

Unchecked conversion requires instantiation of the generic function
:ada:`Ada.Unchecked_Conversion`, including a context clause for that unit,
making it a relatively heavy mechanism. This heaviness is intentional, and the
with_clause at the top of the client unit makes it noticeable. Although
ubiquitous use strongly suggests abuse of the type model, in this case
unchecked conversion is necessary. Nevertheless, we'd hide its use within the
body of some visible unit.

Let's start with a simple example. There is an accelerometer that provides
three signed 16-bit acceleration values, one for each axis. Accelerations can
be both positive and negative so the signed type is appropriate. These values
are read from the device as two unsigned bytes. The two bytes are read
individually so two reads are required per acceleration value. (This is an
actual, real-world device.) Because the acceleration values are signed 16-bit
integers, we need to convert two unsigned bytes into a single signed 16-bit
quantity. We can use type punning, based on a pointer designating the 16-bit
signed type, to achieve that effect. There are certainly other ways to do this,
but we're starting with something simple for the sake of illustrating this
idiom.

In the following fragment, type :ada:`Acceleration` is a signed 16-bit
integer type already declared elsewhere:

.. code-block:: ada

      type Acceleration_Pointer is access all Acceleration
        with Storage_Size => 0;

      function As_Acceleration_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Acceleration_Pointer);

The access type is general, not pool-specific, but that is optional. We tell the
compiler to reserve no storage for the access type because an allocation would
be an error that we want the compiler to catch. Whether or not the compiler
actually reserves storage for an individual access type is
implementation-dependent, but this way we can be sure. In any case the
compiler will reject any allocations.

Given this access type declaration we can then instantiate
:ada:`Ada.Unchecked_Conversion`. The resulting function name is a matter of
style but is appropriate because the function allows us to treat an address as
a pointer to an :ada:`Acceleration` value. We aren't changing the address value,
we're only providing another view of that value, which is why the function name
is not :ada:`To_Acceleration_Pointer`.

The following is the device driver routine for getting the scaled accelerations
from the device. The type :ada:`Three_Axis_Accelerometer` is the device driver
:ref:`ADT <Ada_In_Practice_Abstract_Data_Types>`, and type :ada:`Axes_Accelerations`
is a record type containing the three axis values. The procedure gets the raw
acceleration values from the device and scales them per the current device
sensitivity, returning all three in the mode-out record parameter.

.. code-block:: ada

   procedure Get_Accelerations
     (This : Three_Axis_Accelerometer;
      Axes : out Axes_Accelerations)
   is

      Buffer : array (0 .. 5) of UInt8 with Alignment => 2, Size => 48;
      Scaled : Float;

      type Acceleration_Pointer is access all Acceleration
        with Storage_Size => 0;

      function As_Acceleration_Pointer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Acceleration_Pointer);

   begin
      This.Loc_IO_Read (Buffer (0), OUT_X_L);
      This.Loc_IO_Read (Buffer (1), OUT_X_H);
      This.Loc_IO_Read (Buffer (2), OUT_Y_L);
      This.Loc_IO_Read (Buffer (3), OUT_Y_H);
      This.Loc_IO_Read (Buffer (4), OUT_Z_L);
      This.Loc_IO_Read (Buffer (5), OUT_Z_H);

      Get_X : declare
         Raw : Acceleration renames
                 As_Acceleration_Pointer (Buffer (0)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.X := Acceleration (Scaled);
      end Get_X;

      Get_Y : declare
         Raw : Acceleration renames
                 As_Acceleration_Pointer (Buffer (2)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.Y := Acceleration (Scaled);
      end Get_Y;

      Get_Z : declare
         Raw : Acceleration renames
                 As_Acceleration_Pointer (Buffer (4)'Address).all;
      begin
         Scaled := Float (Raw) * This.Sensitivity;
         Axes.Z := Acceleration (Scaled);
      end Get_Z;
   end Get_Accelerations;

This procedure first reads the six bytes representing all three acceleration
values into the array :ada:`Buffer`. Procedure :ada:`Loc_IO_Read` is defined
by the driver :ref:`ADT <Ada_In_Practice_Abstract_Data_Types>`. The constants
:ada:`OUT_n_L` and :ada:`OUT_n_H`, also defined by the driver, specify the low-order and high-order bytes
requested for the given *n* axis. Then the declare blocks do the actual scaling
and that's where the type punning is applied to the :ada:`Buffer` content.

In each block, the address of one of the bytes in the array is converted into
an access value designating a two-byte :ada:`Acceleration` value.
The :ada:`X` acceleration is first in the buffer, so the address of
:ada:`Buffer (0)` is converted. Likewise, the address of :ada:`Buffer (2)` is
converted for the :ada:`Y` axis value, and for the :ada:`Z` value,
:ada:`Buffer (4)` is converted. (We could have said :ada:`Buffer'Address` instead of
:ada:`Buffer (0)'Address`, they mean the same thing, but an explicit index seemed more
clear, given the need for the other indexes.)

But we want the designated axis acceleration value, not the access value, so
we also dereference the converted access value via :ada:`.all`, and rename the
result for convenience. The name is :ada:`Raw` because the value needs to be
scaled. Each dereference reads two bytes, i.e., the bytes at indexes 0 and 1,
or 2 and 3, or 4 and 5.

That's the way the device driver is written currently, but it could be simpler.
Clients always get all three accelerations via this procedure, so we could have
used unchecked conversion to directly convert the entire array of six bytes
into a value of the record type :ada:`Axes_Accelerations` containing the three
16-bit components. Type punning would not be required in that case. (The
components would still need scaling, of course.)

Note that to get individual values we can't just convert a slice of the array
because that's illegal: array slices cannot be converted. We'd need some other
way to refer to a two-byte pair within the array. Type punning would be an
appropriate approach.

For that matter we could use type punning but have the record type be the
designated type returned from the address conversion, rather than a single axis
value. Then we'd just convert :ada:`Buffer'Address` and not need to specify
array indexes as all. This would be the same as converting the array to the record
type, but with a level of indirection added.

For the network packet example, we want to apply record type views to arbitrary
sequences within an array of raw bytes, so indexing will be required.
Just as we indexed into the accelerometer :ada:`Buffer` for the addresses of
the individual 16-bit acceleration values, we can index into the network packet
array to get the starting addresses of the individual headers. Regular record
component access syntax can then be used. Reading the record components reads
the corresponding raw bytes in the array.

For a specific example, we can read the IP header in a packet's array of bytes
using the header's record type and an access type designating that record type:

.. code-block:: ada

   Min_IP_Header_Length : constant := 20;

   --  IP packet header RFC 791.
   type IP_Header is record
      Version         : UInt4;
      Word_Count      : UInt4;
      Type_of_Service : UInt8;
      Total_Length    : UInt16;
      Identifier      : UInt16;
      Flags_Offset    : UInt16;
      Time_To_Live    : UInt8;
      Protocol        : Transport_Protocol;
      Checksum        : UInt16;
      Source          : IP_Address;
      Destination     : IP_Address;
   end record with
     Alignment => 2,
     Size      => Min_IP_Header_Length * 8;

   for IP_Header use record
      Version         at 0  range 4 .. 7;
      Word_Count      at 0  range 0 .. 3;
      Type_of_Service at 1  range 0 .. 7;
      Total_Length    at 2  range 0 .. 15;
      Identifier      at 4  range 0 .. 15;
      Flags_Offset    at 6  range 0 .. 15;
      Time_To_Live    at 8  range 0 .. 7;
      Protocol        at 9  range 0 .. 7;
      Checksum        at 10 range 0 .. 15;
      Source          at 12 range 0 .. 31;
      Destination     at 16 range 0 .. 31;
   end record;
   for IP_Header'Bit_Order use System.Low_Order_First;
   for IP_Header'Scalar_Storage_Order use System.Low_Order_First;

   type IP_Header_Access is access all IP_Header;
   pragma No_Strict_Aliasing (IP_Header_Access);

   --  and so on, for the other kinds of headers...

Note that :ada:`pragma No_Strict_Aliasing` stops the compiler from doing some
optimizations, based on the assumption of a lack of aliasing, that could cause
unexpected results in this approach.

.. code-block:: ada

   function As_IP_Header_Access is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => IP_Header_Access);

   function As_ARP_Header_Access is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => ARP_Packet_Access);

   --  and so on, for the other kinds of headers...

We can then implement a function, visible to clients, for acquiring the access
value from a given memory buffer's data:

.. code-block:: ada

   function IP_Hdr (This : Memory_Buffer) return IP_Header_Access is
     (As_IP_Header_Access (This.Packet.Data (IP_Pos)'Address));

In the function, :ada:`Data` is the packet's array of raw bytes, and
:ada:`IP_Pos` is a constant specifying the index into the array corresponding
to the first byte of the IP header. As you can see, this is the same approach
as shown earlier for working with an array of bytes containing acceleration
values.

Similar functions support :wikipedia:`ARP <Address_Resolution_Protocol>`
headers, :wikipedia:`TCP <Transmission_Control_Protocol>` headers, and so on.


Pros
----

Both approaches work and are fairly simple, although the first is simplest. The
second approach, based on converting addresses to access values, is more
flexible. That's because the address to be converted can be changed at
run-time, whereas the object overlay specifies the address exactly once during
elaboration.


Cons
----

We're assuming access values are represented as addresses. There's no guarantee
of that. But on typical architectures it will likely work.

That said, not all types can support the address conversion approach. In
particular, unconstrained array types may not work correctly because of the
existence of the additional in-memory representation of the bounds. An access
value designating such an object might point at the bounds of the array whereas
the address of the object would point to the first element.

In either approach, the developer is responsible for the correctness of the
address values applied, either for the second object's declaration or for the
pointer conversion. For example, this includes the alternative type's
alignment. Otherwise, all bets are off.


Relationship With Other Idioms
------------------------------

None.


Notes
-----

Generic package :ada:`System.Address_To_Access_Conversions` is an obvious
alternative to our use of unchecked conversions between addresses and access
values. The generic is convenient: it provides the access type as well as
functions for converting in both directions. But it will require an
instantiation for each designated type, so it offers no reduction in the number
of instantiations required over that of :ada:`Ada.Unchecked_Conversion`.
(For more details on this generic package, please refer to the section on
:ref:`access and address <Adv_Ada_Access_Address>`.)

Moreover, because that generic package is defined by the language, the naïve user might think it
will work for all types. It might not. Unconstrained array types remain a
potential problem. For that reason, the GNAT implementation issues a warning in
such cases.


.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
