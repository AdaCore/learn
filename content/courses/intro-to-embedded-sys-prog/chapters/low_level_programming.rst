Low Level Programming
=====================

.. include:: ../../global.txt

Separation Principle
--------------------

There is a language design principle underlying the Ada facilities
intended for implementing embedded software. This design principle
directly affects how the language is used, and therefore, the
portability and readability of the resulting application code.

This language design principle is known as the "separation principle."
What's being separated? The low-level, less portable aspects of some
piece of code are separated from the usage of that piece of code.

Don't confuse this with hiding unnecessary implementation details via
compile-time visibility control (i.e., information hiding and
encapsulation). That certainly should be done too. Instead, because of
the separation principle, we specify the low-level properties of
something once, when we declare it. From then on, we can use regular Ada
code to interact with it. That way the bulk of the code |mdash| the
usage |mdash| is like any other Ada code, and doesn't propagate the
low-level details all over the client code. This greatly simplifies
usage and understandability as well as easing porting to new
hardware-specific aspects. You change things in one place, rather than
everywhere.

For example, consider a device mapped to the memory address space of the
processor. To interact with the device we interact with one or more
memory cells. Reading input from the device amounts to reading the value
at the associated memory location. Likewise, sending output to the
device amounts to writing to that location.

To represent this device mapping we declare a variable of an appropriate
type and specify the starting address the object should occupy. (There
are other ways too, but for a single, statically mapped object this is
the simplest approach.) We'd want to specify some other characteristics
as well, but let's focus on the address.

.. image:: images/memory-mapped-device.png
  :width: 600
  :alt: Memory-mapped device with Ada object declared at specific address

If the hardware presents an interface consisting of multiple fields
within individual memory cells, we can use a record type instead of a
single unsigned type representing a single word. Ada allows us to
specify the exact record layout, down to the individual bit level, for
any types we may need to use for the record components. When we
declare the object we use that record type, again specifying the
starting address. Then we can just refer to the object's record
components as usual, having the compiler compute the address offsets
required to access the components representing the individual hardware
fields.

Note that we aren't saying that other languages cannot do this too. Many
can, using good programming practices. What we're saying is that those
practices are designed into the Ada way of doing it.


Guaranteed Level of Support
---------------------------

The Ada reference manual has an entire section dedicated to low-level 
programming. That's section 13, "Representation Issues," which provides 
facilities for developers to query and control aspects of various 
entities in their code, and for interfacing to hardware. Want to specify 
the exact layout for a record type's components? Easy, and the compiler 
will check your layout too. Want to specify the alignment of a type? 
That's easy too. And that's just the beginning. We'll talk about these 
facilities as we go, but there's another point to make about this 
section. 

In particular, section 13 includes recommended levels of support to be
provided by language implementations, i.e., compilers and other
associated tools. Although the word "recommended" is used, the
recommendations are meant to be followed.

For example, section 13.3 says that, for some entity named X, "X'Address
should produce a useful result if X is an object that is aliased or of a
by-reference type, or is an entity whose :ada:`Address` has been specified."
So, for example, if the programmer specifies the address for a
memory-mapped variable, the compiler cannot ignore that specification
and instead, for the sake of performance, represent that variable using a
register. The object must be represented as an addressable entity, as
requested by the programmer. (Registers are not addressable.)

We mention this because, although the recommended levels of support are
intended to be followed, those recommendations become **requirements**
if the Systems Programming (SP) Annex is implemented by the vendor. In
that case the vendor's implementation of section 13 must support at
least the recommended levels. The SP Annex defines additional, optional
functionality oriented toward this programming domain; you want it
anyway. (Like all the annexes it adds no new syntax.) Almost all
vendors, if not literally all, implement the Annex so you can rely on
the recommended levels of support.


Querying Implementation Limits
------------------------------

Sometimes you need to know more about the underlying machine than is
typical for general purpose applications. For example, your numerical
analysis algorithm might need to know the maximum number of digits of
precision that a floating-point number can have on this specific
machine. For networking code, you will need to know the "endianness" of
the machine so you can know whether to swap the bytes in an Ethernet
packet. You'd go look in the :file:`limits.h` file in C implementations,
but in Ada we go to a package named `System` to get this information.

Clearly, these implementation values will vary with the hardware, so the
package declares constants with implementation-defined values. The names
of the constants are what's portable, you can count on them being the
same in any Ada implementation.

However, vendors can add implementation-defined declarations to the
language-defined content in package System. You might require some of
those additions, but portability could then suffer when moving to a new
vendor's compiler. Try not to use them unless it is unavoidable. Ideally
these additions will appear in the private part of the package, so the
implementation can use them but application code cannot.

For examples of the useful, language-defined constants, here are those
for the numeric limits of an Ada compiler for an Arm 32-bit SoC:

.. code-block:: ada

   Min_Int               : constant := Long_Long_Integer'First;
   Max_Int               : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus    : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := 63;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

:ada:`Min_Int` and :ada:`Max_Int` supply the most-negative and most-positive
integer values supported by the machine.

:ada:`Max_Binary_Modulus` is the largest power allowed as the modulus of a
modular type definition.

But a modular type need not be defined in terms of powers of two. An
arbitrary modulus is allowed, as long as it is not bigger than the
machine can handle. That's specified by :ada:`Max_Nonbinary_Modulus`, the
largest (positive) value allowed as the modulus of a modular type
definition.

:ada:`Max_Base_Digits` is the largest value allowed for the requested decimal
precision in a floating-point type's definition.

We won't go over all of the above, you get the idea. Let's examine the
more important contents.

Two of the most frequently referenced constants in System are the
following, especially the first. (The values here are again for the Arm
32-bit SoC):

.. code-block:: ada

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;

:ada:`Storage_Unit` is the number of bits per memory storage element. Storage
elements are the components of memory cells, and typically correspond to
the individually addressable memory elements. A "byte" would correspond
to a storage element with the above constant value.

Consider a typical idiom for determining the number of whole storage
elements an object named :ada:`X` occupies:

.. code-block:: ada

  Units : constant Integer := (X'Size + Storage_Unit - 1) / Storage_Unit;

Remember that :ada:`'Size` returns a value in terms of bits. There are more
direct ways to determine that size information but this will serve as an
example of the sort of thing you might do with that constant.

:ada:`Word_Size` is the number of bits in the units that the machine natively
manipulates. On a 32-bit machine we'd expect :ada:`Word` to have a value of 32;
on a 64-bit machine it would probably be 64, and so on.

:ada:`Storage_Unit` and :ada:`Word_Size` are obviously related. For this Arm target a
word consists of four eight-bit storage elements.

Another frequently referenced declaration in package :ada:`System` is that of
the type representing memory addresses, along with a constant for the
null address designating no storage element.

.. code-block:: ada

   type Address is private;
   Null_Address : constant Address;

You may be wondering why type :ada:`Address` is a private type, since that
choice means that we programmers cannot treat it like an ordinary
(unsigned) integer value. Portability is of course the issue, because
addressing, and thus address representation, varies among computer
architectures. Not all architectures have a flat address space directly
referenced by numeric values, although that is common. Some are
represented by a base address plus an offset, for example. Therefore,
the representation for type :ada:`Address` is hidden from us, the clients.
Consequently we cannot simply treat address values as numeric values.
Don't worry, though. The operations we need are provided.

Package `System` declares these comparison functions, for example:

.. code-block:: ada

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

These functions are intrinsic, i.e., built-in, meaning that the compiler
generates the code for them directly at the point of calls. There is no
actual function body for any of them so there is no performance penalty.

Any private type directly supports the equality function, and
consequently the inequality function, as well as assignment. What we
don't get here is address arithmetic, again because we don't have a
compile-time view of the actual representation. That functionality is
provided by package :ada:`System.Storage_Elements`, a child package we will
cover later. We should say though, that the need for address arithmetic
in Ada is rare, especially compared to C.

Having type :ada:`Address` presented as a private type is not, strictly
speaking, required by the language. Doing so is a good idea for the
reasons given above, and is common among vendors. Not all vendors do,
though.

Note that :ada:`Address` is the type of the result of the query attribute
:ada:`Address`.

We mentioned potentially needing to swap bytes in networking
communications software, due to the differences in the "endianness" of
the machines communicating. That characteristic is specified in :ada:`System`
as follows:

.. code-block:: ada

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := implementation-defined;

:ada:`High_Order_First` corresponds to "big-endian" and 
:ada:`Low_Order_First` to little-endian. Here is a real-word example of 
use, in which we retrieve arbitrarily-typed values from a given buffer 
starting at a given index: 

.. code-block:: ada

   generic
      type Retrieved is private;
   procedure Retrieve_4_Bytes
     (Value  : out Retrieved;
      Buffer : Byte_Array;
      Start  : Index)
   with
     Pre => Start in Buffer'Range and then
            Start + 3 in Buffer'Range;

   procedure Retrieve_4_Bytes
     (Value  : out Retrieved;
      Buffer : Byte_Array;
      Start  : Index)
   is
      ...
   begin
      Value := ...;
      if Default_Bit_Order /= High_Order_First then -- we're not on a Big Endian machine
         Value := Byte_Swapped (Value);
      end if;
   end Retrieve_4_Bytes;

:ada:`Value` is the output parameter of type :ada:`Retrieved`, which is a generic
formal type. We can instantiate this generic for lots of different kinds
of types, as long as their values occupy four bytes. (We have removed the
code that checks the size, for the sake of simplicity.) Don't worry
about some of the details for now, we'll get there eventually. The point
here is the if-statement: the expression compares the :ada:`Default_Bit_Order`
constant to :ada:`High_Order_First` to see if this execution is on a
big-endian machine. If not, it swaps the bytes because the incoming
bytes are always received in "wire-order," i.e., big-endian order.

(In reality we would change the left-hand side of the call to "/=" in
the if-statement expression to check more than :ada:`Default_Bit_Order`,
because the code could have used a pragma to change the default scalar
storage order. That's more detail than worth going into here.)

Another important set of declarations in package :ada:`System` define the 
values for priorities, including interrupt priorities. We will ignore 
them until we get to the section on interrupt handling. 

Finally, and perhaps surprisingly, a few declarations in package
:ada:`System` are almost always (if not actually always) ignored. 

.. code-block:: ada

   type Name is implementation-defined-enumeration-type;
   System_Name : constant Name := implementation-defined;

Values of type :ada:`Name` are the names of alternative machine 
configurations supported by the implementation. :ada:`System_Name` represents 
the current machine configuration. We've never seen any actual use of 
this. 

:ada:`Memory_Size` is an implementation-defined value that is intended to
reflect the memory size of the configuration, in units of storage
elements. What the value actually refers to is not specified. Is it the
size of the address space, the amount of physical memory on the machine,
or what? In any case, the amount of memory available to a given computer
is neither dependent upon, nor reflected by, this constant. Consequently,
:ada:`Memory_Size` is not useful either.

Why have something defined in the language that nobody uses? In short, 
it seemed like a good idea at the time when Ada was first defined. 
Upward-compatibility concerns propagate these declarations forward as 
the language evolves, just in case somebody does use them. 


Querying Representation Choices
-------------------------------

Earlier we mentioned the ability to specify a record type's layout, or a
variable's address in memory. These are just some of the possibilities
supported by the language in section 13. For any representation
characteristic you can specify, you can also query that characteristic.
That can be very handy, especially if you did not specify it and want to
know what the compiler chose for the representation.

For example, say you want to express an "overlay," in which an object of
one type is placed at the same memory location as a distinct object of a
distinct type, thus overlaying one object over the other. Doing so
allows you to interact with those memory locations in more than one way:
one way per type, specifically. This is known as `type punning <https://en.wikipedia.org/wiki/Type_punning>`_ in
computer programming. There are other was to achieve that effect as
well, but realize that doing so circumvents the static strong typing
used by Ada to protect us from ourselves and from others. Use it with
care!

For a concrete example, imagine a sensor providing a stream of bytes as
input, and you know that groups of those bytes comprise a composite
value, i.e., a value of some record type. The buffered array of inputs
bytes would be declared in terms of a numeric, probably unsigned integer
:ada:`Byte` type, whereas the composite type would, of course, be the record
type. The clients of the sensor driver call a driver-defined procedure
to get the latest value, passing an output argument of the record type. The
procedure gets the next buffered bytes, converts them to the record type
and puts the value in the parameter, maybe checks it for validity and so
on, and then the procedure returns. An overlay would make that simple.
We would declare an object of the record type located at the same
address in the buffer as the starting byte representing the next
composite value.

But there's a problem: inside the procedure we want to overlay our 
record object onto the address of one of the bytes in the buffer. But we 
didn't declare the :ada:`Buffer` ourselves, at a known location. It is passed 
to the procedure as an argument. Therefore, when writing the procedure 
we don't already know the address of the buffer, nor the addresses of 
the bytes within. As a result, we don't know the address to use in our 
overlay. 

Of course, there is a solution. We can query the address of objects, and
other things too, but objects, especially variables, are the most common
case. In particular, we can say :ada:`X'Address` to query the (starting)
address of object :ada:`X`, including the case where :ada:`X` is a 
component of an array. With that information we know what address to
specify for our record object overlay.

Here's the generic procedure again, now showing the overlay:

.. code-block:: ada

   procedure Retrieve_4_Bytes
     (Value  : out Retrieved;
      Buffer : Byte_Array;
      Start  : Index)
   is
      Buffer_Overlay : Retrieved with Address => Buffer (Start)'Address;
   begin
      ...
      Value := Buffer_Overlay;
      if Default_Bit_Order /= High_Order_First then -- we're not on a Big Endian machine
         GNAT.Byte_Swapping.Swap4 (Value'Address);
      end if;
   end Retrieve_4_Bytes;

When we declare the :ada:`Buffer_Overlay` object we also specify its 
address |mdash| strictly speaking its starting address |mdash| which, 
not surprisingly, is the address of the :ada:`Buffer` component at index 
:ada:`Start`. Applying the :ada:`Address` attribute gives us that 
location. 

There are other characteristics we might want to query too. For example, 
we might want to ask the compiler what size and alignment it chose for a 
given object (or type, for all such objects). Rather than describe all 
the possibilities, we can just say that all the representation 
characteristics that can be specified can also be queried. We cover 
specifying representation characteristics next, so just assume the 
corresponding query is available. 


Specifying Representation
-------------------------

.. todo::

    Complete section!


Unchecked Programming
---------------------

.. todo::

    Complete section!
