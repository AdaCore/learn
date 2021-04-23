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

A machine "word" is the largest amount of storage that can be
conveniently and efficiently manipulated by the hardware, given the
implementation's run-time model. A word consists of a some number of
storage elements, maybe one but typically more than one. As the unit the
machine natively manipulates, words are expected to be independently
addressable. (On some machines only words are independently
addressable.)

:ada:`Word_Size` is the number of bits in the machine word. On a 32-bit
machine we'd expect :ada:`Word_Size` to have a value of 32; on a 64-bit
machine it would probably be 64, and so on.

:ada:`Storage_Unit` and :ada:`Word_Size` are obviously related.

Another frequently referenced declaration in package :ada:`System` is
that of the type representing memory addresses, along with a constant
for the null address designating no storage element.

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
:ada:`Low_Order_First` to little-endian. Here is a real-world example of
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
supported by the language. For any representation characteristic you can
specify, you can also query that characteristic. That can be very handy,
especially if you did not specify it and want to know what the compiler
chose for the representation.

For example, say you want to express an "overlay," in which an object of
one type is placed at the same memory location as a distinct object of a
distinct type, thus overlaying one object over the other. Doing so
allows you to interact with that memory location in more than one way:
one way per type, specifically. This is known as `type punning <https://en.wikipedia.org/wiki/Type_punning>`_ in
computer programming. There are other ways to achieve that effect as
well, but realize that doing so circumvents the static strong typing
used by Ada to protect us from ourselves and from others. Use it with
care!

For a concrete example, imagine a sensor providing a stream of bytes as
input, and you know that groups of those bytes comprise a composite
value, i.e., a value of some record type. (Maybe it is a scalar value
and a checksum.) The buffered array of input bytes would be declared in
terms of a numeric type, probably an unsigned integer :ada:`Byte` type,
whereas the composite type would, of course, be the record type. The
clients of the sensor driver call a driver-defined procedure to get the
latest value, passing an output argument of the record type. The
procedure gets the next available buffered bytes, converts them to the
record type and puts the value in the parameter, maybe checks it for
validity and so on, and then the procedure returns. An overlay would
make that simple. We would declare an object of the record type located
at the same address in the buffer as the starting byte representing the
next composite value.

But there's a problem: inside the procedure we want to overlay our
record object onto the address of one of the bytes in the buffer. But we
didn't declare the buffer ourselves, at a known location. It is passed
to the procedure as an argument. Therefore, when writing the procedure
we don't already know the address of the buffer, nor the addresses of
the bytes within. As a result, we don't know the address to use in our
overlay.

Of course, there is a direct solution. We can query the addresses of
objects, and other things too, but objects, especially variables, are
the most common case. In particular, we can say :ada:`X'Address` to
query the starting address of object :ada:`X`, including the case where
:ada:`X` is a component of an array. With that information we know what
address to specify for our record object overlay.

Here's the generic procedure body again, now showing the overlay that
both specifies and queries an address:

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
we might want to ask the compiler what alignment it chose for a
given object (or type, for all such objects). Rather than describe all
the possibilities, we can just say that all the representation
characteristics that can be specified can also be queried. We cover
specifying representation characteristics next, so just assume the
corresponding queries are available.

That said, there is one particular representation query we need to talk about
explicitly, now, because there is a lot of confusion about it: the
:ada:`'Size` attribute. The confusion stems from the fact that there are
multiple contexts for applying the attribute, and multiple reasonable
interpretations possible. We can apply the :ada:`'Size` attribute to a
type, in an attempt to get information about all objects of the type, or
we can apply it to individual objects to get specific information. In
both cases, what actual information do we get? In the original version
of Ada these questions weren't really answered so vendors did what they
thought was correct. But they did not agree with each other, and
portability became a problem.

For example, suppose you want to convert some value to a series of bytes
in order to send the value over the wire. To do that you need to know
how many bytes are required to represent the value. Many applications
queried the size of the type to determine that, and then, when porting
to a new vendor's compiler, found that their code no longer worked
correctly. The new vendor's implementation wasn't wrong, it was just
different.

Later versions of Ada answered these questions, where possible,
so let's examine the contexts and meaning. Above all, though, remember
that :ada:`'Size` returns values in terms of **bits**.

If we apply :ada:`'Size` to a type, the resulting value depends on the
kind of type.

For scalar types, i.e., those without components such as integers and
enumerations, the attribute returns the *minimum* number of bits
required to represent all the values of the type. Consider type :ada:`Boolean`,
which has two possible values. One bit will suffice, and indeed the
language standard requires :ada:`Boolean'Size` to be the value 1.

This meaning also applies to subtypes, which can constrain the number of
values for a scalar type. Consider subtype :ada:`Natural`. That's a subtype
defined by the language to be type :ada:`Integer` but with a range of 
:ada:`0 .. Integer'Last`. 
On a 32-bit machine we would expect Integer to be a native type, and
thus 32-bits. On such a machine if we say :ada:`Integer'Size` we will
indeed get 32. But if we say :ada:`Natural'Size` we will get 31, not 32,
because only 31 bits are needed to represent that range on that machine.

The size of objects, on the other hand, cannot be just a matter of the
possible values. Consider type :ada:`Boolean` again, where :ada:`Boolean'Size`
is required to be 1. No compiler is likely to allocate one bit to a :ada:`Boolean`
variable, because typical machines don't support
individually-addressable bits. Instead, addresses refer to storage
elements, of a size indicated by the :ada:`Storage_Unit` constant. The compiler
will allocate the smallest number of storage elements necessary,
consistent with other considerations such as alignment. Therefore, for a
machine that has :ada:`Storage_Unit` set to a value of eight, we can 
assume that a compiler for that machine will allocate an entire eight-bit
storage element to a stand-alone :ada:`Boolean` variable. The other seven bits
are simply not used by that variable. Moreover, those seven bits are not
used by any other stand-alone object either, because access would be far
less efficient, and such sharing would require some kind of locking to
prevent tasks from interfering with each other when accessing those
stand-alone objects. (Stand-alone objects are independently addressable;
they wouldn't stand alone otherwise.)

By the same token (and still assuming a 32-bit machine), a compiler will
allocate more than 31 bits to a variable of subtype Natural because
there is no 31-bit addressable unit. The variable will get all 32-bits.

Note that we're talking about individual, stand-alone variables.
Components of composite types, on the other hand, might indeed share
bytes if the individual components don't require all the bits of their
storage elements. You'd have to request that representation, though,
with most implementations, because accessing the components at run-time
would require more machine instructions. We'll go into the details of
that later.

Let's talk further about types.

For record types, :ada:`'Size` gives the minimum number of bits required
to represent the whole composite value. But again, that's not
necessarily the number of bits required for the objects' in-memory
representation. The order of the components within the record can make
a difference, as well as their alignments. The compiler will respect the
alignment requirements of the components, and may add padding bytes
within the record and also at the end to ensure components start at
addresses compatible with their alignment requirements. As a result the
overall size could be larger.

Note that Ada compilers are allowed to reorder the components; the order
in memory might not match the order in the source code.

For example, consider this record type and its components:

.. image:: images/unoptimized-record-component-order.png
  :width: 600
  :alt: Memory allocated to a record with unoptimized layout

In the figure, we see a record type with some components, and a sample
layout for that record type assuming the compiler does not reorder the
components. Observe that some bytes allocated to objects of type :ada:`R` are
unused (the darkly shaded ones). In this case that's because the
alignment of subtype :ada:`S` happens to be 4 on this machine. The component
:ada:`X` of that subtype :ada:`S` cannot start at byte offset 1, or 2, or 3, 
because those addresses would not satisfy the alignment constraint of 
:ada:`S`. (We're assuming byte 0 is at a word-aligned address.) Therefore,
:ada:`X` starts at the object's starting address plus 4. Components
:ada:`B` and :ada:`C` are of types that have an alignment of 1, so they 
can start at any storage element.
They immediately follow the bytes allocated to component :ada:`X`. 
Therefore, :ada:`R'Size` is 80, or 10 bytes. The three bytes following 
component :ada:`M` are simply not used.

But what about the two bytes following component :ada:`C`? They could be
allocated to stand-alone objects if they would fit. More likely, though,
the compiler will allocate those two bytes to objects of type :ada:`R`, that
is, 12 bytes instead of 10 are allocated. As a result, 96 bits are
actually used in memory. The extra, unused 16 bits are "padding."

Why add unused padding? It simplifies the memory allocation of objects of type
:ada:`R`. Suppose some array type has components of record type :ada:`R`.
Assuming the first component is aligned properly, every following
component will also be aligned properly, automatically, because the two
padding bytes are considered parts of the components.

To make that work, the compiler takes the most stringent alignment of
all the record type's components and uses that for the alignment of the
overall record type. That way, any address that satisfies the record
object's alignment will satisfy the components' alignment requirements.
The alignment is component :ada:`X`, of subtype :ada:`S`, is 4. The other 
components have an alignment of 1, therefore :ada:`R'Alignment` is 4. An
aligned address plus 12 will also be an aligned address.

This rounding up based on alignment is recommended behavior for the
compiler, not a requirement, but is reasonable and typical among
vendors. Although it can result in unused storage, that's the price paid
for speed of access (or even correctness for machines that would fault
on misaligned address allocations).


As you can see, alignment is a critical factor in the sizes of composite
objects. If you care about the layout of the type you very likely need
to care about the alignment of the components and overall record type.

Ada compilers are allowed to reorder the components of record types in
order to minimize these gaps or satisfy the alignment requirements of
the components. Some compilers do, some don't. Consider the type :ada:`R`
again, this time with the first two components switched in the component
declaration order:

.. image:: images/optimized-record-component-order.png
  :width: 600
  :alt: Memory allocated to a record with optimized layout

Now :ada:`R'Size` will report 56 bits instead of 80. The one trailing byte will
still be padding, but only that one.

What about unbounded types, for example type :ada:`String`? Querying the
:ada:`'Size` in that case would provide an implementation-defined result. 
A somewhat
silly thing to do, really, since the type |mdash| by definition |mdash|
doesn't specify how many components are involved.

Usually, though, you don't want to query the size of a type. Most of the
time what you want is the size of objects of the type. Going back to
sending values over the wire, the code should query the size of the
*parameter* holding the value to be sent. That will tell you how many
bits are really needed.

One last point. GNAT, and now Ada 202x, define an attribute named
:ada:`Object_Size`. It does just what the name suggests: what :ada:`'Size` does
when applied to objects rather than types. GNAT also defines another attribute, 
named :ada:`Value_Size`, that does what :ada:`'Size` does when applied to
types. The former is far more useful so Ada has standardized it.


Specifying Representation
-------------------------


Recall that we said :ada:`Boolean'Size` is always 1. Suppose we have an array
of 16 Boolean components. How big are objects of the type? For the sake
of efficient access, each component is almost certainly allocated an
individual byte rather than a single bit. Our array of 16 Booleans will
be reported by :ada:`'Size` to be 128 bits. If you wanted a bit-mask, in which
each Boolean component is allocated a single, you have a problem. Naturally
there is a solution.

As you saw earlier, it can pay to consider the component alignments when
declaring the components your record types, because there's no guarantee that the
compiler will optimize (reorder) the layout for you. That said, what you should
really do is specify the actual record layout you want. But when you do, keep 
the component alignments in mind.



.. todo::

    Complete section!


Unchecked Programming
---------------------

.. todo::

    Complete section!
