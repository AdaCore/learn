Low Level Programming
=====================

.. include:: ../../global.txt

This section introduces a number of topics in low-level programming, in 
which the hardware and the compiler's representation choices are much 
more in view at the source code level. In comparatively high level code 
these topics are "abstracted away" in that the programmer can assume 
that the compiler does whatever is necessary on the current target 
machine so that their code executes as intended. That approach is not 
sufficient in low-level programming. 

Note that we do not cover every possibility or language feature.
Instead, we cover the necessary concepts, and also potential surprises
or pitfalls, so that the parts not covered can be learned on your own.


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


Querying Implementation Limits and Characteristics
--------------------------------------------------

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
the machines communicating. That characteristic can be determined via a 
constant declared in package :ada:`System` as follows: 

.. code-block:: ada

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := implementation-defined;

:ada:`High_Order_First` corresponds to "Big Endian" and 
:ada:`Low_Order_First` to "Little Endian." On a Big Endian machine, bit 
0 is the most significant bit. On a Little Endian machine, bit 0 is the 
least significant bit. 

Strictly speaking, this constant gives us the default order for bits 
within storage elements in record representation clauses, not the order 
of bytes within words. However, we can usually use it for the byte order 
too. In particular, if :ada:`Word_Size` is greater than :ada:`Storage_Unit`, a word 
necessarily consists of multiple storage elements, so the default bit 
ordering is the same as the ordering of storage elements in a word. 

Let's take that example of swapping the bytes in a received Ethernet 
packet. The "wire" format is Big Endian so if we are running on a Little 
Endian machine we must swap the bytes received.

Suppose we want to retrieve typed values from a 
given buffer or bytes. We get the bytes from the buffer into a variable named :ada:`Value`, of the type of
interest, and then swap those bytes within :ada:`Value` if necessary.

.. code-block:: ada

      ...
   begin
      Value := ...
      
      if Default_Bit_Order /= High_Order_First then -- we're not on a Big Endian machine
         Value := Byte_Swapped (Value);
      end if;
   end Retrieve_4_Bytes;

We have elided the code that gets the bytes into :ada:`Value`, for the sake of 
simplicity. How the bytes are actually swapped by function :ada:`Byte_Swapped`
is also irrelevant. The point here is the if-statement: the expression 
compares the :ada:`Default_Bit_Order` constant to 
:ada:`High_Order_First` to see if this execution is on a Big Endian 
machine. If not, it swaps the bytes because the incoming bytes are 
always received in "wire-order," i.e., Big Endian order. 

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

:ada:`Memory_Size` is an implementation-defined value that is intended
to reflect the memory size of the configuration, in units of storage
elements. What the value actually refers to is not specified. Is it the
size of the address space, i.e., the amount possible, or is it the
amount of physical memory actually on the machine, or what? In any case,
the amount of memory available to a given computer is neither dependent
upon, nor reflected by, this constant. Consequently, :ada:`Memory_Size`
is not useful either.

Why have something defined in the language that nobody uses? In short,
it seemed like a good idea at the time when Ada was first defined.
Upward-compatibility concerns propagate these declarations forward as
the language evolves, just in case somebody does use them.


Querying Representation Choices
-------------------------------

As we mentioned in the introduction, in low-level programming the 
hardware and the compiler's representation choices can come to the 
forefront. You can, therefore, query many such choices. 

For example, let's say we want to query the addresses of some objects
because we are calling the imported C c:`memcpy` function. That function
requires two addresses to be passed to the call: one for the source, and
one for the destination. We can use the :ada:`'Address` attribute to get
those values.

We will explore importing routines and objects implemented in other
languages elsewhere. For now, just understand that we will have an Ada
declaration for the imported routine that tells the compiler how it
should be called. Let's assume we have an Ada function declared like so:

.. code-block:: ada

   function MemCopy
     (Destination : System.Address;
      Source      : System.Address;
      Length      : Natural)
   return Address
   with
     Import,
     Convention => C,
     Link_Name => "memcpy",
     Pre  => Source /= Null_Address      and then
             Destination /= Null_Address and then
             Source /= Destination       and then
             not Overlapping (Destination, Source, Length),
     Post => MemCopy'Result = Destination;
   --  Copies Length bytes from the object designated by Source to the object
   --  designated by Destination. 

The three aspects that do the importing are specified after the reserved 
word :ada:`with` but can be ignored for this discussion. We'll talk 
about them later. The preconditions make explicit the otherwise implicit 
requirements for the arguments passed to memcpy, and the postcondition 
specifies the expected result returned from a successful call. Neither 
the preconditions nor the postconditions are required for importing 
external entities but they are good "guard-rails" for using those 
entities. If we call it incorrectly the precondition will inform us, and 
likewise, if we misunderstand the result the postcondition will let us 
know (at least to the extent that the return value does that). 

For a sample call to our imported routine, imagine that we have a 
procedure that copies the bytes of a :ada:`String` parameter into a :ada:`Buffer` 
parameter, which is just a contiguous array of bytes. We need to tell 
:ada:`MemCopy` the addresses of the arguments passed so we apply the attribute 
accordingly: 

.. code-block:: ada

   procedure Put (This : in out Buffer; Start : Index; Value : String) is
      Result : System.Address with Unreferenced;
   begin
      Result := MemCopy (Destination => This (Start)'Address,
                         Source      => Value'Address,
                         Length      => Value'Length);
   end Put;
   
The order of the address parameters is easily confused so we use the 
named association format for specifying the actual parameters in the 
call. 

Although we assign :ada:`Result` we don't otherwise use it, so we tell the
compiler this is not a mistake via the :ada:`Unreferenced` aspect. And if we
do turn around and reference it the compiler will complain, as it should.

(We don't show the preconditions for :ada:`Put`, but they would have specified 
that :ada:`Start` must be a valid index into this particular buffer, and that 
there must be room in the :ada:`Buffer` argument for the number of bytes in 
:ada:`Value` when starting at the :ada:`Start` index, so that we don't copy past the 
end of the :ada:`Buffer` argument.) 

There are other characteristics we might want to query too. 

We might want to ask the compiler what alignment it chose for a given 
object (or type, for all such objects). For a type, an :ada:`Alignment` of zero 
means that objects of the type are not normally aligned on a storage 
element boundary at all. That could happen if the type is packed down 
into a composite object, such as an array of :ada:`Booleans`. We'll discuss 
"packing" soon. More commonly, the smallest likely value is 1, meaning 
that any storage element's address will suffice. If the machine has no 
particular natural alignments, then all type :ada:`Alignment`s will probably 
be 1 by default. That would be somewhat rare today, though, because 
modern processors usually have comparatively strict alignment requirements. 

We can ask for the amount of storage associated with various entities. 
For example, when applied to a task, :ada:`'Storage_Size` tells us the 
number of storage elements reserved for the task's execution. The value 
includes the size of the task's stack, if it has one. We aren't told if 
other required storage, used internally in the implementation, is also 
included in this number. Often that other storage is not included in 
this number, but it could be. 

:ada:`Storage_Size` is also defined for access types. The meaning is a 
little complicated. Access types can be classified into those that 
designate only variables ("access-to-object") and those that can also 
designate constants. Constants are never allocated dynamically so we can 
ignore them. Each access-to-object type has an associated storage pool. 
The storage allocated by :ada:`new` comes from the pool, and instances 
of :ada:`Unchecked_Deallocation` return storage to the pool. 

When applied to an access-to-object type, :ada:`Storage_Size` gives us 
the number of storage elements reserved for the corresponding pool. 

Note that :ada:`Storage_Size` doesn't tell us how much available, 
unallocated space remains in a pool. It includes both allocated and 
unallocated space. Note, too, that although each access-to-object type 
has an associated pool, that doesn't mean that each one has a distinct, 
dedicated pool. They might all share one, by default. On an operating 
system, such as Linux, the default shared pool might even be implicit, 
consisting merely of calls to the OS routines in C. 

As a result, querying :ada:`Storage_Size` for access types and tasks is 
not necessarily all that useful. Specifying the sizes, on the other hand, 
definitely can be useful. 

That said, we can create our own pool types and define precisely how 
they are sized and how allocation and deallocation work, so in that case 
querying the size for access types could be more useful. 

For an array type or object, :ada`'Component_Size` provides the size in 
bits of the individual components.

More useful are the following two attributes that query a degree 
of memory sharing between objects. 

Applied to an object, :ada`'Has_Same_Storage` is a Boolean function that takes 
another object of any type as the argument. It returns whether the two 
objects' representations occupy exactly the same bits. If the 
representation is contiguous, the objects sit at the same address and 
occupy the same length of memory. 

Applied to an object, :ada`'Overlaps_Storage` is a Boolean function that takes 
another object of any type as the argument. It returns whether the two 
objects' representations share at least one bit. 

Generally, though, we specify representation characteristics far more 
often than we query them. Rather than describe all the possibilities, we 
can just say that all the representation characteristics that can be 
specified can also be queried. We cover specifying representation 
characteristics next, so just assume the corresponding queries are 
available. 

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

For scalar types, the attribute returns the *minimum* number of bits 
required to represent all the values of the type. Here's a diagram 
showing what the category "scalar types" includes: 

.. image:: images/scalar_types_tree.png
  :width: 600
  :alt: Scalar types tree

Consider type :ada:`Boolean`, which has two possible values. One bit 
will suffice, and indeed the language standard requires 
:ada:`Boolean'Size` to be the value 1. 

This meaning also applies to subtypes, which can constrain the number of
values for a scalar type. Consider subtype :ada:`Natural`. That's a subtype
defined by the language to be type :ada:`Integer` but with a range of
:ada:`0 .. Integer'Last`.
On a 32-bit machine we would expect Integer to be a native type, and
thus 32-bits. On such a machine if we say :ada:`Integer'Size` we will
indeed get 32. But if we say :ada:`Natural'Size` we will get 31, not 32,
because only 31 bits are needed to represent that range on that machine.

The size of objects, on the other hand, cannot be just a matter of the 
possible values. Consider type :ada:`Boolean` again, where 
:ada:`Boolean'Size` is required to be 1. No compiler is likely to 
allocate one bit to a :ada:`Boolean` variable, because typical machines 
don't support individually-addressable bits. Instead, addresses refer to 
storage elements, of a size indicated by the :ada:`Storage_Unit` 
constant. The compiler will allocate the smallest number of storage 
elements necessary, consistent with other considerations such as 
alignment. Therefore, for a machine that has :ada:`Storage_Unit` set to 
a value of eight, we can assume that a compiler for that machine will 
allocate an entire eight-bit storage element to a stand-alone 
:ada:`Boolean` variable. The other seven bits are simply not used by 
that variable. Moreover, those seven bits are not used by any other 
stand-alone object either, because access would be far less efficient, 
and such sharing would require some kind of locking to prevent tasks 
from interfering with each other when accessing those stand-alone 
objects. (Stand-alone objects are independently addressable; they 
wouldn't stand alone otherwise.) 

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

Let's talk further about sizes of types.

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

But what about the two bytes following the last component :ada:`C`? They could be
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
on misaligned component accesses).

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

One last point: GNAT, and now Ada 202x, define an attribute named 
:ada:`Object_Size`. It does just what the name suggests: what 
:ada:`'Size` does when applied to objects rather than types. GNAT also 
defines another attribute, named :ada:`Value_Size`, that does what 
:ada:`'Size` does when applied to types. The former is far more useful 
so Ada has standardized it. 


Specifying Representation
-------------------------

Recall that we said :ada:`Boolean'Size` is required to be 1, and that
stand-alone objects of type :ada:`Boolean` are very likely allocated some
integral number of storage elements (e.g., bytes) in memory, typically
one. What about arrays of Booleans? Suppose we have an array of 16
Boolean components. How big are objects of the array type? It depends on
the machine. Continuing with our hypothetical (but typical)
byte-addressable machine, for the sake of efficient access each
component is almost certainly allocated an individual byte rather than a
single bit, just like stand-alone objects. Consequently, our array of 16
Booleans will be reported by :ada:`'Size` to be 128 bits, i.e., 16
bytes. If you wanted a bit-mask, in which each Boolean component is
allocated a single bit and the total array size is 16 bits, you'd have a
problem. The compiler assumes you want speed of access rather than
storage minimization, and normally that would be the right assumption.

Naturally there is a solution. Ada allows us to specify the 
representation characteristics of types, and thus objects of those 
types, including their bit-wise layouts. It also allows us to specify 
the representation of individual objects. You should understand, though, 
that the compiler is not required to do what you ask. That liberty is 
unavoidable because you might ask the impossible. If you specify that 
the array of 16 Booleans is to be represented completely in 15 bits, 
what can the compiler do? Rejecting that specification is the only 
reasonable response. But if you specify something possible the compiler 
is going to do what you ask, absent some compelling reason to the 
contrary. 

With that in mind, let's examine setting the size for types.

So, how do we specify that the want our array of 16 Boolean components 
to be allocated one bit per component, for a total allocation of 16 
bits? There are a couple of ways, one somewhat better than the other. 

First, you can ask that the compiler "pack" the components into as small
a number of bits as it can:

.. code-block:: ada

   type Bits16 is array (0 .. 15) of Boolean with
     Pack;

That likely does what you want: :ada:`Bits16'Size` will probably be 16.

But realize that the :ada:`Pack` aspect (and corresponding pragma) is merely a
request that the compiler do its best to minimize the number of bits
allocated, not necessarily that it do exactly what you expected or
required.

We could set the size of the entire array type:

.. code-block:: ada

   type Bits16 is array (0 .. 15) of Boolean with
     Size => 16;

But the language standard says that a :ada:`Size` clause on array and record 
types should not affect the internal layout of their components. That's 
Implementation Advice, so not normative, but implementations are really 
expected to follow the advice, absent some compelling reason. That's 
what the :ada:`Pack` aspect, record representation clauses, and :ada:`Component_Size` 
clauses are for. (We'll talk about record representation clauses
momentarily.) That said, at least one other vendor's compiler would have 
changed the size of the array type because of the :ada:`Size` clause, so GNAT 
defines a configuration pragma named :ada:`Implicit_Packing` that overrides the 
default behavior. With that pragma applied, the :ada:`Size` clause would 
compile and suffice to make the overall size be 16. That's a 
vendor-defined pragma though, so not portable. 

Therefore, the best way to set the size for the array type is to set the 
size of the individual components, via the :ada:`Component_Size` aspect as the 
Implementation Advice indicates. That will say what we really want, 
rather than a "best effort" request for the compiler, and is portable: 

.. code-block:: ada

   type Bits16 is array (0 .. 15) of Boolean with
     Component_Size => 1;

With this approach the compiler must either use the specified size for
each component or refuse to compile the code. If it compiles, objects
of the array type will be 16 bits total.

Now that we have a bit-mask array type, let's put it to use when 
specifying the address of an object. 

Let's say you want to access the individual bits of what is usually 
treated as a simple signed integer. One way to do that is to express an 
"overlay," in which an object of one type is placed at the same memory 
location as a distinct object of a distinct type, thus overlaying one 
object over the other. Doing so allows you to interact with that memory 
location in more than one way: one way per type, specifically. This is 
known as `type punning <https://en.wikipedia.org/wiki/Type_punning>`_ in 
computer programming. There are other ways to achieve that effect as 
well, but realize that you're circumventing the static strong typing 
used by Ada to protect us from ourselves and from others. Use it with 
care! 

.. code-block:: ada

   type Bits32 is array (0 .. 31) of Boolean with
     Component_Size => 1;

   X : Integer;
   Y : Bits32 with Address => X'Address;

We can query the addresses of objects, and other things too, but 
objects, especially variables, are the most common case. In the above, 
we say :ada:`X'Address` to query the starting address of object 
:ada:`X`. With that information we know what address to specify for our 
bit-mask overlay :ada:`Y`. Now :ada:`X` and :ada:`Y` are aliases, and therefore we can 
manipulate those memory bytes as either an integer or as an array of 
individual bits. (Note that we could have used a modular type as the 
overlay if all we wanted was an unsigned view.) 

It is worth noting that Ada language rules say that for such an overlaid 
object the compiler should not perform optimizations that it would 
otherwise apply in the absence of aliases. That's necessary, 
functionally, but implies degraded performance, so keep it in mind. 
Aliasing precludes some desirable optimizations. 

You may be asking yourself how to know that type :ada:`Integer` is 32 bits 
wide, so that we know what size array to use for the bit-mask. The 
answer is that you just have to know the target well when doing 
low-level programming, and that portability is not the controlling 
requirement. The hardware becomes much more visible, as we mentioned. 

That said, you could at least verify the assumption:

.. code-block:: ada

   pragma Compile_Time_Error (Integer'Object_Size /= 32, "Integers expected to be 32 bits");
   X : Integer;
   Y : Bits32 with Address => X'Address;

That's a vendor-defined pragma so this is not fully portable. It isn't 
an unusual pragma, though, so at least you can probably get the same 
functionality even if the pragma name varies. 

Let's return, momentarily, to setting the size of entities, but now let's 
focus on setting the size of objects. 

We've said that the size of an object is not necessarily the same as the 
size of the object's type. The object size won't be smaller, but it 
could be larger. Why? For a stand-alone object or a parameter, most 
implementations will round the size up to a storage element boundary, or 
more, so the object size might be greater than that of the type. Think 
back to :ada:`Boolean`, where :ada:`Size` is required to be 1, but stand-alone objects 
are probably allocated 8 bits, i.e., an entire storage element (on our 
hypothetical byte-addressed machine). 

Likewise, recall that numeric type declarations are mapped to underlying 
hardware numeric types. These underlying numeric types provide at least 
the capabilities we request with our type declarations, e.g., the range 
or number of digits, perhaps more. But the mapped numeric hardware type 
cannot provide less than requested. If there is no underlying hardware 
type with at least our requested capabilities, our declarations won't compile. 
That mapping means that specifying the size of a numeric type doesn't  
necessarily affect the size of objects of the type. That numeric 
hardware type is the size that it is, and is fixed by the hardware. 

For example, let's say we have this declaration:

.. code-block:: ada

   type Device_Register is range 0 .. 2**5 - 1 with Size => 5;
   
That will compile successfully, because there will be a signed integer 
hardware type with at least that range. (Not necessarily, legally 
speaking, but realistically speaking, there will be such a hardware 
type.) Indeed, it may be an 8-bit signed integer, in which case 
:ada:`Device_Register'Size` will give us 5, but objects of the type will have a 
size of 8, unavoidably, even though we set :ada:`Size` to 5.

The difference between the type and object sizes can lead to potentially 
problematic code: 

.. code-block:: ada

   type Device_Register is range 0 .. 2**8 - 1 with Size => 8;
   My_Device : Device_Register
     with Address => To_Address (...); 

The code compiles successfully, and tries to map a byte to a hardware 
device that is physically connected to one storage element in the 
processor memory space. The actual address is elided as it is not 
important here. 

That code might work too, but it might not. We might think that 
:ada:`My_Device'Size` is 8, and that :ada:`My_Device'Address` points at an 8-bit 
location. However, this isn't necessarily so, as we saw with the 
supposedly 5-bit example earlier. Maybe the smallest signed integer the 
hardware has is 16-bits wide. The code would compile because a 16-bit 
signed numeric type can certainly handle the 8-bit range requested. 
:ada:`My_Device'Size` would be then 16, and because :ada:`'Address` gives us the 
*starting* storage element, :ada:`My_Device'Address` might designate the 
high-order byte of the overall 16-bit object. When the compiler reads 
the two bytes for :ada:`My_Device` what will happen? One of the bytes will be 
the data presented by the hardware device mapped to the memory. The 
other byte will contain undefined junk, whatever happens to be in the 
memory cell at the time. We might have to debug the code a long time to 
identify that as the problem. More likely we'll conclude we have a 
failed device. 

The correct way to write the code is to specify the size of the 
object instead of the type: 

.. code-block:: ada

   type Device_Register is range 0 .. 2**8 - 1; 
   My_Device : Device_Register with
     Size => 8,
     Address => To_Address (...); 

If the compiler cannot support stand-alone 8-bit objects, the code
won't compile. 

Alternatively, we could change the earlier :ada:`Size` clause on the 
type to apply :ada:`Object_Size` instead: 

.. code-block:: ada

   type Device_Register is range 0 .. 2**8 - 1 with Object_Size => 8; 
   My_Device : Device_Register with
     Address => To_Address (...); 

The choice between the two approaches comes down to personal preference. 
With either approach, if the implementation cannot support 8-bit 
stand-alone objects, we find out that there is a problem at 
compile-time. That's always cheaper than debugging. 

You might conclude that setting the :ada:`Size` for a type serves no purpose. 
That's not an unreasonable conclusion, given what you've seen, but in 
fact there are reasons to do so. However, there are only a few specific 
cases so we will save the reasons for the discussions of the specific 
cases. 

There is one general case, though, for setting the :ada:`'Size` of a type. 
Specifically, you may want to specify the size that you think is the 
minimum possible, and you want the compiler to confirm that belief. This 
would be one of the so-called "confirming" representation clauses, in 
which the representation detail is what the compiler would have chosen 
anyway, absent the specification. You're not actually changing anything, 
you're just getting confirmation via :ada:`Size` whether or not the compiler accepts 
the clause. Suppose, for example, that you have an enumeration type with 
256 values. For enumeration types, the compiler allocates the smallest 
number of bits required to represent all the values, rounded up to the 
nearest storage element. (It's not like C, where enums are just named 
int values.) For 256 values, an eight-bit byte would suffice, so setting 
the size to 8 would be confirming. But suppose we actually had 257 
enumerals, accidentally? Our size clause set to 8 would not compile, and 
we'd be told that something is amiss. 

There are other confirming representation clauses as well. Thinking 
again of enumeration types, the underlying numeric values are 
integers, starting with zero and monotonically increasing from there 
up to N-1, where N is the total number of enumeral values. 

For example: 

.. code-block:: ada

   type Commands is (Off, On);
   
   for Commands use (Off => 0, On => 1);

As a result, Off is encoded as 0 and On as 1. That specific underlying 
encoding is guaranteed by the language, as of Ada 95, so this is just a 
confirming representation clause nowadays. But it was not guaranteed in 
the original version of the language, so if you wanted to be sure of the 
encoding values you would have specified the above. It wasn't confirming 
before Ada 95, in other words. 

But let's also say that the underlying numeric values are not what you 
want because you're interacting with some device and the commands are 
encoded with values other than 0 and 1. Maybe you want to use an 
enumeration type because you want to specify all the possible values 
actually used by clients. If you just used some numeric type instead and 
made up constants for :ada:`On` and :ada:`Off`, there's nothing to keep clients from 
using other numeric values in place of the two constants (absent some 
comparatively heavy code to prevent that from happening). Better to use 
the compiler to make that impossible in the first place, rather than 
debug the code to find the incorrect values used. Therefore, we could 
specify different encodings, as long as the relative order is maintained 
(and no negative values are used): 

.. code-block:: ada

   for Commands use (Off => 2, On => 4);

Now the compiler will use those encoding values instead of 0 and 1, 
transparently to client code. 

Note that the values given are no longer increasing monotonically: 
there's a gap. That is OK, in itself. As long as we use the two 
enumerals the same way we'd use named constants, all is well. Otherwise, 
there is both a storage issue and a performance issue possible. Let's 
say that we use that enumeration type as the index for an array type. 
Perfectly legal, but how much storage is allocated to objects of this 
array type? Enough for two components? Four, with two unused? The answer 
depends on the compiler, and is therefore not portable. The bigger the 
gaps, the bigger the overall storage difference possible. Likewise, 
imagine we have a for-loop iterating over the index values of one of 
these array objects. The for-loop parameter cannot be coded by the 
compiler to start at 0, clearly, because there is no index (enumeration) 
value corresponding to 0. Similarly, to get the next index, the compiler 
cannot have the code simply increment the current value. Working around 
that takes some extra code, and takes some extra time that would not be 
required if we did not have the gaps. 

The performance degradation can be significant compared to the usual 
code generated for a for-loop. Some coding guidelines say that you 
shouldn't use an enumeration representation clause for this reason, with 
or without gaps. Now that Ada has type predicates we could limit the 
values used by clients for a numeric type, so an enumeration type is not 
the only way to get a restricted set of named, encoded values. 

.. code-block:: ada

   type Commands is new Integer with
      Static_Predicate => Commands in 2 | 4;

   On    : constant Commands := 2;
   Off   : constant Commands := 4;
   Silly : constant Commands := 3;  -- won't compile

The storage and performance issues bring us back to confirming clauses. 
We want the compiler to recognize them as such, so that it can generate 
the usual code, thereby avoiding the unnecessary portability and 
performance issues. Why would we have such a confirming clause now? It 
might be left over from the original version of the language, written 
before the Ada 95 change. Some projects have lifetimes of several 
decades, after all, and changing the code can be expensive (certified 
code, for example). Whether the compiler does recognize confirming 
clauses is a feature of the compiler implementation. We can expect a 
mature compiler to do so, but there's no guarantee. 

Now let's turn to what is arguably the most common representation 
specification, that of record type layouts. 

Recall from the discussion above that Ada compilers are allowed to 
reorder record components in physical memory. In other words, the 
textual order in the source code is not necessarily the physical order 
in memory. That's different from, say, C, where what you write is what 
you get, and you better know what you're doing. On some targets a 
misaligned :c:`struct` component access will perform very poorly, or even 
trap and halt, but that's not the C compiler's fault. There's no 
guarantee that the Ada compiler will reorder the components to avoid 
this problem either. Indeed, GNAT did not reorder components until 
relatively recently but does now, at least for the more egregious 
performance cases. It does this reordering silently, too, although there 
is a switch to have it warn you when it does. To prevent reordering, 
GNAT defines a pragma named :ada:`No_Component_Reorder` that does what the name 
suggests. You can apply it to individual record types, or globally, as a 
configuration pragma. But of course because the pragma is vendor defined 
it is not portable. 

Therefore, if you care about the record layout in memory, the best 
approach is to specify it explicitly. You need to do something, because 
if you let the compiler choose, formerly working code might stop working 
with a new release of the compiler, and you might only find the source 
of the problem by debugging. (Perusing the new features list for new 
releases is a good idea.) Fortunately, there is a standard, 
language-defined way to specify a record type's layout. 

The record layout specification consists of the storage places for some 
or all components, specified with a record representation clause. This 
clause specifies the order, position, and size of components (including 
discriminants, if any). 

The approach is to first define the record type, as usual, using any 
component order you like |mdash| you're about to specify the physical 
layout explicitly, in the next step. That said, defining the components 
in the same order as in the physical layout is convenient because you 
can copy-and-paste the components in that second step, as a start. 

Let's reuse that record type from the earlier discussion:

.. code-block:: ada

   type My_Int is range 1 .. 10;
   
   subtype S is Integer range 1 .. 10;
   
   type R is record
      M : My_Int;
      X : S;
      B : Boolean;
      C : Character;
   end record;
   
The resulting layout is like so, assuming the compiler doesn't reorder 
the components: 

.. image:: images/unoptimized-record-component-order.png
  :width: 600
  :alt: Memory allocated to a record with unoptimized layout
   
As a result, :ada:`R'Size` will be 80 bits (10 bytes), but those last two bytes 
will be will be allocated to objects, for an :ada:`Object_Size` of 96 bits (12 
bytes). We'll change that with an explicit layout specification.

Having declared the record type, the second step consists of defining 
the corresponding record representation clause giving the components' 
layout. The clause uses syntax that somewhat mirrors that of a record 
type declaration. The components' names appear, as in a record type 
declaration. But now, we don't repeat the components' types, instead we 
give their relative positions within the record, in terms of a relative 
offset that starts at zero. We also specify the bits we want them to 
occupy within the storage elements starting at that offset. 

.. code-block:: ada

   for R use record
      X at 0 range 0 .. 31;  -- note the order swap,
      M at 4 range 0 ..  7;  -- with this component
      B at 5 range 0 ..  7;
      C at 6 range 0 ..  7;
   end record;

Now we'll get the optimized order, and we'll always get that order, or 
the layout specification won't compile in the first place. In the 
following diagram, both layouts, the default, and the one resulting from 
the record representation clause, are depicted for comparison: 

.. image:: images/record_layout_with_rep_clause.png
  :width: 600
  :alt: Memory allocated to a record with optimized layout
  
:ada:`R'Size` will be 56 bits (7 bytes), but that last padding byte will also 
be allocated to objects, so the :ada:`Object_Size` will be 64 bits (8 bytes). 
  
Notice how we gave each component an offset, after the reserved word 
:ada:`at`. These offsets are in terms of storage elements, and specify their 
positions within the record object as a whole. They are relative to the 
beginning of the memory allocated to the record object so they are 
numbered starting at zero. We want the :ada:`X` component to be the very first 
component in the allocated memory so the offset for that one is zero. 
The :ada:`M` component, in comparison, starts at an offset of 4 because we are 
allocating 4 bytes to the prior component :ada:`X`: bytes 0 through 3 
specifically. :ada:`M` just occupies one storage element so the next component, 
:ada:`B`, starts at offset 5. Likewise, component :ada:`C` starts at offset 6. 

An individual component may occupy part of a single storage element, all 
of a single storage element, multiple contiguous storage elements, or a 
combination of those (i.e., some number of whole storage elements but 
also part of another). The bit "range" specifies this bit-specific 
layout, per component, by specifying the first and last bits occupied. 
The :ada:`X` component occupies 4 complete 8-bit storage elements, so the bit 
range is 0 through 31, for a total of 32 bits. All the other components 
each occupy an entire single storage element so their bit ranges are 0 
through 7, for a total of 8 bits. 

The text specifying the offset and bit range is known as a 
"component_clause" in the syntax productions. Not all components need be 
specified by component_clauses, but (not surprisingly) at most one 
clause is allowed per component. Really none are required but it would 
be strange not to have some. Typically, all the components are given 
positions. If component_clauses are given for all components, the 
record_representation_clause completely specifies the representation of 
the type and will be obeyed exactly by the implementation. 

Components not otherwise given an explicit placement are given positions 
chosen by the compiler. We don't say that they "follow" those explicitly 
positioned because there's no requirement that the explicit positions 
start at offset 0, although it would be unusual not to start there. 

Placements must not make components overlap, except for components of 
variant parts, a topic covered elsewhere. You can also specify the 
placement of implementation-defined components, as long as you have a 
name to refer to them. (In addition to the components listed in the 
source code, the implementation can add components to help implement 
what you wrote explicitly.) Such names are always attribute 
references but the specific attributes, if any, are 
implementation-defined. It would be a mistake for the compiler to define 
such implicit components without giving you a way to refer to them. 
Otherwise they might go exactly where you want some other component to 
be placed, or overlap that place. 

The positions (offsets) and the bit numbers must be static, informally 
meaning that they are known at compile-time. They don't have to be 
numeric literals, though. Numeric constants would work, but literals are 
the most common by far. 

Note that the language does not limit support for component clauses to 
specific component types. They need not be one of the integer types, in 
particular. For example, a position can be given for components that are 
themselves record types, or array types. Even task types are allowed as 
far as the language goes, although the implementation might require a 
specific representation, such as the component taking no bits whatsoever 
(:ada:`0 .. -1`). There are restrictions that keep things sane, for 
example rules about how a component name can be used within the overall 
record layout construct, but not restrictions on the types allowed for 
individual components. For example, here is a record layout containing a 
:ada:`String` component, arbitrarily set to contain 11 characters: 

.. code-block:: ada

   type R is record
      S : String (1 .. 11);
      B : Boolean;
   end record;

   for R use record
      S at 0  range 0 .. 87;
      B at 11 range 0 .. 7;
   end record;
   
Component :ada:`S` is to be the first component in memory in this example, 
hence the position offset is 0, for the first byte of :ada:`S`. Next, :ada:`S` is 11 
characters long, or 88 bits, so the bit range is 0 .. 87. That's 11 
bytes of course, so :ada:`S` occupies storage elements 0 .. 10. Therefore, the 
next component position must be at least 11, unless there is to be a 
gap, in which case it would be greater than 11. We'll place :ada:`B` 
immediately after the last character of :ada:`S`, so :ada:`B` is at storage element 
offset 11 and occupying all that one byte's bits. 

We'll have more to say about record type layouts but first we need to talk 
about alignment. 

Modern target architectures are comparatively strict about the address 
alignments for some of their types. If the alignment is off, an access 
to the memory for objects of the type can have highly undesirable 
consequences. Some targets will experience seriously degraded 
performance. On others, the target will halt altogether. As you can see, 
getting the alignment correct is a low-level, but vital, part of correct 
code on these machines. 

Normally the compiler does this work for us, choosing an alignment that 
is both possible for the target and also optimal for speed of access. 
You can, however, override the compiler's alignment choice using an 
attribute definition clause or the :ada:`Alignment` aspect. You can do 
so on types other than record types, but specifying it on record types 
is typical. Here's our example record type with the alignment specified 
via the aspect: 

.. code-block:: ada

   type My_Int is range 1 .. 10;
   
   subtype S is Integer range 1 .. 10;
   
   type R is record
      M : My_Int;
      X : S;
      B : Boolean;
      C : Character;
   end record with
      Alignment => 1;

Alignment values are in terms of storage elements. The effect of the 
aspect or attribute clause is to ensure that the starting address of the 
memory allocated to objects of the type will be a multiple of the 
specified value. 

In fact, whenever we specify a record type layout we really should also 
specify the record type's alignment, even though doing so is optional. 
Why? The alignment makes a difference in the overall record object's 
size. We've seen that already, with the padding bytes: the compiler will 
respect the alignment requirements of the components, and may add 
padding bytes within the record and also at the end to ensure components 
start at addresses compatible with their alignment requirements. The 
alignment also affects the size allocated to the record type even when 
the components are already aligned. As a result the overall size could 
be larger than we want for the sake of space. Additionally, when we pass 
such objects to code written in other languages, we want to ensure that 
the starting address of these objects is aligned as the external code 
expects. The compiler might not choose that required alignment by 
default. 

Specifying alignment for record types is so useful that in the first 
version of Ada there was no syntax to specify alignment for anything 
other than record types (via the obsolete :ada:`at mod` clause on record 
representation clauses). 

For that reason GNAT provides a pragma named :ada:`Optimize_Alignment`. This is 
a configuration pragma that affects the compiler's choice of default 
alignments where no alignment is explicitly specified. There is a 
time/space trade-off in the selection of these values, as we've seen. 
The normal choice tries to balance these two characteristics, but with 
an argument to the pragma you can give more weight to one or the other. 
The best approach is to specify the alignments explicitly, per type, for 
those that require specific alignment values. The pragma has the nice 
property of giving general guidance to the compiler for what should be 
done for the other types and objects not explicitly specified. 

Now let's look into the details. We'll use a case study for this 
purpose, including specifying sizes as well as alignments. 

The code for the case study is as follows. It uses :ada:`Size` clauses to 
specify the :ada:`Size`s, instead of the :ada:`Size` aspect, just to emphasize that 
the :ada:`Size` clause approach is not obsolete. 

.. code-block:: ada

   package Some_Types is
   
      type Temperature is range -275 .. 1_000;
   
      type Identity is range 1 .. 127;
   
      type Info is record
         T  : Temperature;
         Id : Identity;
      end record;
   
      for Info use record
         T  at 0 range 0 .. 15;
         Id at 2 range 0 .. 7;
      end record;
   
      for Info'Size use 24;
   
      type List is array (1 .. 3) of Info;
      for List'Size use 24 * 3;
   
   end Some_Types;
     
When we compile this, the compiler will complain that the size for 
:ada:`List` is too small, i.e., that the minimum allowed is 96 bits instead 
of the 72 we specified. We specified 24 * 3 because we said the record 
size should be 24 bits, and we want our array to contain 3 record 
components of that size, so 72 seems right.

What's wrong? As we've shown earlier, specifying the record type size 
doesn't necessarily mean that objects (in this case array components) 
are that size. The object size could be bigger than we specified for the 
type. In this case, the compiler says we need 96 total bits for the 
array type, meaning that each of the 3 array components is 32 bits wide 
instead of 24. 

Why is it 32 bits? Because the alignment for :ada:`Info` is 2 (on this 
machine). The record alignment is a multiple of the largest 
alignment of the enclosed components. The alignment for type :ada:`Temperature` 
(2), is larger than the alignment for type :ada:`Identity` (1), therefore the 
alignment for the whole record type is 2. We need to go from that number 
of storage elements to a number of bits for the size. 

Here's where it gets subtle. The alignment is in terms of storage 
elements. Each storage element is of a size in bits given by 
:ada:`System.Storage_Unit`. We've said that on our hypothetical machine 
:ada:`Storage_Unit` is 8, so storage elements are 8 bits wide on this machine. 
Bytes, in other words. Therefore, to get the required size in bits, we 
have to find a multiple of the two 8-bit bytes (specified by the 
alignment) that has at least the number of bits we gave in the :ada:`Size` 
clause. Two bytes only provides 16 bits, so that's not big enough, we 
need at least 24 bits. The next multiple of 2 bytes is 4 bytes, 
providing 32 bits, which is indeed larger than 24. Therefore, the 
overall size of the record type, consistent with the alignment, is 4 
bytes, or 32 bits. That's why the compiler says each array component is 
32 bits wide. 

But for our example let's say that we really want to use only 72 total 
bits for the array type (and that we want three array components). 
That's the size we specified, after all. So how do we get the record 
type to be 24 bits instead of 32? Yes, you guessed it, we change the 
alignment for the record type. If we change it from 2 to 1, the size of 
24 bits will work. Adding this :ada:`Alignment` clause line will do that: 

.. code-block:: ada

   for Info'Alignment use 1;

An alignment of 1 means that any address will work, assuming that 
addresses refer to entire storage elements. (An alignment of 0 would 
mean that the address need not start on a storage element boundary, but 
we know of no such machines.) 

We can even entirely replace the :ada:`Size` clause with the :ada:`Alignment` clause, 
because the :ada:`Size` clause specifying 24 bits is just confirming: it's the 
value that :ada:`'Size` would return anyway. The problem is the object size. 

Now, you may be wondering why an alignment of 1 would work, given that 
the alignment of the :ada:`Temperature` component is 2. Wouldn't it slow down 
the code, or even trap? Well, maybe. It depends on the machine. If it 
doesn't work we would just have to use 32 bits for the record type, with 
the original alignment of 2, for a larger total array size. 

We said earlier that there are only a small number of reasons to specify 
:ada:`'Size` for a type. We can mention one of them now. Setting :ada:`'Size` 
can be useful to give the minimum number of bits to use for a component 
of a packed composite type, that is, within either a record type or an 
array type that is explicitly packed via the aspect or pragma :ada:`Pack`. It 
says that the compiler, when giving its best effort, shouldn't compress 
components of the type any smaller than the number of bits specified. 
No, it isn't earth-shattering, but other uses are more valuable, to be 
discussed soon. 

One thing we will leave unaddressed (pun intended) is the question of 
bit ordering and byte ordering within our record layouts. In other 
words, the "endian-ness". That's a subject beyond the scope of this 
course. Suffice it to say that GNAT provides a way to specify record 
layouts that are independent of the endian-ness of the machine, within 
some implementation-oriented limits. That's obviously useful when the 
code might be compiled for a different ISA in the future. On the other 
hand, if your code is specifically for a single ISA, e.g. Arm, even if 
different boards and hardware vendors are involved, there's no need to 
be independent of the endian-ness. It will always be the same in that 
case. (Those are "famous last words" though.) 

Although specifying record type layouts and alignments are perhaps the 
most common representation characteristics expressed, there are a couple 
of other useful cases. Both involve storage allocation.

One useful scenario concerns tasking. We can specify the number of 
storage elements reserved for the execution of a task object, or all 
objects of a task type. You use the :ada:`Storage_Size` aspect to do so: 

.. code-block:: ada

   task Servo with
     Storage_Size => 1 * 1024,
     ...
     
Or the corresponding pragma:

.. code-block:: ada

   task Servo is
      pragma Storage_Size (1 * 1024);
   end Servo;     

The aspect seems textually cleaner and lighter unless you have task 
entries to declare as well. In that case the line for the pragma 
wouldn't add all that much. That's a matter of personal aesthetics 
anyway. 

The specified number of storage elements includes the size of 
the task's stack (GNAT does have one, per task). The language does not 
specify whether or not it includes other storage associated with the 
task used for implementing and managing the task execution. With GNAT, 
the extent of the primary stack size is the value returned, ignoring any 
other storage used internally in the run-time library for managing the 
task. 

The GNAT run-time library allocates a default stack amount to each task, 
with different defaults depending on the underlying O.S., or lack 
thereof, and the target. You need to read the documentation to find the 
actual amount, or, with GNAT, read the code (or just ask for help via 
your GNAT account number). 

You would need to specify this amount in order to either increase or 
decrease the allocated storage. If the task won't run properly, perhaps 
crashing at strange and seemingly random places, there's a decent chance 
it is running out of stack space. That might also be the reason if you 
have a really deep series of subprogram calls that fails. The correction 
is to increase the allocation, as shown above. How much? Depends on the 
application code. Try increasing it until it runs properly. Then, 
iteratively decrease it a little at a time until it starts to fail 
again. Add a little back until it runs, and leave it there. 

Even if the task doesn't seem to run out of task stack, you might want 
to reduce it anyway, to the extent possible, because the total amount of 
storage on your target might be limited. Some of the GNAT bare-metal 
embedded targets have very small amounts of memory available, so much so that 
the default task stack allocations would exhaust the memory available 
quickly. That's what the example above does: empirical data showed that 
the :ada:`Servo` task could run with just 1K bytes allocated, so we reduced it 
from the default accordingly. (We specified the size with that 
expression for the sake of readability, relative to using literals 
directly.) 

Notice we said "empirical data" above. How do we know that we exercised 
the task's thread of control exhaustively, such that the arrived-at 
allocation value covers the worst case? We don't, not with certainty. If 
we really must know the allocation will suffice for all cases, say 
because this is a high-integrity application, we would use :program:`GNATstack`. 
GNATstack is an offline tool that exploits data generated by the 
compiler to compute worst-case stack requirements per subprogram and per 
task. As a static analysis tool, its computation is based on information 
known at compile time. It does not rely on empirical run-time 
information.

The other useful scenario for allocating storage concerns access types, 
specifically access types whose values designate objects, as opposed to 
designating subprograms. (Remember, objects are either variables or 
constants.) There is no notion of dynamically allocating procedures and 
functions in Ada so access-to-subprogram types are not relevant here. 
But objects can be of protected types (or task types), and protected 
objects can "contain" entries and protected subprograms, so there's a lot 
of expressive power available. You just don't dynamically 
allocate procedures or functions as such. 

First, a little background.

By default, the implementation chooses a standard storage pool for each 
access-to-object type. The storage allocated by an allocator (i.e., 
:ada:`new`) for a given access-to-object type comes from the associated 
pool. 

Several access types can share the same pool. By default, the 
implementation might choose to have a single global storage pool, used 
by all such access types. This global pool might consist merely of calls 
to operating system routines (e.g., :c:`malloc`), or it might be a 
vendor-defined pool instead. Alternatively, the implementation might 
choose to create a new pool for each access-to-object type, reclaiming 
the pool's memory when the access type goes out of scope (if ever). 
Other schemes are possible. 

Finally, users may define new pool types, and may override the choice of 
pool for an access-to-object type by specifying :ada:`Storage_Pool` for 
the type. In this case, allocation (via :ada:`new`) takes memory from 
the user-defined pool and deallocation puts it back into that pool, 
transparently. 

With that said, here's how to specify the storage to be used for an 
access-to-object type. There are two ways to do it.

If you specify :ada:`Storage_Pool` for an access type, you indicate a 
specific pool object to be used (user-defined or vendor-defined). The 
pool object determines how much storage is available for allocation via 
:ada:`new` for that access type. 

Alternatively, you can specify :ada:`Storage_Size` for the access type. 
In this case, an implementation-defined pool is used for the access 
type, and the storage available is at least the amount requested, maybe 
more (it might round up to some advantageous block size, for example). 
If the implementation cannot satisfy the request, :ada:`Storage_Error` is 
raised. 

It should be clear that that the two alternatives are mutually 
exclusive. Therefore the compiler will not allow you to specify both.

Each alternative has advantages. If your only concern is the total 
number of allocations possible, use :ada:`Storage_Size` and let the 
implementation do the rest. However, maybe you also care about the 
behavior of the allocation and deallocation routines themselves, beyond 
just providing and returning the storage. In that case, use 
:ada:`Storage_Pool` and specify a pool object of the appropriate type. 
For example, you (or the vendor, or someone else) might create a pool 
type in which the allocation routine performs in constant time, because 
you want to do :ada:`new` in a real-time application where 
predictability is essential. 

Lastly, an idiom: when using :ada:`Storage_Size` you may want to specify 
a value of zero. That means you intend to do no allocations whatsoever, 
and the compiler complain if you try. Why would you want an access type 
that doesn't allow dynamically allocating objects? It isn't as 
unreasonable as it might sound. If you plan to use the access type 
strictly with aliased objects, never doing any allocations, you can have 
the compiler enforce your intent. There are application domains that 
prohibit dynamic allocations due to the difficulties in analyzing their 
behavior, including issues of fragmentation and exhaustion. Access types 
themselves are allowed. You'd simply use them to designate aliased 
objects alone. In addition, in this usage scenario, if the 
implementation associates an actual pool with each access type, the 
pool's storage would be wasted since you never intend to allocate any 
storage from it. Specifying a size of 0 tells the implementation not to 
waste that storage. 

We didn't mention :ada:`Storage_Size` when we covered querying 
representation choices, earlier. It is worth mentioning, however, now 
that you know about how things work under the hood. 

When you have specified :ada:`Storage_Size` for some access-to-object 
type, querying it just returns the number you specified. 

Alternatively, if :ada:`Storage_Pool` and a pool object was specified, 
querying :ada:`Storage_Size` returns the value indicated by the pool 
object you specified. 
 
Finally, if neither :ada:`Storage_Pool` nor :ada:`Storage_Size` were 
specified, the meaning of querying :ada:`Storage_Size` is 
implementation-defined. There are many choices possible when you leave 
it up to the implementation, so only the implementation can say what the 
query would mean. 

Before we end this section, there is a GNAT compiler switch you should 
know about. Th ``-gnatR?`` switch instructs the compiler to list the 
representation details for the types, objects and subprograms in the 
compiled file(s). Both implementation-defined and user-defined 
representation details are presented. The '?' is just a placeholder and 
can be one of the following characters: 

   ``[0|1|2|3|4][e][j][m][s]``

Increasing numeric values provide increasing amounts of information. The 
default is '1' and usually will suffice. See the GNAT User's Guide for 
Native Platforms for the details of the switch in section 4.3.15 
Debugging Control, available online here: 

https://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#debugging-control

You'll have to scroll down some to find that specific switch but it is 
worth finding and remembering. When you cannot understand what the 
compiler is telling you about the representation of something, this 
switch is your best friend. 


Unchecked Programming
---------------------

[setting object and type size larger than required for the sake of unchecked conversion (eg our Commands enum type will only be 8 bits wide, and maybe we want it to be the size of an int so we can pass it to C), 
and the other two reasons...]

.. todo::

    Complete section!
    
    
Data Validity
-------------

.. todo::

    Complete section!
