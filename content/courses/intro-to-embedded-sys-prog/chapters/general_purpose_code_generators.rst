General-Purpose Code Generators
===============================

.. include:: ../../../global.txt

In :doc:`another chapter <./multi_language_development>`,
we mentioned that the best way to get a
specific set of machine instructions emitted from the compiler is to
write them ourselves, in the Ada source code, using machine-code
insertions (MCI). The rationale was that the code generator will make
reasonable assumptions, including the assumption that performance is of
uppermost importance, but that these assumptions can conflict with device
requirements.

For example, the code generator might not issue the specific sequence of
machine code instructions required by the hardware. The GPIO pin "lock"
sequence in that referenced chapter is a good example. Similarly, the
optimizer might remove what would otherwise be "redundant" read/writes
to a memory-mapped variable.

The code generator might issue instructions to read a small field in a
memory-mapped record object using byte-sized accesses, when instead the
device requires whole-word or half-word access instructions.

The code generator might decide to load a variable from memory into a
register, accessing the register when the value is required. Typically
that approach will yield far better performance than going to memory
every time the value is read or updated. But suppose the variable is for
a memory-mapped device? In that case we really need the generated code
to go to memory every time.

As you can see, there are times when we cannot let the code generator
make the usual assumptions. Therefore, Ada provides aspects and pragmas
that developers can use to inform the compiler of facts that affect code
generation in this regard.

These facilities are defined in the Systems Programming Annex, C.6,
specifically. The title of that sub-clause is "Shared Variables" because
the objects (memory) can be shared between tasks as well as between
hardware devices and the host computer. We ignore the context of
variables shared between tasks, focusing instead of shared memory-mapped
devices, as this course is about embedded systems.

When describing these facilities we will use aspects, but remember that
the corresponding pragmas are defined as well, except for one. (We'll
mention it later.) For the other aspects, the pragmas existed first and,
although obsolescent, remain part of the language and supported. There's
no need to change your existing source code using the pragmas to use the
aspects instead, unless you need to change it for some other reason.

As this is an introduction, we will not go into absolutely all the details,
but will instead give a sense of what the language provides, and why.


Aspect :ada:`Independent`
-------------------------

To interface with a memory-mapped device, there will be an Ada object of an
appropriate type that is mapped to one or more bytes of memory. The
software interacts with the device by reading and/or writing to the
memory locations mapped to the device, using the operations defined by
the type in terms of normal Ada semantics.

Some memory-mapped devices can be directly represented by a single
scalar value, usually of some signed or unsigned numeric type.
More sophisticated devices almost always involve several distinct
input and output fields. Therefore, representation in the software as a
record object is very common. Ada record types have such extensive and
flexible support for controlling their representation, down to the
individual bit level, that using a record type makes sense. (And as
mentioned, using normal record component access via the "dot notation"
offloads to the compiler the address arithmetic needed to access
individual memory locations mapped to the device.) And of course the
components of the mapped record type can themselves be of scalar and
composite types too, so an extensive descriptive capability exists with
Ada.

Let's say that one of these record components is smaller than the size
of the smallest addressable memory unit on the machine, which is to say,
smaller than the machine instructions can read/write memory
individually. A Boolean record component is a good example, and very
common. The machine cannot usually read/write single bits in memory, so
the generated code will almost certainly read or write a byte to get the
enclosed single-bit Boolean component. It might use a larger sized access too,
a half-word or word. Then the generated code masks off the bits that are
not of interest and does some shifts to get the desired component.

Reading and writing the bytes surrounding the component accessed in the
source code can cause a problem. In particular, some devices react to
being read or written by doing something physical in the hardware.
That's the device designer's intent for the software. But we don't want
that to happen accidentally due to surrounding bytes being accessed.

Therefore, to prevent these "extra" bytes from being accessed, we need a
way to tell the compiler that we need the read or write accesses for the
given object to be independent of the surrounding memory. If the
compiler cannot do so, we'll get an error and the compilation will fail.
That beats debugging, every time.

Therefore, the aspect :ada:`Independent` specifies that the code
generated by the compiler must be able to load and store the memory for
the specified object without also accessing surrounding memory. More
completely, it declares that a type, object, or component must be
independently addressable by the hardware. If applied to a type, it
applies to all objects of the type.

Likewise, aspect :ada:`Independent_Components` declares that the individual
components of an array or record type must be independently addressable.

With either aspect the compiler will reject the declaration if independent
access is not possible for the type/object in question.

For example, if we try to mark each Boolean component of a record type
as :ada:`Independent` we can do so, either individually or via
:ada:`Indepndent_Components`, but doing so will require that each
component is a byte in size (or whatever the smallest addressable unit
happens to be on this machine). We cannot make each Boolean component
occupy one bit within a given byte if we want them to be independently
accessed.

.. code-block:: ada

   package P is

      type R is record
         B0 : Boolean;
         B1 : Boolean;
         B2 : Boolean;
         B3 : Boolean;
         B4 : Boolean;
         B5 : Boolean;
      end record with
        Size => 8,
        Independent_Components;

      for R use record
         B0 at 0 range 0 .. 0;
         B1 at 0 range 1 .. 1;
         B2 at 0 range 2 .. 2;
         B3 at 0 range 3 .. 3;
         B4 at 0 range 4 .. 4;
         B5 at 0 range 5 .. 5;
      end record;

   end P;

For a typical target machine the compiler will reject that code,
complaining that the :ada:`Size` for :ada:`R`' must be at least 48 bits,
i.e., 8 bits per component. That's because the smallest quantity this
machine can independently address is an 8-bit byte.

But if we don't really need the individual bits to be independently
accessed |mdash| and let's hope no hardware designer would define such a
device |mdash| then we have more flexibility. We could, for example,
require that objects of the entire record type be independently
accessible:

.. code-block:: ada

   package Q is

      type R is record
         B0 : Boolean;
         B1 : Boolean;
         B2 : Boolean;
         B3 : Boolean;
         B4 : Boolean;
         B5 : Boolean;
      end record with
        Size => 8,
        Independent;

      for R use record
         B0 at 0 range 0 .. 0;
         B1 at 0 range 1 .. 1;
         B2 at 0 range 2 .. 2;
         B3 at 0 range 3 .. 3;
         B4 at 0 range 4 .. 4;
         B5 at 0 range 5 .. 5;
      end record;

   end Q;

This the compiler should accept, assuming a machine that can access
bytes in memory individually, without having to read some number of
other bytes.

But for another twist, suppose we need one of the components
to be aliased, so that we can construct access values designating it via
the :ada:`Access` attribute? For example, given the record type :ada:`R`
above, and some object :ada:`Foo` of that type, suppose we want to say
:ada:`Foo.B0'Access`? We'd need to mark the component as :ada:`aliased`:

.. code-block:: ada

   package QQ is

      type R is record
         B0 : aliased Boolean;
         B1 : Boolean;
         B2 : Boolean;
         B3 : Boolean;
         B4 : Boolean;
         B5 : Boolean;
      end record with
        Size => 8,
        Independent;

      for R use record
         B0 at 0 range 0 .. 0;
         B1 at 0 range 1 .. 1;
         B2 at 0 range 2 .. 2;
         B3 at 0 range 3 .. 3;
         B4 at 0 range 4 .. 4;
         B5 at 0 range 5 .. 5;
      end record;

   end QQ;

The compiler will once again reject the code, complaining that the size
of B0 must be a multiple of a :ada:`Storage_Unit`, in other words, the
size of something independently accessible in memory on this machine.

Why? The issue here is that aliased objects, including components of
composite types, must be represented in such a way that creating the
designating access ("pointer") value is possible. The component B0, if
allocated only one bit, would not allow an access value to be created
due to the usual machine accessibility limitation we've been discussing.

Similarly, a record component that is of some by-reference type, such as
any tagged type, introduces the same issues as an aliased component. That's
because the underlying implementation of by-reference parameter passing is
much like a :ada:`'Access` attribute reference.

As important as the effect of this aspect is, you probably won't see it
specified. There are other aspects that are more typically required.
However, the semantics of :ada:`Independent` are part of the semantics
of some of these other aspects. Applying them applies :ada:`Independent`
too, in effect. So even though you don't typically apply it directly,
you need to understand the independent access semantics. We discuss
these other, more commonly applied aspects next.

These representation aspects may be specified for an object declaration,
a component declaration, a full type declaration, or a generic formal
(complete) type declaration. If any of these aspects are specified True
for a type, then the corresponding aspect is True for all objects of the
type.


Aspect :ada:`Volatile`
----------------------

Earlier we said that the compiler (specifically the optimizer) might
decide to load a variable from memory into a register, accessing the
register when the value is required or updated. Similarly, the compiler
might reorder instructions, and remove instructions corresponding to
redundant assignments in the source code. Ordinarily we'd want those
optimizations, but in the context of embedded memory-mapped devices they
can be problematic.

The hardware might indeed require the source code to read or write to
the device in a way that the optimizer would consider redundant, and in
order to interact with the device we need every read and write to go to
the actual memory for the mapped device, rather than a register. As
developers we have knowledge about the context that the compiler lacks.

The compiler is aware of the fact that the Ada object is memory-mapped
because of the address clause placing the object at a specific address.
But the compiler does not know we are interacting with an external
hardware device. Perhaps, instead, the object is mapped to a specific
location because some software written in another language expects to
access it there. In that case redundant reads or writes of the same
object really would be redundant. The fact that we are interacting with
a hardware device makes a difference.

In terms of the language rules, we need reading from, and writing to,
such devices to be part of what the language refers to as the "external
effects" of the software. These effects are what the code must actually
produce. Anything else |mdash| the internal effects |mdash| could be
removed by the optimizer.

For example, suppose you have a program that writes a value to some
variable and also writes the string literal "42" to a file. That's is
absolutely all that the program contains.

.. code-block:: ada

   with Ada.Text_IO;  use Ada.Text_IO;

   procedure Demo is
      Output : File_Type;
      Silly  : Integer;
   begin
      Silly := 0;
      Create (Output, Out_File, "output.txt");
      Put (Output, "42");
      Close (Output);
   end Demo;

The value of the variable :ada:`Silly` is not used in any way so there
is no point in even declaring the variable, much less generating code to
implement the assignment. The update to the variable has only
an internal effect. With warnings enabled we'll receive notice from
the compiler, but they're just warnings.

However, writing to the file is an external effect because the file
persists beyond the end of the program's execution. The optimizer (when
enabled) would be free to remove any access to the variable
:ada:`Silly`, but not the write to the file.

We can make the compiler recognize that a software object is part of an
external effect by applying the aspect :ada:`Volatile`. (Aspect
:ada:`Atomic` is pertinent too. More in a moment.) As a result, the
compiler will generate memory load or store instructions for every read or
update to the object that occurs in the source code. Furthermore, it
cannot generate any additional loads or stores to that variable, and it
cannot reorder loads or stores from their order in the source code.
"What You See Is What You Get" in other words.

.. code-block:: ada

   with Ada.Text_IO;  use Ada.Text_IO;

   procedure Demo is
      Output : File_Type;
      Silly  : Integer with Volatile;
   begin
      Silly := 0;
      Create (Output, Out_File, "output.txt");
      Put (Output, "42");
      Close (Output);
   end Demo;

If we compile the above, we won't get the warning we got earlier
because the compiler is now required to generate the assignment for
:ada:`Silly`.

The variable :ada:`Silly` is not even a memory-mapped object, but
remember that we said these aspects are important to the tasking context
too, for shared variables. We're ignoring that context in this course.

There is another reason to mark a variable as :ada:`Volatile`. Sometimes
you want to have exactly the load and store instructions generated that
match those of the Ada code, even though the volatile object is not a
memory-mapped object.
For example, :doc:`elsewhere <./multi_language_development>`
we said that the best way to achieve exact assembly instruction
sequences is the use of machine-code inserts (MCI). That's true, but for
the moment let's say we want to write it in Ada without the MCIs. Our
earlier example was the memory-mapped GPIO ports on Arm microcontrollers
produced by ST Microelectronics. Specifically, these ports have a "lock"
per GPIO pin that allows the developer to configure the pin and then
lock it so that no other configuration can accidentally change the
configuration of that pin. Doing so requires an exact sequence of loads
and stores. If we wrote this in Ada it would look like this:

.. code-block:: ada

   procedure Lock
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin)
   is
      Temp : Word with Volatile;
   begin
      --  set the lock control bit and the pin
      --  bit, clear the others
      Temp := LCCK or Pin'Enum_Rep;

      --  write the lock and pin bits
      Port.LCKR := Temp;

      --  clear the lock bit in the upper half
      Port.LCKR := Pin'Enum_Rep;

      --  write the lock bit again
      Port.LCKR := Temp;

      --  read the lock bit
      Temp := Port.LCKR;

      --  read the lock bit again
      Temp := Port.LCKR;
   end Lock;

:ada:`Temp` is marked volatile for the sake of getting exactly the load
and stores that we express in the source code, corresponding to the
hardware locking protocol. It's true that :ada:`Port` is a memory-mapped
object, so it too would be volatile, but we also need :ada:`Temp` to be
volatile.

This high-level coding approach will work, and is simple enough that
MCIs might not be needed. However, what really argues against it is that
the correct sequence of emitted code requires the optimizer to remove
all the other cruft that the code generator would otherwise include.
(The gcc code generator used by the GNAT compiler generates initially
poor code, by design, relying on the optimizer to clean it up.) In other
words, we've told the optimizer not to change or add loads and stores
for :ada:`Temp`, but without the optimizer enabled the code generator
generates other code that gets in the way. That's OK in itself, as far
as procedure :ada:`Lock` is concerned, but if the optimizer is
sufficiently enabled we cannot debug the rest of the code. Using MCIs
avoids these issues. The point, though, is that not all volatile objects
are memory mapped.

So far we've been illustrating volatility with scalar objects, such as
:ada:`Lock.Temp` above. What about objects of array and record types?
(There are other "composite" types in Ada but they are not pertinent
here.)

When aspect :ada:`Volatile` is applied to a record type or an object of
such a type, all the record components are automatically volatile too.

For an array type (but not a record type), a related aspect
:ada:`Volatile_Components` declares that the components of the array
type |mdash| but not the array type itself |mdash| are volatile. However, if
the :ada:`Volatile` aspect is specified, then the :ada:`Volatile_Components`
aspect is automatically applied too, and vice versa. Thus components of array
types are covered automatically.

If an object (of an array type or record type) is marked volatile then
so are all of its subcomponents, even if the type itself is not marked
volatile.

Therefore aspects :ada:`Volatile` and :ada:`Volatile_Components` are nearly
equivalent. In fact, :ada:`Volatile_Components` is superfluous. The
language provides the :ada:`Volatile_Components` aspect only to give
symmetry with the :ada:`Atomic_Components` and
:ada:`Independent_Components` aspects. You can simply apply
:ada:`Volatile` and be done with it.

Finally, note that applying aspect :ada:`Volatile` does not implicitly
apply :ada:`Independent`, although you can specify it explicitly if need
be.


Aspect :ada:`Atomic`
--------------------

Consider the GPIO pin configuration lock we've mentioned a few times
now, that freezes the configuration of a given pin on a given GPIO port.
The register, named LCKR for "lock register", occupies 32-bits, but only
uses 17 total bits (currently). The low-order 16 bits, [0:15], represent
the 16 GPIO pins on the given port. Bit #16 is the lock bit. That bit is
the first bit in the upper half of the entire word. To freeze the
configuration of a given pin in [0:15], the lock bit must be set at the
same time as the bit to be frozen. In other words, the lower half and
the upper half of the 32-bit word representing the register must be
written together, at the same time. That way, accidental (un)freezing is
unlikely to occur, because the most efficient, hence typical way for the
generated code to access individual bits is for the compiler to load or
store just the single byte that contains the bit or bits in question.

This indivisibility effect can be specified via aspect :ada:`Atomic`. As
a result, all reads and updates of such an object as a whole
are indivisible. In practice that means that the entire object
is accessed with one load or store instruction. For a 16-bit object, all
16-bits are loaded and stored at once. For a 32-bit object, all 32-bits
at once, and so on. The upper limit is the size of the largest machine
scalar that the processor can manipulate with one instruction, as
defined by the target processor. The typical lower bound is 8, for a
byte-addressable machine.

Therefore, within the record type representing a GPIO port, we include
the lock register component and apply the aspect :ada:`Atomic`:

.. code-block:: ada

   type GPIO_Port is limited record
      ...
      LCKR : UInt32 with Atomic;
      ...
   end record with
     ...
     Size => 16#400# * 8;

Hence loads and stores to the :ada:`LCKR` component will be done
atomically, otherwise the compiler will let us know that it is
impossible. That's all we need to do for the lock register to be read
and updated atomically.

You should understand that only accesses to the whole, entire object are
atomic. In the case of the lock register, the entire object is a record
component, but that causes no problems here.

There is, however, something we must keep in mind when manipulating the
values of atomic objects. For the lock register we're using a scalar
type to represent the register, an unsigned 32-bit integer. There are no
sub-components because scalar types don't have components, by
definition. We simply use the bit-level operations to set and clear the
individual bits. But we cannot set the bits |mdash| the lock bit and the
bit for the I/O pin to freeze |mdash| one at a time because the locking
protocol requires all the bits to be written at the same time, and only
the entire 32-bit load and stores are atomic. Likewise, if instead of a
scalar we used a record type or an array type to represent the bits in
the lock register, we could not write individual record or array
components one at a time, for the same reason we could not write
individual bits using the unsigned scalar. The :ada:`Atomic` aspect only
applies to loads and stores of the entire register.

Therefore, to update or read individual parts of an atomic object we
must use a coding idiom in which we explicitly read or write the entire
object to get to the parts. For example, to read an individual record
component, we'd first read the entire record object into a temporary
variable, and then access the component of that temporary variable.
Likewise, to update one or more individual components, we'd first read
the record object into a temporary variable, update the component or
components within that temporary, and then write the temporary back to
the mapped device object. This is known as the "read-modify-write"
idiom. You'll see this idiom often, regardless of the programming
language, because the hardware requirement is not unusual. Fortunately
Ada defines another aspect that makes the compiler do this for us. We'll
describe it in the next section.

Finally, there are issues to consider regarding the other aspects
described in this section.

If you think about atomic behavior in the context of machine
instructions, loading and storing from/to memory atomically can only be
performed for quantities that are independently addressable.
Consequently, all atomic objects are considered to be specified as
independently addressable too. Aspect specifications and representation
items cannot change that fact. You can expect the compiler to reject any
aspect or representation choice that would prevent this from being true.

Likewise, atomic accesses only make sense on actual memory locations, not
registers. Therefore all atomic objects are volatile objects too,
automatically.

However, unlike volatile objects, the components of an atomic object are
not automatically atomic themselves. You'd have to mark these types or
objects explicitly, using aspect :ada:`Atomic_Components`. Unlike
:ada:`Volatile_Components`, aspect :ada:`Atomic_Components` is thus
useful.

As is usual with Ada programming, you can rely on the compiler to inform
you of problems. The compiler will reject an attempt to specify
:ada:`Atomic` or :ada:`Atomic_Components` for an object or type if the
implementation cannot support the indivisible and independent reads and
updates required.


Aspect :ada:`Full_Access_Only`
------------------------------

Many devices have single-bit flags in the hardware that are not
allocated to distinct bytes. They're packed into bytes and words shared
with other flags. It isn't just individual bits either. Multi-bit fields
that are smaller than a byte, e.g., two 4-bit quantities packed into a
byte, are common. We saw that with the GPIO alternate functions codes earlier.

Ordinarily in Ada we represent such composite hardware interfaces using a
record type. (Sometimes an array type makes more sense. That doesn't
change anything here.) Compared to using bit-patterns, and the resulting
bit shifting and masking in the source code, a record type representation
and the resulting "dot notation" for accessing components is far more
readable. It is also more robust because the compiler does all the work of
retrieving these individual bits and bit-fields for us, doing any shifting
and masking required in the generated code. The loads and stores are done
by the compiler in whatever manner the compiler thinks most efficient.

When the hardware device requires atomic accesses to the memory mapped
to such flags, we cannot let the compiler generate whatever width load
and store accesses it thinks best. If full-word access is required, for
example, then only loads and stores for full words can work. Yet aspect
:ada:`Atomic` only guarantees that the entire object, in this case the
record object, is loaded and stored indivisibly, via one instruction.
The aspect doesn't apply to reads and updates to individual record
components.

In the section on :ada:`Atomic` above, we mentioned that proper access to
individual components of atomic types/objects can be achieved by a
"read-modify-write" idiom. In this idiom, to read a component you first
read into a temporary the entire enclosing atomic object. Then you read
the individual component from that temporary variable. Likewise, to update
an individual component, you start with the same approach but then update
the component(s) within the temporary, then store the entire temporary
back into the mapped atomic object. Applying aspect :ada:`Atomic` to the
enclosing object ensures that reading and writing the temporary will be
atomic, as required.

Using bit masks and bit patterns to access logical components as an
alternative to a record type doesn't change the requirement for the
idiom.

Consider the STM32F4 DMA device. The device contains a 32-bit stream
configuration register that requires 32-bit reads and writes. We can map
that register to an Ada record type like so:

.. code-block:: ada

   type Stream_Config_Register is record
      --  ...
      Direction         : DMA_Data_Transfer_Direction;
      P_Flow_Controller : Boolean;
      TCI_Enabled       : Boolean;  -- transfer complete
      HTI_Enabled       : Boolean;  -- half-transfer complete
      TEI_Enabled       : Boolean;  -- transfer error
      DMEI_Enabled      : Boolean;  -- direct mode error
      Stream_Enabled    : Boolean;
   end record
      with Atomic, Size => 32;

The "confirming" size clause ensures we have declared the type correctly
such that it will fit into 32-bits. There will also be a record
representation clause to ensure the record components are located
internally as required by the hardware. We don't show that part.

The aspect :ada:`Atomic` is applied to the entire record type, ensuring that
the memory mapped to the hardware register is loaded and stored only as
32-bit quantities. In this example it isn't that we want the loads and stores
to be indivisible. Rather, we want the generated machine instructions that load
and store the object to use 32-bit word instructions, even if we are only
reading or updating a component of the object. That's what the hardware
requires for all accesses.

Next we'd use that type declaration to declare one of the components of
an enclosing record type representing one entire DMA "stream":

.. code-block:: ada

   type DMA_Stream is record
      CR   : Stream_Config_Register;
      NDTR : Word;    -- upper half must remain at reset value
      PAR  : Address; -- peripheral address register
      M0AR : Address; -- memory 0 address register
      M1AR : Address; -- memory 1 address register
      FCR  : FIFO_Control_Register;
   end record
      with Volatile, Size => 192;  -- 24 bytes

Hence any individual DMA stream record object has a component named
:ada:`CR` that represents the corresponding configuration register.

The DMA controllers have multiple streams per unit so we'd declare an
array of :ada:`DMA_Stream` components. This array would then be part of
another record type representing a DMA controller. Objects of the
:ada:`DMA_Controller` type would be mapped to memory, thus mapping the
stream configuration registers to memory.

Now, given all that, suppose we want to enable a stream on a given DMA
controller. Using the read-modify-write idiom we would do it like so:

.. code-block:: ada

   procedure Enable
      (Unit   : in out DMA_Controller;
       Stream : DMA_Stream_Selector)
   is
      Temp : Stream_Config_Register;
      --  these registers require 32-bit accesses, hence the temporary
   begin
      Temp := Unit.Streams (Stream).CR;  --  read entire CR register
      Temp.Stream_Enabled := True;
      Unit.Streams (Stream).CR := Temp;  --  write entire CR register
   end Enable;

That works, and of course the procedural interface presented to clients
hides the details, as it should.

To be fair, the bit-pattern approach can express the idiom concisely, as
long as you're careful. Here's the C code to enable and disable a
selected stream:

.. code-block:: c

   #define DMA_SxCR_EN    ((uint32_t)0x00000001)

   /* Enable the selected DMAy Streamx by setting EN bit */
   DMAy_Streamx->CR  |=  DMA_SxCR_EN;

   /* Disable the selected DMAy Streamx by clearing EN bit */
   DMAy_Streamx->CR  &=  ~DMA_SxCR_EN;

The code reads and writes the entire CR register each time it is
referenced so the requirement is met.

Nevertheless, the idiom is error-prone. We might forget to use it at all,
or we might get it wrong in one of the very many places where we need to
access individual components.

Fortunately, Ada provides a way to have the compiler implement the idiom
for us, in the generated code. Aspect :ada:`Full_Access_Only` specifies
that all reads of, or writes to, a component are performed by reading
and/or writing all of the nearest enclosing full access object. Hence we
add this aspect to the declaration of :ada:`Stream_Config_Register` like
so:

.. code-block:: ada

   type Stream_Config_Register is record
      --  ...
      Direction         : DMA_Data_Transfer_Direction;
      P_Flow_Controller : Boolean;
      TCI_Enabled       : Boolean;  -- transfer complete interrupt
      HTI_Enabled       : Boolean;  -- half-transfer complete
      TEI_Enabled       : Boolean;  -- transfer error interrupt
      DMEI_Enabled      : Boolean;  -- direct mode error interrupt
      Stream_Enabled    : Boolean;
   end record
      with Atomic, Full_Access_Only, Size => 32;

Everything else in the declaration remains unchanged.

Note that :ada:`Full_Access_Only` can only be applied to :ada:`Volatile`
types or objects. :ada:`Atomic` types are automatically :ada:`Volatile`
too, so either one is allowed. You'd need one of those aspects anyway
because :ada:`Full_Access_Only` just specifies the accessing instruction
requirements for the generated code when accessing components.

The big benefit comes in the source code accessing the components.
Procedure :ada:`Enable` is now merely:

.. code-block:: ada

   procedure Enable
      (Unit   : in out DMA_Controller;
       Stream : DMA_Stream_Selector)
   is
   begin
      Unit.Streams (Stream).CR.Stream_Enabled := True;
   end Enable;

This code works because the compiler implements the
read-modify-write idiom for us in the generated code.

The aspect :ada:`Full_Access_Only` is new in Ada 2022, and is based on
an implementation-defined aspect that GNAT first defined named
:ada:`Volatile_Full_Access`. You'll see that GNAT aspect throughout the
Arm device drivers in the Ada Drivers Library, available here:
https://github.com/AdaCore/Ada_Drivers_Library. Those drivers were the
motivation for the GNAT aspect.

Unlike the other aspects above, there is no pragma corresponding to the
aspect :ada:`Full_Access_Only` defined by Ada 2022. (There is such a
pragma for the GNAT-specific version named :ada:`Volatile_Full_Access`,
as well as an aspect.)
