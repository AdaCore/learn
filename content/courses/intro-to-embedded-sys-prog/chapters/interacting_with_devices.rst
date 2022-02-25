Interacting with Devices
========================

.. include:: ../../global.txt

Interacting with hardware devices is one of the more frequent activities
in embedded systems programming. It is also one of the most enjoyable
because you can make something happen in the physical world. There's a
reason that making an LED blink is the "hello world" of embedded
programming. Not only is it easy to do, it is surprisingly satisfying. I
suspect that even the developers of "Full Authority Digital Engine
Controllers" (FADEC) |mdash| the computers that are in complete, total
control of commercial airline engines |mdash| have fond memories of
making an LED blink early in their careers. And of course a blinking LED
is a good way to indicate application status, especially if off-board
I/O is limited, which is often the case.

Working at the device register level can be error prone and relatively
slow, in terms of source-lines-of-code (SLOC) produced. That's partly
because the hardware is in some cases complicated, and partly because of
the way the software is written. Using bit masks for setting and
clearing bits is not a readable approach, comparatively speaking.
There's just not enough information transmitted to the reader. It might
be clear enough when written, but will you see it that way months later?
Readability is important because programs are read many more times than
they are written. Also, an unreadable program is more difficult to
maintain, and maintenance is where most money is spent in long-lived
applications. Comments can help, until they are out of date. Then they
are an active hindrance.

For example, what do you think the following code does? This is real
code, where :c:`temp` and :c:`temp2` are unsigned 32-bit integers:

.. code-block:: c

   temp = ((uint32_t)(GPIO_AF) << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
   GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
   temp_2 = GPIOx->AFR[GPIO_PinSource >> 0x03] | temp;
   GPIOx->AFR[GPIO_PinSource >> 0x03] = temp_2;

That's unfair to ask, absent any context. The code configures a general
purpose I/O (GPIO) pin on an Arm microcontroller for one of the
"alternate functions". :c:`GPIOx` is a pointer to a GPIO port,
:c:`GPIO_PinSource` is a GPIO pin number, and :c:`GPIO_AF` is the
alternate function number. But let's say you knew that. Is the code
correct? The longer it takes to know, the less productive you are. (By
the way, the fact that the code above is in C is beside the point. If we
wrote it the same way in Ada it would be equally opaque. There are more
readable approaches.)

Some devices are very simple. In these cases the application may
interact directly with the device without unduly affecting productivity.
For example, there was a board that had a user-accessible rotary switch
with sixteen distinct positions. Users could set the switch to whatever
the application code required, e.g., to indicate some configuration
information. The entire interface to this device was a single read-only
8-bit byte in memory. That's all there was to it: you read the memory
and thus got the numeric setting of the switch. (More on that sort of
thing in a moment.)

More complex devices, however, usually rely on software abstraction to
deal with the complexity. Just as abstraction is a fundamental way to
combat complexity in software, abstraction also can be used to combat
the complexity of driving complex hardware. The abstraction is presented
to users by a software "device driver" that exists as a layer between
the application code and the hardware device. The layer hides the gory
details of the hardware manipulation behind subprograms, types, and
parameters.

We say that the device driver layer is an abstraction because, at the
least, the names of the procedures and functions indicate what they do,
so at the call site you can tell *what* is being done. That's the point
of abstraction: it allows us to focus on what, rather than how. Consider
that GPIO pin configuration code block again. Instead of writing that
block every time we need to configure the alternate function for a pin,
suppose we called a function:

.. code-block:: c

   GPIO_PinAFConfig(USARTx_TX_GPIO_PORT, USARTx_TX_SOURCE, USARTx_TX_AF);

The :c:`GPIO_PinAFConfig` function is part of the GPIO device driver
provided by the STM32 Standard Peripherals Library (SPL). Even though
that's not the best function name conceivable, calls to the function
will be far more readable than the code of the body, and we only have to
make sure the function implementation is correct once. And assuming the
device drivers' subprograms can be inlined, the subprogram call imposes
no performance penalty.

Note the first parameter to the call above: :c:`USARTx_TX_GPIO_PORT`.
There are multiple GPIO ports on an Arm implementation; the vendor
decides how many. In this case one of them has been connected to a USART
(Universal Synchronous Asynchronous Receiver Transmitter), another
device on the Arm chip. When there are multiple devices, good software
engineering suggests that the device driver present a given device as
one of a type. That's what an "abstract data type" (ADT) provides for
software and so the device driver applies the same design. An ADT is
essentially a class, in class-oriented languages. In Ada it is a
private type declared in a package, along with subprograms that take
the type as a parameter.

The Ada Drivers Library (ADL) provided by AdaCore and the Ada community
uses this design to supply Ada drivers for the timers, I2C, A/D and D/A
converters, and other devices common to microcontrollers. Multiple
devices are presented as instances abstract data types. A variety of
development platforms from various vendors are supported, including the
STM32 series boards. The library is available on GitHub for both
non-proprietary and commercial use here:
https://github.com/AdaCore/Ada_Drivers_Library. We are going to use some
of these drivers as illustrations in the following sections.


Non-Memory-Mapped Abstractions
------------------------------

Some devices are connected to the processor on a dedicated bus that is
separate from the memory bus. The Intel processors, for example, used to
have (and may still have) instructions for sending and receiving data on
this bus. These are the "in" and "out" instructions, and their
data-length specific variants.

The original version of Ada defined a package named :ada:`Low_Level_IO`
for such architectures, but there were very few implementations (maybe
just one, known to support the Intel processors). As a result, the
package was actually removed from the language standard. Implementations
could still support the package, it just wouldn't be a standard package.
That's different from constructs that are marked as "obsolescent" by the
standard, e.g., the pragmas replaced by aspects, among other things.
Obsolescent constructs are still part of the standard.

If a given target machine has such I/O instructions for the device bus,
these can be invoked in Ada via machine-code insertions. For example:

.. code-block:: ada

   procedure Send_Control (Device : Port;  Data : Unsigned_16) is
      pragma Suppress (All_Checks);
   begin
      asm ("outw %1, (%0)",
           Inputs  => (Port'Asm_Input("dx",Device), Unsigned_16'Asm_Input("ax",Data)),
           Clobber => "ax, dx");
   end Send_Control;


   procedure Receive_Control (Device : Port;  Data : out Unsigned_16) is
      pragma Suppress (All_Checks);
   begin
      asm ("inw (%1), %0",
           Inputs   => (Port'Asm_Input("dx",Device)),
           Outputs  => (Unsigned_16'Asm_Output("=ax",Data)),
           Clobber  => "ax, dx",
           Volatile => True);
   end Receive_Control;

Applications could use these subprograms to set the frequency of the
Intel PC tone generator, for example, and to turn it on and off. (You
can't do that any more in application code because modern operating
systems don't give applications direct access to the hardware, at least
not by default.)

Although the :ada:`Low_Level_IO` package is no longer part of the language, you
can write this sort of thing yourself, or vendors can do it. That's
possible because the Systems Programming Annex, when implemented,
guarantees fully effective use of machine-code inserts. That means you
can express anything the compiler could emit. The guarantee is important
because otherwise the compiler might "get in the way." For example,
absent the guarantee, the compiler would be allowed to insert additional
assembly language statements in between yours. That can be a real
problem, depending on what your statements do. For instance, if your MCI
assembly statements do something and then check a resulting condition
code, such as the overflow flag, those interleaved compiler-injected
statements might clear that condition code before your code can check
it. Fortunately, the annex guarantees that sort of thing cannot happen.


Memory-Mapped Abstractions
--------------------------

In :doc:`another earlier chapter <./low_level_programming>`,
we said that we could query the address of some object, and we also
showed how to use that result to specify the address of some other
object. We used that capability to create an "overlay," in which two
objects are used to refer to the same memory locations. As we indicated
in that discussion, you would not use the same type for each object
|mdash| the point, after all, is to provide a view of the shared
underlying memory cells that is not already available otherwise. Each
distinct type would provide a distinct view of the memory values, that
is, a set of operations providing some required functionality.

For example, here's an overlay composed of a 32-bit signed integer object
and a 32-bit array object:

.. code-block:: ada

   type Bits32 is array (0 .. 31) of Boolean
      with Component_Size => 1;

   X : Integer_32;
   Y : Bits32 with Address => X'Address;

Because one view is as an integer and the other as an array, we can
access that memory using the two different views' operations. Via the
view as an array object (:ada:`Y`) we can access individual bits of the
memory shared with :ada:`X`. Via the view as an integer (:ada:`X`), we
can do arithmetic on the contents of that memory. (We could have used an
unsigned integer instead of the signed type, and thereby gained the
bit-oriented operations, but that's not the point.)

Very often, though, there is only one Ada object that we place at some
specific address. That's because the Ada object is meant to be the
software interface to some memory-mapped hardware device. In this
scenario we don't have two overlaid Ada objects, we just have one. The
other "object" is the hardware device mapped to that starting address.
Since they are at the same memory location(s), accessing the Ada object
accesses the hardware device.

For a real-world but nonetheless simple example, recall that example of
a rotary switch on the front of our embedded computer that we mentioned
in the introduction. This switch allows humans to provide some very
simple input to the software running on the computer.

.. code-block:: ada

   Rotary_Switch : Unsigned_8 with
     Address => System.Storage_Elements.To_Address (16#FFC0_0801#);

We declare the object and also specify the address, but not by querying
some entity. We already know the address from the hardware
documentation. But we cannot simply use an integer address literal from
that documentation because type :ada:`System.Address` is almost always a
private type. We need a way to compose an :ada:`Address` value from an
integer value. The package :ada:`System.Storage_Elements` defines an
integer representation for :ada:`Address` values, among other useful
things, and a way to convert those integer values to :ada:`Address`
values. The function :ada:`To_Address` does that conversion.

As a result, in the Ada code, reading the value of the variable
:ada:`Rotary_Switch` reads the number on the actual hardware switch.

Note that if you specify the wrong address, it is hard to say what
happens. Likewise, it is an error for an address clause to disobey the
object's alignment. The error cannot be detected at compile time, in
general, because the address is not necessarily known at compile time.
There's no requirement for a run-time check for the sake of efficiency,
since efficiency seems paramount here. Consequently, this misuse of
address clauses is just like any other misuse of address clauses |mdash|
execution of the code is erroneous, meaning all bets are off. You need
to know what you're doing.

What about writing to the variable? Is that meaningful? In this
particular rotary switch case, no. It is effectively read-only memory.
But for some other device it very well could be meaningful, certainly.
It depends on the hardware. But in this case, assigning a value to the
:ada:`Rotary_Switch` variable would have no effect, which could be confusing to
programmers. It looks like a variable, after all. We wouldn't declare it
as a constant because the human user could rotate the switch, resulting
in a different value read. Therefore, we would hid the Ada variable
behind a function, obviating the entire question. Clients of the
function can then use it for whatever purpose they require, e.g., as the
unique identifier for a computer in a rack.

Let's talk more about the type we use to represent a memory-mapped
device. As we said, that type defines the view we have for the object,
and hence the operations we have available for accessing the underlying
mapped device.

We choose the type for the representative Ada variable based on the
interface of the hardware mapped to the memory. If the interface is a
single monolithic register, for example, then an integer, either signed
or unsigned, and of the necessary size, will suffice. But suppose the
interface is several bytes wide, and some of the bytes have different
purposes from the others? In that case, a record type is the obvious
solution, with distinct record components dedicated to the different
parts of the hardware interface. We could use individual bits too, of
course, if that's what the hardware does. Ada is particularly good at
this fine-degree of representation because record components of any
types can be specified in the layout, down to the bit level, within the
record.

In addition, we might want to apply more than one type, at any one time,
to a given memory-mapped device. Doing so allows the client code some
flexibility, or it might facilitate an internal implementation. For
example, the STM32 boards from ST Microelectronics include a 96-bit
device unique identifier on-board. Much like the rotary switch example
above, the unique identifier is located at a fixed memory location.
(Unlike the rotary switch, the value is a constant.) The primary
difference in this case is that we provide two different views |mdash|
types |mdash| for this unique identifier. One type provides the
identifier as a String containing twelve characters, whereas another
type provides the value as an array of three 32-bit unsigned words
(i.e., 12 bytes). The two types are applied by two overloaded functions
that are distinguished by their return type.

.. code-block:: ada

   package STM32.Device_Id is

      subtype Device_Id_Image is String (1 .. 12);

      function Unique_Id return Device_Id_Image;

      type Device_Id_Tuple is array (1 .. 3) of UInt32
        with Component_Size => 32;

      function Unique_Id return Device_Id_Tuple;

   end STM32.Device_Id;

The subtype :ada:`Device_Id_Image` is the view of the 96-bits as an
array of twelve characters. (Using type :ada:`String` here isn't essential. We
could have defined an array of bytes instead of :ada:`Character`.) Likewise,
subtype :ada:`Device_Id_Tuple` is the view of the 96-bits as an array of
three 32-bit unsigned integers. Clients can then choose how they want to
view the unique id by choosing which function to call.

In the package body we implement the functions as two ways to access the
same shared memory:

.. code-block:: ada

   with System;

   package body STM32.Device_Id is

      ID_Address : constant System.Address := System'To_Address (16#1FFF_7A10#);

      function Unique_Id return Device_Id_Image is
         Result : Device_Id_Image with
           Address => ID_Address, Import;
      begin
         return Result;
      end Unique_Id;

      function Unique_Id return Device_Id_Tuple is
         Result : Device_Id_Tuple with
           Address => ID_Address, Import;
      begin
         return Result;
      end Unique_Id;

   end STM32.Device_Id;

The GNAT-defined attribute :ada:`System'To_Address` in the declaration
of :ada:`ID_Address` is the same as the function
:ada:`System.Storage_Elements.To_Address` except that, if the argument is
static, the function result is static. This means that such an
expression can be used in contexts (e.g., preelaborable packages) which
require a static expression and where the function call could not be
used (because the function call is always non-static, even if its argument
is static).

Earlier we indicated that the bit-pattern oriented implementation of the
GPIO function could be expressed differently, resulting in more
readable, therefore maintainable, code. The fact that the code is in C
is irrelevant; the same approach in Ada would not be any better. Here's
the complete code for the function body:

.. code-block:: c

   void GPIO_PinAFConfig(GPIO_TypeDef* GPIOx, uint16_t GPIO_PinSource, uint8_t GPIO_AF)
   {
     uint32_t temp = 0x00;
     uint32_t temp_2 = 0x00;

     /* Check the parameters */
     assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
     assert_param(IS_GPIO_PIN_SOURCE(GPIO_PinSource));
     assert_param(IS_GPIO_AF(GPIO_AF));

     temp = ((uint32_t)(GPIO_AF) << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
     GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
     temp_2 = GPIOx->AFR[GPIO_PinSource >> 0x03] | temp;
     GPIOx->AFR[GPIO_PinSource >> 0x03] = temp_2;
   }

The problem, other than the magic numbers (some named constants would
have helped), is that the code is doing nearly all the work instead of
off-loading it to the compiler. Partly that's because in C we cannot
declare a numeric type representing a 4-bit quantity, so everything is done in
terms of machine units, in this case 32-bit unsigned integers.

Why do we need 4-bit values? At the hardware level, each memory-mapped
GPIO port has a sequence of 16 4-bit quantities, one for each of the 16
pins on the port. Those 4-bit quantities specify the "alternate
functions" that the pin can take on, if needed. The alternate functions
allow a given pin to do more than act as a single discrete I/O pin. For
example, a pin could be connected to the incoming lines of a USART. We
use the configuration routine to apply the specific code representing
the alternate function required for our application.

These 16 4-bit alternate function code fields are contiguous in the
register (hence memory) so we can represent them as an array with a
total size of 64-bits (i.e., 16 times 4). In the C version this array
has as two components of type :c:`uint32_t` so it must compute where the
corresponding 4-bit value for the pin is located within those two words.
In contrast, the Ada version of the array has components of the 4-bit
type, rather than two 32-bit components, and simply uses the pin number
as the index. The resulting Ada procedure body is extremely simple:

.. code-block:: ada

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_Alternate_Function_Code)
   is
   begin
      Port.AFR (Pin) := AF;
   end Configure_Alternate_Function;

In the Ada version, :ada:`AFR` is a component within the
:ada:`GPIO_Port` record type, much like in the C code's struct. However,
Ada allows us to declare a much more descriptive set of types, and it is
these types that allows the developer to off-load the work to the compiler.

First, in Ada we can declare a 4-bit numeric type:

.. code-block:: ada

   type Bits_4 is mod 2**4 with Size => 4;

The :ada:`Bits_4` type was already globally defined elsewhere so we just
derive our 4-bit "alternate function code" type from it. Doing so allows the
compiler to enforce simple strong typing so that the two value spaces
are not accidentally mixed. This approach also increases understanding
for the reader:

.. code-block:: ada

   type GPIO_Alternate_Function_Code is new Bits_4;
   --  We cannot use an enumeration type because there are duplicate binary
   --  values

Hence type :ada:`GPIO_Alternate_Function_Code` is a copy of
:ada:`Bits_4` in terms of operations and values, but is not the same
type as :ada:`Bits_4` so the compiler will keep them separate for us.

We can then use that type as the array component type for the representation
of the :ada:`AFR`:

.. code-block:: ada

   type Alternate_Function_Fields is array (GPIO_Pin) of GPIO_Alternate_Function_Code
     with Component_Size => 4, Size => 64;  -- both in units of bits

Note that we can use the GPIO :ada:`Pin` parameter directly as the index into
the array type, obviating any need to massage the :ada:`Pin` value in
the procedure. That's possible because the type :ada:`GPIO_Pin` is an
enumeration type.

Type :ada:`Alternate_Function_Fields` is then used to declare the
:ada:`AFR` record component in the :ada:`GPIO_Port` record type:

.. code-block:: ada

   type GPIO_Port is limited record
      MODER      : Pin_Modes_Register;
      OTYPER     : Output_Types_Register;
      Reserved_1 : Half_Word;
      OSPEEDR    : Output_Speeds_Register;
      PUPDR      : Resistors_Register;
      IDR        : Half_Word;       --  input data register
      Reserved_2 : Half_Word;
      ODR        : Half_Word;       --  output data register
      Reserved_3 : Half_Word;
      BSRR_Set   : Half_Word;       --  bit set register
      BSRR_Reset : Half_Word;       --  bit reset register
      LCKR       : Word with Atomic;
      AFR        : Alternate_Function_Fields;
      Reserved_4 : Reserved_246x32;
   end record with
     Size => 16#400# * 8;

   for GPIO_Port use record
      MODER      at 0  range 0 .. 31;
      OTYPER     at 4  range 0 .. 15;
      Reserved_1 at 6  range 0 .. 15;
      OSPEEDR    at 8  range 0 .. 31;
      PUPDR      at 12 range 0 .. 31;
      IDR        at 16 range 0 .. 15;
      Reserved_2 at 18 range 0 .. 15;
      ODR        at 20 range 0 .. 15;
      Reserved_3 at 22 range 0 .. 15;
      BSRR_Set   at 24 range 0 .. 15;
      BSRR_Reset at 26 range 0 .. 15;
      LCKR       at 28 range 0 .. 31;
      AFR        at 32 range 0 .. 63;
      Reserved_4 at 40 range 0 .. 7871;
   end record;

These declarations define a record type that matches the content and
layout of the STM32 GPIO Port memory-mapped device.

Let's compare the two procedure implementations again. Here they are, for
convenience:

.. code-block:: c

   void GPIO_PinAFConfig(GPIO_TypeDef* GPIOx, uint16_t GPIO_PinSource, uint8_t GPIO_AF)
   {
     uint32_t temp = 0x00;
     uint32_t temp_2 = 0x00;

     /* Check the parameters */
     assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
     assert_param(IS_GPIO_PIN_SOURCE(GPIO_PinSource));
     assert_param(IS_GPIO_AF(GPIO_AF));

     temp = ((uint32_t)(GPIO_AF) << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
     GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF << ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4)) ;
     temp_2 = GPIOx->AFR[GPIO_PinSource >> 0x03] | temp;
     GPIOx->AFR[GPIO_PinSource >> 0x03] = temp_2;
   }

.. code-block:: ada

   procedure Configure_Alternate_Function
     (Port : in out GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_Alternate_Function_Code)
   is
   begin
      Port.AFR (Pin) := AF;
   end Configure_Alternate_Function;

Which one is correct? Both. But clearly, the Ada version is far simpler,
so much so that it is immediately obvious that it is correct. Not so for
the coding approach used in the C version, comparatively speaking. It is
true that the Ada version required a couple more type declarations, but
those make the procedure body far simpler. That resulting simplicity is
a reflection of the balance between data structures and executable
statements that we should always try to achieve.

Of course, the underlying hardware likely has no machine-supported 4-bit
unsigned type so larger hardware numeric types are used. Hence there are
shifts and masking being done in the Ada version as well, but they do
not appear in the source code. The developer has let the compiler do
that work. An additional benefit of this approach is that the compiler
will change the shifting and masking code for us if we change the
explicit type declarations.

Why is simplicity so important? Simplicity directly increases
understandability, which directly affects correctness and
maintainability, which greatly affects the economic cost of the
software. In large, long-lived projects, maintenance is by far the
largest economic cost driver. In high-integrity applications,
correctness is essential. Therefore, doing anything reasonable to keep
the code as simple as possible is usually worth the effort. In some
projects the non-functional requirements, especially performance, can
dictate less simple code, but that won't apply to all of the code. Where
possible, simplicity rules.

One more point about the GPIO ports. There are as many of these ports as
the Arm microcontroller vendor decides to implement. And as we said,
they are memory-mapped, at addresses specified by the vendor. If the
memory used by all the ports is contiguous, we can conveniently use an
array of the :ada:`GPIO_Port` record type to represent all the ports
implemented. We would just set the array object's address at the address
specified for the first port object in memory. Then, normal array
indexing will provide access to any given port in the memory-mapped
hardware. This array approach requires each array component |mdash| the
:ada:`GPIO_Port` record type |mdash| to be the right size so that all
the array components start on addresses corresponding to the start of
the next port in hardware.

That starting address correspondence for the array components is
obtained automatically as long as the record type includes all the
memory used by any individual device. In that case the next array
component will indeed start at an address matching the next device in
hardware. Note that this assumes the first array component matches the
address of the first hardware device in memory. The first array
component is at the same address as the whole array object itself (a
fact that is guaranteed by the language), so the array address must be
set to whatever the vendor documentation specified for the first port.

However, in some cases the vendor will leave gaps of unused memory for
complicated memory-mapped objects like the ports. They do so for the
sake of future expansion of the implementation, e.g., to add new
features or capacity. The gaps are thus between consecutive hardware
devices.

These gaps are not necessarily included in the memory layout documented
for the device. You should check, therefore, that the documented
starting addresses of the second and subsequent array components are
what you will get with a simple array object having components of that
record type.

To represent the gap, an "extra", unused record component can be added,
with the necessary location and size specified within the record type,
so that the unused memory is included in the software representation.
Then each port, i.e., array component, will start at the right address
(again, as long as the first one does). Telling the compiler, and future
maintainers, that this extra component is not meant to be referenced by
the software would not hurt. You can use the pragma or aspect
:ada:`Unreferenced` for that purpose.

An equally good |mdash| arguably better |mdash| approach would be to set
the size of the record type to the larger value, i.e., the nominal size
plus the gap. Or, set the array component size to that same combined
value. The reason to prefer setting the record type's size (to the
larger value) is that doing so would also support individual object
declarations, as opposed to using an array. Future maintenance might
cause such a change; at least it is possible. Setting the record size to
the larger value would ensure that, given some declared variable of the
record type, no subsequently declared variables would occupy the unused
part of that record object. Those unrelated variables might not be a
problem until the vendor started to use that previously unused space
after a hardware update. If that happened, identifying the problem would
be slow and expensive. Better to associate the additional space with the
record type, as opposed to doing it in the array type declaration.
Functionally, either approach would work, but the software engineering
aspects argue for setting the record size. In any case we must document
the additional space allocated, for the sake of future maintainers (one
of whom might be yourself).

Dynamic Address Conversion
--------------------------

In the overlay example there were two distinct Ada objects, of two different
types, sharing one (starting) address. The overlay provides two
views of the memory at that address because there are two types
involved. In this idiom the address is known when the code is written,
either because it is a literal value specified in some hardware spec, or
it is simply the address of the other object (in which case the actual
address value is neither known nor relevant).

When there are several views required, declaring multiple overlaid
variables at the same address absolutely can work, but can be less
convenient than an alternative idiom. The alternative is to convert an
address value to a value of an access type. Dereferencing the resulting
access value provides a view of the memory corresponding to the
designated type, starting at the converted address value.

For example, perhaps a networking component is given a buffer |mdash| an
unconstrained array of bytes |mdash| representing a received message. A
subprogram is called with the buffer as a parameter, or the parameter
can be the address of the buffer. If the subprogram must interpret this
array via different views, this alternative approach works well. We
could have an access type designating a message preamble, for example,
and convert the first byte's address into such an access value.
Dereferencing the conversion gives the preamble value. Likewise, the
subprogram might need to compute a checksum over some of the bytes, so a
different view, one of an array of a certain set size, could be used.
Again, we could do that with overlaid objects but the alternative can be
more convenient.

Here's a simple concrete example to illustrate the approach. Suppose we
want to have a utility to swap the two bytes at any arbitrary address.
Here's the declaration:

.. code-block:: ada

   procedure Swap2 (Location : System.Address);

Callers pass the address of an object intended to have its (first) two
bytes swapped:

.. code-block:: ada

   Swap2 (Z'Address);

In the call, :ada`Z` is of type :ada`Interfaces.Integer_16`, for
example, or :ada`Unsigned_16`, or even something bigger as long as you
only care about swapping the first two bytes.

The incomplete implementation using the conversion idiom could be like so:

.. code-block:: ada

   procedure Swap2 (Location : System.Address) is
      X : Word renames To_Pointer(Location).all;
   begin
      X :=  Shift_Left (X, 8) or Shift_Right (X, 8);
   end Swap2;

The declaration of :ada:`X` is the pertinent part.

In the declaration, :ada:`X` is of type :ada:`Word`, a type (not yet
shown) derived from :ada:`Interfaces.Unsigned_16`. Hence :ada:`X` can
have the inherited shift and logical :ada:`or` operations applied.

The :ada:`To_Pointer(Location)` part of the declaration is a function
call. The function returns the conversion of the incoming address value
in :ada:`Location` into an access value designating :ada:`Word` values.
We'll explain how to do that momentarily. The :ada:`.all` explicitly
dereferences the access value resulting from the function call.

Finally, :ada:`X` renames the :ada:`Word` value designated by the
converted access value. (Functions return objects, so this renaming is
allowed.) The benefit of the renaming, in addition to the simpler name,
is that the function is only called once, and the access value deference
is only evaluated once.

Now for the rest of the implementation not shown earlier.

.. code-block:: ada

   type Word is new Interfaces.Unsigned_16;

   package Word_Ops is new System.Address_To_Access_Conversions (Word);
   use Word_Ops;

:ada:`System.Address_To_Access_Conversions` is a language-defined
generic package that provides just two functions: one to convert an
address value to an access type, and one to convert in the opposite
direction:

.. code-block:: ada

   generic
      type Object (<>) is limited private;
   package System.Address_To_Access_Conversions is

      type Object_Pointer is access all Object;

      function To_Pointer (Value : Address)        return Object_Pointer;
      function To_Address (Value : Object_Pointer) return Address;

      pragma Convention (Intrinsic, To_Pointer);
      pragma Convention (Intrinsic, To_Address);

   end System.Address_To_Access_Conversions;

:ada:`Object` is the generic formal type parameter, i.e., the type we
want our converted addresses to designate via the type
:ada:`Object_Pointer`. In the byte-swapping example, the type
:ada:`Word` was passed to :ada:`Object` in the instantiation.

The access type used by the functions is :ada:`Object_Pointer`,
declared along with the functions. :ada:`Object_Pointer` designates
values of the type used for the generic actual parameter, in this case
:ada:`Word`.

Note the pragma :ada:`Convention` applied to each function, indicating
that there is no actual function call involved; the compiler emits the
code directly, if any code is actually required. Otherwise the compiler
just treats the incoming :ada:`Address` bits as a value of type
:ada:`Object_Pointer`.

The instantiation specifies type :ada:`Word` as the generic actual type
parameter, so now we have a set of functions for that type, in
particular :ada:`To_Pointer`.

Let's look at the code again, this time with the additional declarations:

.. code-block:: ada

   type Word is new Interfaces.Unsigned_16;

   package Word_Ops is new System.Address_To_Access_Conversions (Word);
   use Word_Ops;

   procedure Swap2 (Location : System.Address) is
      X : Word renames To_Pointer(Location).all;
   begin
      X :=  Shift_Left (X, 8) or Shift_Right (X, 8);
   end Swap2;

:ada:`Word_Ops` is the generic instance, followed immediately by a
:ada:`use` clause so that we can refer to the visible content of the
package instance conveniently.

In the reaming expression, :ada:`To_Pointer(Location)` converts the
incoming address in :ada:`Location` to a pointer designating the
:ada:`Word` at that address. The :ada:`.all` dereferences the resulting
access value to get the designated :ada:`Word` value. Hence :ada:`X`
refers to that two-byte value in memory.

We could very likely achieve the same affect by replacing the call to
the function in :ada:`To_Pointer` with a call to an instance of
:ada:`Ada.Unchecked_Conversion`. The conversion would still be between
an access type and a value of type :ada:`System.Address`, but the access type
would require declaration by the user. In both cases there would be an
instantiation of a language-defined facility, so there's not much saving
in lines of source code, other than the access type declaration. Because
:ada:`System.Address_To_Access_Conversions` is explicitly intended for
this purpose, good style suggests its use in preference to unchecked
conversion, but both approaches are common in production code.

In either case, the conversion is not required to work, although in
practice it will, most of the time. Representing an access value as an
address value is quite common because it matches the typical underlying
hardware's memory model. But even so, a single address is not
necessarily sufficient to represent an access value for any given
designated type. In that case problems arise, and they are difficult to
debug.

For example, in GNAT, access values designating values of unconstrained
array types, such as :ada:`String`, are represented as two addresses,
known as "fat pointers". One address points to the bounds for the
specific array object, since they can vary. The other address designates
the characters. Therefore, conversions of a single address to an access
value requiring fat pointers will not work, using either unchecked
conversions or :ada:`System.Address_To_Access_Conversions`. (There is a
way, however, to tell GNAT to use a single address value, but it is an
explicit step in the code. Once done, though, conversions would then
work correctly.)

To at least warn users of the possibility of this problem, the GNAT
implementation of :ada:`System.Address_To_Access_Conversions` includes
the following:

.. code-block:: ada

     pragma Compile_Time_Warning
        (Object'Unconstrained_Array,
         "Object is unconstrained array type" & ASCII.LF &
         "To_Pointer results may not have bounds");

:ada:`Object` is the generic formal type parameter, i.e., the type we
want our converted addresses to designate via the type
:ada:`Object_Pointer`. The pragma instructs the compiler to issue the
indicated warning text if :ada:`Object` is an unconstrained array type,
e.g. :ada:`String`. The instantiation will still compile, unless you
also have the compiler treat warnings as errors. Treating them as errors
is not a bad idea.


Address Arithmetic
------------------

Part of "letting the compiler do the work for you" is not doing address
arithmetic in the source code if you can avoid it. Instead, for
instance, use the normal "dot notation" to reference components, and let
the compiler compute the offsets to those components. Doing so may
require an additional view, but you've seen how to do that now.

That said, sometimes address arithmetic is the most direct expression of
what you're trying to implement. For example, when implementing your own
memory allocator, you'll need to do address arithmetic.

Earlier in this section we mentioned the package
:ada:`System.Storage_Elements`, for the sake of the function that
converts integer values to address values. The package also defines
functions that provide address arithmetic. These functions work in terms
of type :ada:`System.Address` and the package-defined type
:ada:`Storage_Offset`. The type :ada:`Storage_Offset` is an integer type
with an implementation-defined range. As a result you can have positive
and negative offsets, as needed. Addition and subtraction of offsets
to/from addresses is supported, as well as the :ada:`mod` operator.

Combined with package :ada:`System` (for type :ada:`System.Address`),
the functions and types in this package provide the kinds of address
arithmetic other languages provide. Nevertheless, you should prefer
having the compiler do these computations for you, if possible.

Here's an example illustrating the facilities. The procedure defines an
array of record values, then walks the array, printing the array
components as it goes. It is not the way to really write this code.

.. code-block:: ada

   with Ada.Text_IO;               use Ada.Text_IO;
   with System.Storage_Elements;   use System.Storage_Elements;
   with System.Address_To_Access_Conversions;

   procedure Demo_Address_Arithmetic is

      type R is record
         X : Integer;
         Y : Integer;
      end record;

      R_Size : constant Storage_Offset := R'Object_Size / System.Storage_Unit;

      Objects : aliased array (1 .. 10) of aliased R;     --  arbitrary bounds

      Objects_Base : System.Address;

      Offset : Storage_Offset;

      --  display the object of type R at the address specified by Location
      procedure Display_R (Location : in System.Address) is

         package R_Pointers is new System.Address_To_Access_Conversions (R);
         use R_Pointers;

          Value : R renames To_Pointer(Location).all;
         --  The above converts the address to a pointer designating an R value
         --  and dereferences it, using the name Value to refer to the
         --  dereferenced R value.
      begin
         Put (Integer'Image (Value.X));
         Put (", ");
         Put (Integer'Image (Value.Y));
         New_Line;
      end Display_R;

   begin
      Objects := ((0,0), (1,1), (2,2), (3,3), (4,4),
                  (5,5), (6,6), (7,7), (8,8), (9,9));

      Offset := 0;
      Objects_Base := Objects'Address;

      --  walk the array of R objects, displaying each one individually by
      --  adding the offset to the base address of the array
      for K in Objects'Range loop
         Display_R (Objects_Base + Offset);
         Offset := Offset + R_Size;
      end loop;
   end Demo_Address_Arithmetic;

Seriously, this is just for the purpose of illustration. It would be
much better to just index into the array directly.


General-Purpose Code Generators
-------------------------------

In :doc:`another chapter <./multi_language_development>`,
we mentioned that the best way to get a
specific set of machine instructions emitted from the compiler is to
write them ourselves, in the Ada source code, using machine-code
insertions (MCI). The rationale was that the code generator will make
reasonable assumptions, including the assumption that performance is of
uppermost importance, but these assumptions can conflict with device
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
metion it later.) For the other aspects, the pragmas existed first and,
although obsolescent, remain part of the language and supported. There's
no need to change your existing source code using the pragmas to use the
aspects instead, unless you need to change it for some other reason.

As this is an introduction, we will not go into absolutely all the details,
but will, instead, give a sense of what the language provides, and why.


Aspect :ada:`Independent`
^^^^^^^^^^^^^^^^^^^^^^^^^

To interface with a memory-mapped device, there is an Ada object of an
appropriate type that is mapped to one or more bytes of memory. The
software interacts with the device by reading and/or writing to the
memory locations mapped to the device, using the operations defined by
the type and performing assignments in terms of Ada semantics.

Some memory-mapped devices can be directly represented by a single
scalar value, usually of some signed or unsigned numeric type. Other,
more sophisticated devices almost always involve several distinct
controls and values. Therefore, representation in the software as a
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
common. It could be a single numeric bit, just as easily. Mapping that
bit to a Boolean value tends to make for more readable code. The point
is that the machine cannot usually read/write single bits in memory, so
the generated code will almost certainly read a byte to get the enclosed
single-bit Boolean component. It might use a larger sized access too, a
half-word or word. Then the generated code masks off the bits that are
not of interest and does some shifts to get the desired value.

Reading and writing more than just the single component accessed in the
source code can cause a problem. In particular, some devices react to
being read or written by doing something physical in the hardware.
That's the device designer's intent for the software.

Therefore, to prevent these "extra" bytes from being accessed, we need a
way to tell the compiler that we need the read or write accesses for the
given object to be independent of the surrounding memory. If the
compiler cannot do so, we'll get an error and the compilation will fail.
That beats debugging every time.

Therefore, the aspect :ada:`Independent` specifies whether the code
generated by the compiler may access the memory surrounding some object
when reading or writing that object. It declares that a type, object, or
component must be independently addressable by the hardware. If applied
to a type, it applies to all components and objects of the type.
Likewise, aspect :ada:`Independent_Components` declares that the
components of an array or record type are independently addressable. The
compiler will reject the aspect if independent access is not possible
for the type/object in question.

For example, if we try to mark each Boolean component of a record type
as :ada:`Independent` we can do so, but that will require that each
component is a byte in size (or whatever the smallest addressable unit
happens to be on this machine). We cannot make each Boolean component
occupy one bit within a given byte if we want them to be independently
accessed.

.. code-block:: ada

   package P is

      type R is record
         B0 : Boolean with Independent;
         B1 : Boolean with Independent;
         B2 : Boolean with Independent;
         B3 : Boolean with Independent;
         B4 : Boolean with Independent;
         B5 : Boolean with Independent;
      end record with
        Size => 8;

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
i.e, 8 bits per component. That's because the smallest quantity this
machine can independently address is an 8-bit byte.

But if we don't really need the individual bits to be independently
accessed |mdash| and let's hope no hardware designer would define such a
device |mdash| then we have more flexibility. We could, for example,
require that objects of the entire record type be independently
accessible. To do so, we can apply the aspect to the entire type:

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
all tagged types, introduces the same issue, albeit indirectly, and in
this case for the entire record type. When a record component is of some
type that is passed by-reference, the only sensible approach is to pass
the entire record object by reference. So now the compiler will complain
about the size of the entire record type. (This applies to arrays types
too.) The problem is that the by-reference component needs to be
represented in a manner that supports constructing valid pointers,
"under the hood" so to speak, because that's what passing by-reference
does.

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
^^^^^^^^^^^^^^^^^^^^^^

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
implement assigning a value to it. The update to the variable has only
an internal effect. With warnings enabled we'll receive notice from
the compiler, but they're just warnings.

However, writing to the file is an external effect because the file
persists beyond the end of the program's execution. The optimizer (when
enabled) would be free to remove any access to the variable
:ada:`Silly`, but not the write to the file.

We can make the compiler recognize that a software object is part of an
external effect by applying the aspect :ada:`Volatile`. (Aspect
:ada:`Atomic` is pertinent too. More in a moment.) As a result, the
compiler will generate load or store instructions for every read or
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

If we compile the above, even with warnings enabled we won't get any
because the compiler is now required to generate the assignment for
:ada:`Silly`.

The variable :ada:`Silly` is not even a memory-mapped object, but
remember that we said these aspects are important to the tasking context
too, for shared variables. We're ignoring that context in this course.

Perhaps the issue is that you want to write some Ada code that interacts
with a memory-mapped device, and you want to have exactly the load and
store instructions generated that match those of the Ada code. The
memory-mapped device will be volatile, in other words, but some other
variables might need to be volatile too.

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

   procedure Lock (Port : in out GPIO_Port;  Pin : GPIO_Pin) is
      Temp : Word with Volatile;
   begin
      --  set the lock control bit and the pin bit, clear the others
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
hardware locking protocol.

This high-level coding approach will work, and is simple enough that not
using MCIs is not unreasonable. However, what really argues against it
is that the correct sequence requires the optimizer to remove all the
other cruft that the code generator would otherwise include. (The gcc
code generator used by the GNAT compiler generates initially poor code,
by design, relying on the optimizer to clean it up.) In other words,
we've told the optimizer not to change or add loads and stores for
:ada:`Temp`, but without the optimizer enabled the code generator
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

For an array type (not a record type), a related aspect
:ada:`Volatile_Components` declares that the components of the array
type are volatile. However, if the :ada:`Volatile` aspect is specified,
then the :ada:`Volatile_Components` aspect is automatically applied too,
and vice versa. Thus components of array types are covered
automatically.

If an object (of an array type or record type) is marked volatile then
so are all of its subcomponents, even if the type itself is not marked
volatile.

Therefore aspects :ada:`Volatile` and :ada:`Volatile_Components` are
equivalent. In fact, :ada:`Volatile_Components` is superfluous. The
language provides the :ada:`Volatile_Components` aspect only to give
symmetry with the :ada:`Atomic_Components` and
:ada:`Independent_Components` aspects. You can simply apply
:ada:`Volatile` and be done with it.

Finally, note that applying aspect :ada:`Volatile` does not implicitly
apply :ada:`Independent`, although you can specify it explicitly if need
be.


Aspect :ada:`Atomic`
^^^^^^^^^^^^^^^^^^^^

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
is accessed with one instruction per access. For a 16-bit object, all
16-bits are loaded and stored at once. For a 32-bit object, all 32-bits
at once, and so on. The upper limit is the size of the largest machine
scalar that the processor can manipulate with one instruction, as
defined by the target processor. The typical lower bound is 8, for a
byte-addressable machine.

Therefore, within the record type representing a GPIO port, we include
the lock register component and apply the aspect :ada:`Atomic`:

.. code-block:: ada

   type GPIO_Port is limited record
      --  ...
      LCKR       : UInt32 with Atomic;
      --  ...
   end record with
     --  ...
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

If you think about about atomic behavior in the context of machine
instructions, loading and storing from/to memory atomically can only be
performed for quantities that are independently addressable. For
example, individual bits don't have distinct addresses on the typical
machine. Consequently, all atomic objects are considered to be specified as
independently addressable too. Aspect_specifications and representation
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Many devices have single-bit flags in the hardware that are not
allocated to distinct bytes. They're packed into bytes and words shared
with other flags. It isn't just individual bits either. Multi-bit fields
that are smaller than a byte, e.g., two 4-bit quantities packed into a
byte, are common.

Ordinarily in Ada we represent such composite hardware interfaces using a
record type. (Sometimes an array type makes more sense. That doesn't
change anything here.) Compared to using bit-patterns, and the resulting
bit shifting and masking in the source code, a record type representation
and the resulting "dot notation" for accessing components is the far more
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
      TCI_Enabled       : Boolean;  -- transfer complete interrupt enabled
      HTI_Enabled       : Boolean;  -- half-transfer complete enabled
      TEI_Enabled       : Boolean;  -- transfer error interrupt enabled
      DMEI_Enabled      : Boolean;  -- direct mode error interrupt enabled
      Stream_Enabled    : Boolean;
   end record
      with Atomic, Size => 32;

The "confirming" size clause ensures we have declared the type correctly
such that it will fit into 32-bits. There will also be a record
representation clause to ensure the record components are located
internally as required by the hardware. We don't show that part.

The aspect :ada:`Atomic` is applied to the entire record type, ensuring that
the memory mapped to the hardware register is loaded and stored only as
32-bit quantities. It isn't that we want the loads and stores to be
indivisible. Rather, we want the generated machine instructions that load
and store the object to use 32-bit word instructions, even if we are only
reading or updating a component of the object. That's what the hardware
requires.

Next we'd use that type declaration to declare one of the components of
an enclosing record type representing one entire DMA "stream":

.. code-block:: ada

   type DMA_Stream is record
      CR   : Stream_Config_Register;
      NDTR : Word;    -- note that the upper half must remain at reset value
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
      Temp := Unit.Streams (Stream).CR;  --  read entire register
      Temp.Stream_Enabled := True;
      Unit.Streams (Stream).CR := Temp;  --  write entire register
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
      TCI_Enabled       : Boolean;  -- transfer complete interrupt enabled
      HTI_Enabled       : Boolean;  -- half-transfer complete enabled
      TEI_Enabled       : Boolean;  -- transfer error interrupt enabled
      DMEI_Enabled      : Boolean;  -- direct mode error interrupt enabled
      Stream_Enabled    : Boolean;
   end record
      with Atomic, Full_Access_Only, Size => 32;

Everything else in the declaration remains unchanged.

Note that :ada:`Full_Access_Only` can only be applied to :ada:`Volatile`
types or objects. :ada`Atomic` types are automatically :ada:`Volatile`
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
