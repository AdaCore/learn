Interacting with Devices
========================

.. include:: ../../../global.txt

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

   temp = ((uint32_t)(GPIO_AF) <<
             ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
   GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF <<
             ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
   temp_2 = GPIOx->AFR[GPIO_PinSource >> 0x03] | temp;
   GPIOx->AFR[GPIO_PinSource >> 0x03] = temp_2;

That's unfair to ask, absent any context. The code configures a general
purpose I/O (GPIO) pin on an Arm microcontroller for one of the
"alternate functions". :c:`GPIOx` is a pointer to a GPIO port,
:c:`GPIO_PinSource` is a GPIO pin number, and :c:`GPIO_AF` is the
alternate function number. But let's say you knew that. Is the code
correct? The longer it takes to know, the less productive you are.

The fact that the code above is in C is beside the point. If we wrote it the
same way in Ada it would be equally opaque, if not more so. There are
simpler approaches. Judicious use of record and array types is one. We'll
say more about that later, but the underlying idea is to let the compiler do
as much work for us as possible. For example, the data structures used in
the code above require explicit shifting whenever they are accessed. If we
can avoid that at the source code level |mdash| by having the compiler do it
for us |mdash| we will have simplified the code considerably. Furthermore,
letting the compiler do the work for us makes the code more maintainable
(which is where the money is). For example, if the code does the shifting
explicitly and the data structures are changed, we'll have to change the
number of bits to shift left or right. Constants will help there, but we
still have to remember to change them; the compiler won't complain if we
forget. In contrast, if we let the compiler do this shifting for us, the
amounts to shift will be changed automatically.

Some devices are very simple. In these cases the application may
interact directly with the device without unduly affecting productivity.
For example, there was a board that had a user-accessible rotary switch
with sixteen distinct positions. Users could set the switch to whatever
the application code required, e.g., to indicate some configuration
information. The entire software interface to this device consisted of a single
read-only 8-bit byte in memory. That's all there was to it: you read the memory
and thus got the numeric setting of the switch.

More complex devices, however, usually rely on software abstraction to
deal with the complexity. Just as abstraction is a fundamental way to
combat complexity in software, abstraction also can be used to combat
the complexity of driving sophisticated hardware. The abstraction is presented
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
(Universal Synchronous Asynchronous Receiver Transmitter), an external
device for sending and receiving serial data. When there are multiple devices,
good software engineering suggests that the device driver present a given
device as one of a type. That's what an "abstract data type" (ADT) provides for
software and so the device driver applies the same design. An ADT is
essentially a class, in class-oriented languages. In Ada, an ADT is represented
as a private type declared in a package, along with subprograms that take
the type as a parameter.

The Ada Drivers Library (ADL) provided by AdaCore and the Ada community
uses this design to supply Ada drivers for the timers, I2C, A/D and D/A
converters, and other devices common to microcontrollers. Multiple
devices are presented as instances of abstract data types. A variety of
development platforms from various vendors are supported, including the
STM32 series boards. The library is available on GitHub for both
non-proprietary and commercial use here:
https://github.com/AdaCore/Ada_Drivers_Library. We are going to use some
of these drivers as illustrations in the following sections.


Non-Memory-Mapped Devices
-------------------------

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
           Inputs  => (Port'Asm_Input("dx",Device),
                       Unsigned_16'Asm_Input("ax",Data)),
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


Memory-Mapped Devices
---------------------

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

   X : aliased Integer_32;
   Y : Bits32 with Address => X'Address;

Because one view is as an integer and the other as an array, we can
access that memory using the two different views' operations. Using the
view as an array object (:ada:`Y`) we can access individual bits of the
memory shared with :ada:`X`. Using the view as an integer (:ada:`X`), we
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
particular example, no. It is effectively read-only memory.
But for some other device it very well could be meaningful, certainly.
It depends on the hardware. But in this case, assigning a value to the
:ada:`Rotary_Switch` variable would have no effect, which could be confusing to
programmers. It looks like a variable, after all. We wouldn't declare it
as a constant because the human user could rotate the switch, resulting
in a different value read. Therefore, we would hide the Ada variable
behind a function, precluding the entire issue. Clients of the
function can then use it for whatever purpose they require, e.g., as the
unique identifier for a computer in a rack.

Let's talk more about the type we use to represent a memory-mapped
device. As we said, that type defines the view we have for the object,
and hence the operations we have available for accessing the underlying
mapped device.

We choose the type for the representative Ada variable based on the
interface of the hardware mapped to the memory. If the interface is a
single monolithic register, for example, then an integer (signed or
unsigned) of the necessary size will suffice. But suppose the interface
is several bytes wide, and some of the bytes have different purposes
from the others? In that case, a record type is the obvious solution,
with distinct record components dedicated to the different parts of the
hardware interface. We could use individual bits too, of course, if
that's what the hardware does. Ada is particularly good at this
fine-degree of representation because record components of any types can
be specified in the layout, down to the bit level, within the record.

In addition, we might want to apply more than one type, at any one time,
to a given memory-mapped device. Doing so allows the client code some
flexibility, or it might facilitate an internal implementation. For
example, the STM32 boards from ST Microelectronics include a 96-bit
device unique identifier on each board. The identifier starts at a fixed memory
location. In this example we provide two different views |mdash|
types |mdash| for the value. One type provides the
identifier as a String containing twelve characters, whereas another
type provides the value as an array of three 32-bit unsigned words
(i.e., 12 bytes). The two types are applied by two overloaded functions
that are distinguished by their return type:

.. code-block:: ada

   package STM32.Device_Id is

      subtype Device_Id_Image is String (1 .. 12);

      function Unique_Id return Device_Id_Image;

      type Device_Id_Tuple is array (1 .. 3) of UInt32
        with Component_Size => 32;

      function Unique_Id return Device_Id_Tuple;

   end STM32.Device_Id;

The subtype :ada:`Device_Id_Image` is the view of the 96-bits as an
array of twelve 8-bit characters. (Using type :ada:`String` here isn't essential. We
could have defined an array of bytes instead of :ada:`Character`.) Similarly,
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
         Result : Device_Id_Image with Address => ID_Address, Import;
      begin
         return Result;
      end Unique_Id;

      function Unique_Id return Device_Id_Tuple is
         Result : Device_Id_Tuple with Address => ID_Address, Import;
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

The only difference in the bodies is the return type and matching type
for the local :ada:`Result` variable. Both functions read from the same
location in memory.

Earlier we indicated that the bit-pattern implementation of the
GPIO function could be expressed differently, resulting in more
readable, therefore maintainable, code. The fact that the code is in C
is irrelevant; the same approach in Ada would not be any better. Here's
the complete code for the function body:

.. code-block:: c

   void GPIO_PinAFConfig(GPIO_TypeDef *GPIOx,
                         uint16_t      GPIO_PinSource,
                         uint8_t       GPIO_AF)
   {
     uint32_t temp = 0x00;
     uint32_t temp_2 = 0x00;

     /* Check the parameters */
     assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
     assert_param(IS_GPIO_PIN_SOURCE(GPIO_PinSource));
     assert_param(IS_GPIO_AF(GPIO_AF));

     temp = ((uint32_t)(GPIO_AF) <<
               ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
     GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF <<
               ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
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
use the configuration routine to apply the specific 4-bit code representing
the alternate function required for our application.

These 16 4-bit alternate function fields are contiguous in the
register (hence memory) so we can represent them as an array with a
total size of 64-bits (i.e., 16 times 4). In the C version this array
has two components of type :c:`uint32_t` so it must compute where the
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

   type Alternate_Function_Fields is
     array (GPIO_Pin) of GPIO_Alternate_Function_Code
     with Component_Size => 4, Size => 64;  -- both in units of bits

Note that we can use the GPIO :ada:`Pin` parameter directly as the index into
the array type, obviating any need to massage the :ada:`Pin` value in
the procedure. That's possible because the type :ada:`GPIO_Pin` is an
enumeration type:

.. code-block:: ada

   type GPIO_Pin is
     (Pin_0, Pin_1, Pin_2,  Pin_3,  Pin_4,  Pin_5,  Pin_6,  Pin_7,
      Pin_8, Pin_9, Pin_10, Pin_11, Pin_12, Pin_13, Pin_14, Pin_15);

   for GPIO_Pin use
     (Pin_0  => 16#0001#,
      Pin_1  => 16#0002#,
      Pin_2  => 16#0004#,
      Pin_3  => 16#0008#,
      Pin_4  => 16#0010#,
      Pin_5  => 16#0020#,
      Pin_6  => 16#0040#,
      Pin_7  => 16#0080#,
      Pin_8  => 16#0100#,
      Pin_9  => 16#0200#,
      Pin_10 => 16#0400#,
      Pin_11 => 16#0800#,
      Pin_12 => 16#1000#,
      Pin_13 => 16#2000#,
      Pin_14 => 16#4000#,
      Pin_15 => 16#8000#);

In the hardware, the GPIO_Pin values don't start at zero and
monotonically increase. Instead, the values are bit patterns, where one
bit within each value is used. The enumeration representation clause
allows us to express that representation.

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
      Unused     : Unaccessed_Gap;
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
      Unused     at 40 range 0 .. 7871;
   end record;

These declarations define a record type that matches the content and
layout of the STM32 GPIO Port memory-mapped device.

Let's compare the two procedure implementations again. Here they are, for
convenience:

.. code-block:: c

   void GPIO_PinAFConfig(GPIO_TypeDef *GPIOx,
                         uint16_t      GPIO_PinSource,
                         uint8_t       GPIO_AF)
   {
     uint32_t temp = 0x00;
     uint32_t temp_2 = 0x00;

     /* Check the parameters */
     assert_param(IS_GPIO_ALL_PERIPH(GPIOx));
     assert_param(IS_GPIO_PIN_SOURCE(GPIO_PinSource));
     assert_param(IS_GPIO_AF(GPIO_AF));

     temp = ((uint32_t)(GPIO_AF) <<
               ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
     GPIOx->AFR[GPIO_PinSource >> 0x03] &= ~((uint32_t)0xF <<
               ((uint32_t)((uint32_t)GPIO_PinSource & (uint32_t)0x07) * 4));
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
statements that we should always try to achieve. Ada just makes that
easier to achieve than in some other languages.

Of course, the underlying hardware likely has no machine-supported 4-bit
unsigned type so larger hardware numeric types are used in the generated code. Hence there are
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
hardware.

This array approach requires each array component |mdash| the
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
complicated memory-mapped objects like these ports. They do so for the
sake of future expansion of the implementation, e.g., to add new
features or capacity. The gaps are thus between consecutive hardware
devices.

These gaps are presumably (hopefully!) included in the memory layout
documented for the device, but it won't be highlighted particularly. You
should check, therefore, that the documented starting addresses of the
second and subsequent array components are what you will get with a
simple array object having components of that record type.

For example, the datasheet for the STM32F407 Arm implementation indicates
that the GPIO ports start at address 16#4002_0000#. That's where GPIO_A begins.
The next port, GPIO_B, starts at address 16#4002_0400#, or a byte offset
of 1024 in decimal. In the STM32F4 Reference Manual, however, the
GPIO port register layout indicates a size for any one port that is much
less than 1024 bytes. As you saw earlier in the corresponding record type
declaration, on the STM32F4 each port only requires 40 (decimal) bytes.
Hence there's a gap of unused memory between the ports, including after
the last port, of 984 bytes (7872 bits).

To represent the gap, an "extra", unused record component was added,
with the necessary location and size specified within the record type,
so that the unused memory is included in the representation. As a
result, each array component will start at the right address (again, as
long as the first one does). Telling the compiler, and future
maintainers, that this extra component is not meant to be referenced by
the software would not hurt. You can use the pragma or aspect
:ada:`Unreferenced` for that purpose.  Here's the code again, for
convenience:

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
      Unused     : Unaccessed_Gap with Unreferenced;
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
      Unused     at 40 range 0 .. 7871;
   end record;

The type for the gap, :ada:`Unaccessed_Gap`, must represent 984
bytes so we declared an array like so:

.. code-block:: ada

   Gap_Size : constant := 984;  -- bytes
   --  There is a gap of unused, reserved memory after the end of the
   --  bytes used by any given memory-mapped GPIO port. The size of the
   --  gap is indicated in the STM32F405xx etc. Reference Manual, RM 0090.
   --  Specifically, Table 1 shows the starting and ending addresses mapped
   --  to the GPIO ports, for an allocated size of 16#400#, or 1024 (decimal)
   --  bytes per port. However, in the same document, the register map for
   --  these ports shows only 40 bytes currently in use. Presumably this gap is
   --  for future expansion when additional functionality or capacity is added,
   --  such as more pins per port.

   type Unaccessed_Gap is array (1 .. Gap_Size) of Unsigned_8 with
      Component_Size => Unsigned_8'Size,
      Size           => Gap_Size * Unsigned_8'Size;
   --  This type is used to represent the necessary gaps between GPIO
   --  ports in memory. We explicitly allocate a record component of
   --  this type at the end of the record type for that purpose.

We also set the size of the entire record type to :ada:`16#400#` bytes since
that is the total of the required bytes plus the gap, as per the
documentation. As such, this is a "confirming" size clause because the
reserved gap component increases the required size to that value (which
is the point). We don't really need to do both, i.e., declare the reserved
gap component and also set the record type size to the larger value. We
could have done either one alone. One could argue that setting the size
alone would have been simpler, in that it would obviate the type
declaration and corresponding record component declaration. Being doubly
explicit seemed a good idea at the time.


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
array of bytes |mdash| representing a received message. A
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

In the call, :ada:`Z` is of type :ada:`Interfaces.Integer_16`, for
example, or :ada:`Unsigned_16`, or even something bigger as long as you
only care about swapping the first two bytes.

The incomplete implementation using the conversion idiom could be like so:

.. code-block:: ada

   procedure Swap2 (Location : System.Address) is
      X : Word renames To_Pointer (Location).all;
   begin
      X :=  Shift_Left (X, 8) or Shift_Right (X, 8);
   end Swap2;

The declaration of :ada:`X` is the pertinent part.

In the declaration, :ada:`X` is of type :ada:`Word`, a type (not yet
shown) derived from :ada:`Interfaces.Unsigned_16`. Hence :ada:`X` can
have the inherited shift and logical :ada:`or` operations applied.

The :ada:`To_Pointer (Location)` part of the declaration is a function
call. The function returns the conversion of the incoming address value
in :ada:`Location` into an access value designating :ada:`Word` values.
We'll explain how to do that momentarily. The :ada:`.all` explicitly
dereferences the access value resulting from the function call.

Finally, :ada:`X` renames the :ada:`Word` designated by the
converted access value. The benefit of the renaming, in addition to the
simpler name, is that the function is only called once, and the access value
deference is only evaluated once.

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

      function To_Pointer (Value : Address) return Object_Pointer;
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

In the renaming expression, :ada:`To_Pointer (Location)` converts the
incoming address in :ada:`Location` to a pointer designating the
:ada:`Word` at that address. The :ada:`.all` dereferences the resulting
access value to get the designated :ada:`Word` value. Hence :ada:`X`
refers to that two-byte value in memory.

We could almost certainly achieve the same affect by replacing the call to
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
value requiring fat pointers will not work using unchecked
conversions. (There is a way, however, to tell GNAT to use a single address
value, but it is an explicit step in the code. Once done, though, unchecked
conversions would then work correctly.)

You can alternatively use generic package
:ada:`System.Address_To_Access_Conversions`.  That generic is defined for the
purpose of converting addresses to access values, and vice versa. But note
that the implementation of the generic's routines must account for the
representation their compiler uses for unbounded types like :ada:`String`.


Address Arithmetic
------------------

Part of "letting the compiler do the work for you" is not doing address
arithmetic in the source code if you can avoid it. Instead, for
instance, use the normal "dot notation" to reference components, and let
the compiler compute the offsets to those components. The approach to
implementing procedure :ada:`Configure_Alternate_Function` for a
:ada:`GPIO_Port` is a good example.

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
array of record values, then traverses the array, printing the array
components as it goes. (This is not the way to really implement such code.
It's just an illustration for address arithmetic.)

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

      Objects_Base : constant System.Address := Objects'Address;

      Offset : Storage_Offset;

      --  display the object of type R at the address specified by Location
      procedure Display_R (Location : in System.Address) is

         package R_Pointers is new System.Address_To_Access_Conversions (R);
         use R_Pointers;

          Value : R renames To_Pointer (Location).all;
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

      --  walk the array of R objects, displaying each one individually by
      --  adding the offset to the base address of the array
      for K in Objects'Range loop
         Display_R (Objects_Base + Offset);
         Offset := Offset + R_Size;
      end loop;
   end Demo_Address_Arithmetic;

Seriously, this is just for the purpose of illustration. It would be
much better to just index into the array directly.
