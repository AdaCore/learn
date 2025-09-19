Low Level Programming
-----------------------

.. include:: ../../../global.txt

Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~~~

We've seen in the previous chapters how Ada can be used to describe high level semantics and architecture. The beauty of the language, however, is that it can be used all the way down to the lowest levels of the development, including embedded assembly code or bit-level data management.

One very interesting feature of the language is that, unlike C, for example, there are no data representation constraints unless specified by the developer. This means that the compiler is free to choose the best trade-off in terms of representation vs. performance. Let's start with the following example:

[Ada]

.. code-block:: ada

   type R is record
      V  : Integer range 0 .. 255;
      B1 : Boolean;
      B2 : Boolean;
   end record
   with Pack;

[C++]

.. code-block:: cpp

   struct R {
      unsigned int v:8;
      bool b1;
      bool b2;
   };

[Java]

.. code-block:: java

   public class R {
      public byte v;
      public boolean b1;
      public boolean b2;
   }

The Ada and the C++ code above both represent efforts to create an object that's as small as possible. Controlling data size is not possible in Java, but the language does specify the size of values for the primitive types.

Although the C++ and Ada code are equivalent in this particular example, there's an interesting semantic difference. In C++, the number of bits required by each field needs to be specified. Here, we're stating that :cpp:`v` is only 8 bits, effectively representing values from 0 to 255. In Ada, it's the other way around: the developer specifies the range of values required and the compiler decides how to represent things, optimizing for speed or size. The :ada:`Pack` aspect declared at the end of the record specifies that the compiler should optimize for size even at the expense of decreased speed in accessing record components.

Other representation clauses can be specified as well, along with compile-time consistency checks between requirements in terms of available values and specified sizes. This is particularly useful when a specific layout is necessary; for example when interfacing with hardware, a driver, or a communication protocol. Here's how to specify a specific data layout based on the previous example:

.. code-block:: ada

   type R is record
      V  : Integer range 0 .. 255;
      B1 : Boolean;
      B2 : Boolean;
   end record;

   for R use record
      --  Occupy the first bit of the first byte.
      B1 at 0 range 0 .. 0;

      --  Occupy the last 7 bits of the first byte,
      --  as well as the first bit of the second byte.
      V  at 0 range 1 .. 8;

      --  Occupy the second bit of the second byte.
      B2 at 1 range 1 .. 1;
   end record;

We omit the :ada:`with Pack` directive and instead use a record representation clause following the record declaration. The compiler is directed to spread objects of type :ada:`R` across two bytes. The layout we're specifying here is fairly inefficient to work with on any machine, but you can have the compiler construct the most efficient methods for access, rather than coding your own machine-dependent bit-level methods manually.

Embedded Assembly Code
~~~~~~~~~~~~~~~~~~~~~~~~

When performing low-level development, such as at the kernel or hardware driver level, there can be times when it is necessary to implement functionality with assembly code.

Every Ada compiler has its own conventions for embedding assembly code, based on the hardware platform and the supported assembler(s). Our examples here will work with GNAT and GCC on the x86 architecture.

All x86 processors since the Intel Pentium offer the :assembly:`rdtsc` instruction, which tells us the number of cycles since the last processor reset. It takes no inputs and places an unsigned 64 bit value split between the :assembly:`edx` and :assembly:`eax` registers.

GNAT provides a subprogram called :ada:`System.Machine_Code.Asm` that can be used for assembly code insertion. You can specify a string to pass to the assembler as well as source-level variables to be used for input and output:

.. code-block:: ada

   with System.Machine_Code; use System.Machine_Code;
   with Interfaces;          use Interfaces;

   function Get_Processor_Cycles return Unsigned_64 is
      Low, High : Unsigned_32;
      Counter   : Unsigned_64;
   begin
      Asm ("rdtsc",
           Outputs =>
             (Unsigned_32'Asm_Output ("=a", Low),
              Unsigned_32'Asm_Output ("=d", High)),
           Volatile => True);

      Counter :=
        Unsigned_64 (High) * 2 ** 32 +
        Unsigned_64 (Low);

      return Counter;
   end Get_Processor_Cycles;

The :ada:`Unsigned_32'Asm_Output` clauses above provide associations between machine registers and source-level variables to be updated. :ada:`"=a"` and :ada:`"=d"` refer to the :assembly:`eax` and :assembly:`edx` machine registers, respectively. The use of the :ada:`Unsigned_32` and :ada:`Unsigned_64` types from package :ada:`Interfaces` ensures correct representation of the data. We assemble the two 32-bit values to form a single 64 bit value.

We set the :ada:`Volatile` parameter to :ada:`True` to tell the compiler that invoking this instruction multiple times with the same inputs can result in different outputs. This eliminates the possibility that the compiler will optimize multiple invocations into a single call.

With optimization turned on, the GNAT compiler is smart enough to use the :assembly:`eax` and :assembly:`edx` registers to implement the :ada:`High` and :ada:`Low` variables, resulting in zero overhead for the assembly interface.

The machine code insertion interface provides many features beyond what was shown here. More information can be found in the GNAT User's Guide, and the GNAT Reference manual.

Interfacing with C
~~~~~~~~~~~~~~~~~~~~~

Much effort was spent making Ada easy to interface with other languages. The :ada:`Interfaces` package hierarchy and the pragmas :ada:`Convention`, :ada:`Import`, and :ada:`Export` allow you to make inter-language calls while observing proper data representation for each language.

Let's start with the following C code:

.. code-block:: c

   struct my_struct {
      int A, B;
   };

   void call (my_struct * p) {
      printf ("%d", p->A);
   }

To call that function from Ada, the Ada compiler requires a description of the data structure to pass as well as a description of the function itself. To capture how the C :c:`struct my_struct` is represented, we can use the following record along with a :ada:`pragma Convention`. The pragma directs the compiler to lay out the data in memory the way a C compiler would.

.. code-block:: ada

   type my_struct is record
      A : Interfaces.C.int;
      B : Interfaces.C.int;
   end record;
   pragma Convention (C, my_struct);

Describing a foreign subprogram call to Ada code is called "binding" and it is performed in two stages. First, an Ada subprogram specification equivalent to the C function is coded. A C function returning a value maps to an Ada function, and a :c:`void` function maps to an Ada procedure. Then, rather than implementing the subprogram using Ada code, we use a :ada:`pragma Import`:

.. code-block:: ada

   procedure Call (V : my_struct);
   pragma Import (C, Call, "call"); -- Third argument optional

The :ada:`Import` pragma specifies that whenever :ada:`Call` is invoked by Ada code, it should invoke the :c:`call` function with the C calling convention.

And that's all that's necessary. Here's an example of a call to :ada:`Call`:

.. code-block:: ada

   declare
      V : my_struct := (A => 1, B => 2);
   begin
      Call (V);
   end;

You can also make Ada subprograms available to C code, and examples of this can be found in the GNAT User's Guide. Interfacing with C++ and Java use implementation-dependent features that are also available with GNAT.
