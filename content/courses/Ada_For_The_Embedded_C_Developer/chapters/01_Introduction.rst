:prev_state: False

Introduction
==============

.. include:: ../../../global.txt

So, what is this Ada thing anyway?
-----------------------------------

To answer this question let's introduce Ada as it compares to C for an embedded
application. C developers are used to a certain coding semantic and style of
programming. Especially in the embedded domain, developers are used to working
at a very low level near the hardware to directly manipulate memory and
registers. Normal operations involve mathematical operations on pointers,
complex bit shifts, and logical bitwise operations. C is well designed for such
operations as it is a low level language that was designed to replace assembly
language for faster, more efficient programming. Because of this minimal
abstraction, the programmer has to model the data that represents the problem
they are trying to solve using the language of the physical hardware.

Let's look at an example of this problem in action by comparing the same
program in Ada and C:

[C]

.. code:: c cli_input run_button project=Courses.Ada_For_Embedded_C_Dev.Introduction.Add_Angles_C

   !main.c
   #include <stdio.h>
   #include <stdlib.h>

   #define DEGREES_MAX           (360)
   typedef unsigned int degrees;

   #define MOD_DEGREES(x)   (x % DEGREES_MAX)

   degrees add_angles(degrees* list, int length)
   {
     degrees sum = 0;
     for(int i = 0; i < length; ++i) {
       sum += list[i];
     }

     return sum;
   }

   int main(int argc, char** argv)
   {
     degrees list[argc - 1];

     for(int i = 1; i < argc; ++i) {
       list[i - 1] = MOD_DEGREES(atoi(argv[i]));
     }

     printf("Sum: %d\n", add_angles(list, argc - 1));

     return 0;
   }

[Ada]

.. code:: ada cli_input run_button project=Courses.Ada_For_Embedded_C_Dev.Introduction.Add_Angles_Ada

   with Ada.Command_Line; use Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;

   procedure Sum_Angles is

      DEGREES_MAX : constant := 360;
      type Degrees is mod DEGREES_MAX;

      type Degrees_List is array (Natural range <>) of Degrees;

      function Add_Angles (List : Degrees_List) return Degrees
      is
         Sum : Degrees := 0;
      begin
         for I in List'Range loop
            Sum := Sum + List (I);
         end loop;

         return Sum;
      end Add_Angles;

      List : Degrees_List (1 .. Argument_Count);
   begin
      for I in List'Range loop
         List (I) := Degrees (Integer'Value (Argument (I)));
      end loop;

      Put_Line ("Sum:" & Add_Angles (List)'Img);
   end Sum_Angles;

Here we have a piece of code in C and in Ada that takes some numbers from the
command line and stores them in an array. We then sum all of the values in the
array and print the result. The tricky part here is that we are working with
values that model an angle in degrees. We know that angles are modular types,
meaning that angles greater than 360° can also be represented as :ada:`Angle
mod 360`. So if we have an angle of 400°, this is equivalent to 40°. In order
to model this behavior in C we had to create the :c:`MOD_DEGREES` macro, which
performs the modulus operation. As we read values from the command line, we
convert them to integers and perform the modulus before storing them into the
array. We then call add_angles which returns the sum of the values in the
array. Can you spot the problem with the C code?

Try running the Ada and C examples using the input sequence :ada:`340 2 50 70`.
What does the C program output? What does the Ada program output? Why are they
different?

The problem with the C code is that we forgot to call :c:`MOD_DEGREES` in the
for loop of add_angles. This means that it is possible for add_angles to return
values greater than :c:`DEGREES_MAX`. Let's look at the equivalent Ada code now
to see how Ada handles the situation. The first thing we do in the Ada code is
to create the type :ada:`Degrees` which is a modular type. This means that the
compiler is going to handle performing the modulus operation for us. If we use
the same for loop in the :ada:`Add_Angles` function, we can see that we aren't
doing anything special to make sure that our resulting value is within the 360°
range we need it to be in.

The takeaway from this example is that Ada tries to abstract some concepts from
the developer so that the developer can focus on solving the problem at hand
using a data model that models the real world rather than using data types
prescribed by the hardware. The main benefit of this is that the compiler takes
some responsibility from the developer for generating correct code. In this
example we forgot to put in a check in the C code. The compiler inserted the
check for us in the Ada code because we told the compiler what we were trying
to accomplish by defining strong types.

Ideally, we want all the power that the C programming language can give us to
manipulate the hardware we are working on while also allowing us the ability to
more accurately model data in a safe way. So, we have a dilemma; what can give
us the power of operations like the C language, but also provide us with
features that can minimize the potential for developer error? Since this course
is about Ada, it's a good bet we're about to introduce the Ada language as the
answer to this question…

Unlike C, the Ada language was designed as a higher level language from its
conception; giving more responsibility to the compiler to generate correct
code. As mentioned above, with C, developers are constantly shifting, masking,
and accessing bits directly on memory pointers. In Ada, all of these operations
are possible, but in most cases, there is a better way to perform these
operations using higher level constructs that are less prone to mistakes, like
off-by-one or unintentional buffer overflows. If we were to compare the same
application written using C and with Ada using high level constructs, we would
see similar performance in terms of speed and memory efficiency. If we compare
the object code generated by both compilers, it's possible that they even look
identical!

Ada |mdash| The Technical Details
---------------------------------

Like C, Ada is a compiled language. This means that the compiler will parse the
source code and emit machine code native to the target hardware. The Ada
compiler we will be discussing in this course is the GNAT compiler. This
compiler is based on the GCC technology like many C and C++ compilers
available. When the GNAT compiler is invoked on Ada code, the GNAT front-end
expands and translates the Ada code into an intermediate language which is
passed to GCC where the code is optimized and translated to machine code. A C
compiler based on GCC performs the same steps and uses the same intermediate
GCC representation. This means that the optimizations we are used to seeing
with a GCC based C compiler can also be applied to Ada code. The main
difference between the two compilers is that the Ada compiler is expanding high
level constructs into intermediate code. After expansion, the Ada code will be
very similar to the equivalent C code.

It is possible to do a line-by-line translation of C code to Ada. This feels
like a natural step for a developer used to C paradigms. However, there may be
very little benefit to doing so. For the purpose of this course, we're going to
assume that the choice of Ada over C is guided by considerations linked to
reliability, safety or security. In order to improve upon the reliability,
safety and security of our application, Ada paradigms should be applied in
replacement of those usually applied in C. Constructs such as pointers,
preprocessor macros, bitwise operations and defensive code typically get
expressed in Ada in very different ways, improving the overall reliability and
readability of the applications. Learning these new ways of coding, often,
requires effort by the developer at first, but proves more efficient once the
paradigms are understood.

In this course we will also introduce the SPARK subset of the Ada programming
language. The SPARK subset removes a few features of the language, i.e., those
that make proof difficult, such as pointer aliasing. By removing these features
we can write code that is fit for sound static analysis techniques. This means
that we can run mathematical provers on the SPARK code to prove certain safety
or security properties about the code.
