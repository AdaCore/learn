.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

The C Developer's Perspective on Ada
======================================

What we mean by Embedded Software
------------------------------------
The Ada programming language is a general programming language, which means it can be used for many different types of applications. One type of application where it particularly shines is reliable and safety-critical embedded software; meaning, a platform with a microprocessor such as ARM, PowerPC, x86, or RISC-V. The application may be running on top of an embedded operating system, such as an embedded Linux, or directly on bare metal. And the application domain can range from small entities such as firmware or device controllers to flight management system, communication based train control systems, or advanced driver assistance systems.

The GNAT Toolchain
-------------------

The toolchain used throughout this book is called GNAT, which is a suite of tools with a compiler based on the GCC environment. It can be obtained from AdaCore, either as part of a commercial contract with `GNAT Pro <https://www.adacore.com/gnatpro)>`_ or at no charge with the `GNAT Community edition <https://www.adacore.com/community>`_. The information on this book will be relevant no matter which edition you’re using. Most examples will be runnable on the native Linux or Windows version for convenience. Some will only be relevant in the context of a cross toolchain, in which case we’ll be using the embedded ARM bare metal toolchain.

As for any Ada compiler, GNAT takes advantage of implementation permissions and offers a project management system. Because we’re talking about embedded platforms, there are a lot of topics that we’ll go over which will be specific to GNAT, and sometimes to specific platforms supported by GNAT. We’ll try to make the distinction between what is GNAT-specific and Ada generic as much as possible through this book.

The GNAT Toolchain for Embedded Targets
-----------------------------------------

.. todo::
   AI - Rob: complete this section (explain run-time differences, how to compile, the concept of BSP, etc)

Hello World in Ada
--------------------

The first piece of code to translate from C to Ada is the usual Hello World program:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
     printf("Hello World\n");
     return 0;
   }

.. code:: ada

   with Ada.Text_IO;

   procedure Hello_World
   is
   begin
      Ada.Text_IO.Put_Line ("Hello World");
   end Hello_World;

The resulting program will print :ada:`Hello World` on the screen. Let’s now dissect the Ada version to describe what is going on:

The first line of the Ada code is giving us access to the :ada:`Ada.Text_IO` library which contains the :ada:`Put_Line` function we will use to print the text to the console. This is similar to C’s :c:`#include <stdio.h>`. We then create a procedure which executes :ada:`Put_Line` which prints to the console. This is similar to C’s :c:`printf` statement. For now, we can assume these Ada and C features have similar functionality. In reality, they are very different. We will explore that more as we delve further into the Ada language.

You may have noticed that the Ada syntax is more verbose than C. Instead of using braces :c:`{}` to declare scope, Ada uses keywords. :ada:`is` opens a declarative scope |mdash| which is empty here as there’s no variable to declare. :ada:`begin` opens a sequence of statements. Within this sequence, we’re calling the function :ada:`Put_Line`, prefixing explicitly by the name of the library unit where it’s declared, :ada:`Ada.Text_IO`. The absence of the end of line :c:`\n` can also be noted, as :ada:`Put_Line` always terminates by an end of line.

The Ada Syntax
----------------

Ada syntax might seem peculiar at first glance. Unlike many other languages, it’s not derived from the popular C style of notation with its ample use of brackets; rather, it uses a more expository syntax coming from Pascal. In many ways, Ada is a more explicit language |mdash| its syntax was designed to increase readability and maintainability, rather than making it faster to write in a condensed manner. For example, full words like :ada:`begin` and :ada:`end` are used in place of curly braces. Conditions are written using :ada:`if`, :ada:`then`, :ada:`elsif`, :ada:`else`, and :ada:`end if`. Ada’s assignment operator does not double as an expression, eliminating potential mistakes that could be caused by :c:`=` being used where :c:`==` should be.

All languages provide one or more ways to express comments. In Ada, two consecutive hyphens :ada:`--` mark the start of a comment that continues to the end of the line. This is exactly the same as using :c:`//` for comments in C. Multi line comments like C’s :c:`/* */` do not exist in Ada.

Ada compilers are stricter with type and range checking than most C programmers are used to. Most beginning Ada programmers encounter a variety of warnings and error messages when coding, but this helps detect problems and vulnerabilities at compile time |mdash| early on in the development cycle. In addition, checks (such as array bounds checks) provide verification that could not be done at compile time but can be performed either at run-time, or through formal proof (with the SPARK tooling).

Ada identifiers and reserved words are case insensitive. The identifiers :ada:`VAR`, :ada:`var` and :ada:`VaR` are treated as the same identifier; likewise :ada:`begin`, :ada:`BEGIN`, :ada:`Begin`, etc. Identifiers may include letters, digits, and underscores, but must always start with a letter. There are 73 reserved keywords in Ada that may not be used as identifiers, and these are:

  ======== ========= ========== ============
  abort    else      null       select
  abs      elsif     of         separate
  abstract end       or         some
  accept   entry     others     subtype
  access   exception out        synchronized
  aliased  exit      overriding tagged
  all      for       package    task
  and      function  pragma     terminate
  array    generic   private    then
  at       goto      procedure  type
  begin    if        protected  until
  body     in        raise      use
  case     interface range      when
  constant is        record     while
  declare  limited   rem        with
  delay    loop      renames    xor
  delta    mod       requeue
  digits   new       return
  do       not       reverse
  ======== ========= ========== ============

Compilation Unit Structure
----------------------------

Both C and Ada were designed with the idea that the code specification and code implementation could be separated into two files. In C, the specification typically lives in the .h, or header file, and the implementation lives in the .c file. Ada is superficially similar to C. With the GNAT toolchain, compilation units are stored in files with an .ads extension for specifications and with an .adb extension for implementations.

One main difference between the C and Ada compilation structure is that Ada compilation units are structured into something called packages. A specification defines a package and the implementation implements the package. We saw this in an earlier example when we included the :ada:`Ada.Text_IO` package into our application. The package specification has the structure:

.. code-block:: ada

   --  my_package.ads
   package My_Package is

      --  public declarations

   private

      --  private declarations

   end My_Package;

The package implementation, or body, has the structure:

.. code-block:: ada

   --  my_package.adb
   package body My_Package is

      --  implementation

   end My_Package;

Something that might stick out in this example is the use of the reserve word :ada:`private` in the package specification. This acts as a partition in the package |mdash| anything declared before this keyword is publicly visible to other units that may :ada:`with` this package. Anything declared after the private keyword is only visible to the package implementation. A package specification, or spec, does not require a private section. One typical use-case for the private section in a package is when you want to declare a heterogeneous data type, called a record in Ada or a struct in C, but you want to stop the user of the package from accessing the record components directly.

.. code-block:: ada

   package Containers is

      type Stack is private;

      procedure Push (St   : in out Stack;
                      Elem : Integer);
      function Pop (St : in out Stack) return Integer;

      --  more accessors go here

   private
      type Integer_Array is array (Natural range <>) of Integer;

      type Stack is record
         Data : Integer_Array (1 .. 100);
         Top : Natural := 0;
      end record;

   end Containers;

In this example we have a specification for a Stack data type. We don't really want the user to be manipulating the underlying array or index of the top of the array directly. To accomplish this "hiding" we can, in the public section of the package, declare a Stack data type as a private type and some accessors which take a parameter of type stack. In the private section we actually declare the Stack as a record with its components. The user of this package **cannot** access :ada:`Data` or :ada:`Top` directly in this example.

However, from the package body, we **can** access :ada:`Data` and :ada:`Top`.

.. code-block:: ada

   package body Containers is

      procedure Push (St   : in out Stack;
                      Elem : Integer)
      is
      begin
         --  some defensive code should go here
         St.Top := St.Top + 1;
         St.Data (St.Top) := Elem;
      end Push;

      function Pop (St : in out Stack) return Integer
      is
         Ret : Integer;
      begin
         --  some defensive code should go here
         Ret := St.Data (St.Top);
         St.Top := St.Top - 1;

         return Ret;
      end Pop;

   end Containers;

Statements and Declarations
----------------------------

The following code samples are all equivalent, and illustrate the use of comments and working with integer variables:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      // variable declarations
      int a = 0, b = 0, c = 100, d;

      // c shorthand for increment
      a++;

      // regular addition
      d = a + b + c;

      // printing the result
      printf("d = %d\n", d);

      return 0;
   }

.. code:: ada

   with Ada.Text_IO;

   procedure Main
   is
      --  variable declaration
      A, B : Integer := 0;
      C    : Integer := 100;
      D    : Integer;
   begin
      --  Ada does not have a shortcut format for increment like in C
      A := A + 1;

      --  regular addition
      D := A + B + C;

      --  printing the result
      Ada.Text_IO.Put_Line ("D =" & D'Img);
   end Main;

You'll notice that, in both languages, statements are terminated with a semicolon. This means that you can have multi-line statements.

In the Ada example above, there are two distinct sections to the :ada:`procedure Main`. This first section is delimited by the :ada:`is` keyword and the :ada:`begin` keyword. This section is called the declarative block of the subprogram. The declarative block is where you will define all the local variables which will be used in the subprogram. C89 had something similar, where developers were required to declare their variables at the top of the scope block. Most C developers may have run into this before when trying to write a for loop:

.. code-block:: c

   /* The C89 version */
   int average(int* list, int length)
   {
      int i;
      int sum = 0;

      for(i = 0; i < length; ++i) {
         sum += list[i];
      }
      return (sum / length);
   }

.. code-block:: c

   // The modern C way
   int average(int* list, int length)
   {
      int sum = 0;

      for(int i = 0; i < length; ++i) {
         sum += list[i];
      }

      return (sum / length);
   }

For the fun of it, let's also see the Ada way to do this:

.. code-block:: ada

   type Int_Array is array (Natural range <>) of Integer;

   function Average (List : Int_Array) return Integer
   is
      Sum : Integer;
   begin

      for I in List'Range loop
         Sum := Sum + List (I);
      end loop;

      return (Sum / List'Length);
   end Average;

We will explore more about the syntax of loops in Ada in a future section of this book; but for now, notice that the :ada:`I` variable used as the loop index is not declared in the declarative section!

.. admonition:: Declaration Flippy Floppy

   Something peculiar that you may have noticed about declarations in Ada is that they are backwards from the way C does declarations. The C language expects the type followed by the variable name. Ada expects the variable name followed by a semicolon and then the type.

The next block in the Ada example is between the :ada:`begin` and :ada:`end` keywords. This is where your statements will live. You can create new scopes by using the :ada:`declare` keyword:

.. code:: ada

   with Ada.Text_IO;

   procedure Main
   is
      --  variable declaration
      A, B : Integer := 0;
      C    : Integer := 100;
      D    : Integer;
   begin
      --  Ada does not have a shortcut format for increment like in C
      A := A + 1;

      --  regular addition
      D := A + B + C;

      --  printing the result
      Ada.Text_IO.Put_Line ("D =" & D'Img);

      declare
         E : Integer := D * 100;
      begin
         --  printing the result
         Ada.Text_IO.Put_Line ("E =" & E'Img);
      end;

   end Main;

Notice that we declared a new variable :ada:`E` whose scope only exists in our newly defined block. The equivalent C code is:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      // variable declarations
      int a = 0, b = 0, c = 100, d;

      // c shorthand for increment
      a++;

      // regular addition
      d = a + b + c;

      // printing the result
      printf("d = %d\n", d);

      {
         int e = d * 100;
         printf("e = %d\n", e);
      }

      return 0;
   }

.. admonition:: The shortcuts of incrementing and decrementing

   You may have noticed that Ada does not have something similar to the :c:`a++` or :c:`a--` operators. Instead you must use the full assignment :ada:`A := A + 1` or :ada:`A := A - 1`.

**Fun Fact** about the C language assignment operator :c:`=`: Did you know that an assignment in C can be used in an expression? Let's look at an example:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      int a = 0;

      if(a = 10)
         printf("True\n");
      else
         printf("False\n");

      return 0;
   }

Run the above code example. What does it output? Is that what you were expecting?

The author of the above code example probably meant to test if :c:`a == 10` in the if statement but accidentally typed :c:`=` instead of :c:`==`. Because C treats assignment as an expression, it was able to evaluate :c:`a = 10`.

Let's look at the equivalent Ada code:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      A : Integer := 0;
   begin

      if A := 10 then
         Put_Line ("True");
      else
         Put_Line ("False");
      end if;
   end Main;

THe above code will not compile. This is because Ada does no allow assignment as an expression.

.. admonition:: The "use" clause

   You'll notice in the above code example, after :ada:`with Ada.Text_IO;` there is a new statement we haven't seen before |mdash| :ada:`use Ada.Text_IO;`. You may also notice that we are not using the :ada:`Ada.Text_IO` prefix before the :ada:`Put_Line` statements. When we add the use clause it tells the compiler that we won't be using the prefix in the call to subprograms of that package. The use clause is something to use with caution. For example: if we use the :ada:`Ada.Text_IO` package and we also have a :ada:`Put_Line` subprogram in our current compilation unit with the same signature, we have a conflict!

Conditions
------------

The syntax of an if statement:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      // try changing the initial value to change the
      //    output of the program
      int v = 0;

      if(v > 0) {
         printf("Positive\n");
      }
      else if(v < 0) {
         printf("Negative\n");
      }
      else {
         printf("Zero\n");
      }

      return 0;
   }


.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      --  try changing the initial value to change the
      --    output of the program
      V : Integer := 0;
   begin
      if v > 0 then
         Put_Line ("Positive");
      elsif v < 0 then
         Put_Line ("Negative");
      else
         Put_Line ("Zero");
      end if;
   end Main;

In Ada, everything that appears between the :ada:`if` and :ada:`then` keywords is the conditional expression, no parentheses required. Comparison operators are the same except for:

========== ======= ==========
Operator   C       Ada
========== ======= ==========
Equality   :c:`==` :ada:`=`
Inequality :c:`!=` :ada:`/=`
Not        :c:`!`  :ada:`not`
And        :c:`&&` :ada:`and`
Or         :c:`||` :ada:`or`
========== ======= ==========

The syntax of a switch/case statement:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      // try changing the initial value to change the
      //    output of the program
      int v = 0;

      switch(v) {
         case 0:
            printf("Zero\n");
            break;
         case 1: case 2: case 3: case 4: case 5:
         case 6: case 7: case 8: case 9:
            printf("Positive\n");
            break;
         case 10: case 12: case 14: case 16: case 18:
            printf("Even number between 10 and 18\n");
            break;
         default:
            printf("Something else\n");
            break;
      }

      return 0;
   }

.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      --  try changing the initial value to change the
      --    output of the program
      V : Integer := 0;
   begin
      case V is
         when 0 =>
            Put_Line ("Zero");
         when 1 .. 9 =>
            Put_Line ("Positive");
         when 10 | 12 | 14| 16 | 18 =>
            Put_Line ("Even number between 10 and 18");
         when others =>
            Put_Line ("Something else");
      end case;
   end Main;

.. admonition:: Switch or Case?

   A switch statement in C is the same as a case statement in Ada. This may be a little strange because C uses both keywords in the statement syntax. Lets make an analogy between C and Ada: C's :c:`switch` is to Ada's :ada:`case` as C's :c:`case` is to Ada's :ada:`when`.

Notice that in Ada, the case statement does not use the :c:`break` keyword. In C, we use :c:`break` to stop the execution of a case branch from falling through to the next branch. Here is an example:

.. code:: c cli_input

   !main.c
   #include <stdio.h>

   int main()
   {
      int v = 0;

      switch(v) {
         case 0:
            printf("Zero\n");
         case 1:
            printf("One\n");
         default:
            printf("Other\n");
      }

      return 0;
   }

Run the above code with :c:`v = 0`. What prints? What prints when we change the assignment to :c:`v = 1`?

When :c:`v = 0` the program outputs the strings :c:`Zero` then :c:`One` then :c:`Other`. This is called fall through. If you add the :c:`break` statements back into the :c:`switch` you can stop this fall through behavior from happening. The reason why fall through is allowed in C is to allow the behavior from the previous example where we want a specific branch to execute for multiple inputs. Ada solves this a different way because it is possible, or even probable, that the developer might forget a :c:`break` statement accidentally. So Ada does not allow fall through. Instead, you can use Ada's semantic to identify when a specific branch can be executed by more than one input. If you want a range of values for a specific branch you can use the :ada:`First .. Last` notation. If you want a few non-consecutive values you can use the :ada:`Value1 | Value2 | Value3` notation.

Instead of using the word :c:`default` to denote the catch-all case, Ada uses the :ada:`others` keyword.

Loops
------

Let's start with some syntax:

.. code-block:: c

   // this is a while loop
   while(v < 10000) {
      v *= 2;
   }

   // this is a do while loop
   do {
      v *= 2;
   } while(v < 10000);

   // this is a for loop
   for(int i = 0; i < 10000; ++i) {
      v *= (i * i);
   }

   // this is a forever loop with a conditional exit
   while(1) {
      // do stuff here
      if(condition)
         break;
   }

   // this is a loop over an array
   {
      #define ARR_SIZE (10)
      int arr[ARR_SIZE];
      int sum = 0;

      for(int i = 0; i < ARR_SIZE; ++i) {
         sum += arr[i];
      }
   }


.. code-block:: ada

   --  this is a while loop
   while V < 10_000 loop
      V := V * 2;
   end loop;

   --  Ada doesn't have an explicit do while loop
   --    instead you can use the loop and exit keywords
   loop
      V := V * 2;
      exit when V >= 10_000;
   end loop;

   --  this is a for loop
   for I in 1 .. 10_000 loop
      V := V * (I * I);
   end loop;

   --  this is a forever loop with a conditional exit
   loop
      --  do stuff here
      exit when condition;
   end loop;

   --  this is a loop over an array
   declare
      type Int_Array is array (Natural range 1 .. 10) of Integer;

      Arr : Int_Array;
      Sum : Integer := 0;
   begin
      for I in Arr'Range loop
         Sum := Sum + Arr (I);
      end loop;
   end;

The loop syntax in Ada is pretty straightforward. The :ada:`loop` and :ada:`end loop` keywords are used to open and close the loop scope. Instead of using the :c:`break` keyword to exit the loop, Ada has the :ada:`exit` statement. The :ada:`exit` statement can be combined with a logic expression using the :ada:`exit when` syntax.

The major deviation in loop syntax is regarding for loops. You'll notice, in C, that you sometimes declare, and at least initialize a loop counter variable, specify a loop predicate, or an expression that indicates when the loop should continue executing or complete, and last you specify an expression to update the loop counter.

.. code-block:: c

   for(initialization expression; loop predicate; update expression) {
      // some statements
   }

In Ada, you don't declare or initialize a loop counter or specify an update expression. You only name the loop counter and give it a range to loop over. The loop counter is **read-only**! You cannot modify the loop counter inside the loop like you can in C. And the loop counter will increment consecutively along the specified range. But what if you want to loop over the range in reverse order?

.. code:: c

   !main.c
   #include <stdio.h>

   #define MY_RANGE (10)

   int main()
   {

      for(int i = MY_RANGE; i >= 0; --i) {
         printf("%d\n", i);
      }

      return 0;
   }

.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      My_Range : constant := 10;
   begin
      for I in reverse 0 .. My_Range loop
         Put_Line (I'Img);
      end loop;
   end Main;

.. admonition:: Tick Image

   Strangely enough, Ada people call the single apostrophe symbol, :ada:`'`, "tick". This "tick" says the we are accessing an attribute of the variable. When we do :ada:`'Img` on a variable of a numerical type, we are going to return the string version of that numerical type. So in the for loop above, :ada:`I'Img`, or "I tick image" will return the string representation of the numerical value stored in I. We have to do this because Put_Line is expecting a string as an input parameter.

In the above example, we are traversing over the range in reverse order. In Ada, we use the :ada:`reverse` keyword to accomplish this.

In many cases, when we are writing a for loop, it has something to do with traversing an array. In C, this is a classic location for off-by-one errors. Lets see an example in action:

.. code:: c

   !main.c
   #include <stdio.h>

   #define LIST_LENGTH (100)

   int main()
   {
      int list[LIST_LENGTH];

      for(int i = LIST_LENGTH; i > 0; --i) {
         list[i] = LIST_LENGTH - i;
      }

      for (int i = 0; i < LIST_LENGTH; ++i)
      {
         printf("%d ", list[i]);
      }
      printf("\n");

      return 0;
   }

.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      type Int_Array is array (Natural range 1 .. 100) of Integer;

      List : Int_Array;
   begin

      for I in reverse List'Range loop
         List (I) := List'Last - I;
      end loop;

      for I in List'Range loop
         Put (List (I)'Img & " ");
      end loop;

      New_Line;
   end Main;

The above Ada and C code should initialize an array using a for loop. The initial values in the array should be contiguously decreasing from 99 to 0 as we index from the first index to the last index. In other words, the first index has a value of 99, the next has 98, the next 97 ... the last has a value of 0.

If you run both the C and Ada code above you'll notice that the outputs of the two programs are different. Can you spot why?

In the C code there are two problems:

#. There's a buffer overflow in the first iteration of the loop. We would need to modify the loop initialization to :c:`int i = LIST_LENGTH - 1;`.

#. The loop predicate should be modified to :c:`i >= 0;`

#. The C code also has another off-by-one problem in the math to compute the value stored in :c:`list[i]`. The expression should be changed to be :c:`list[i] = LIST_LENGTH - i - 1;`.

These are typical off-by-one problems that plagues C programs. You'll notice that we didn't have this problem with the Ada code because we aren't defining the loop with arbitrary numeric literals. Instead we are accessing attributes of the array we want to manipulate and are using a keyword to determine the indexing direction.

We can actually simplify the Ada for loop a little further using iterators:

.. code:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      type Int_Array is array (Natural range 1 .. 100) of Integer;

      List : Int_Array;
   begin

      for I in reverse List'Range loop
         List (I) := List'Last - I;
      end loop;

      for I of List loop
         Put (I'Img & " ");
      end loop;

      New_Line;
   end Main;

In the second for loop, we changed the syntax to :ada:`for I of List`. Instead of I being the index counter, it is now an iterator that references the underlying element. This example of Ada code is identical to the last bit of Ada code. We just used a different method to index over the second for loop. There is no C equivalent to this Ada feature, but it is similar to C++'s range based for loop.

Type System
--------------

Ada is considered a "strongly typed" language. This means that the language does not define any implicit type conversions. C does define implicit type conversions, sometimes referred to as
*integer promotion*. The rules for promotion are fairly straightforward in simple expressions but can get confusing very quickly. Let's look at a typical place of confusion with implicit type conversion:

.. code:: c

   !main.c
   #include <stdio.h>

   int main()
   {
      unsigned char a = 0xFF;
      char b = 0xFF;

      printf("Does a == b?\n");
      if(a == b)
         printf("Yes.\n");
      else
         printf("No.\n");

      printf("a: 0x%08X, b: 0x%08X\n", a, b);

      return 0;
   }

Run the above code. You will notice that :c:`a != b`! If we look at the output of the last :c:`printf` statement we will see the problem. :c:`a` is an unsigned number where :c:`b` is a signed number. We stored a value of :c:`0xFF` in both variables, but :c:`a` treated this as the decimal number :c:`255` while b treated this as the decimal number :c:`-1`. When we compare the two variables, of course they aren't equal; but thats not very intuitive. Lets look at the equivalent Ada example:

.. code:: ada
   :class: ada-expect-compile-error

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
      type Char is range 0 .. 255;
      type Unsigned_Char is mod 256;

      A : Char := 16#FF#;
      B : Unsigned_Char := 16#FF#;
   begin

      Put_Line ("Does A = B?");

      if A = B then
         Put_Line ("Yes");
      else
         Put_Line ("No");
      end if;

   end Main;

If you try to run this Ada example you will get a compilation error. This is because the compiler is telling you that you cannot compare variables of two different types. We would need to explicitly cast one side to make the comparison against two variables of the same type. By enforcing the explicit cast we can't accidentally end up in a situation where we assume something will happen implicitly when, in fact, our assumption is incorrect.

