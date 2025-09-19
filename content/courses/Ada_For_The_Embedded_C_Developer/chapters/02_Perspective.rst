The C Developer's Perspective on Ada
======================================

.. include:: ../../../global.txt

What we mean by Embedded Software
------------------------------------

The Ada programming language is a general programming language, which means it
can be used for many different types of applications. One type of application
where it particularly shines is reliable and safety-critical embedded software;
meaning, a platform with a microprocessor such as ARM, PowerPC, x86, or RISC-V.
The application may be running on top of an embedded operating system, such as
an embedded Linux, or directly on bare metal. And the application domain can
range from small entities such as firmware or device controllers to flight
management systems, communication based train control systems, or advanced
driver assistance systems.

The GNAT Toolchain
-------------------

The toolchain used throughout this course is called GNAT, which is a suite of
tools with a compiler based on the GCC environment. It can be obtained from
AdaCore, either as part of a commercial contract with
`GNAT Pro <https://www.adacore.com/gnatpro>`_ or at no charge with the
`GNAT Community edition <https://www.adacore.com/community>`_. The information
in this course  will be relevant no matter which edition you're using. Most
examples will be runnable on the native Linux or Windows version for
convenience. Some will only be relevant in the context of a cross toolchain, in
which case we'll be using the embedded ARM bare metal toolchain.

As for any Ada compiler, GNAT takes advantage of implementation permissions and
offers a project management system. Because we're talking about embedded
platforms, there are a lot of topics that we'll go over which will be specific
to GNAT, and sometimes to specific platforms supported by GNAT. We'll try to
make the distinction between what is GNAT-specific and Ada generic as much as
possible throughout this course.

For an introduction to the GNAT Toolchain for the GNAT Community edition, you
may refer to the
:doc:`Introduction to GNAT Toolchain </courses/GNAT_Toolchain_Intro/index>`
course.

The GNAT Toolchain for Embedded Targets
-----------------------------------------

When we're discussing embedded programming, our target device is often
different from the host, which is the device we're using to actually write and
build an application. In this case, we're talking about cross compilation
platforms (concisely referred to as cross platforms).

The GNAT toolchain supports cross platform compilation for various
target devices. This section provides a short introduction to the topic. For
more details, please refer to the
`GNAT User’s Guide Supplement for Cross Platforms <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx.html>`_

GNAT supports two types of cross platforms:

- **cross targets**, where the target device has an embedded operating system.

    - ARM-Linux, which is commonly found in a Raspberry-Pi, is a prominent
      example.

- **bareboard targets**, where the run-times do not depend on an operating
  system.

    - In this case, the application has direct access to the system hardware.

For each platform, a set of run-time libraries is available. Run-time libraries
implement a subset of the Ada language for different use cases, and they're
different for each target platform. They may be selected via an attribute in
the project's GPR project file or as a command-line switch to
:program:`GPRbuild`. Although the run-time libraries may vary from target to
target, the user interface stays the same, providing portability for the
application.

Run-time libraries consists of:

#. Files that are dependent on the target board.

    - These files are responsible for configuring and interacting with the
      hardware.

    - They are known as a Board Support Package |mdash| commonly referred to by
      their abbrevation *BSP*.

#. Code that is target-independent.

    - This code implements language-defined functionality.

The bareboard run-time libraries are provided as customized run-times that are
configured to target a very specific micro-controller or processor. Therefore,
for different micro-controllers and processors, the run-time libraries need to
be ported to the specific target. These are some examples of what needs to be
ported:

- startup code / scripts;

- clock frequency initializations;

- memory mapping / allocation;

- interrupts and interrupt priorities;

- register descriptions.

For more details on the topic, please refer to the following chapters of the
`GNAT User’s Guide Supplement for Cross Platforms <https://docs.adacore.com/gnat_ugx-docs/html/gnat_ugx/gnat_ugx.html>`_:

- `Bareboard Topics <http://docs.adacore.com/live/wave/gnat_ugx/html/gnat_ugx/gnat_ugx/bareboard_topics.html>`_

- `Customized Run-Time Libraries <http://docs.adacore.com/live/wave/gnat_ugx/html/gnat_ugx/gnat_ugx/customized_run-time_libraries.html>`_

Hello World in Ada
--------------------

The first piece of code to translate from C to Ada is the usual Hello World
program:

[C]

.. code:: c run_button manual_chop project=Courses.Ada_For_Embedded_C_Dev.Perspective.Hello_World_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        printf("Hello World\n");
        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Hello_World_Ada

    with Ada.Text_IO;

    procedure Hello_World
    is
    begin
       Ada.Text_IO.Put_Line ("Hello World");
    end Hello_World;

The resulting program will print :ada:`Hello World` on the screen. Let's now
dissect the Ada version to describe what is going on:

The first line of the Ada code is giving us access to the :ada:`Ada.Text_IO`
library which contains the :ada:`Put_Line` function we will use to print the
text to the console. This is similar to C's :c:`#include <stdio.h>`. We then
create a procedure which executes :ada:`Put_Line` which prints to the console.
This is similar to C's :c:`printf` function. For now, we can assume these Ada
and C features have similar functionality. In reality, they are very different.
We will explore that more as we delve further into the Ada language.

You may have noticed that the Ada syntax is more verbose than C. Instead of
using braces :c:`{}` to declare scope, Ada uses keywords. :ada:`is` opens a
declarative scope |mdash| which is empty here as there's no variable to
declare. :ada:`begin` opens a sequence of statements. Within this sequence,
we're calling the function :ada:`Put_Line`, prefixing explicitly with the name
of the library unit where it's declared, :ada:`Ada.Text_IO`. The absence of the
end of line ``\n`` can also be noted, as :ada:`Put_Line` always terminates by
an end of line.

The Ada Syntax
----------------

Ada syntax might seem peculiar at first glance. Unlike many other languages,
it's not derived from the popular C style of notation with its ample use of
brackets; rather, it uses a more expository syntax coming from Pascal. In many
ways, Ada is a more explicit language |mdash| its syntax was designed to
increase readability and maintainability, rather than making it faster to write
in a condensed manner. For example:

- full words like :ada:`begin` and :ada:`end` are used in place of curly
  braces.

- Conditions are written using :ada:`if`, :ada:`then`, :ada:`elsif`,
  :ada:`else`, and :ada:`end if`.

- Ada's assignment operator does not double as an expression, eliminating
  potential mistakes that could be caused by :c:`=` being used where :c:`==`
  should be.

All languages provide one or more ways to express comments. In Ada, two
consecutive hyphens :ada:`--` mark the start of a comment that continues to the
end of the line. This is exactly the same as using :c:`//` for comments in C.
Multi line comments like C's :c:`/* */` do not exist in Ada.

Ada compilers are stricter with type and range checking than most C programmers
are used to. Most beginning Ada programmers encounter a variety of warnings and
error messages when coding, but this helps detect problems and vulnerabilities
at compile time |mdash| early on in the development cycle. In addition, checks
(such as array bounds checks) provide verification that could not be done at
compile time but can be performed either at run-time, or through formal proof
(with the SPARK tooling).

Ada identifiers and reserved words are case insensitive. The identifiers
:ada:`VAR`, :ada:`var` and :ada:`VaR` are treated as the same identifier;
likewise :ada:`begin`, :ada:`BEGIN`, :ada:`Begin`, etc. Identifiers may include
letters, digits, and underscores, but must always start with a letter. There
are 73 reserved keywords in Ada that may not be used as identifiers, and these
are:

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

Both C and Ada were designed with the idea that the code specification and code
implementation could be separated into two files. In C, the specification
typically lives in the .h, or header file, and the implementation lives in the
.c file. Ada is superficially similar to C. With the GNAT toolchain,
compilation units are stored in files with an .ads extension for specifications
and with an .adb extension for implementations.

One main difference between the C and Ada compilation structure is that Ada
compilation units are structured into something called packages.

Packages
--------
The package is the basic modularization unit of the Ada language, as is the
class for Java and the header and implementation pair for C.
A specification defines a package and the implementation implements the package.
We saw this in an earlier example when we included the :ada:`Ada.Text_IO`
package into our application. The package specification has the structure:

[Ada]

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


Declaration Protection
~~~~~~~~~~~~~~~~~~~~~~

An Ada package contains three parts that, for GNAT, are separated into two files:
:file:`.ads` files contain public and private Ada specifications, and
:file:`.adb` files contain the implementation, or Ada bodies.

[Ada]

.. code-block:: ada

    package Package_Name is
       -- public specifications
    private
       -- private specifications
    end Package_Name;

    package body Package_Name is
       -- implementation
    end Package_Name;

Private types are useful for preventing the users of a package's types from
depending on the types' implementation details. Another use-case is the prevention
of package users from accessing package state/data arbitrarily. The private
reserved word splits the package spec into *public* and *private* parts.
For example:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Private_Types

    package Types is
       type Type_1 is private;
       type Type_2 is private;
       type Type_3 is private;
       procedure P (X : Type_1);
       --  ...
    private
       procedure Q (Y : Type_1);
       type Type_1 is new Integer range 1 .. 1000;
       type Type_2 is array (Integer range 1 .. 1000) of Integer;
       type Type_3 is record
          A, B : Integer;
       end record;
    end Types;

Subprograms declared above the :ada:`private` separator (such as :ada:`P`) will
be visible to the package user, and the ones below (such as :ada:`Q`) will not.
The body of the package, the implementation, has access to both parts.
A package specification does not require a private section.

Hierarchical Packages
~~~~~~~~~~~~~~~~~~~~~

Ada packages can be organized into hierarchies. A child unit can be declared in
the following way:

[Ada]

.. code-block:: ada

    -- root-child.ads

    package Root.Child is
       --  package spec goes here
    end Root.Child;

    -- root-child.adb

    package body Root.Child is
       --  package body goes here
    end Root.Child;

Here, :ada:`Root.Child` is a child package of :ada:`Root`. The public part of
:ada:`Root.Child` has access to the public part of :ada:`Root`. The private
part of :ada:`Child` has access to the private part of :ada:`Root`, which is
one of the main advantages of child packages. However, there is no visibility
relationship between the two bodies. One common way to use this capability is
to define subsystems around a hierarchical naming scheme.

Using Entities from Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Entities declared in the visible part of a package specification can be made
accessible using a :ada:`with` clause that references the package, which is
similar to the C :c:`#include` directive. After a :ada:`with` clause makes a
package available, references to the package contents require the name of the
package as a prefix, with a dot after the package name.
This prefix can be omitted if a :ada:`use` clause is employed.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Using_Pkg_Entities

    --  pck.ads

    package Pck is
       My_Glob : Integer;
    end Pck;

    --  main.adb

    with Pck;

    procedure Main is
    begin
       Pck.My_Glob := 0;
    end Main;

In contrast to C, the Ada :ada:`with` clause is a *semantic inclusion*
mechanism rather than a *text inclusion* mechanism; for more information on
this difference please refer to
:doc:`Packages </courses/intro-to-ada/chapters/modular_programming>`.

Statements and Declarations
----------------------------

The following code samples are all equivalent, and illustrate the use of
comments and working with integer variables:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Var_Decl_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
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

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Var_Decl_Ada

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

You'll notice that, in both languages, statements are terminated with a
semicolon. This means that you can have multi-line statements.

.. admonition:: The shortcuts of incrementing and decrementing

    You may have noticed that Ada does not have something similar to the
    :c:`a++` or :c:`a--` operators. Instead you must use the full assignment
    :ada:`A := A + 1` or :ada:`A := A - 1`.

In the Ada example above, there are two distinct sections to the
:ada:`procedure Main`. This first section is delimited by the :ada:`is` keyword
and the :ada:`begin` keyword. This section is called the declarative block of
the subprogram. The declarative block is where you will define all the local
variables which will be used in the subprogram. C89 had something similar,
where developers were required to declare their variables at the top of the
scope block. Most C developers may have run into this before when trying to
write a for loop:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Average_C89

    !main.c
    /* The C89 version */

    #include <stdio.h>

    int average(int* list, int length)
    {
        int i;
        int sum = 0;

        for(i = 0; i < length; ++i) {
            sum += list[i];
        }
        return (sum / length);
    }

    int main(int argc, const char * argv[])
    {
        int vals[] = { 2, 2, 4, 4 };

        printf("Average: %d\n", average(vals, 4));

        return 0;
    }

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Average_C_Modern

    !main.c
    // The modern C way

    #include <stdio.h>

    int average(int* list, int length)
    {
        int sum = 0;

        for(int i = 0; i < length; ++i) {
            sum += list[i];
        }

        return (sum / length);
    }

    int main(int argc, const char * argv[])
    {
        int vals[] = { 2, 2, 4, 4 };

        printf("Average: %d\n", average(vals, 4));

        return 0;
    }

For the fun of it, let's also see the Ada way to do this:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Average_Ada

    with Ada.Text_IO;

    procedure Main is
       type Int_Array is array (Natural range <>) of Integer;

       function Average (List : Int_Array) return Integer
       is
          Sum : Integer := 0;
       begin
          for I in List'Range loop
             Sum := Sum + List (I);
          end loop;

          return (Sum / List'Length);
       end Average;

       Vals : constant Int_Array (1 .. 4) := (2, 2, 4, 4);
    begin
       Ada.Text_IO.Put_Line ("Average: " & Integer'Image (Average (Vals)));
    end Main;

We will explore more about the syntax of loops in Ada in a future section of
this course; but for now, notice that the :ada:`I` variable used as the loop
index is not declared in the declarative section!

.. admonition:: Declaration Flippy Floppy

    Something peculiar that you may have noticed about declarations in Ada is
    that they are backwards from the way C does declarations. The C language
    expects the type followed by the variable name. Ada expects the variable
    name followed by a colon and then the type.

The next block in the Ada example is between the :ada:`begin` and :ada:`end`
keywords. This is where your statements will live. You can create new scopes by
using the :ada:`declare` keyword:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Var_Decl_Block_Ada

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
          E : constant Integer := D * 100;
       begin
          --  printing the result
          Ada.Text_IO.Put_Line ("E =" & E'Img);
       end;

    end Main;

Notice that we declared a new variable :ada:`E` whose scope only exists in our
newly defined block. The equivalent C code is:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Var_Decl_Block_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
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
            const int e = d * 100;
            printf("e = %d\n", e);
        }

        return 0;
    }

**Fun Fact** about the C language assignment operator :c:`=`: Did you know that
an assignment in C can be used in an expression? Let's look at an example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Equal_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int a = 0;

        if (a = 10)
            printf("True\n");
        else
            printf("False\n");

        return 0;
    }

Run the above code example. What does it output? Is that what you were
expecting?

The author of the above code example probably meant to test if :c:`a == 10` in
the if statement but accidentally typed :c:`=` instead of :c:`==`. Because C
treats assignment as an expression, it was able to evaluate :c:`a = 10`.

Let's look at the equivalent Ada code:

[Ada]

.. code:: ada manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Equal_Ada
    :class: ada-nocheck

    !main.adb
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

The above code will not compile. This is because Ada does no allow assignment
as an expression.

.. admonition:: The "use" clause

    You'll notice in the above code example, after :ada:`with Ada.Text_IO;`
    there is a new statement we haven't seen before |mdash|
    :ada:`use Ada.Text_IO;`. You may also notice that we are not using the
    :ada:`Ada.Text_IO` prefix before the :ada:`Put_Line` statements. When we
    add the use clause it tells the compiler that we won't be using the prefix
    in the call to subprograms of that package. The use clause is something to
    use with caution. For example: if we use the :ada:`Ada.Text_IO` package and
    we also have a :ada:`Put_Line` subprogram in our current compilation unit
    with the same signature, we have a (potential) collision!

Conditions
------------

The syntax of an if statement:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Condition_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        // try changing the initial value to change the
        //    output of the program
        int v = 0;

        if (v > 0) {
            printf("Positive\n");
        }
        else if (v < 0) {
            printf("Negative\n");
        }
        else {
            printf("Zero\n");
        }

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Condition_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main
    is
       --  try changing the initial value to change the
       --    output of the program
       V : constant Integer := 0;
    begin
       if V > 0 then
          Put_Line ("Positive");
       elsif V < 0 then
          Put_Line ("Negative");
       else
          Put_Line ("Zero");
       end if;
    end Main;

In Ada, everything that appears between the :ada:`if` and :ada:`then` keywords
is the conditional expression, no parentheses are required. Comparison operators
are the same except for:

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

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Switch_Case_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
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

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Switch_Case_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main
    is
       --  try changing the initial value to change the
       --    output of the program
       V : constant Integer := 0;
    begin
       case V is
          when 0 =>
             Put_Line ("Zero");
          when 1 .. 9 =>
             Put_Line ("Positive");
          when 10 | 12 | 14 | 16 | 18 =>
             Put_Line ("Even number between 10 and 18");
          when others =>
             Put_Line ("Something else");
       end case;
    end Main;

.. admonition:: Switch or Case?

    A switch statement in C is the same as a case statement in Ada. This may be
    a little strange because C uses both keywords in the statement syntax.
    Let's make an analogy between C and Ada: C's :c:`switch` is to Ada's
    :ada:`case` as C's :c:`case` is to Ada's :ada:`when`.

Notice that in Ada, the case statement does not use the :c:`break` keyword. In
C, we use :c:`break` to stop the execution of a case branch from falling
through to the next branch. Here is an example:

[C]

.. code:: c manual_chop cli_input run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Switch_Case_Break_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
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

Run the above code with :c:`v = 0`. What prints? What prints when we change the
assignment to :c:`v = 1`?

When :c:`v = 0` the program outputs the strings :c:`Zero` then :c:`One` then
:c:`Other`. This is called fall through. If you add the :c:`break` statements
back into the :c:`switch` you can stop this fall through behavior from
happening. The reason why fall through is allowed in C is to allow the behavior
from the previous example where we want a specific branch to execute for
multiple inputs. Ada solves this a different way because it is possible, or
even probable, that the developer might forget a :c:`break` statement
accidentally. So Ada does not allow fall through. Instead, you can use Ada's
syntax to identify when a specific branch can be executed by more than one
input. If you want a range of values for a specific branch you can use the
:ada:`First .. Last` notation. If you want a few non-consecutive values you can
use the :ada:`Value1 | Value2 | Value3` notation.

Instead of using the word :c:`default` to denote the catch-all case, Ada uses
the :ada:`others` keyword.

Loops
------

Let's start with some syntax:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loops_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int v;

        // this is a while loop
        v = 1;
        while(v < 100) {
            v *= 2;
        }
        printf("v = %d\n", v);

        // this is a do while loop
        v = 1;
        do {
            v *= 2;
        } while(v < 200);
        printf("v = %d\n", v);

        // this is a for loop
        v = 0;
        for(int i = 0; i < 5; ++i) {
            v += (i * i);
        }
        printf("v = %d\n", v);

        // this is a forever loop with a conditional exit
        v = 0;
        while(1) {
            // do stuff here
            v += 1;
            if(v == 10)
                break;
        }
        printf("v = %d\n", v);

        // this is a loop over an array
        {
            #define ARR_SIZE (10)
            const int arr[ARR_SIZE] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            int sum = 0;

            for(int i = 0; i < ARR_SIZE; ++i) {
                sum += arr[i];
            }
            printf("sum = %d\n", sum);
        }

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loops_Ada

    with Ada.Text_IO;

    procedure Main is
       V : Integer;
    begin
       --  this is a while loop
       V := 1;
       while V < 100 loop
          V := V * 2;
       end loop;
       Ada.Text_IO.Put_Line ("V = " & Integer'Image (V));

       --  Ada doesn't have an explicit do while loop
       --  instead you can use the loop and exit keywords
       V := 1;
       loop
          V := V * 2;
          exit when V >= 200;
       end loop;
       Ada.Text_IO.Put_Line ("V = " & Integer'Image (V));

       --  this is a for loop
       V := 0;
       for I in 0 .. 4 loop
          V := V + (I * I);
       end loop;
       Ada.Text_IO.Put_Line ("V = " & Integer'Image (V));

       --  this is a forever loop with a conditional exit
       V := 0;
       loop
          --  do stuff here
          V := V + 1;
          exit when V = 10;
       end loop;
       Ada.Text_IO.Put_Line ("V = " & Integer'Image (V));

       --  this is a loop over an array
       declare
          type Int_Array is array (Natural range 1 .. 10) of Integer;

          Arr : constant Int_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
          Sum : Integer := 0;
       begin
          for I in Arr'Range loop
             Sum := Sum + Arr (I);
          end loop;
          Ada.Text_IO.Put_Line ("Sum = " & Integer'Image (Sum));
       end;
    end Main;

The loop syntax in Ada is pretty straightforward. The :ada:`loop` and :ada:`end
loop` keywords are used to open and close the loop scope. Instead of using the
:c:`break` keyword to exit the loop, Ada has the :ada:`exit` statement. The
:ada:`exit` statement can be combined with a logic expression using the
:ada:`exit when` syntax.

The major deviation in loop syntax is regarding for loops. You'll notice, in C,
that you sometimes declare, and at least initialize a loop counter variable,
specify a loop predicate, or an expression that indicates when the loop should
continue executing or complete, and last you specify an expression to update
the loop counter.

[C]

.. code-block:: c

    for (initialization expression; loop predicate; update expression) {
        // some statements
    }

In Ada, you don't declare or initialize a loop counter or specify an update
expression. You only name the loop counter and give it a range to loop over.
The loop counter is **read-only**! You cannot modify the loop counter inside
the loop like you can in C. And the loop counter will increment consecutively
along the specified range. But what if you want to loop over the range in
reverse order?

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Counter_C

    !main.c
    #include <stdio.h>

    #define MY_RANGE (10)

    int main(int argc, const char * argv[])
    {

        for (int i = MY_RANGE; i >= 0; --i) {
            printf("%d\n", i);
        }

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Counter_Ada

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

    Strangely enough, Ada people call the single apostrophe symbol, :ada:`'`,
    "tick". This "tick" says the we are accessing an attribute of the variable.
    When we do :ada:`'Img` on a variable of a numerical type, we are going to
    return the string version of that numerical type. So in the for loop above,
    :ada:`I'Img`, or "I tick image" will return the string representation of
    the numerical value stored in I. We have to do this because Put_Line is
    expecting a string as an input parameter.

    We'll discuss attributes in more details
    :ref:`later in this chapter <Ada_For_Embedded_C_Dev_Attributes>`.

In the above example, we are traversing over the range in reverse order. In
Ada, we use the :ada:`reverse` keyword to accomplish this.

In many cases, when we are writing a for loop, it has something to do with
traversing an array. In C, this is a classic location for off-by-one errors.
Let's see an example in action:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Reverse_C

    !main.c
    #include <stdio.h>

    #define LIST_LENGTH (100)

    int main(int argc, const char * argv[])
    {
        int list[LIST_LENGTH];

        for(int i = LIST_LENGTH; i > 0; --i) {
            list[i] = LIST_LENGTH - i;
        }

        for (int i = 0; i < LIST_LENGTH; ++i)
        {
            printf("%d ", list[i]);

            if (i % 10 == 0) {
                printf("\n");
            }
        }

        return 0;
    }

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Reverse_Ada

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

          if I mod 10 = 0 then
             New_Line;
          end if;
       end loop;

    end Main;

The above Ada and C code should initialize an array using a for loop. The
initial values in the array should be contiguously decreasing from 99 to 0 as
we index from the first index to the last index. In other words, the first
index has a value of 99, the next has 98, the next 97 ... the last has a value
of 0.

If you run both the C and Ada code above you'll notice that the outputs of the
two programs are different. Can you spot why?

In the C code there are two problems:

#. There's a buffer overflow in the first iteration of the loop. We would need
   to modify the loop initialization to :c:`int i = LIST_LENGTH - 1;`. The loop
   predicate should be modified to :c:`i >= 0;`

#. The C code also has another off-by-one problem in the math to compute the
   value stored in :c:`list[i]`. The expression should be changed to be
   :c:`list[i] = LIST_LENGTH - i - 1;`.

These are typical off-by-one problems that plagues C programs. You'll notice
that we didn't have this problem with the Ada code because we aren't defining
the loop with arbitrary numeric literals. Instead we are accessing attributes
of the array we want to manipulate and are using a keyword to determine the
indexing direction.

We can actually simplify the Ada for loop a little further using iterators:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Reverse_Ada_Simplified

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

          if I mod 10 = 0 then
             New_Line;
          end if;
       end loop;

    end Main;

In the second for loop, we changed the syntax to :ada:`for I of List`. Instead
of I being the index counter, it is now an iterator that references the
underlying element. This example of Ada code is identical to the last bit of
Ada code. We just used a different method to index over the second for loop.
There is no C equivalent to this Ada feature, but it is similar to C++'s range
based for loop.

Type System
--------------

Strong Typing
~~~~~~~~~~~~~

Ada is considered a "strongly typed" language. This means that the language
does not define any implicit type conversions. C does define implicit type
conversions, sometimes referred to as *integer promotion*. The rules for
promotion are fairly straightforward in simple expressions but can get
confusing very quickly. Let's look at a typical place of confusion with
implicit type conversion:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Strong_Typing_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
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

Run the above code. You will notice that :c:`a != b`! If we look at the output
of the last :c:`printf` statement we will see the problem. :c:`a` is an
unsigned number where :c:`b` is a signed number. We stored a value of :c:`0xFF`
in both variables, but :c:`a` treated this as the decimal number :c:`255` while
b treated this as the decimal number :c:`-1`. When we compare the two
variables, of course they aren't equal; but that's not very intuitive. Let's
look at the equivalent Ada example:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Strong_Typing_Ada
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

If you try to run this Ada example you will get a compilation error. This is
because the compiler is telling you that you cannot compare variables of two
different types. We would need to explicitly cast one side to make the
comparison against two variables of the same type. By enforcing the explicit
cast we can't accidentally end up in a situation where we assume something will
happen implicitly when, in fact, our assumption is incorrect.

Another example: you can't divide an integer by a float. You need to perform
the division operation using values of the same type, so one value must be
explicitly converted to match the type of the other (in this case the more
likely conversion is from integer to float). Ada is designed to guarantee that
what's done by the program is what's meant by the programmer, leaving as little
room for compiler interpretation as possible. Let's have a look at the
following example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Strong_Typing_Ada_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Strong_Typing is
       Alpha  : constant Integer := 1;
       Beta   : constant Integer := 10;
       Result : Float;
    begin
       Result := Float (Alpha) / Float (Beta);

       Put_Line (Float'Image (Result));
    end Strong_Typing;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Strong_Typing_C_2

    !main.c
    #include <stdio.h>

    void weakTyping (void) {
        const int   alpha = 1;
        const int   beta = 10;
        float result;

        result = alpha / beta;

        printf("%f\n", result);
    }

    int main(int argc, const char * argv[])
    {
        weakTyping();

        return 0;
    }

Are the three programs above equivalent? It may seem like Ada is just adding
extra complexity by forcing you to make the conversion from :ada:`Integer` to
:ada:`Float` explicit. In fact, it significantly changes the behavior of the
computation. While the Ada code performs a floating point operation 1.0 / 10.0
and stores 0.1 in :ada:`Result`, the C version instead store 0.0 in
:c:`result`. This is because the C version perform an integer operation between
two integer variables: 1 / 10 is 0. The
result of the integer division is then converted to a :c:`float` and stored.
Errors of this sort can be very hard to locate in complex pieces of code, and
systematic specification of how the operation should be interpreted helps to
avoid this class of errors. If an integer division was actually intended in the
Ada case, it is still necessary to explicitly convert the final result to
:ada:`Float`:

[Ada]

.. code-block:: ada

    -- Perform an Integer division then convert to Float
    Result := Float (Alpha / Beta);

The complete example would then be:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Strong_Typing_Ada_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Strong_Typing is
       Alpha  : constant Integer := 1;
       Beta   : constant Integer := 10;
       Result : Float;
    begin
       Result := Float (Alpha / Beta);

       Put_Line (Float'Image (Result));
    end Strong_Typing;

.. admonition:: Floating Point Literals

    In Ada, a floating point literal must be written with both an integral and
    decimal part. :ada:`10` is not a valid literal for a floating point value,
    while :ada:`10.0` is.

Language-Defined Types
~~~~~~~~~~~~~~~~~~~~~~

The principal scalar types predefined by Ada are :ada:`Integer`, :ada:`Float`,
:ada:`Boolean`, and :ada:`Character`. These correspond to :c:`int`, :c:`float`,
:c:`int` (when used for Booleans), and :c:`char`, respectively. The names for
these types are not reserved words; they are regular identifiers. There are
other language-defined integer and floating-point types as well. All have
implementation-defined ranges and precision.

Application-Defined Types
~~~~~~~~~~~~~~~~~~~~~~~~~

Ada's type system encourages programmers to think about data at a high level of
abstraction. The compiler will at times output a simple efficient machine
instruction for a full line of source code (and some instructions can be
eliminated entirely). The careful programmer's concern that the operation
really makes sense in the real world would be satisfied, and so would the
programmer's concern about performance.

The next example below defines two different metrics: area and distance. Mixing
these two metrics must be done with great care, as certain operations do not
make sense, like adding an area to a distance. Others require knowledge of the
expected semantics; for example, multiplying two distances. To help avoid
errors, Ada requires that each of the binary operators ``+``, ``-``, ``*``, and
``/`` for integer and floating-point types take operands of the same type and
return a value of that type.

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Application_Defined_Types
    :class: ada-expect-compile-error

    procedure Main is
       type Distance is new Float;
       type Area is new Float;

       D1 : Distance := 2.0;
       D2 : Distance := 3.0;
       A  : Area;
    begin
       D1 := D1 + D2; -- OK
       D1 := D1 + A;  -- NOT OK: incompatible types for "+"
       A  := D1 * D2; -- NOT OK: incompatible types for ":="
       A  := Area (D1 * D2); -- OK
    end Main;

Even though the :ada:`Distance` and :ada:`Area` types above are just
:ada:`Float`, the compiler does not allow arbitrary mixing of values of these
different types. An explicit conversion (which does not necessarily mean any
additional object code) is necessary.

The predefined Ada rules are not perfect; they admit some problematic cases
(for example multiplying two :ada:`Distance` yields a :ada:`Distance`) and
prohibit some useful cases (for example multiplying two :ada:`Distances` should
deliver an :ada:`Area`). These situations can be handled through other
mechanisms. A predefined operation can be identified as abstract to make it
unavailable; overloading can be used to give new interpretations to existing
operator symbols, for example allowing an operator to return a value from a
type different from its operands; and more generally, GNAT has introduced a
facility that helps perform dimensionality checking.

Ada enumerations work similarly to C :c:`enum`:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enumeration_Ada

    procedure Main is
       type Day is
         (Monday,
          Tuesday,
          Wednesday,
          Thursday,
          Friday,
          Saturday,
          Sunday);

       D : Day := Monday;
    begin
       null;
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enumeration_C

    !main.c
    enum Day {
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
    };

    int main(int argc, const char * argv[])
    {
        enum Day d = Monday;

        return 0;
    }

But even though such enumerations may be implemented by the compiler as numeric
values, at the language level Ada will not confuse the fact that :ada:`Monday`
is a :ada:`Day` and is not an :ada:`Integer`. You can compare a :ada:`Day` with
another :ada:`Day`, though. To specify implementation details like the numeric
values that correspond with enumeration values in C you include them in the
original :c:`enum` declaration:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enumeration_Values_C

    !main.c
    #include <stdio.h>

    enum Day {
        Monday    = 10,
        Tuesday   = 11,
        Wednesday = 12,
        Thursday  = 13,
        Friday    = 14,
        Saturday  = 15,
        Sunday    = 16
    };

    int main(int argc, const char * argv[])
    {
        enum Day d = Monday;

        printf("d = %d\n", d);

        return 0;
    }

But in Ada you must use both a type definition for :ada:`Day` as well as a
separate representation clause for it like:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enumeration_Values

    with Ada.Text_IO;

    procedure Main is
       type Day is
         (Monday,
          Tuesday,
          Wednesday,
          Thursday,
          Friday,
          Saturday,
          Sunday);

       --  Representation clause for Day type:
       for Day use
         (Monday    => 10,
          Tuesday   => 11,
          Wednesday => 12,
          Thursday  => 13,
          Friday    => 14,
          Saturday  => 15,
          Sunday    => 16);

       D : Day := Monday;
       V : Integer;
    begin
       V := Day'Enum_Rep (D);
       Ada.Text_IO.Put_Line (Integer'Image (V));
    end Main;

Note that however, unlike C, values for enumerations in Ada have to be unique.

Type Ranges
~~~~~~~~~~~

Contracts can be associated with types and variables, to refine values and
define what are considered valid values. The most common kind of contract is a
*range constraint* introduced with the :ada:`range` reserved word, for example:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Range_Check
    :class: ada-expect-compile-error

    procedure Main is
       type Grade is range 0 .. 100;

       G1, G2  : Grade;
       N       : Integer;
    begin
       --  ...              --  Initialization of N
       G1 := 80;            --  OK
       G1 := N;             --  Illegal (type mismatch)
       G1 := Grade (N);     --  Legal, run-time range check
       G2 := G1 + 10;       --  Legal, run-time range check
       G1 := (G1 + G2) / 2; --  Legal, run-time range check
    end Main;

In the above example, :ada:`Grade` is a new integer type associated with a
range check. Range checks are dynamic and are meant to enforce the property
that no object of the given type can have a value outside the specified range.
In this example, the first assignment to :ada:`G1` is correct and will not
raise a run-time exception. Assigning :ada:`N` to :ada:`G1` is illegal since
:ada:`Grade` is a different type than :ada:`Integer`. Converting :ada:`N` to
:ada:`Grade` makes the assignment legal, and a range check on the conversion
confirms that the value is within :ada:`0 .. 100`.  Assigning :ada:`G1 + 10` to
:ada:`G2` is legal since :ada:`+` for :ada:`Grade` returns a :ada:`Grade` (note
that the literal :ada:`10` is interpreted as a :ada:`Grade` value in this
context), and again there is a range check.

The final assignment illustrates an interesting but subtle point. The
subexpression :ada:`G1 + G2` may be outside the range of :ada:`Grade`, but the
final result will be in range. Nevertheless, depending on the representation
chosen for :ada:`Grade`, the addition may overflow. If the compiler represents
:ada:`Grade` values as signed 8-bit integers (i.e., machine numbers in the
range :ada:`-128 .. 127`) then the sum :ada:`G1 + G2` may exceed 127, resulting
in an integer overflow. To prevent this, you can use explicit conversions and
perform the computation in a sufficiently large integer type, for example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Range_And_Explicit_Conversion

    with Ada.Text_IO;

    procedure Main is
       type Grade is range 0 .. 100;

       G1, G2 : Grade := 99;
    begin
       G1 := Grade ((Integer (G1) + Integer (G2)) / 2);
       Ada.Text_IO.Put_Line (Grade'Image (G1));
    end Main;

Range checks are useful for detecting errors as early as possible. However,
there may be some impact on performance. Modern compilers do know how to remove
redundant checks, and you can deactivate these checks altogether if you have
sufficient confidence that your code will function correctly.

Types can be derived from the representation of any other type. The new derived
type can be associated with new constraints and operations. Going back to the
:ada:`Day` example, one can write:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enum_Ranges_1

    procedure Main is
       type Day is
         (Monday,
          Tuesday,
          Wednesday,
          Thursday,
          Friday,
          Saturday,
          Sunday);

       type Business_Day is new Day range Monday .. Friday;
       type Weekend_Day is new Day range Saturday .. Sunday;
    begin
       null;
    end Main;

Since these are new types, implicit conversions are not allowed. In this case,
it's more natural to create a new set of constraints for the same type, instead
of making completely new ones. This is the idea behind *subtypes* in Ada. A
subtype is a type with optional additional constraints. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Enum_Ranges_2

    procedure Main is
       type Day is
         (Monday,
          Tuesday,
          Wednesday,
          Thursday,
          Friday,
          Saturday,
          Sunday);

       subtype Business_Day is Day range Monday .. Friday;
       subtype Weekend_Day is Day range Saturday .. Sunday;
       subtype Dice_Throw is Integer range 1 .. 6;
    begin
       null;
    end Main;

These declarations don't create new types, just new names for constrained
ranges of their base types.

The purpose of numeric ranges is to express some application-specific
constraint that we want the compiler to help us enforce. More importantly,
we want the compiler to tell us when that constraint cannot be met |mdash| when
the underlying hardware cannot support the range given. There are two things to
consider:

- just a range constraint, such as :ada:`A : Integer range 0 .. 10;`, or

- a type declaration, such as :ada:`type Result is range 0 .. 1_000_000_000;`.

Both represent some sort of application-specific constraint, but in addition,
the type declaration promotes portability because it won't compile on targets
that do not have a sufficiently large hardware numeric type. That's a
definition of portability that is preferable to having something compile
anywhere but not run correctly, as in C.

Unsigned And Modular Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

Unsigned integer numbers are quite common in embedded applications. In C, you
can use them by declaring :c:`unsigned int` variables. In Ada, you have two
options:

- declare custom *unsigned* range types;

    - In addition, you can declare custom range *subtypes* or use existing
      subtypes such as :ada:`Natural`.

- declare custom modular types.

The following table presents the main features of each type. We discuss these
types right after.

+-------------------+-----------------------+----------------------+---------------+
| Feature           | [C] :c:`unsigned int` | [Ada] Unsigned range | [Ada] Modular |
+===================+=======================+======================+===============+
| Excludes negative | |check|               | |check|              | |check|       |
| value             |                       |                      |               |
+-------------------+-----------------------+----------------------+---------------+
| Wraparound        | |check|               |                      | |check|       |
+-------------------+-----------------------+----------------------+---------------+

When declaring custom range types in Ada, you may use the full range in the
same way as in C. For example, this is the declaration of a 32-bit unsigned
integer type and the :ada:`X` variable in Ada:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Unsigned_32_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Unsigned_Int_32 is range 0 .. 2 ** 32 - 1;

       X : Unsigned_Int_32 := 42;
    begin
       Put_Line ("X = " & Unsigned_Int_32'Image (X));
    end Main;

In C, when :c:`unsigned int` has a size of 32 bits, this corresponds to the
following declaration:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Unsigned_32_C

    !main.c
    #include <stdio.h>
    #include <limits.h>

    int main(int argc, const char * argv[])
    {
        unsigned int x = 42;
        printf("x = %u\n", x);

        return 0;
    }

Another strategy is to declare subtypes for existing signed types and specify
just the range that excludes negative numbers. For example, let's declare a
custom 32-bit signed type and its unsigned subtype:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Unsigned_31_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Signed_Int_32 is range -2 ** 31 .. 2 ** 31 - 1;

       subtype Unsigned_Int_31 is Signed_Int_32 range 0 .. Signed_Int_32'Last;
       --  Equivalent to:
       --  subtype Unsigned_Int_31 is Signed_Int_32 range 0 .. 2 ** 31 - 1;

       X : Unsigned_Int_31 := 42;
    begin
       Put_Line ("X = " & Unsigned_Int_31'Image (X));
    end Main;

In this case, we're just skipping the sign bit of the :ada:`Signed_Int_32`
type. In other words, while :ada:`Signed_Int_32` has a size of 32 bits,
:ada:`Unsigned_Int_31` has a range of 31 bits, even if the base type has
32 bits.

Note that the declaration above is actually similar to the existing
:ada:`Natural` subtype. Ada provides the following standard subtypes:

.. code-block:: ada

    subtype Natural  is Integer range 0..Integer'Last;
    subtype Positive is Integer range 1..Integer'Last;

Since they're standard subtypes, you can declare variables of those subtypes
directly in your implementation, in the same way as you can declare
:ada:`Integer` variables.

As indicated in the table above, however, there is a difference in behavior for
the variables we just declared, which occurs in case of overflow. Let's
consider this C example:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Overflow_Wraparound_C

    !main.c
    #include <stdio.h>
    #include <limits.h>

    int main(int argc, const char * argv[])
    {
        unsigned int x = UINT_MAX + 1;
        /* Now: x == 0 */

        printf("x = %u\n", x);

        return 0;
    }

The corresponding code in Ada raises an exception:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Overflow_Wraparound_Ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Unsigned_Int_32 is range 0 .. 2 ** 32 - 1;

       X : Unsigned_Int_32 := Unsigned_Int_32'Last + 1;
       --  Overflow: exception is raised!
    begin
       Put_Line ("X = " & Unsigned_Int_32'Image (X));
    end Main;

While the C uses modulo arithmetic for unsigned integer, Ada doesn't use it for
the :ada:`Unsigned_Int_32` type. Ada does, however, support modular types
via type definitions using the :ada:`mod` keyword. In this example, we declare
a 32-bit modular type:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Overflow_Wraparound_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Unsigned_32 is mod 2**32;

       X : Unsigned_32 := Unsigned_32'Last + 1;
       --  Now: X = 0
    begin
       Put_Line ("X = " & Unsigned_32'Image (X));
    end Main;

In this case, the behavior is the same as in the C declaration above.

Modular types, unlike Ada's signed integers, also provide bit-wise operations,
a typical application for unsigned integers in C. In Ada, you can use operators
such as :ada:`and`, :ada:`or`, :ada:`xor` and :ada:`not`. You can also use
typical bit-shifting operations, such as :ada:`Shift_Left`, :ada:`Shift_Right`,
:ada:`Shift_Right_Arithmetic`, :ada:`Rotate_Left` and :ada:`Rotate_Right`.

.. _Ada_For_Embedded_C_Dev_Attributes:

Attributes
~~~~~~~~~~

Attributes start with a single apostrophe ("tick"), and they allow you to query
properties of, and perform certain actions on, declared entities such as types,
objects, and subprograms. For example, you can determine the first and last
bounds of scalar types, get the sizes of objects and types, and convert values
to and from strings. This section provides an overview of how attributes work.
For more information on the many attributes defined by the language, you can
refer directly to the Ada Language Reference Manual.

The :ada:`'Image` and :ada:`'Value` attributes allow you to transform a scalar
value into a :ada:`String` and vice-versa. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Image_Attribute

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       A : Integer := 10;
    begin
       Put_Line (Integer'Image (A));
       A := Integer'Value ("99");
       Put_Line (Integer'Image (A));
    end Main;

.. admonition:: Important

    Semantically, attributes are equivalent to subprograms. For example,
    :ada:`Integer'Image` is defined as follows:

    .. code-block:: ada

        function Integer'Image(Arg : Integer'Base) return String;

Certain attributes are provided only for certain kinds of types. For example,
the :ada:`'Val` and :ada:`'Pos` attributes for an enumeration type associates a
discrete value with its position among its peers. One circuitous way of moving
to the next character of the ASCII table is:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Character_1

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       C : Character := 'a';
    begin
       Put (C);
       C := Character'Val (Character'Pos (C) + 1);
       Put (C);
    end Main;

A more concise way to get the next value in Ada is to use the :ada:`'Succ`
attribute:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Character_1

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       C : Character := 'a';
    begin
       Put (C);
       C := Character'Succ (C);
       Put (C);
    end Main;

You can get the previous value using the :ada:`'Pred` attribute. Here is the
equivalent in C:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Loop_Reverse_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        char c = 'a';
        printf("%c", c);
        c++;
        printf("%c", c);

        return 0;
    }

Other interesting examples are the :ada:`'First` and :ada:`'Last` attributes
which, respectively, return the first and last values of a scalar type. Using
32-bit integers, for instance, :ada:`Integer'First` returns -2\ :sup:`31` and
:ada:`Integer'Last` returns 2\ :sup:`31` - 1.

Arrays and Strings
~~~~~~~~~~~~~~~~~~

C arrays are pointers with offsets, but the same is not the case for Ada.
Arrays in Ada are not interchangeable with operations on pointers, and array
types are considered first-class citizens. They have dedicated semantics such
as the availability of the array's boundaries at run-time. Therefore, unhandled
array overflows are impossible unless checks are suppressed. Any discrete type
can serve as an array index, and you can specify both the starting and ending
bounds |mdash| the lower bound doesn't necessarily have to be 0. Most of the
time, array types need to be explicitly declared prior to the declaration of an
object of that array type.

Here's an example of declaring an array of 26 characters, initializing the
values from :ada:`'a'` to :ada:`'z'`:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Range_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Character;
       Arr : Arr_Type (1 .. 26);
       C : Character := 'a';
    begin
       for I in Arr'Range loop
          Arr (I) := C;
          C := Character'Succ (C);

          Put (Arr (I) & " ");

          if I mod 7 = 0 then
             New_Line;
          end if;
       end loop;
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Range_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        char Arr [26];
        char C = 'a';

        for (int I = 0; I < 26; ++I) {
            Arr [I] = C++;
            printf ("%c ", Arr [I]);

            if ((I + 1) % 7 == 0) {
                printf ("\n");
            }
        }

        return 0;
    }

In C, only the size of the array is given during declaration. In Ada, array
index ranges are specified using two values of a discrete type. In this
example, the array type declaration specifies the use of :ada:`Integer` as the
index type, but does not provide any constraints (use :ada:`<>`, pronounced
*box*, to specify "no constraints"). The constraints are defined in the object
declaration to be 1 to 26, inclusive. Arrays have an attribute called
:ada:`'Range`. In our example, :ada:`Arr'Range` can also be expressed as
:ada:`Arr'First .. Arr'Last`; both expressions will resolve to :ada:`1 .. 26`.
So the :ada:`'Range` attribute supplies the bounds for our :ada:`for` loop.
There is no risk of stating either of the bounds incorrectly, as one might do
in C where :c:`I <= 26` may be specified as the end-of-loop condition.

As in C, Ada :ada:`String` is an array of :ada:`Character`. Ada strings,
importantly, are not delimited with the special character :c:`'\0'` like they
are in C. It is not necessary because Ada uses the array's bounds to determine
where the string starts and stops.

Ada's predefined :ada:`String` type is very straightforward to use:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Constrained_String

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       My_String : String (1 .. 19) := "This is an example!";
    begin
       Put_Line (My_String);
    end Main;

Unlike C, Ada does not offer escape sequences such as :c:`'\n'`. Instead,
explicit values from the ASCII package must be concatenated (via the
concatenation operator, :ada:`&`). Here for example, is how to initialize a
line of text ending with a new line:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Constrained_String

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       My_String : String := "This is a line" & ASCII.LF;
    begin
       Put (My_String);
    end Main;

You see here that no constraints are necessary for this variable definition.
The initial value given allows the automatic determination of
:ada:`My_String`'s bounds.

Ada offers high-level operations for copying, slicing, and assigning values to
arrays. We'll start with assignment. In C, the assignment operator doesn't make
a copy of the value of an array, but only copies the address or reference to
the target variable. In Ada, the actual array contents are duplicated. To get
the above behavior, actual pointer types would have to be defined and used.

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Copy_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Integer;
       A1 : Arr_Type (1 .. 2);
       A2 : Arr_Type (1 .. 2);
    begin
       A1 (1) := 0;
       A1 (2) := 1;

       A2 := A1;

       for I in A2'Range loop
          Put_Line (Integer'Image (A2 (I)));
       end loop;
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Copy_C

    !main.c
    #include <stdio.h>
    #include <string.h>

    int main(int argc, const char * argv[])
    {
        int A1 [2];
        int A2 [2];

        A1 [0] = 0;
        A1 [1] = 1;

        memcpy (A2, A1, sizeof (int) * 2);

        for (int i = 0; i < 2; i++) {
            printf("%d\n", A2[i]);
        }

        return 0;
    }

In all of the examples above, the source and destination arrays must have
precisely the same number of elements. Ada allows you to easily specify a
portion, or slice, of an array. So you can write the following:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Slice

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Integer;
       A1 : Arr_Type (1 .. 10) := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
       A2 : Arr_Type (1 .. 5)  := (1, 2, 3, 4, 5);
    begin
       A2 (1 .. 3) := A1 (4 .. 6);

       for I in A2'Range loop
          Put_Line (Integer'Image (A2 (I)));
       end loop;
    end Main;

This assigns the 4th, 5th, and 6th elements of :ada:`A1` into the 1st, 2nd, and
3rd elements of :ada:`A2`. Note that only the length matters here: the values
of the indexes don't have to be equal; they slide automatically.

Ada also offers high level comparison operations which compare the contents of
arrays as opposed to their addresses:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Equal_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Integer;
       A1 : Arr_Type (1 .. 2) := (10, 20);
       A2 : Arr_Type (1 .. 2) := (10, 20);
    begin
       if A1 = A2 then
         Put_Line ("A1 = A2");
       else
         Put_Line ("A1 /= A2");
       end if;
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Equal_C

    !main.c
    #include <stdio.h>

    int main(int argc, const char * argv[])
    {
        int A1 [2] = { 10, 20 };
        int A2 [2] = { 10, 20 };

        int eq = 1;

        for (int i = 0; i < 2; ++i) {
            if (A1 [i] != A2 [i]) {
                eq = 0;
                break;
            }
        }

        if (eq) {
            printf("A1 == A2\n");
        }
        else {
            printf("A1 != A2\n");
        }

        return 0;
    }

You can assign to all the elements of an array in each language in different
ways. In Ada, the number of elements to assign can be determined by looking at
the right-hand side, the left-hand side, or both sides of the assignment. When
bounds are known on the left-hand side, it's possible to use the others
expression to define a default value for all the unspecified array elements.
Therefore, you can write:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Assignment_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Integer;
       A1 : Arr_Type (-2 .. 42) := (others => 0);
    begin
       --  use a slice to assign A1 elements 11 .. 19 to 1
       A1 (11 .. 19) := (others => 1);

       Put_Line ("---- A1 ----");
       for I in A1'Range loop
          Put_Line (Integer'Image (I) & " => " &
                    Integer'Image (A1 (I)));
       end loop;
    end Main;

In this example, we're specifying that :ada:`A1` has a range between -2 and 42.
We use :ada:`(others => 0)` to initialize all array elements with zero. In the
next example, the number of elements is determined by looking at the right-hand
side:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Array_Assignment_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Arr_Type is array (Integer range <>) of Integer;
       A1 : Arr_Type := (1, 2, 3, 4, 5, 6, 7, 8, 9);
    begin
       A1 := (1, 2, 3, others => 10);

       Put_Line ("---- A1 ----");
       for I in A1'Range loop
          Put_Line (Integer'Image (I) & " => " &
                    Integer'Image (A1 (I)));
       end loop;
    end Main;

Since :ada:`A1` is initialized with an aggregate of 9 elements, :ada:`A1`
automatically has 9 elements. Also, we're not specifying any range in the
declaration of :ada:`A1`. Therefore, the compiler uses the default range of the
underlying array type :ada:`Arr_Type`, which has an unconstrained range based
on the :ada:`Integer` type. The compiler selects the first element of that type
(:ada:`Integer'First`) as the start index of :ada:`A1`. If you replaced
:ada:`Integer range <>` in the declaration of the :ada:`Arr_Type` by
:ada:`Positive range <>`, then :ada:`A1`'s start index would be
:ada:`Positive'First` |mdash| which corresponds to one.

Heterogeneous Data Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The structure corresponding to a C :c:`struct` is an Ada :ada:`record`. Here
are some simple records:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Struct_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type R is record
          A, B : Integer;
          C    : Float;
       end record;

       V : R;
    begin
       V.A := 0;
       Put_Line ("V.A = " & Integer'Image (V.A));
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Struct_C

    !main.c
    #include <stdio.h>

    struct R {
        int A, B;
        float C;
    };

    int main(int argc, const char * argv[])
    {
        struct R V;
        V.A = 0;
        printf("V.A = %d\n", V.A);

        return 0;
    }

Ada allows specification of default values for fields just like C. The values
specified can take the form of an ordered list of values, a named list of
values, or an incomplete list followed by others :ada:`=> <>` to specify that
fields not listed will take their default values. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Struct_Default_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type R is record
          A, B : Integer := 0;
          C    : Float   := 0.0;
       end record;

       procedure Put_R (V : R; Name : String) is
       begin
          Put_Line (Name & " = ("
                    & Integer'Image (V.A) & ", "
                    & Integer'Image (V.B) & ", "
                    & Float'Image (V.C) & ")");
       end Put_R;

       V1 : constant R := (1, 2, 1.0);
       V2 : constant R := (A => 1, B => 2, C => 1.0);
       V3 : constant R := (C => 1.0, A => 1, B => 2);
       V4 : constant R := (C => 1.0, others => <>);

    begin
       Put_R (V1, "V1");
       Put_R (V2, "V2");
       Put_R (V3, "V3");
       Put_R (V4, "V4");
    end Main;

Pointers
~~~~~~~~

As a foreword to the topic of pointers, it's important to keep in mind the fact
that most situations that would require a pointer in C do not in Ada. In the
vast majority of cases, indirect memory management can be hidden from the
developer and thus saves from many potential errors. However, there are
situation that do require the use of pointers, or said differently that require
to make memory indirection explicit. This section will present Ada access
types, the equivalent of C pointers. A further section will provide more
details as to how situations that require pointers in C can be done without
access types in Ada.

We'll continue this section by explaining the difference between objects
allocated on the stack and objects allocated on the heap using the following
example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Pointers_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type R is record
          A, B : Integer;
       end record;

       procedure Put_R (V : R; Name : String) is
       begin
          Put_Line (Name & " = ("
                    & Integer'Image (V.A) & ", "
                    & Integer'Image (V.B) & ")");
       end Put_R;

       V1, V2 : R;

    begin
       V1.A := 0;
       V1.B := 0;
       V2 := V1;
       V2.A := 1;

       Put_R (V1, "V1");
       Put_R (V2, "V2");
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Pointers_C

    !main.c
    #include <stdio.h>

    struct R {
        int A, B;
    };

    void print_r(const struct R *v,
                 const char     *name)
    {
        printf("%s = (%d, %d)\n", name, v->A, v->B);
    }

    int main(int argc, const char * argv[])
    {
        struct R V1, V2;
        V1.A = 0;
        V1.B = 0;
        V2 = V1;
        V2.A = 1;

        print_r(&V1, "V1");
        print_r(&V2, "V2");

        return 0;
    }

There are many commonalities between the Ada and C semantics above. In Ada and
C, objects are allocated on the stack and are directly accessed. :ada:`V1` and
:ada:`V2` are two different objects and the assignment statement copies the
value of :ada:`V1` into :ada:`V2`. :ada:`V1` and :ada:`V2` are two distinct
objects.

Here's now a similar example, but using heap allocation instead:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Heap_Alloc_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type R is record
          A, B : Integer;
       end record;

       type R_Access is access R;

       procedure Put_R (V : R; Name : String) is
       begin
          Put_Line (Name & " = ("
                    & Integer'Image (V.A) & ", "
                    & Integer'Image (V.B) & ")");
       end Put_R;

       V1 : R_Access;
       V2 : R_Access;
    begin
       V1 := new R;
       V1.A := 0;
       V2 := V1;
       V2.A := 1;

       Put_R (V1.all, "V1");
       Put_R (V2.all, "V2");
    end Main;

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Heap_Alloc_C

    !main.c
    #include <stdio.h>
    #include <stdlib.h>

    struct R {
        int A, B;
    };

    void print_r(const struct R *v,
                 const char     *name)
    {
        printf("%s = (%d, %d)\n", name, v->A, v->B);
    }

    int main(int argc, const char * argv[])
    {
        struct R * V1, * V2;
        V1 = malloc(sizeof(struct R));
        V1->A = 0;
        V2 = V1;
        V2->A = 1;

        print_r(V1, "V1");
        print_r(V2, "V2");

        return 0;
    }

In this example, an object of type :ada:`R` is allocated on the heap. The same
object is then referred to through :ada:`V1` and :ada:`V2`. As in C, there's
no garbage collector in Ada, so objects allocated by the new operator need to
be expressly freed (which is not the case here).

Dereferencing is performed automatically in certain situations, for instance
when it is clear that the type required is the dereferenced object rather than
the pointer itself, or when accessing record members via a pointer. To
explicitly dereference an access variable, append :ada:`.all`. The equivalent
of :c:`V1->A` in C can be written either as :ada:`V1.A` or :ada:`V1.all.A`.

Pointers to scalar objects in Ada and C look like:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Access_To_Scalars

    procedure Main is
       type A_Int is access Integer;
       Var : A_Int := new Integer;
    begin
       Var.all := 0;
    end Main;

[C]

.. code:: c run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Pointers_To_Scalars

    !main.c
    #include <stdlib.h>

    int main(int argc, const char * argv[])
    {
        int * Var = malloc (sizeof(int));
        *Var = 0;
        return 0;
    }

In Ada, an initializer can be specified with the allocation by appending
:ada:`'(value)`:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Access_Initialization

    procedure Main is
       type A_Int is access Integer;

       Var : A_Int := new Integer'(0);
    begin
       null;
    end Main;

When using Ada pointers to reference objects on the stack, the referenced
objects must be declared as being aliased. This directs the compiler to
implement the object using a memory region, rather than using registers or
eliminating it entirely via optimization. The access type needs to be declared
as either :ada:`access all` (if the referenced object needs to be assigned to)
or :ada:`access constant` (if the referenced object is a constant). The
:ada:`'Access` attribute works like the C :c:`&` operator to get a pointer to
the object, but with a *scope accessibility* check to prevent references to
objects that have gone out of scope. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Access_All

    procedure Main is
       type A_Int is access all Integer;
       Var : aliased Integer;
       Ptr : A_Int := Var'Access;
    begin
       null;
    end Main;

[C]

.. code:: c run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Access_All_C

    !main.c
    int main(int argc, const char * argv[])
    {
        int Var;
        int * Ptr = &Var;

        return 0;
    }

To deallocate objects from the heap in Ada, it is necessary to use a
deallocation subprogram that accepts a specific access type. A generic
procedure is provided that can be customized to fit your needs, it's called
:ada:`Ada.Unchecked_Deallocation`. To create your customized deallocator (that
is, to instantiate this generic), you must provide the object type as well as
the access type as follows:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    procedure Main is
       type Integer_Access is access all Integer;
       procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
       My_Pointer : Integer_Access := new Integer;
    begin
       Free (My_Pointer);
    end Main;

[C]

.. code:: c run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Free

    !main.c
    #include <stdlib.h>

    int main(int argc, const char * argv[])
    {
        int * my_pointer = malloc (sizeof(int));
        free (my_pointer);

        return 0;
    }

We'll discuss generics later :ref:`in this section <Ada_For_Embedded_C_Dev_Genericity>`.

Functions and Procedures
------------------------

General Form
~~~~~~~~~~~~

Subroutines in C are always expressed as functions which may or may not return a
value. Ada explicitly differentiates between functions and procedures.
Functions must return a value and procedures must not. Ada uses the more
general term *subprogram* to refer to both functions and procedures.

Parameters can be passed in three distinct modes:

- :ada:`in`, which is the default, is for input parameters, whose value is
  provided by the caller and cannot be changed by the subprogram.

- :ada:`out` is for output parameters, with no initial value, to be assigned by
  the subprogram and returned to the caller.

- :ada:`in out` is a parameter with an initial value provided by the caller,
  which can be modified by the subprogram and returned to the caller (more or
  less the equivalent of a non-constant pointer in C).

Ada also provides :ada:`access` and :ada:`aliased` parameters, which are in
effect explicit pass-by-reference indicators.

In Ada, the programmer specifies how the parameter will be used and in general
the compiler decides how it will be passed (i.e., by copy or by reference). C
has the programmer specify how to pass the parameter.

.. admonition:: Important

    There are some exceptions to the "general" rule in Ada. For example,
    parameters of scalar types are always passed by copy, for all three modes.

Here's a first example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Subroutines_Ada

    procedure Proc
     (Var1 : Integer;
      Var2 : out Integer;
      Var3 : in out Integer);

    function Func (Var : Integer) return Integer;

    with Func;

    procedure Proc
     (Var1 : Integer;
      Var2 : out Integer;
      Var3 : in out Integer)
    is
    begin
       Var2 := Func (Var1);
       Var3 := Var3 + 1;
    end Proc;

    function Func (Var : Integer) return Integer
    is
    begin
       return Var + 1;
    end Func;

    with Ada.Text_IO; use Ada.Text_IO;
    with Proc;

    procedure Main is
       V1, V2 : Integer;
    begin
       V2 := 2;
       Proc (5, V1, V2);

       Put_Line ("V1: " & Integer'Image (V1));
       Put_Line ("V2: " & Integer'Image (V2));
    end Main;

[C]

.. code:: c run_button manual_chop project=Courses.Ada_For_Embedded_C_Dev.Perspective.Subroutines_C

    !proc.h
    void Proc
        (int   Var1,
         int * Var2,
         int * Var3);

    !func.h
    int Func (int Var);

    !proc.c
    #include "func.h"

    void Proc
        (int   Var1,
         int * Var2,
         int * Var3)
    {
        *Var2 = Func (Var1);
        *Var3 += 1;
    }

    !func.c
    int Func (int Var)
    {
        return Var + 1;
    }

    !main.c
    #include <stdio.h>
    #include "proc.h"

    int main(int argc, const char * argv[])
    {
        int v1, v2;

        v2 = 2;
        Proc (5, &v1, &v2);

        printf("v1: %d\n", v1);
        printf("v2: %d\n", v2);

        return 0;
    }

The first two declarations for :ada:`Proc` and :ada:`Func` are specifications
of the subprograms which are being provided later. Although optional here, it's
still considered good practice to separately define specifications and
implementations in order to make it easier to read the program. In Ada and C, a
function that has not yet been seen cannot be used. Here, :ada:`Proc` can call
:ada:`Func` because its specification has been declared.

Parameters in Ada subprogram declarations are separated with semicolons,
because commas are reserved for listing multiple parameters of the same type.
Parameter declaration syntax is the same as variable declaration syntax (except
for the modes), including default values for parameters. If there are no
parameters, the parentheses must be omitted entirely from both the declaration
and invocation of the subprogram.

.. admonition:: In Ada 202X

    Ada 202X allows for using static expression functions, which are evaluated
    at compile time. To achieve this, we can use an aspect |mdash| we'll
    discuss aspects :ref:`later in this chapter <Ada_For_Embedded_C_Dev_Aspects>`.

    An expression function is static when the :ada:`Static`
    aspect is specified. For example:

    .. code-block:: ada

        procedure Main is

           X1 : constant := (if True then 37 else 42);

           function If_Then_Else (Flag : Boolean; X, Y : Integer)
             return Integer is
              (if Flag then X else Y) with Static;

           X2 : constant := If_Then_Else (True, 37, 42);

        begin
           null;
        end Main;

    In this example, we declare :ada:`X1` using an expression. In the
    declaration of :ada:`X2`, we call the static expression function
    :ada:`If_Then_Else`. Both :ada:`X1` and :ada:`X2` have the same constant
    value.

.. _Ada_For_Embedded_C_Dev_Overloading:

Overloading
~~~~~~~~~~~

In C, function names must be unique. Ada allows overloading, in which multiple
subprograms can share the same name as long as the subprogram signatures
(the parameter types, and function return types) are different. The compiler
will be able to resolve the calls to the proper routines or it will reject the
calls. For example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Overloading_Ada

    package Machine is
       type Status is (Off, On);
       type Code is new Integer range 0 .. 3;
       type Threshold is new Float range 0.0 .. 10.0;

       function Get (S : Status) return Code;
       function Get (S : Status) return Threshold;

    end Machine;

    package body Machine is

       function Get (S : Status) return Code is
       begin
          case S is
             when Off => return 1;
             when On  => return 3;
          end case;
       end Get;

       function Get (S : Status) return Threshold is
       begin
          case S is
             when Off => return  2.0;
             when On  => return 10.0;
          end case;
       end Get;

    end Machine;

    with Ada.Text_IO; use Ada.Text_IO;
    with Machine;     use Machine;

    procedure Main is
       S : Status;
       C : Code;
       T : Threshold;
    begin
       S := On;
       C := Get (S);
       T := Get (S);

       Put_Line ("S: " & Status'Image (S));
       Put_Line ("C: " & Code'Image (C));
       Put_Line ("T: " & Threshold'Image (T));
    end Main;

The Ada compiler knows that an assignment to :ada:`C` requires a
:ada:`Code` value. So, it chooses the :ada:`Get` function that returns a
:ada:`Code` to satisfy this requirement.

Operators in Ada are functions too. This allows you to define
local operators that override operators defined at an outer scope, and provide
overloaded operators that operate on and compare different types. To declare an
operator as a function, enclose its "name" in quotes:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Overloading_Eq

    package Machine_2 is
       type Status is (Off, Waiting, On);
       type Input is new Float range 0.0 .. 10.0;

       function Get (I : Input) return Status;

       function "=" (Left : Input; Right : Status) return Boolean;

    end Machine_2;

    package body Machine_2 is

       function Get (I : Input) return Status is
       begin
          if I >= 0.0 and I < 3.0 then
             return Off;
          elsif I >= 3.0 and I < 6.5 then
             return Waiting;
          else
             return On;
          end if;
       end Get;

       function "=" (Left : Input; Right : Status) return Boolean is
       begin
          return Get (Left) = Right;
       end "=";

    end Machine_2;

    with Ada.Text_IO; use Ada.Text_IO;
    with Machine_2;   use Machine_2;

    procedure Main is
       I : Input;
    begin
       I := 3.0;
       if I = Off then
          Put_Line ("Machine is off.");
       else
          Put_Line ("Machine is not off.");
       end if;
    end Main;

.. _Ada_For_Embedded_C_Dev_Aspects:

Aspects
~~~~~~~

Aspect specifications allow you to define certain characteristics of a
declaration using the :ada:`with` keyword after the declaration:

.. code-block:: ada

    procedure Some_Procedure is <procedure_definition>
      with Some_Aspect => <aspect_specification>;

    function Some_Function is <function_definition>
      with Some_Aspect => <aspect_specification>;

    type Some_Type is <type_definition>
      with Some_Aspect => <aspect_specification>;

    Obj : Some_Type with Some_Aspect => <aspect_specification>;

For example, you can inline a subprogram by specifying the :ada:`Inline`
aspect:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Inline_Aspect

    package Float_Arrays is

       type Float_Array is array (Positive range <>) of Float;

       function Average (Data : Float_Array) return Float
         with Inline;

    end Float_Arrays;

We'll discuss inlining :ref:`later in this course <Ada_For_Embedded_C_Dev_Inlining>`.

Aspect specifications were introduced in Ada 2012. In previous versions of Ada,
you had to use a :ada:`pragma` instead. The previous example would be written
as follows:

[Ada]

.. code:: ada no_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Inline_Aspect

    package Float_Arrays is

       type Float_Array is array (Positive range <>) of Float;

       function Average (Data : Float_Array) return Float;

       pragma Inline (Average);

    end Float_Arrays;

Aspects and attributes might refer to the same kind of information. For
example, we can use the :ada:`Size` aspect to define the expected minimum size
of objects of a certain type:

[Ada]

.. code:: ada compile_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Size_Aspect

    package My_Device_Types is

       type UInt10 is mod 2 ** 10
         with Size => 10;

    end My_Device_Types;

In the same way, we can use the size attribute to retrieve the size of a type
or of an object:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.Perspective.Size_Aspect

    with Ada.Text_IO;     use Ada.Text_IO;

    with My_Device_Types; use My_Device_Types;

    procedure Show_Device_Types is
       UInt10_Obj : constant UInt10 := 0;
    begin
       Put_Line ("Size of UInt10 type:   " & Positive'Image (UInt10'Size));
       Put_Line ("Size of UInt10 object: " & Positive'Image (UInt10_Obj'Size));
    end Show_Device_Types;

We'll explain both :ada:`Size` aspect and :ada:`Size` attribute
:ref:`later in this course <Ada_For_Embedded_C_Dev_Size_Aspect_Attribute>`.
