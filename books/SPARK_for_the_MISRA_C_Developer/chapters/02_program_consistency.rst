:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Enforcing Basic Program Consistency
-----------------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Many consistency properties that are taken for granted in other languages are
not enforced in C. The basic property that all uses of a variable or function
are consistent with its type is not enforced by the language and is also very
difficult to enforce by a tool. Three features of C contribute to that situation:

* the textual-based inclusion of files means that every included declaration is
  subject to a possibly different reinterpretation depending on context.
* the lack of consistency requirements across translation units means that type
  inconsistencies can only be detected at link time, something linkers are
  ill-equipped to do.
* the default of making a declaration externally visible means
  that declarations that should be local will be visible to
  the rest of the program, increasing the chances for inconsistencies.

MISRA C contains guidelines on all three fronts to enforce basic program
consistency.

Taming Text-Based Inclusion
***************************

The text-based inclusion of files is one of the dated idiosyncracies of the C
programming language that was inherited by C++ and that is known to cause
quality problems, especially during maintenance. Although multiple inclusion of
a file in the same translation unit can be used to emulate template
programming, it is generally undesirable. Indeed, MISRA C defines
Directive 4.10 precisely to forbid it for header files: `"Precautions shall be
taken in order to prevent the contents of a header file being included more
than once"`.

The subsequent section on "Preprocessing Directives" contains 14
rules restricting the use of text-based inclusion through preprocessing.
Among other things these rules forbid the use of the ``#undef`` directive
(which works around conflicts in macro definitions introduced by text-based
inclusion) and enforces the well-known practice of enclosing macro arguments
in parentheses (to avoid syntactic reinterpretations in the context of the
macro use).

SPARK (and more generally Ada) does not suffer from these problems, as it
relies on semantic inclusion of context instead of textual inclusion of content,
using ``with`` clauses:

.. code:: ada

    with Ada.Text_IO;

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

Note that ``with`` clauses are only allowed at the beginning of files;
the compiler issues an error if they are used elsewhere:

.. code:: ada manual_chop
   :class: ada-nocheck

    !hello_world.adb
    procedure Hello_World is
       with Ada.Text_IO; -- Illegal
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

Importing a unit (i.e., specifying it in a ``with`` clause) multiple times is
harmless, as it is equivalent to importing it once, but a compiler warning lets
us know about the redundancy:

.. code:: ada

    with Ada.Text_IO;
    with Ada.Text_IO; -- Legal but useless

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

The order in which units are imported is irrelevant. All orders are valid and
have the same semantics.

No conflict arises from importing multiple units, even if the same name is
defined in several, since each unit serves as namespace for the entities which it
defines. So we can define our own version of ``Put_Line`` in some ``Helper``
unit and import it together with the standard version defined in
``Ada.Text_IO``:

.. code:: ada

    package Helper is
       procedure Put_Line (S : String);
    end Helper;

    with Ada.Text_IO;

    package body Helper is
       procedure Put_Line (S : String) is
       begin
          Ada.Text_IO.Put_Line ("Start helper version");
          Ada.Text_IO.Put_Line (S);
          Ada.Text_IO.Put_Line ("End helper version");
       end Put_Line;
    end Helper;

    with Ada.Text_IO;
    with Helper;

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
       Helper.Put_Line ("hello, world!");
    end Hello_World;

The only way a conflict can arise is if we want to be able to reference ``Put_Line``
directly, without using the qualified name ``Ada.Text_IO.Put_Line`` or
``Helper.Put_Line``. The ``use clause`` makes public declarations from a
unit available directly:

.. code:: ada
    :class: ada-expect-compile-error

    package Helper is
       procedure Put_Line (S : String);
    end Helper;

    with Ada.Text_IO;

    package body Helper is
       procedure Put_Line (S : String) is
       begin
          Ada.Text_IO.Put_Line ("Start helper version");
          Ada.Text_IO.Put_Line (S);
          Ada.Text_IO.Put_Line ("End helper version");
       end Put_Line;
    end Helper;

    with Ada.Text_IO; use Ada.Text_IO;
    with Helper; use Helper;

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
       Helper.Put_Line ("hello, world!");
       Put_Line ("hello, world!");  --  ERROR
    end Hello_World;

Here, both units ``Ada.Text_IO`` and ``Helper`` define a procedure ``Put_Line``
taking a String as argument, so the compiler cannot disambiguate the direct
call to ``Put_Line`` and issues an error. Here is output from AdaCore's
GNAT Ada compiler:

::

     1.     with Ada.Text_IO; use Ada.Text_IO;
     2.     with Helper; use Helper;
     3.
     4.     procedure Hello_World is
     5.     begin
     6.        Ada.Text_IO.Put_Line ("hello, world!");
     7.        Helper.Put_Line ("hello, world!");
     8.        Put_Line ("hello, world!");  --  ERROR
               |
        >>> ambiguous expression (cannot resolve "Put_Line")
        >>> possible interpretation at helper.ads:2
        >>> possible interpretation at a-textio.ads:263

     9.     end Hello_World;

Note that it helpfully points to
candidate declarations, so that the user can decide which qualified name to use
as in the previous two calls.

Issues arising in C as a result of text-based inclusion of files are thus
completely prevented in SPARK (and Ada) thanks to semantic import of units.
Note that the C++ committee identified this weakness some time ago and
`has approved <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/n4720.pdf>`_
the addition of `modules` to C++20, which provide a mechanism for semantic import of
units.

Hardening Link-Time Checking
****************************

An issue related to text-based inclusion of files is that there is no
single source for declaring the type of a variable or function. If a file
``origin.c`` defines a variable ``var`` and functions ``fun`` and ``print``:

:code-config:`accumulate_code=True`

.. code:: c no_button

   !origin.c
   #include <stdio.h>

   int var = 0;
   int fun() {
      return 1;
   }
   void print() {
      printf("var = %d\n", var);
   }

and the corresponding header file ``origin.h`` declares ``var``, ``fun`` and
``print`` as having external linkage:

.. code:: c no_button

   !origin.h
   extern int var;
   extern int fun();
   extern void print();

then client code can include ``origin.h`` with declarations
for ``var`` and ``fun``:

.. code:: c

   !main.c
   #include "origin.h"

   int main() {
      var = fun();
      print();
      return 0;
   }

or, equivalently, repeat these declarations directly:

.. code:: c

   !main.c
   extern int var;
   extern int fun();
   extern void print();

   int main() {
      var = fun();
      print();
      return 0;
   }

:code-config:`accumulate_code=False`

Then, if an inconsistency is introduced in the type of ``var`` of ``fun``
between these alternative declarations and their actual type, the compiler
cannot detect it. Only the linker, which has access to the set of object files
for a program, can detect such inconsistencies. However, a linker's main
task is to link, not to detect inconsistencies, and so inconsistencies in the
type of variables and functions in most cases cannot be detected. For example,
most linkers cannot detect if the type of ``var`` or the return type of ``fun``
is changed to ``float`` in the declarations above. With the declaration of
``var`` changed to ``float``, the above program compiles and runs without
errors, producing the erroneous output ``var = 1065353216`` instead of ``var =
1``. With the return type of ``fun`` changed to ``float`` instead, the program
still compiles and runs without errors, producing this time the erroneous
output ``var = 0``.

The inconsistency just discussed is prevented by MISRA C Rule 8.3 `"All
declarations of an object or function shall use the same names and type
qualifiers"`. This is a decidable rule, but it must be enforced at system
level, looking at all translation units of the complete program. MISRA C Rule
8.6 also requires a unique definition for a given identifier
across translation units, and Rule 8.5 requires that an external declaration
shared between translation units comes from the same file. There is even a
specific section on "Identifiers" containing 9 rules requiring uniqueness of
various categories of identifiers.

SPARK (and more generally Ada) does not suffer from these problems, as it
relies on semantic inclusion of context using ``with`` clauses to provide a
unique declaration for each entity.

Going Towards Encapsulation
***************************

Many problems in C stem from the lack of encapsulation.
There is no notion of namespace that would allow a
file to make its declarations available without risking a conflict with other
files. Thus MISRA C has a number of guidelines that discourage the use of external
declarations:

* Directive 4.8 encourages hiding the definition of structures and unions in
  implementation files (``.c`` files) when possible: `"If a pointer to a structure
  or union is never dereferenced within a translation unit, then the
  implementation of the object should be hidden."`

* Rule 8.7 forbids the use of external declarations when not needed:
  `"Functions and objects should not be defined with external linkage if they
  are referenced in only one translation unit."`

* Rule 8.8 forces the explicit use of keyword ``static`` when appropriate:
  `"The static storage class specifier shall be used in all declarations of
  objects and functions that have internal linkage."`

The basic unit of modularization in SPARK, as in Ada, is the *package*.
A package always has a spec (in an ``.ads`` file), which defines the interface
to other units. It generally also has a body (in an ``.adb`` file), which
completes the spec with an implementation. Only declarations from the package spec are
visible from other units when they import (``with``) the package. In fact, only
declarations from what is called the "visible part" of the spec
(before the keyword ``private``) are visible from units that ``with`` the package.

.. code:: ada
    :class: ada-expect-compile-error

    package Helper is
       procedure Public_Put_Line (S : String);
    private
       procedure Private_Put_Line (S : String);
    end Helper;

    with Ada.Text_IO;

    package body Helper is
       procedure Public_Put_Line (S : String) is
       begin
          Ada.Text_IO.Put_Line (S);
       end Public_Put_Line;

       procedure Private_Put_Line (S : String) is
       begin
          Ada.Text_IO.Put_Line (S);
       end Private_Put_Line;

       procedure Body_Put_Line (S : String) is
       begin
          Ada.Text_IO.Put_Line (S);
       end Body_Put_Line;
    end Helper;

    with Helper; use Helper;

    procedure Hello_World is
    begin
       Public_Put_Line ("hello, world!");
       Private_Put_Line ("hello, world!");  --  ERROR
       Body_Put_Line ("hello, world!");  --  ERROR
    end Hello_World;

Here's the output from AdaCore's GNAT compiler:

::

     1.     with Helper; use Helper;
     2.
     3.     procedure Hello_World is
     4.     begin
     5.        Public_Put_Line ("hello, world!");
     6.        Private_Put_Line ("hello, world!");  --  ERROR
               |
        >>> "Private_Put_Line" is not visible
        >>> non-visible (private) declaration at helper.ads:4

     7.        Body_Put_Line ("hello, world!");  --  ERROR
               |
        >>> "Body_Put_Line" is undefined

     8.     end Hello_World;


Note the different errors on the calls to the private and body versions of
``Put_Line``. In the first case the compiler can locate the candidate procedure
but it is illegal to call it from ``Hello_World``, in the second case the
compiler does not even know about any ``Body_Put_Line`` when compiling
``Hello_World`` since it only looks at the spec and not the body.

SPARK (and Ada) also allow defining a type in the private part of a package spec while
simply declaring the type name in the public ("visible") part of the spec. This way,
client code -- i.e., code that ``with``\ s the package -- can use the type,
typically through a public API, but have no access to how the type is implemented:

.. code:: ada
    :class: ada-expect-compile-error

    package Vault is
       type Data is private;
       function Get (X : Data) return Integer;
       procedure Set (X : out Data; Value : Integer);
    private
       type Data is record
          Val : Integer;
       end record;
    end Vault;

    package body Vault is
       function Get (X : Data) return Integer is (X.Val);
       procedure Set (X : out Data; Value : Integer) is
       begin
          X.Val := Value;
       end Set;
    end Vault;

    with Vault;

    package Information_System is
       Archive : Vault.Data;
    end Information_System;

    with Information_System;
    with Vault;

    procedure Hacker is
       V : Integer := Vault.Get (Information_System.Archive);
    begin
       Vault.Set (Information_System.Archive, V + 1);
       Information_System.Archive.Val := 0;  --  ERROR
    end Hacker;

Note that it is possible to declare a variable of type ``Vault.Data`` in
package ``Information_System`` and to get/set it through its API in procedure
``Hacker``, but not to directly access its ``Val`` field.
