:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Enforcing Basic Program Consistency
-----------------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Many consistency properties that are taken for granted in other languages are
not enforced in C. The basic property that all uses of a variable or function
agree on its type is both not enforced by the language and very difficult to
enforce by a tool. Three features of C contribute to that situation:

* the textual-based inclusion of files means that every included declaration is
  subject to a possibly different reinterpretation.
* the lack of consistency requirements across translation units means that type
  inconsistencies can only be detected at link time, something linkers are
  ill-equipped to do.
* the default that consists in making a declaration externally visible means
  that programmer oversight will promote local declarations to be visible to
  the rest of the program, increasing the chances for inconsistencies.

MISRA-C contains guidelines on all three fronts to enforce basic program
consistency.

Taming Text-Based Inclusion
***************************

The text-based inclusion of files is among the dated oddities of the C
programming language that were inherited by C++ and that are known to cause
quality problems, especially during maintenance. Although multiple inclusion of
a file in the same translation unit can be used to emulate template
programming, it's in general undesirable, so much that MISRA-C defines
Directive 4.10 precisely to forbid it for header files: `"Precautions shall be
taken in order to prevent the contents of a header file being included more
than once"`.

It follows with a specific section on "Preprocessing Directives" containing 14
rules restricting the use of text-based inclusion through preprocessing,
forbidding in particular the use of #undef directive (used to work around
conflicts in macro definitions introduced by text-based inclusion) and
enforcing the well-known practice that macro arguments should be parenthesized
(to avoid syntactic reinterpretations in the context of the macro use).

SPARK does not suffer from these problems, as it relies on semantic inclusion
of context instead of textual inclusion of content, using ``with`` clauses:

.. code:: ada

    with Ada.Text_IO;

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

In particular, ``with`` clauses are only allowed at the beginning of files, and
the compiler issues an error if they are used elsewhere:

.. code:: ada
    :class: ada-nocheck

    procedure Hello_World is
         with Ada.Text_IO;
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

Importing a unit multiple times is harmless, as it is equivalent to importing
it once, but a compiler warning lets us know about the redundant ``with``
clause:

.. code:: ada

    with Ada.Text_IO;
    with Ada.Text_IO;

    procedure Hello_World is
    begin
       Ada.Text_IO.Put_Line ("hello, world!");
    end Hello_World;

The order in which units are imported is irrelevant. All orders are valid and
have the same semantics. There is no possible conflict as a result of importing
units, as each imported unit serves as namespace for the entities which it
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

The only possible conflict arises if we want ``Put_Line`` to be directly
available without using the qualified name ``Ada.Text_IO.Put_Line`` or
``Helper.Put_Line``. We use a ``use clause`` to make public declarations from a
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
taking a string in argument, so the compiler cannot disambiguate the direct
call to ``Put_Line`` and issues an error. Note that it helpfully points to
candidate declarations, so that the user can decide which qualified name to use
as in the previous two calls.

Issues arising in C as a result of text-based inclusion of files are thus
completely prevented in SPARK thanks to semantic import of units. Note that the
C++ committee has identified this weakness for a long time, and `has finally
approved <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/n4720.pdf>`_
the addition of `modules` to C++20 that add a mechanism for semantic import of
units.

Hardening Link-Time Checking
****************************

An issue which is related to text-based inclusion of files is that there is no
single source for declaring the type of a variable or function. If a file
``origin.c`` defines a variable ``var`` and functions ``fun`` and ``print``:

.. code-block:: c

   #include <stdio.h>

   int var = 0;
   int fun() {
      return 1;
   }
   void print() {
      printf("var = %d\n", var);
   }

and the corresponding header file declares ``var``, ``fun`` and ``print`` as
having external linkage:

.. code-block:: c

   extern int var;
   extern int fun();
   extern void print();

then it is equivalent for client code to include ``origin.h`` with declarations
for ``var`` and ``fun``:

.. code-block:: c

   #include "origin.h"

   int main() {
      var = fun();
      print();
      return 0;
   }

or to repeat these declarations directy:

.. code-block:: c

   extern int var;
   extern int fun();
   extern void print();

   int main() {
      var = fun();
      print();
      return 0;
   }

Then, if an inconsistency is introduced in the type of ``var`` of ``fun``
between these alternative declarations and their actual type, the compiler
cannot detect it. Only the linker which has access to the set of object files
for a program could detect such inconsistencies. Unfortunately, a linker's main
task is to link, not to detect inconsistencies, and so inconsistencies in the
type of variables and functions in most cases cannot be detected. For example,
most linkers cannot detect if the type of ``var`` or the return type of ``fun``
is changed to ``float`` in the declarations above. With the declaration of
``var`` changed to ``float``, the above program compiles and runs without
errors, producing the erroneous output ``var = 1065353216`` instead of ``var =
1``. With the return type of ``fun`` changed to ``float`` instead, the program
still compiles and runs without errors, producing this time the erroneous
output ``var = 0``. With both ``var`` and ``fun`` changed from ``int`` to
``float``, the erroneous output is yet different: ``var = 16777215``.

The inconsistency just discussed is prevented by MISRA-C Rule 8.3 `"All
declarations of an object or function shall use the same names and type
qualifiers"`. This is a decidable rule, but it must be enforced at system
level, looking at all translation units of the complete program. MISRA-C Rule
8.6 also imposes that there is a unique definition for a given identifier
across translation units, and Rule 8.5 imposes that an external declaration
shared between translation units comes from the same file. There is even a
specific section on "Identifiers" containing 9 rules imposing uniqueness of
various categories of identifiers.

SPARK does not suffer from these problems, as it relies on semantic inclusion
of context using ``with`` clauses to provide the unique declaration for all
entities.

Going Towards Encapsulation
***************************

The root cause for most of the problems seen in this section stems from the
lack of encapsulation in C. There is no notion of namespace that would allow
files to publish their declarations without risking a conflict with other
files. Thus MISRA-C discourages the use of external declarations with multiple
guidelines:

* Directive 4.8 encourages to hide the definition of structure and unions in
  implementation files (.c files) when possible: `"If a pointer to a structure
  or union is never dereferenced within a translation unit, then the
  implementation of the object should be hidden."`

* Rule 8.7 forbids the use of external declarations when not needed:
  `"Functions and objects should not be defined with external linkage if they
  are referenced in only one translation unit."`

* Rule 8.8 forces the explicit use of keyword ``static`` when appropriate:
  `"The static storage class specifier shall be used in all declarations of
  objects and functions that have internal linkage."`

In SPARK, every unit is divided into a spec (.ads file) and a body (.adb
file). Only declarations from the spec are visible from other units when they
import that unit. In fact, only declarations from the visible part of the spec
(before keyword ``private``) are visible from other units.

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

Note the different errors on the calls to the private and body versions of
``Put_Line``: in the first case the compiler can locate the candidate procedure
but it is illegal to call it from ``Hello_World``, in the second case the
compiler does not even known about any ``Body_Put_Line`` when compiling
``Hello_World``.

SPARK also allows to define a type in the private part of a spec (so that
during the compilation of client units its definition is known to the compiler,
say for allocating the correct memory for a variable of such a type) while
simply announcing such a declaration in the public part of the spec. This way,
client code can use the type, typically through a public API, but not any
specifics on the implementation of the type.

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

Note that it is possible to both declare a variable of type ``Vault.Data`` in
package ``Information_System`` and to get/set it through its API in procedure
``Hacker``, but not to access directly its ``Val`` field.
