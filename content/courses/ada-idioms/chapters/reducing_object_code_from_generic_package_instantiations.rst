Reducing Object Code from Generic Package Instantiations
========================================================

.. include:: ../../global.txt

Motivation
----------

Generic unit instantiations are often, but not always, implemented by an Ada
compiler as a form of *macro expansion*. In this approach, the compiler
produces separate, dedicated object code for every instantiation.  The macro
expansion approach can produce better run-time performance but can result in
large total object code size for the executable when there are many instances,
especially when the generic packages instantiated contain a lot of unit
declarations. For example, the generic I/O packages contained within package
:ada:`Ada.Text_IO` are themselves relatively large.

The alternative compiler implementation approach is *code-sharing*, in which
distinct instantiations of a given generic unit are implemented with shared
object code in a single module.

Clearly, sharing the object code can reduce the total size, but code-sharing
can be very complicated to implement, depending on the generic unit itself. For
a trivial example, consider the following package:

.. code-block:: ada

    generic
    package P is
       Error : exception;
    end P;

The semantics of the language require that every instantiation of generic
package :ada:`P` be a distinct package, as if each instance was instead written
explicitly as a non-generic package (at the point of instantiation) with the
instance name. As a result, each package instance declares an exception, and
these exceptions must be treated as distinct from each other. A code-sharing
implementation must maintain that distinction with one object code module.

In the example above, there are no generic formal parameters, nor other
declarations within the package declarative part besides the exception, because
they are not necessary for that example.  However, generic formal parameters
can be a problem for code-sharing too. For example, consider this generic
package:

.. code-block:: ada

    generic
       Formal_Object : in out Integer;
    package P is
       --  ...
    end P;

This generic package has a generic formal object parameter with mode
:ada:`in out`. (We chose type :ada:`Integer` purely for convenience.)  That
specific mode can cause a similar problem as seen in the exception example,
because the mode allows the generic package to update the generic actual
object passed to it. The shared object code must keep track of which object is
updated during execution.

Therefore, when writing the application source code that instantiates generic
packages, developers should do so in a manner that minimizes the amount of
object code that might result.


Solution
--------

The application source code should be written in a manner that shares the
instantiations themselves, when possible, thereby reducing the number of
instantiations that exist.

For example, let’s say that several units in the application code require the
ability to do I/O on some floating-point type. For simplicity, let’s say that
this is a type named :ada:`Real`, declared in a package named :ada:`Common`.
Here is a declaration for an example package body that requires the I/O
capability:

.. code-block:: ada

    with Ada.Text_IO, Common;
    package body User1 is
        package Real_IO is new Ada.Text_IO.Float_IO (Common.Real);
        -- ...
    end User1;

That’s certainly legal, and works, but we’ve said that several units require
I/O for type :ada:`Real`. Let’s say there are in fact twenty such units. They
all do something similar:

.. code-block:: ada

    with Ada.Text_IO, Common;
    package body User20 is
        package Real_IO is new Ada.Text_IO.Float_IO (Common.Real);
        --  ...
    end User20;

As a result, the application has twenty instantiations (at least) of
:ada:`Ada.Text_IO.Float_IO`. There will be instances named
:ada:`User1.Real_IO`, :ada:`User2.Real_IO`, and so on, up to
:ada:`User20.Real_IO`. The fact that the local names are all
:ada:`Real_IO` is irrelevant.

If the compiler happens to use the macro-expansion implementation, that means
the application executable will have twenty copies of the object code defined
by the generic :ada:`Float_IO`. For example, GNAT performs some internal
restructuring to avoid this problem for these specific language-defined generic
units, but not for application-defined generics.

Instead, we can simply instantiate the generic at the library level:

.. code-block:: ada

    with Ada.Text_IO, Common;
    package Real_IO is new Ada.Text_IO.Float_IO (Common.Real);

Because the instantiation occurred at the library level, the resulting instance
is declared at the library level, and can therefore be named in a "with_clause"
like any other library package.

.. code-block:: ada

    with Real_IO;
    package body User1 is
        --  ...
    end User1;

Each client package can use the same instance via the with_clause, and there’s
only one instance so there’s only one copy of the object code.


Pros
----

The total object code size is reduced, compared to the alternative of many
local instantiations.


Cons
----

What would otherwise be an implementation detail hidden from clients can now
become visible to them because a (public) library unit can be named in
with_clause by any other unit. As a result, this approach should not be used in
all cases, not even as a default design approach. Restricting the visibility of
the instance may be more important than the amount of object code it
contributes. Hiding implementation artifacts allows more freedom to change the
implementation without requiring changes to client code.


Relationship With Other Idioms
------------------------------

None.


Notes
-----

1. The reader should understand that this issue is not about the number of
   subprograms within any given package, whether or not the package is a
   generic package. In the past, some linkers included the entire object code
   for a given package (instance or not), regardless of the number of
   subprograms actually used from that package in the application code. That
   was an issue with reusable library code, for example packages providing
   mathematical functions. Modern linkers can be told not to include those
   subprograms not called by the application. For example, with gcc, the
   compiler can be told to put each subprogram in a separate section, and then
   the linker can be told to only include in the executable those sections
   actually referenced. (Data declarations can be reduced that way as well.)
