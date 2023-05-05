Interfacing with C and C++
==========================

.. include:: ../../../global.txt

Interfacing with C
------------------

Using unconstrained types
~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous examples, we're being careful about the data types: all of
them are coming from the :ada:`Interfaces.C` package. Using Ada built-in
types when interfacing with C can be problematic, especially in case of
unconstrained types. For example:

.. code-block:: c

    /*% filename: test.h */

    char * my_func (void);

This is the function implementation:

.. code-block:: c

    #include <stdio.h>
    #include "test.h"

    char * my_func (void)
    {
      return "hello";
    }

In the Ada application, we try to import this as a :ada:`String` type:

.. code-block:: ada

    with Interfaces.C;
    use  Interfaces.C;

    with Ada.Text_IO;
    use  Ada.Text_IO;

    procedure Show_C_Func is

       function my_func return String
         with
           Import        => True,
           Convention    => C;

       S : String := my_func;

    begin
       Put_Line (S);
    end Show_C_Func;

When running this application, we'll get a :ada:`Storage_Error` exception.
Therefore, the recommendation is to be very careful about the data types
and use the :ada:`Interfaces.C` package whenever possible for interfacing
with C.

Interfacing with C++
--------------------

All the previous examples focused on interfacing with C code. For C++, the
same methods apply. However, there are a few differences that we need to
take into account:

- When importing or exporting variables and subprograms, we replace 'C'
  by 'Cpp' in the :ada:`Convention` aspect of their declaration.

- In the project file for :program:`gprbuild`, we replace 'C' by 'C++' in the
  ``Languages`` entry.

There are other aspects specific to C++ that we also have to take into
account. This section will discuss them.

C++ symbol mangling
~~~~~~~~~~~~~~~~~~~

Let's start by adapting a previous example and *converting* it to C++
(actually, mainly just replacing the C compiler by a C++ compiler). The
header file is still basically the same:

.. code-block:: c

    extern int func_cnt;
    int my_func (int a);

And this is the corresponding implementation:

.. code-block:: c

    #include "test.hh"

    int func_cnt = 0;

    int my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

In the Ada application, as mentioned before, we need to replace 'C' by
'Cpp' in the :ada:`Convention` of the declarations:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_Cpp_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => Cpp,
           External_Name => "_Z7my_funci";

       V : int;

       func_cnt : int
         with
           Import        => True,
           Convention    => Cpp;

    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is "
                 & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (func_cnt)
                 & " times");

    end Show_Cpp_Func;

Also, in the declaration of :cpp:`my_func`, we need to include a reference to
the original name using :ada:`External_Name`. If we leave this out, the
linker won't be able to find the original implementation of :cpp:`my_func`,
so it won't build the application. Note that the function name is not
:cpp:`my_func` anymore (as it was the case for the C version). Instead, it is
now called :cpp:`_Z7my_funci`.  This situation is caused by symbol mangling.

In C, the symbol names in object files match the symbol name in the
source-code. In C++, due to symbol mangling, the symbol names of
subprograms in the object files are different from the corresponding
source-code implementation. Also, because symbol mangling is not
standardized, different compilers might use different methods. The most
prominent example is the difference between the gcc and MSVC compilers.
However, since GNAT is based on gcc, we can build applications using Ada
and C++ code without issues |mdash| as long as we use the same compiler.

In order to retrieved the mangled symbol names, we can simply generate
bindings automatically by using :program:`g++` with the ``-fdump-ada-spec``
option:

.. code-block:: sh

    g++ -c -fdump-ada-spec -C ./test.hh

Alternatively, we could use binary examination tools to retrieve the
symbol names from a library. Examples of such tools are :program:`nm` for Mac and
Linux, and :program:`dumpbin.exe` for Windows.

C++ classes
~~~~~~~~~~~

We'll now focus on binding object-oriented features of C++ into Ada.
Let's adapt the previous example to make use of classes. This is adapted
header file:

.. code-block:: c

    class Test {
    public:
      Test();
      int my_func (int a);
      int get_cnt();
    private:
      int func_cnt;
    };

And this is the corresponding implementation:

.. code-block:: c

    #include "test.hh"

    Test::Test() :
      func_cnt(0)
    {
    };

    int
    Test::my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

    int
    Test::get_cnt()
    {
      return func_cnt;
    }

Because of the more complex structure, the recommendation is to generate
bindings using :program:`g++` and, if needed, adapt the file. Let's first run
:program:`g++`:

.. code-block:: sh

    g++ -c -fdump-ada-spec -C ./test.hh

The generated bindings look like this:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_hh is

       package Class_Test is
          type Test is limited record
             func_cnt : aliased int;  -- ./test.hh:7
          end record;
          pragma Import (CPP, Test);

          function New_Test return Test;  -- ./test.hh:3
          pragma CPP_Constructor (New_Test, "_ZN4TestC1Ev");

          function my_func (this : access Test; a : int) return int;  -- ./test.hh:4
          pragma Import (CPP, my_func, "_ZN4Test7my_funcEi");

          function get_cnt (this : access Test) return int;  -- ./test.hh:5
          pragma Import (CPP, get_cnt, "_ZN4Test7get_cntEv");
       end;
       use Class_Test;
    end test_hh;

As we can see, the original C++ class (:cpp:`Test`) is represented as a
nested package (:ada:`test_hh.Class_Test`) in the Ada bindings.

The Ada application can then use the bindings:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_hh;      use test_hh;

    procedure Show_Cpp_Class is
       use Class_Test;

       V : int;

       T  : aliased Test := New_Test;
       TA : access Test := T'Access;

    begin
       V := my_func (TA, 1);
       V := my_func (TA, 2);
       V := my_func (TA, 3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (get_cnt (TA))
                 & " times");

    end Show_Cpp_Class;

Note that, in the Ada application, we cannot use the prefixed notation.
This notation would be more similar to the corresponding syntax in C++.
This restriction is caused by the fact that the automatic generated
bindings don't use tagged types. However, if we adapt the declaration of
:cpp:`Test` and replace it by :ada:`type Test is tagged limited record ...`,
we'll be able to write :ada:`TA.my_func(1)` and :ada:`TA.get_cnt` in our
application.

Another correction we might want to make is in the visibility of the
:ada:`Test` record. In the original C++ class, the :cpp:`func_cnt` element was
declared in the private part of the :cpp:`Test` class. However, in the
generated bindings, this element has been exposed, so it could be accessed
directly in our application. In order to correct that, we can simply move
the type declaration to the private part of the :ada:`Class_Test` package and
indicate that in the public part of the package (by using
:ada:`type Test is limited private;`).

After these adaptations, we get the following bindings:

.. code-block:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_hh is

       package Class_Test is
          type Test is tagged limited private;
          pragma Import (CPP, Test);

          function New_Test return Test;  -- ./test.hh:3
          pragma CPP_Constructor (New_Test, "_ZN4TestC1Ev");

          function my_func (this : access Test; a : int) return int;  -- ./test.hh:4
          pragma Import (CPP, my_func, "_ZN4Test7my_funcEi");

          function get_cnt (this : access Test) return int;  -- ./test.hh:5
          pragma Import (CPP, get_cnt, "_ZN4Test7get_cntEv");
       private
          type Test is tagged limited record
             func_cnt : aliased int;  -- ./test.hh:7
          end record;
       end;
       use Class_Test;
    end test_hh;

And this is the adapted Ada application:

.. code-block:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_hh;      use test_hh;

    procedure Show_Cpp_Class is
       use Class_Test;

       V : int;

       T : aliased Test := New_Test;
       TA : access Test := T'Access;

    begin
       V := TA.my_func (1);
       V := TA.my_func (2);
       V := TA.my_func (3);
       Put_Line ("Result is "
                 & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (TA.get_cnt)
                 & " times");

    end Show_Cpp_Class;


C++ constructors
~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Javier Miranda and Arnaud Charlet,
    and published as
    `Gem #61: Interfacing with C++ constructors <https://www.adacore.com/gems/gem-61>`_
    and
    `Gem #62: C++ constructors and Ada 2005 <https://www.adacore.com/gems/gem-62>`_

.. todo::

    Complete section!
