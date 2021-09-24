.. include:: ../../global.txt

Interfacing C variadic functions
================================

.. note::

    Variadic convention is supported by

    * GNAT Community Edition 2020
    * GCC 11

In C, `variadic functions`_ take a variable number of arguments and
have an ellipsis as the last parameter of the declaration. The typical
example is

.. code-block:: c

   int printf(const char* format, ...);

Usually, in Ada, we bind such a function with required parameters:

.. code-block:: ada

   procedure printf_double
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.double)
        with Import,
          Convention    => C,
          External_Name => "printf";

Then we call it as usual Ada function:

.. code-block:: ada

   printf_double (Interfaces.C.To_C ("Pi=%f"), Ada.Numerics.π);

Unfortunately, often it just doesn't work this way. Some `ABI`_ use
different calling conventions for variadic functions. For instance
the `AMD64 ABI`_ specifies:

 * %rax - with variable arguments passes information about the number
   of vector registers used
 * %xmm0–%xmm1 - used to pass and return floating point arguments

This means, if we write (in C):

.. code-block:: c

   printf("%d", 5);

Then the compiler will place 0 into %rax, because we don't pass any
float argument (but we could). And in Ada, if we write:

.. code-block:: ada

   procedure printf_int
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.int)
        with Import,
          Convention    => C,
          External_Name => "printf";

   printf_int (Interfaces.C.To_C ("d=%d"), 5);

The Ada compiler will not use %rax register at all (since you can't
put any float argument, because there is no float parameter in the
Ada wrapper function declaration). As result, you will get crash,
stack corruption or any other undefined behavior.

To fix this, Ada 2020 provides a new family of calling convention
names - C_Variadic_N:

   The convention C_Variadic_n is the calling convention for a variadic
   C function taking `n` fixed parameters and then a variable number of
   additional parameters.

So, the right way to bind printf function is:

.. code-block:: ada

   procedure printf_int
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.int)
        with Import,
          Convention    => C_Variadic_1,
          External_Name => "printf";

And the next call won't crash on any supported platform:

.. code-block:: ada

   printf_int (Interfaces.C.To_C ("d=%d"), 5);

This issue is hard to debug. So, I consider this as a very useful fix
for Ada-to-C interfacing facility.

Complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Variadic_Import

   with Interfaces.C;

   procedure Main is

      procedure printf_int
        (format : Interfaces.C.char_array;
         value  : Interfaces.C.int)
        with Import,
          Convention    => C_Variadic_1,
          External_Name => "printf";

   begin
      printf_int (Interfaces.C.To_C ("d=%d"), 5);
   end Main;

References:
-----------

* `ARM B.3 Interfacing with C and C++`_
* AI12-0028-1_

.. _`ARM B.3 Interfacing with C and C++`: http://www.ada-auth.org/standards/2xaarm/html/AA-B-3.html
.. _AI12-0028-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0028-1.TXT

.. _`variadic functions`: https://en.cppreference.com/w/c/variadic
.. _`ABI`: https://en.wikipedia.org/wiki/Application_binary_interface
.. _`AMD64 ABI`: https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf