:next_state: False

.. include:: ../../../global.txt

Interfacing C variadic functions
================================

.. note::

    Variadic convention is supported by

    * GNAT Community Edition 2020
    * GCC 11

In C, `variadic functions`_ take a variable number of arguments and an
ellipsis as the last parameter of the declaration. A typical and well-known
example is:

.. code-block:: c

   int printf (const char* format, ...);

Usually, in Ada, we bind such a function with just the parameters we want
to use:

.. code-block:: ada

   procedure printf_double
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.double)
        with Import,
          Convention    => C,
          External_Name => "printf";

Then we call it as a normal Ada function:

.. code-block:: ada

   printf_double (Interfaces.C.To_C ("Pi=%f"), Ada.Numerics.π);

Unfortunately, doing it this way doesn't always work because some
:wikipedia:`ABI <Application_binary_interface>`\ s use different calling
conventions for variadic functions. For
example, the `AMD64 ABI`_ specifies:

 * ``%rax`` |mdash| with variable arguments passes information about the number
   of vector registers used;
 * ``%xmm0–%xmm1`` |mdash| used to pass and return floating point arguments.

This means, if we write (in C):

.. code-block:: c

   printf("%d", 5);

The compiler will place 0 into ``%rax``, because we don't pass any float
argument. But in Ada, if we write:

.. code-block:: ada

   procedure printf_int
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.int)
        with Import,
          Convention    => C,
          External_Name => "printf";

   printf_int (Interfaces.C.To_C ("d=%d"), 5);

the compiler won't use the ``%rax`` register at all. (You can't include
any float argument because there's no float parameter in the Ada
wrapper function declaration.) As result, you will get a crash, stack
corruption, or other undefined behavior.

To fix this, Ada 2022 provides a new family of calling convention
names |mdash| :ada:`C_Variadic_N`:

   The convention :ada:`C_Variadic_n` is the calling convention for a variadic
   C function taking `n` fixed parameters and then a variable number of
   additional parameters.

Therefore, the correct way to bind the :c:`printf` function is:

.. code-block:: ada

   procedure printf_int
     (format : Interfaces.C.char_array;
      value  : Interfaces.C.int)
        with Import,
          Convention    => C_Variadic_1,
          External_Name => "printf";

And the following call won't crash on any supported platform:

.. code-block:: ada

   printf_int (Interfaces.C.To_C ("d=%d"), 5);

Without this convention, problems cause by this mismatch can be very hard
to debug. So, this is a very useful extension to the Ada-to-C interfacing
facility.

Here is the complete code snippet:

.. code:: ada no_button project=Courses.Ada_2022_Whats_New.Variadic_Import switches=Compiler(-gnat2022);

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

References
----------

* :aarm22:`ARM B.3 Interfacing with C and C++ <B-3>`
* AI12-0028-1_

.. _AI12-0028-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0028-1.TXT

.. _`variadic functions`: https://en.cppreference.com/w/c/variadic
.. _`AMD64 ABI`: https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf
