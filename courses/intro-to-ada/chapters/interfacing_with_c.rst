Interfacing with C
==================
:code-config:`reset_accumulator=True`

Ada allows for interfacing with existing code in C and C++. This section will
discuss how to interface with C specifically.

.. TODO: Add link to advanced course on C++

Multi-language project
----------------------

When using ``gprbuild``, we can only compile Ada source-code files by
default. In order to compile C files in addition to Ada files, we need to
adapt the project file used by ``gprbuild``. This can be achieved by
using the ``Languages`` entry, as in the following example:

.. code-block:: ada

    project Multilang is

       for Languages use ("ada", "c");

       for Source_Dirs use ("src");
       for Main use ("main.adb");
       for Object_Dir use "obj";

    end Multilang;

Type convention
---------------

In order to interface with data types declared in a C application, the
convention aspect needs to be specified. In the following example, we
interface with the ``C_Enum`` enumeration declared in a C source-code
file:

.. code:: ada

    procedure Show_C_Enum is

       type C_Enum is (A, B, C) with Convention => C;
       --  Use C convention for C_Enum
    begin
       null;
    end Show_C_Enum;

In order to interface with C built-in types, we need to reference to the
:ada:`Interfaces.C` package, which contains all type definitions that we
need. For example:

.. code:: ada

    with Interfaces.C; use Interfaces.C;

    procedure Show_C_Struct is

       type c_struct is record
          a : int;
          b : long;
          c : unsigned;
          d : double;
       end record
         with Convention => C;

    begin
       null;
    end Show_C_Struct;

In this example, we're interfacing with a C struct (``C_Struct``) and
making use of the corresponding data types in C (:c:`int`, :c:`long`,
:c:`unsigned` and :c:`double`). This is the original declaration:

.. code-block:: c

    struct c_struct
    {
        int         a;
        long        b;
        unsigned    c;
        double      d;
    };

Foreign subprograms
-------------------

Calling C subprograms in Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A similar approach is used when interfacing with subprograms written in C.
In this case, an additional aspect is required: :ada:`Import`. For example:

.. code:: ada

    with Interfaces.C; use Interfaces.C;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;
       --  Imports function 'my_func' from C.
       --  You can now call it from Ada.

    begin
       null;
    end Show_C_Func;

This code interfaces with the following declaration in the C header file:

.. code-block:: c

    int my_func (int a);

This is the corresponding implementation:

.. code-block:: c

    #include "my_func.h"

    int my_func (int a)
    {
        return a * 2;
    }

It is possible to use a different subprogram name in the Ada code. For
example, we could rename the original C function to ``Get_Value`` in the
Ada code:

.. code:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function Get_Value (a : int) return int
         with
           Import        => True,
           Convention    => C,
           External_Name => "my_func";

       --  Imports function 'my_func' from C and
       --  rename it to 'Get_Value'

       V : int;
    begin
       V := Get_Value (2);
       Put_Line ("Result is " & int'Image (V));
    end Show_C_Func;

As the example shows, we can make use of the ``Get_Value`` function and
retrieve information without additional efforts.

Calling Ada subprograms in C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to call Ada subprograms in C applications. This
requires the use of the :ada:`Export` aspect. For example:

.. code:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

This is the corresponding implementation:

.. code:: ada

    package body C_API is

       function My_Func (a : int) return int is
       begin
          return a * 2;
       end My_Func;

    end C_API;

In the C code, we simply have to declare the function using the :c:`extern`
keyword. For example:

.. code-block:: c

    #include <stdio.h>

    extern int my_func (int a);

    int main (int argc, char **argv) {

      int v = my_func(2);

      printf("Result is %d\n", v);

      return 0;
    }

Foreign variables
-----------------

Using C global variables in Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to use global variables from C code, we can apply the same method
as for subprograms: we just specify the :ada:`Import` and :ada:`Convention`
aspects for the variable we want to import.

Let's reuse an example from the previous section. We'll add a global
variable (``func_cnt``) that counts the number of times that a the
function (``my_func``) was called:

.. code-block:: c

    /*% filename: test.h */

    extern int func_cnt;

    int my_func (int a);

The variable is then declared in the C file and increment in ``my_func``:

.. code-block:: c

    #include "test.h"

    int func_cnt = 0;

    int my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

In the Ada application, we just need to reference the foreign variable:

.. code:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;

       V : int;

       func_cnt : int
         with
           Import        => True,
           Convention    => C;
       --  We can access the func_cnt variable from test.c

    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called " & int'Image (func_cnt) & " times");
    end Show_C_Func;

As we can see by running the application, the value from the counter will
contain the correct number of times that ``my_func`` was called.

Similar to subprograms, we could use the :ada:`External_Name` aspect to
rename the variable in the Ada application.

Using Ada variables in C
~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to use variables declared in Ada files in C
applications. Similarly to subprogram, this requires the use of the
:ada:`Export` aspect.

Let's reuse the previous example and add a counter, as we did in the
previous example:

.. code:: ada

    with Interfaces.C; use Interfaces.C;

    package C_API is

       func_cnt : int := 0
         with
           Export     => True,
           Convention => C;

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

The variable is then increment in ``My_Func``:

.. code:: ada

    --% filename: c_api.adb
    package body C_API is

       function My_Func (a : int) return int is
       begin
          func_cnt := func_cnt + 1;
          return a * 2;
       end My_Func;

    end C_API;

In the C application, we just need to declare the variable and use it:

.. code-block:: c

    #include <stdio.h>

    extern int my_func (int a);

    extern int func_cnt;

    int main (int argc, char **argv) {

      int v;

      v = my_func(1);
      v = my_func(2);
      v = my_func(3);

      printf("Result is %d\n", v);

      printf("Function was called %d times\n", func_cnt);

      return 0;
    }

Again, by running the application, we see that the value from the counter
will contain the correct number of times that ``my_func`` was called.

Generating bindings
-------------------

In the examples above, we have manually created the Ada bindings for the
C source-code we wanted to interface with. It is possible to automate this
process by using the *Ada spec dump* compiler option:
``-fdump-ada-spec``. We'll discuss details by revisiting our previous
example.

This was the C header file that we had:

.. code-block:: c

    extern int func_cnt;

    int my_func (int a);

In order to create bindings, we'll call the compiler like this:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

This will create an Ada specification file called ``test_h.ads``:

.. code:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_h is

       func_cnt : aliased int;  -- ./test.h:3
       pragma Import (C, func_cnt, "func_cnt");

       function my_func (arg1 : int) return int;  -- ./test.h:5
       pragma Import (C, my_func, "my_func");

    end test_h;

Now, we can simply refer to ``test_h`` package in the Ada application:

.. code:: ada

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_h;       use test_h;

    procedure Show_C_Func is
       V : int;
    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);
       Put_Line ("Result is " & int'Image (V));

       Put_Line ("Function was called " & int'Image (func_cnt) & " times");
    end Show_C_Func;

Note that, in addition to ``fdump-ada-spec``, you can also specify the
parent unit for the bindings you're creating. For example:

.. code-block:: sh

    gcc -c -fdump-ada-spec -fada-spec-parent=Ext_C_Code -C ./test.h

This will create the file ``ext_c_code-test_h.ads``:

.. code:: ada
    :class: ada-syntax-only

    package Ext_C_Code.test_h is

       --  automatic generated bindings...

    end Ext_C_Code.test_h;

Adapting bindings
~~~~~~~~~~~~~~~~~

When creating bindings for a C header file, the compiler tries to do the
best guess it can. However, the generated bindings do not always match the
expectations we might have. This can happen, for example, when creating
bindings for functions that deal with pointers. In this case, the compiler
may just use :ada:`System.Address` for the pointers. Although this approach
works fine (as we'll see later), this is not necessarily how developers
would interpret the C header file. The following example will clarify this
problem.

Let's start with the following C header file:

.. code-block:: c

    /*% filename: test.h */

    struct test;

    struct test * test_create(void);

    void test_destroy(struct test *t);

    void test_reset(struct test *t);

    void test_set_name(struct test *t, char *name);

    void test_set_address(struct test *t, char *address);

    void test_display(const struct test *t);

This is the corresponding implementation:

.. code-block:: c

    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    #include "test.h"

    struct test {
      char name[80];
      char address[120];
    };

    static size_t
    strlcpy(char *dst, const char *src, size_t dstsize)
    {
      size_t len = strlen(src);
      if (dstsize) {
        size_t bl = (len < dstsize-1 ? len : dstsize-1);
        ((char*)memcpy(dst, src, bl))[bl] = 0;
      }
      return len;
    }

    struct test * test_create(void)
    {
      return malloc (sizeof (struct test));
    }

    void test_destroy(struct test *t)
    {
      if (t != NULL) {
        free(t);
      }
    }

    void test_reset(struct test *t)
    {
      t->name[0]    = '\0';
      t->address[0] = '\0';
    }

    void test_set_name(struct test *t, char *name)
    {
      strlcpy(t->name, name, sizeof(t->name));
    }

    void test_set_address(struct test *t, char *address)
    {
      strlcpy(t->address, address, sizeof(t->address));
    }

    void test_display(const struct test *t)
    {
      printf("Name:    %s\n", t->name);
      printf("Address: %s\n", t->address);
    }

Next, we'll create our bindings by running gcc:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

This creates the following specification in ``test_h.ads``:

.. code:: ada

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package test_h is

       --  skipped empty struct test

       function test_create return System.Address;  -- ./test.h:5
       pragma Import (C, test_create, "test_create");

       procedure test_destroy (arg1 : System.Address);  -- ./test.h:7
       pragma Import (C, test_destroy, "test_destroy");

       procedure test_reset (arg1 : System.Address);  -- ./test.h:9
       pragma Import (C, test_reset, "test_reset");

       procedure test_set_name (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, test_set_name, "test_set_name");

       procedure test_set_address (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- ./test.h:13
       pragma Import (C, test_set_address, "test_set_address");

       procedure test_display (arg1 : System.Address);  -- ./test.h:15
       pragma Import (C, test_display, "test_display");

    end test_h;

As we can see, the bindings generator completely ignores the specification
of :c:`struct test`. Also, all references to the ``test`` are replaced by
simple addresses (:ada:`System.Address`). Of course, these bindings are good
enough for creating a test application in Ada:

.. code:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with Ada.Text_IO;          use Ada.Text_IO;
    with test_h;               use test_h;

    with System;

    procedure Show_Automatic_C_Struct_Bindings is

       Name    : constant chars_ptr := New_String ("John Doe");
       Address : constant chars_ptr := New_String ("Small Town");

       T : System.Address := test_create;

    begin
       test_reset (T);
       test_set_name (T, Name);
       test_set_address (T, Address);

       test_display (T);
       test_destroy (T);
    end Show_Automatic_C_Struct_Bindings;

Even though we can successfully bind our C code with Ada using the
automatic generated bindings, they are not ideal. Instead, we would like
to have Ada bindings that match our (human) interpretation of the C header
file. This will require manual analysis of the header file. The good news
are that, at least, we can use the automatic generated bindings as a
starting point and adapt them to our needs. For example, we can:

    #. Define a ``Test`` type based on :ada:`System.Address` and use it in
       all relevant function.

    #. Remove the ``test_`` prefix in all operations on the ``Test``
       type.

This would be the resulting specification:

.. code:: ada

    with Interfaces.C; use Interfaces.C;
    with System;
    with Interfaces.C.Strings;

    package adapted_test_h is

       type Test is new System.Address;

       function Create return Test;
       pragma Import (C, Create, "test_create");

       procedure Destroy (T : Test);
       pragma Import (C, Destroy, "test_destroy");

       procedure Reset (T : Test);
       pragma Import (C, Reset, "test_reset");

       procedure Set_Name (T    : Test;
                           Name : Interfaces.C.Strings.chars_ptr);  -- ./test.h:11
       pragma Import (C, Set_Name, "test_set_name");

       procedure Set_Address (T       : Test;
                              Address : Interfaces.C.Strings.chars_ptr);
       pragma Import (C, Set_Address, "test_set_address");

       procedure Display (T : Test);  -- ./test.h:15
       pragma Import (C, Display, "test_display");

    end adapted_test_h;

This would be the corresponding Ada application:

.. code:: ada

    with Interfaces.C;         use Interfaces.C;
    with Interfaces.C.Strings; use Interfaces.C.Strings;
    with Ada.Text_IO;          use  Ada.Text_IO;
    with adapted_test_h;       use  adapted_test_h;

    with System;

    procedure Show_Adapted_C_Struct_Bindings is

       Name    : constant chars_ptr := New_String ("John Doe");
       Address : constant chars_ptr := New_String ("Small Town");

       T : Test := Create;

    begin
       Reset (T);
       Set_Name (T, Name);
       Set_Address (T, Address);

       Display (T);
       Destroy (T);
    end Show_Adapted_C_Struct_Bindings;

Now, we're able to use the ``Test`` type and its operations in a clean,
readable way.
