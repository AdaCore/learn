Interfacing with C
==================

.. include:: ../../global.txt

Ada allows us to interface with code in many languages, including C
and C++. This section discusses how to interface with C.

.. TODO: Add link to advanced course on C++

Multi-language project
----------------------

By default, when using :program:`gprbuild` we only compile Ada source files.
To compile C files as well, we need to modify the project file used by
:program:`gprbuild`. We use the ``Languages`` entry, as in the following
example:

.. code-block:: ada

    project Multilang is

       for Languages use ("ada", "c");

       for Source_Dirs use ("src");
       for Main use ("main.adb");
       for Object_Dir use "obj";

    end Multilang;

Type convention
---------------

To interface with data types declared in a C application, you specify
the :ada:`Convention` aspect on the corresponding Ada type
declaration. In the following example, we interface with the
:ada:`C_Enum` enumeration declared in a C source file:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Enum

    procedure Show_C_Enum is

       type C_Enum is (A, B, C)
         with Convention => C;
       --  Use C convention for C_Enum
    begin
       null;
    end Show_C_Enum;

To interface with C's built-in types, we use the :ada:`Interfaces.C`
package, which contains most of the type definitions we need. For example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Struct

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

Here, we're interfacing with a C struct (:ada:`C_Struct`) and using the
corresponding data types in C (:c:`int`, :c:`long`, :c:`unsigned` and
:c:`double`). This is the declaration in C:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Struct

    !c_struct.h
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

We use a similar approach when interfacing with subprograms written in C.
Consider the following declaration in the C header file:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Func

    !my_func.h
    int my_func (int a);

Here's the corresponding C definition:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Func

    !my_func.c
    #include "my_func.h"

    int my_func (int a)
    {
        return a * 2;
    }

We can interface this code in Ada using the :ada:`Import` aspect. For example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Func

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import        => True,
           Convention    => C;

       --  Imports function 'my_func' from C.
       --  You can now call it from Ada.

       V : int;
    begin
       V := my_func (2);
       Put_Line ("Result is " & int'Image (V));
    end Show_C_Func;

If you want, you can use a different subprogram name in the Ada code. For
example, we could call the C function :ada:`Get_Value`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Func

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function Get_Value (a : int) return int
         with
           Import        => True,
           Convention    => C,
           External_Name => "my_func";

       --  Imports function 'my_func' from C and
       --  renames it to 'Get_Value'

       V : int;
    begin
       V := Get_Value (2);
       Put_Line ("Result is " & int'Image (V));
    end Show_C_Func;

Calling Ada subprograms in C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can also call Ada subprograms from C applications. You do this with
the :ada:`Export` aspect. For example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Func

    with Interfaces.C; use Interfaces.C;

    package C_API is

       function My_Func (a : int) return int
         with
           Export        => True,
           Convention    => C,
           External_Name => "my_func";

    end C_API;

This is the corresponding body that implements that function:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Func

    package body C_API is

       function My_Func (a : int) return int is
       begin
          return a * 2;
       end My_Func;

    end C_API;

On the C side, we do the same as we would if the function were written
in C: simply declare it using the :c:`extern` keyword.  For example:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Func

    !main.c
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

To use global variables from C code, we use the same method as
subprograms: we specify the :ada:`Import` and :ada:`Convention`
aspects for each variable we want to import.

Let's reuse an example from the previous section. We'll add a global
variable (:c:`func_cnt`) to count the number of times the function
(:c:`my_func`) is called:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Vars

    !test.h
    extern int func_cnt;

    int my_func (int a);

The variable is declared in the C file and incremented in :c:`my_func`:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Vars

    !test.c
    #include "test.h"

    int func_cnt = 0;

    int my_func (int a)
    {
      func_cnt++;

      return a * 2;
    }

In the Ada application, we just reference the foreign variable:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.Ada_C_Vars

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_C_Func is

       function my_func (a : int) return int
         with
           Import     => True,
           Convention => C;

       V : int;

       func_cnt : int
         with
           Import        => True,
           Convention    => C;
       --  We can access the func_cnt variable
       --  from test.c

    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);

       Put_Line ("Result is "
                 & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (func_cnt)
                 & " times");
    end Show_C_Func;

As we see by running the application, the value of the counter is the
number of times :c:`my_func` was called.

We can use the :ada:`External_Name` aspect to give a different name
for the variable in the Ada application in the same we do for
subprograms.

Using Ada variables in C
~~~~~~~~~~~~~~~~~~~~~~~~

You can also use variables declared in Ada files in C applications. In
the same way as we did for subprograms, you do this with the
:ada:`Export` aspect.

Let's reuse a past example and add a counter, as in the previous
example, but this time have the counter incremented in Ada code:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Vars

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

The variable is then increment in :ada:`My_Func`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Vars

    package body C_API is

       function My_Func (a : int) return int is
       begin
          func_cnt := func_cnt + 1;
          return a * 2;
       end My_Func;

    end C_API;

In the C application, we just need to declare the variable and use it:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.C_Ada_Vars

    !main.c
    #include <stdio.h>

    extern int my_func (int a);

    extern int func_cnt;

    int main (int argc, char **argv) {

      int v;

      v = my_func(1);
      v = my_func(2);
      v = my_func(3);

      printf("Result is %d\n", v);

      printf("Function was called %d times\n",
             func_cnt);

      return 0;
    }

Again, by running the application, we see that the value from the counter
is the number of times that :c:`my_func` was called.

Generating bindings
-------------------

In the examples above, we manually added aspects to our Ada code to
correspond to the C source-code we're interfacing with. This is called
creating a *binding*. We can automate this process by using the *Ada spec
dump* compiler option: ``-fdump-ada-spec``. We illustrate this by
revisiting our previous example.

This was our C header file:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds

    !my_func.c
    extern int func_cnt;

    int my_func (int a);

To create Ada bindings, we'll call the compiler like this:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

The result is an Ada spec file called :file:`test_h.ads`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds
    :class: nosyntax-check

    pragma Ada_2005;
    pragma Style_Checks (Off);

    with Interfaces.C; use Interfaces.C;

    package test_h is

       func_cnt : aliased int;  -- ./test.h:3
       pragma Import (C, func_cnt, "func_cnt");

       function my_func (arg1 : int) return int;  -- ./test.h:5
       pragma Import (C, my_func, "my_func");

    end test_h;

Now we simply refer to this :file:`test_h` package in our Ada application:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds

    with Interfaces.C; use Interfaces.C;
    with Ada.Text_IO;  use Ada.Text_IO;
    with test_h;       use test_h;

    procedure Show_C_Func is
       V : int;
    begin
       V := my_func (1);
       V := my_func (2);
       V := my_func (3);

       Put_Line ("Result is "
                 & int'Image (V));

       Put_Line ("Function was called "
                 & int'Image (func_cnt)
                 & " times");
    end Show_C_Func;

You can specify the name of the parent unit for the bindings you're
creating as the operand to ``fdump-ada-spec``:

.. code-block:: sh

    gcc -c -fdump-ada-spec -fada-spec-parent=Ext_C_Code -C ./test.h

This creates the file :file:`ext_c_code-test_h.ads`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_2
    :class: ada-syntax-only

    package Ext_C_Code.test_h is

       --  automatic generated bindings...

    end Ext_C_Code.test_h;

Adapting bindings
~~~~~~~~~~~~~~~~~

The compiler does the best it can when creating bindings for a C header
file. However, sometimes it has to guess about the translation and the
generated bindings don't always match our expectations. For example,
this can happen when creating bindings for functions that have
pointers as arguments. In this case, the compiler may use
:ada:`System.Address` as the type of one or more pointers. Although
this approach works fine (as we'll see later), this is usually not how
a human would interpret the C header file. The following example
illustrates this issue.

Let's start with this C header file:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3

    !test.h
    struct test;

    struct test * test_create(void);

    void test_destroy(struct test *t);

    void test_reset(struct test *t);

    void test_set_name(struct test *t,
                       char        *name);

    void test_set_address(struct test *t,
                          char        *address);

    void test_display(const struct test *t);

And the corresponding C implementation:

.. code:: c no_button manual_chop project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3

    !test.c
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    #include "test.h"

    struct test {
      char name[80];
      char address[120];
    };

    static size_t
    strlcpy(char       *dst,
            const char *src,
            size_t      dstsize)
    {
      size_t len = strlen(src);
      if (dstsize) {
        size_t bl = (len < dstsize-1 ?
                       len : dstsize-1);
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

    void test_set_name(struct test *t,
                       char        *name)
    {
      strlcpy(t->name,
              name,
              sizeof(t->name));
    }

    void test_set_address(struct test *t,
                          char        *address)
    {
      strlcpy(t->address,
              address,
              sizeof(t->address));
    }

    void test_display(const struct test *t)
    {
      printf("Name:    %s\n", t->name);
      printf("Address: %s\n", t->address);
    }

Next, we'll create our bindings:

.. code-block:: sh

    gcc -c -fdump-ada-spec -C ./test.h

This creates the following specification in :file:`test_h.ads`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3
    :class: nosyntax-check

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

As we can see, the binding generator completely ignores the
declaration :c:`struct test` and all references to the :c:`test` struct
are replaced by addresses (:ada:`System.Address`). Nevertheless, these
bindings are good enough to allow us to create a test application in
Ada:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3

    with Ada.Text_IO; use Ada.Text_IO;

    with Interfaces.C;
    use  Interfaces.C;

    with Interfaces.C.Strings;
    use  Interfaces.C.Strings;

    with test_h; use test_h;

    with System;

    procedure Show_Automatic_C_Struct_Bindings is

       Name    : constant chars_ptr :=
         New_String ("John Doe");
       Address : constant chars_ptr :=
         New_String ("Small Town");

       T : System.Address := test_create;

    begin
       test_reset (T);
       test_set_name (T, Name);
       test_set_address (T, Address);

       test_display (T);
       test_destroy (T);
    end Show_Automatic_C_Struct_Bindings;

We can successfully bind our C code with Ada using the
automatically-generated bindings, but they aren't ideal. Instead, we would
prefer Ada bindings that match our (human) interpretation of the C header
file. This requires manual analysis of the header file. The good news is
that we can use the automatic generated bindings as a starting point and
adapt them to our needs. For example, we can:

    #. Define a :ada:`Test` type based on :ada:`System.Address` and use it in
       all relevant functions.

    #. Remove the ``test_`` prefix in all operations on the :ada:`Test`
       type.

This is the resulting specification:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3
    :class: nosyntax-check

    with System;

    with Interfaces.C; use Interfaces.C;
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

And this is the corresponding Ada body:

.. code:: ada no_button project=Courses.Intro_To_Ada.Interfacing_With_C.C_Binds_3

    with Interfaces.C;
    use  Interfaces.C;

    with Interfaces.C.Strings;
    use  Interfaces.C.Strings;

    with adapted_test_h; use  adapted_test_h;

    with System;

    procedure Show_Adapted_C_Struct_Bindings is

       Name    : constant chars_ptr :=
         New_String ("John Doe");
       Address : constant chars_ptr :=
         New_String ("Small Town");

       T : Test := Create;

    begin
       Reset (T);
       Set_Name (T, Name);
       Set_Address (T, Address);

       Display (T);
       Destroy (T);
    end Show_Adapted_C_Struct_Bindings;

Now we can use the :ada:`Test` type and its operations in a clean, readable
way.
