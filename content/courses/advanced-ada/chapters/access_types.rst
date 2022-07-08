Access Types
============

.. include:: ../../global.txt

.. _Adv_Ada_Ragged_Arrays:

Ragged arrays
-------------

Ragged arrays |mdash| also known as jagged arrays |mdash| are non-uniform,
multidimensional arrays. They can be useful to implement tables with varying
number of coefficients, as we discuss as an example in this section.

Uniform multidimensional arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider an algorithm that processes data based on coefficients that depends on
a selected quality level:

+------------------------+--------------+-----+-----+-----+-----+-----+
| Quality level          | Number of    |   #1|   #2|   #3|   #4|   #5|
|                        | coefficients |     |     |     |     |     |
+========================+==============+=====+=====+=====+=====+=====+
| Simplified             |            1 | 0.15|     |     |     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Better                 |            3 | 0.02| 0.16| 0.27|     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Best                   |            5 | 0.01| 0.08| 0.12| 0.20| 0.34|
+------------------------+--------------+-----+-----+-----+-----+-----+

(Note that this is just a bogus table with no real purpose, as we're not
trying to implement any actual algorithm.)

We can implement this table as a two-dimensional array (:ada:`Calc_Table`),
where each quality level has an associated array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

    private

       Calc_Table : constant array (Quality_Level, 1 .. 5) of Float :=
         (Simplified => (0.15, 0.00, 0.00, 0.00, 0.00),
          Better     => (0.02, 0.16, 0.27, 0.00, 0.00),
          Best       => (0.01, 0.08, 0.12, 0.20, 0.34));

       Last : constant array (Quality_Level) of Positive :=
         (Simplified => 1,
          Better     => 3,
          Best       => 5);

    end Data_Processing;

Note that, in this implementation, we have a separate table :ada:`Last` that
indicates the actual number of coefficients of each quality level.

Alternatively, we could use a record (:ada:`Table_Coefficient`) that stores the
number of coefficients and the actual coefficients:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is array (Positive range <>) of Float;

    private

       type Table_Coefficient is record
          Last : Positive;
          Coef : Data (1 .. 5);
       end record;

       Calc_Table : constant array (Quality_Level) of Table_Coefficient :=
         (Simplified => (1, (0.15, 0.00, 0.00, 0.00, 0.00)),
          Better     => (3, (0.02, 0.16, 0.27, 0.00, 0.00)),
          Best       => (5, (0.01, 0.08, 0.12, 0.20, 0.34)));

    end Data_Processing;

In this case, we have a unidimensional array where each component (of
:ada:`Table_Coefficient` type) contains an array (:ada:`Coef`) with the
coefficients.

This is an example of a :ada:`Process` procedure that references the
:ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in 1 .. Calc_Table (Q).Last loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Note that, to loop over the coefficients, we're using
:ada:`for J in 1 .. Calc_Table (Q).Last loop` instead of
:ada:`for J in Calc_Table (Q)'Range loop`. As we're trying to make a
non-uniform array fit in a uniform array, we cannot simply loop over all
elements using the :ada:`Range` attribute, but must be careful to use the
correct number of elements in the loop instead.

Also, note that :ada:`Calc_Table` has 15 coefficients in total. Out of those
coefficients, 6 coefficients (or 40 percent of the table) aren't being used.
Naturally, this is wasted memory space. We can improve this by using ragged
arrays.


Non-uniform multidimensional array
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ragged arrays are declared by using an access type to an array. By doing that,
each array can be declared with a different size, thereby creating a
non-uniform multidimensional array.

For example, we can declare a constant array :ada:`Table` as a ragged array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Ragged_Array

    package Data_Processing is

       type Integer_Array is array (Positive range <>) of Integer;

    private

       type Integer_Array_Access is access constant Integer_Array;

       Table : constant array (1 .. 3) of Integer_Array_Access :=
         (1 => new Integer_Array'(1 => 15),
          2 => new Integer_Array'(1 => 12, 2 => 15, 3 => 20),
          3 => new Integer_Array'(1 => 12, 2 => 15, 3 => 20,
                                  4 => 20, 5 => 25, 6 => 30));

    end Data_Processing;

Here, each component of :ada:`Table` is an access to another array. As each
array is allocated via :ada:`new`, those arrays may have different sizes.

We can rewrite the example from the previous subsection using a ragged array
for the :ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Ragged_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is array (Positive range <>) of Float;

    private

       type Coefficients is access constant Data;

       Calc_Table : constant array (Quality_Level) of Coefficients :=
         (Simplified => new Data'(1 => 0.15),
          Better     => new Data'(0.02, 0.16, 0.27),
          Best       => new Data'(0.01, 0.08, 0.12, 0.20, 0.34));

    end Data_Processing;

Now, we aren't wasting memory space because each data component has the right
size that is required for each quality level. Also, we don't need to store the
number of coefficients, as this information is automatically available from the
array initialization (via the allocation of the :ada:`Data` array for the
:ada:`Coefficients` type).

This is the adapted :ada:`Process` procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Ragged_Table


    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in Calc_Table (Q)'Range loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Now, we can simply loop over the coefficients by writing
:ada:`for J in Calc_Table (Q)'Range loop`, as each element of :ada:`Calc_Table`
automatically has the correct range.


Aliasing
--------

The term `aliasing <https://en.wikipedia.org/wiki/Aliasing_(computing)>`_
refers to objects in memory that we can access using more than a single
reference. In Ada, if we allocate an object via :ada:`new`, we have a
potentially aliased object. We can then have multiple references to this
object:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Aliasing_Via_Access

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliasing is
       type Integer_Access is access Integer;

       A1, A2 : Integer_Access;
    begin
       A1 := new Integer;
       A2 := A1;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

       A2.all := 24;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));
    end Show_Aliasing;

In this example, we access the object allocated via :ada:`new` by using either
:ada:`A1` or :ada:`A2`, as both refer to the same *aliased* object. In other
words, :ada:`A1` or :ada:`A2` allow us to access the same object in memory.

.. admonition:: Important

    Note that aliasing is unrelated to renaming. For example, we could use
    renaming to write a program that looks similar to the one above:

    .. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Renaming

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Show_Renaming is
           A1 : Integer;
           A2 : Integer renames A1;
        begin
           A1 := 22;
           Put_Line ("A1: " & Integer'Image (A1));
           Put_Line ("A2: " & Integer'Image (A2));

           A2 := 24;
           Put_Line ("A1: " & Integer'Image (A1));
           Put_Line ("A2: " & Integer'Image (A2));
        end Show_Renaming;

    Here, :ada:`A1` or :ada:`A2` are two different names for the same object.
    However, the object itself isn't aliased.

.. admonition:: In the Ada Reference Manual

    - `3.10 Access Types <http://www.ada-auth.org/standards/12rm/html/RM-3-10.html>`__


Aliased objects
~~~~~~~~~~~~~~~

In addition to using :ada:`new` to create aliased objects, we can indicate
that an object is aliased by using the :ada:`aliased` keyword in the object's
declaration: :ada:`Obj : aliased Integer;`.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Access_Aliased_Obj

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Obj is
       type Integer_Access is access all Integer;

       I_Var : aliased Integer;
       A1    : Integer_Access;
    begin
       A1 := I_Var'Access;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Aliased_Obj;

Here, we declare :ada:`I_Var` as an aliased integer variable and get a
reference to it, which we assign to :ada:`A1`. Naturally, we could also have
two accesses :ada:`A1` and :ada:`A2`:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Access_Aliased_Obj

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Obj is
       type Integer_Access is access all Integer;

       I_Var  : aliased Integer;
       A1, A2 : Integer_Access;
    begin
       A1 := I_Var'Access;
       A2 := A1;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

       A2.all := 24;
       Put_Line ("A1: " & Integer'Image (A1.all));
       Put_Line ("A2: " & Integer'Image (A2.all));

    end Show_Aliased_Obj;

In this example, both :ada:`A1` and :ada:`A2` refer to the :ada:`I_Var`
variable.

Note that these examples make use of these two features:

1. The declaration of an access-to-variable type (:ada:`Integer_Access`)
   using :ada:`access all`.

2. The retrieval of a reference to :ada:`I_Var` using the :ada:`Access`
   attribute.

In the next sections, we discuss these features in more details.

.. admonition:: In the Ada Reference Manual

    - `3.3.1 Object Declarations <http://www.ada-auth.org/standards/12rm/html/RM-3-3-1.html>`__
    - `3.10 Access Types <http://www.ada-auth.org/standards/12rm/html/RM-3-10.html>`__

General access modifiers
^^^^^^^^^^^^^^^^^^^^^^^^

In addition to the *standard* access type declarations, Ada provides two access
modifiers:

+--------------------+----------------------------------------+
| Type               | Declaration                            |
+====================+========================================+
| Access-to-variable | :ada:`type T_Acc is access all T`      |
+--------------------+----------------------------------------+
| Access-to-constant | :ada:`type T_Acc is access constant T` |
+--------------------+----------------------------------------+

Let's look at an example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Show_Access_Modifiers

    package Integer_Access_Types is

       type Integer_Access is
         access Integer;

       type Integer_Access_All is
         access all Integer;

       type Integer_Access_Const is
         access constant Integer;

    end Integer_Access_Types;

As we've seen previously, we can use a type such as :ada:`Integer_Access` to
allocate objects dynamically. However, we cannot use this type to refer to
declared objects, for example. In this case, we have to use an
access-to-variable type such as :ada:`Integer_Access_All`. Also, if we want to
access constants |mdash| or access objects that we want to treat as constants
|mdash|, we use a type such as :ada:`Integer_Access_Const`.


Access attribute
^^^^^^^^^^^^^^^^

To get access to a variable or a constant, we make use of the :ada:`'Access`
attribute. For example, :ada:`I_Var'Access` gives us access to the :ada:`I_Var`
object.

Let's look at an example of how to use the integer access types from the
previous code snippet:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Show_Access_Modifiers

    package Integer_Access_Types is

       type Integer_Access is
         access Integer;

       type Integer_Access_All is
         access all Integer;

       type Integer_Access_Const is
         access constant Integer;

       procedure Show;

    end Integer_Access_Types;

    with Ada.Text_IO;          use Ada.Text_IO;

    package body Integer_Access_Types is

       I_Var : aliased          Integer :=  0;
       Fact  : aliased constant Integer := 42;

       Dyn_Ptr     : constant Integer_Access       := new Integer'(30);
       I_Var_Ptr   : constant Integer_Access_All   := I_Var'Access;
       I_Var_C_Ptr : constant Integer_Access_Const := I_Var'Access;
       Fact_Ptr    : constant Integer_Access_Const := Fact'Access;

       procedure Show is
       begin
          Put_Line ("Dyn_Ptr:     " & Integer'Image (Dyn_Ptr.all));
          Put_Line ("I_Var_Ptr:   " & Integer'Image (I_Var_Ptr.all));
          Put_Line ("I_Var_C_Ptr: " & Integer'Image (I_Var_C_Ptr.all));
          Put_Line ("Fact_Ptr:    " & Integer'Image (Fact_Ptr.all));
       end Show;

    end Integer_Access_Types;

    with Integer_Access_Types;

    procedure Show_Access_Modifiers is
    begin
       Integer_Access_Types.Show;
    end Show_Access_Modifiers;

In this example, :ada:`Dyn_Ptr` refers to a dynamically allocated object,
:ada:`I_Var_Ptr` refers to the :ada:`I_Var` variable, and :ada:`Fact_Ptr`
refers to the :ada:`Fact` constant. We get access to the variable and the
constant objects by using the :ada:`'Access` attribute.

Also, we declare :ada:`I_Var_C_Ptr` as an access-to-constant, but we get
access to the :ada:`I_Var` variable. This simply means the object
:ada:`I_Var_C_Ptr` refers to is treated as a constant. Therefore, we can
write :ada:`I_Var := 22;`, but we cannot write :ada:`I_Var_C_Ptr.all := 22;`.

.. admonition:: In the Ada Reference Manual

    - `3.10.2 Operations of Access Types <http://www.ada-auth.org/standards/12rm/html/RM-3-10-2.html>`_


Non-aliased objects
^^^^^^^^^^^^^^^^^^^

By default, declared objects |mdash| i.e. without using :ada:`new`
|mdash| are not aliased. Therefore, we cannot have get a reference to those
objects. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Access_Non_Aliased_Obj
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Access_Error is
       type Integer_Access is access all Integer;
       I_Var : Integer;
       A1    : Integer_Access;
    begin
       A1 := I_Var'Access;

       A1.all := 22;
       Put_Line ("A1: " & Integer'Image (A1.all));
    end Show_Access_Error;

In this example, the compiler complains that we cannot get a reference to
:ada:`I_Var` because :ada:`I_Var` is not aliased.


Ragged arrays using aliased objects
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can use aliased objects to declare
:ref:`ragged arrays <Adv_Ada_Ragged_Arrays>`. For example, we can rewrite a
previous program using aliased constant objects:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Ragged_Array_Aliased_Objs

    package Data_Processing is

       type Integer_Array is array (Positive range <>) of Integer;

    private

       type Integer_Array_Access is access constant Integer_Array;

       Tab_1 : aliased constant Integer_Array := (1 => 15);
       Tab_2 : aliased constant Integer_Array := (12, 15, 20);
       Tab_3 : aliased constant Integer_Array := (12, 15, 20,
                                                  20, 25, 30);

       Table : constant array (1 .. 3) of Integer_Array_Access :=
         (1 => Tab_1'Access,
          2 => Tab_2'Access,
          3 => Tab_3'Access);

    end Data_Processing;

Here, instead of allocating the constant arrays dynamically via :ada:`new`, we
declare three aliased arrays (:ada:`Tab_1`, :ada:`Tab_2` and :ada:`Tab_3`) and
get a reference to them in the declaration of :ada:`Table`.


Aliased access objects
^^^^^^^^^^^^^^^^^^^^^^

It's interesting to mention that access objects can be aliased themselves.
Consider this example where we declare the :ada:`Integer_Access_Access` type
to refer to an access object:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Aliased_Access

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Access_Obj is

       type Integer_Access        is access all Integer;
       type Integer_Access_Access is access all Integer_Access;

       I_Var : aliased Integer;
       A     : aliased Integer_Access;
       B     : Integer_Access_Access;
    begin
       A := I_Var'Access;
       B := A'Access;

       B.all.all := 22;
       Put_Line ("A: " & Integer'Image (A.all));
       Put_Line ("B: " & Integer'Image (B.all.all));
    end Show_Aliased_Access_Obj;

After the assignments in this example, :ada:`B` refers to :ada:`A`, which in
turn refers to :ada:`I_Var`. Note that this code only compiles because we
declare :ada:`A` as an aliased (access) object.


Aliased components
~~~~~~~~~~~~~~~~~~

Components of an array or a record can be aliased. This allows us to get access
to those components:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Aliased_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Aliased_Components is

       type Integer_Access is access all Integer;

       type Rec is record
          I_Var_1 :         Integer;
          I_Var_2 : aliased Integer;
       end record;

       type Integer_Array is array (Positive range <>) of
         aliased Integer;

       R   : Rec := (22, 24);
       Arr : Integer_Array (1 .. 3) := (others => 42);
       A   : Integer_Access;
    begin
       --  A := R.I_Var_1'Access;
       --                 ^ ERROR: cannot access
       --                          non-aliased component

       A := R.I_Var_2'Access;
       Put_Line ("A: " & Integer'Image (A.all));

       A := Arr (2)'Access;
       Put_Line ("A: " & Integer'Image (A.all));
    end Show_Aliased_Components;

In this example, we get access to the :ada:`I_Var_2` component of record
:ada:`R`. (Note that trying to access the :ada:`I_Var_1` component would gives us
a compilation error, as this component is not aliased.) Similarly, we get
access to the second component of array :ada:`Arr`.

Declaring components with the :ada:`aliased` keyword allows us to specify that
those are accessible via other paths besides the component name. Therefore, the
compiler won't store them in registers. This can be essential when doing
low-level programming |mdash| for example, when accessing memory-mapped
registers. In this case, we want to ensure that the compiler uses the memory
address we're specifying (instead of assigning registers for those components).

.. admonition:: In the Ada Reference Manual

    - `3.6 Array Types <http://www.ada-auth.org/standards/12rm/html/RM-3-6.html>`_


Aliased parameters
~~~~~~~~~~~~~~~~~~

In addition to objects and components, we can declare aliased parameters. This
allows us to get access to those parameters in the body of a subprogram. We do
this by using the :ada:`aliased` keyword before the parameter mode.

The parameter mode indicates which type we must use for the access type, as
we'll discuss soon:

+------------------------+--------------------+
| Parameter mode         | Type               |
+========================+====================+
| :ada:`aliased in`      | Access-to-constant |
+------------------------+--------------------+
| :ada:`aliased out`     | Access-to-variable |
+------------------------+--------------------+
| :ada:`aliased in out`  | Access-to-variable |
+------------------------+--------------------+

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Aliased_Rec_Component

    package Data_Processing is

       procedure Proc (I : aliased in out Integer);

    end Data_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Data_Processing is

       procedure Show (I : aliased Integer) is
          --               ^ equivalent to
          --                 "aliased in Integer"

          type Integer_Constant_Access is access constant Integer;

          A : constant Integer_Constant_Access := I'Access;
       begin
          Put_Line ("Value : I " & Integer'Image (A.all));
       end Show;

       procedure Set_One (I : aliased out Integer) is

          type Integer_Access is access all Integer;

          procedure Local_Set_One (A : Integer_Access) is
          begin
             A.all := 1;
          end Local_Set_One;

       begin
          Local_Set_One (I'Access);
       end Set_One;

       procedure Proc (I : aliased in out Integer) is

          type Integer_Access is access all Integer;

          procedure Add_One (A : Integer_Access) is
          begin
             A.all := A.all + 1;
          end Add_One;

       begin
          Show (I);
          Add_One (I'Access);
          Show (I);
       end Proc;

    end Data_Processing;

    with Data_Processing; use Data_Processing;

    procedure Show_Aliased_Param is
       I : aliased Integer := 22;
    begin
       Proc (I);
    end Show_Aliased_Param;

Here, :ada:`Proc` has an :ada:`aliased in out` parameter. In :ada:`Proc` \'s
body, we declare the :ada:`Integer_Access` type as an :ada:`access all` type.
We use the same approach in body of the :ada:`Set_One` procedure, which has an
:ada:`aliased out` parameter. Finally, the :ada:`Show` procedure has
an :ada:`aliased in` parameter. Therefore, we declare the
:ada:`Integer_Constant_Access` as an :ada:`access constant` type.

Note that parameter aliasing has an influence on how arguments are passed to a
subprogram when the parameter is of scalar type. When a scalar parameter is
declared as aliased, the corresponding argument is passed by reference.
For example, if we had declared :ada:`procedure Show (I : Integer)`, the
argument for :ada:`I` would be passed by value. However, since we're declaring
it as :ada:`aliased Integer`, it is passed by reference.

.. admonition:: In the Ada Reference Manual

    - `6.1 Subprogram Declarations <http://www.ada-auth.org/standards/12rm/html/RM-6-1.html>`_
    - `6.2 Formal Parameter Modes <http://www.ada-auth.org/standards/12rm/html/RM-6-2.html>`_
    - `6.4.1 Parameter Associations <http://www.ada-auth.org/standards/12rm/html/RM-6-4-1.html>`_


Dereferencing
-------------

.. todo::

    Complete section!


Implicit Dereferencing
----------------------

.. admonition:: Relevant topics

    - `User-Defined References <http://www.ada-auth.org/standards/2xrm/html/RM-4-1-5.html>`_

.. todo::

    Complete section!


Anonymous Access Types
----------------------

.. admonition:: In the Ada Reference Manual

    - `3.10 Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10.html>`_

.. todo::

    Complete section!


Accessibility Levels: An Introduction
-------------------------------------

.. admonition:: In the Ada Reference Manual

    - `3.10.2 Operations of Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10-2.html>`_

.. todo::

    Complete section!


Unchecked Access
----------------

.. admonition:: Relevant topics

    - `Unchecked Access Value Creation <http://www.ada-auth.org/standards/2xrm/html/RM-13-10.html>`_

.. todo::

    Complete section!


Unchecked Deallocation
----------------------

.. admonition:: Relevant topics

    - `Unchecked Storage Deallocation <http://www.ada-auth.org/standards/2xrm/html/RM-13-11-2.html>`_

.. todo::

    Complete section!


Null & Not Null Access
----------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #23: Null Considered Harmful <https://www.adacore.com/gems/ada-gem-23>`_
    and `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`_.

Ada, like many languages, defines a special :ada:`null` value for access
types. All values of an access type designate some object of the
designated type, except for :ada:`null`, which does not designate any
object. The null value can be used as a special flag. For example, a
singly-linked list can be null-terminated. A :ada:`Lookup` function can
return :ada:`null` to mean "not found", presuming the result is of an
access type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Return
    :class: ada-syntax-only

    package Show_Null_Return is

       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.
    end Show_Null_Return;

An alternative design for :ada:`Lookup` would be to raise an exception:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Not_Found_Exception
    :class: ada-syntax-only

    package Show_Not_Found_Exception is
       Not_Found : exception;

       function Lookup (T : Table) return Ref_Element;
       --  Raises Not_Found if not found.
       --  Never returns null.
    end Show_Not_Found_Exception;

Neither design is better in all situations; it depends in part on whether
we consider the "not found" situation to be exceptional.

Clearly, the client calling :ada:`Lookup` needs to know whether it can
return :ada:`null`, and if so, what that means. In general, it's a good
idea to document whether things can be null or not, especially for formal
parameters and function results. Prior to Ada 2005, we would do that with
comments. Since Ada 2005, we can use the :ada:`not null` syntax:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Not_Null_Return
    :class: ada-syntax-only

    package Show_Not_Null_Return is
       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return not null Ref_Element;
       --  Possible since Ada 2005.
    end Show_Not_Null_Return;

This is a complete package for the code snippets above:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Complete_Null_Return

    package Example is

       type Element is limited private;
       type Ref_Element is access all Element;

       type Table is limited private;

       Not_Found : constant Ref_Element := null;
       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.

       Not_Found_2 : exception;
       function Lookup_2 (T : Table) return not null Ref_Element;
       --  Raises Not_Found_2 if not found.

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    private
       type Element is limited
          record
             Component : Integer;
          end record;
       type Table is limited null record;
    end Example;

    package body Example is

       An_Element : aliased Element;

       function Lookup (T : Table) return Ref_Element is
          pragma Unreferenced (T);
       begin
          --  ...
          return Not_Found;
       end Lookup;

       function Lookup_2 (T : Table) return not null Ref_Element is
       begin
          --  ...
          raise Not_Found_2;

          return An_Element'Access;
          --  suppress error: 'missing "return" statement in function body'
       end Lookup_2;

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

       procedure R is
       begin
          Q (An_Element'Access);
       end R;

      pragma Unreferenced (R);

    end Example;

In general, it's better to use the language proper for documentation, when
possible, rather than comments, because compile-time and/or run-time
checks can help ensure that the "documentation" is actually true. With
comments, there's a greater danger that the comment will become false
during maintenance, and false documentation is obviously a menace.

In many, perhaps most cases, :ada:`null` is just a tripping hazard. It's
a good idea to put in :ada:`not null` when possible. In fact, a good
argument can be made that :ada:`not null` should be the default, with
extra syntax required when :ada:`null` is wanted. This is the way
`Standard ML <https://en.wikipedia.org/wiki/Standard_ML>`_ works, for
example |mdash| you don't get any special null-like value unless you ask
for it. Of course, because Ada 2005 needs to be compatible with previous
versions of the language, :ada:`not null` cannot be the default for Ada.

One word of caution: access objects are default-initialized to
:ada:`null`, so if you have a :ada:`not null` object (or component) you
had better initialize it explicitly, or you will get
:ada:`Constraint_Error`. :ada:`not null` is more often useful on
parameters and function results, for this reason.

Another advantage of :ada:`not null` over comments is for efficiency.
Consider procedures :ada:`P` and :ada:`Q` in this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Complete_Null_Return

    package Example.Processing is

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    end Example.Processing;

    package body Example.Processing is

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

    end Example.Processing;

Without :ada:`not null`, the generated code for :ada:`P` will do a check
that :ada:`X /= null`, which may be costly on some systems. :ada:`P` is
called in a loop, so this check will likely occur many times. With
:ada:`not null`, the check is pushed to the call site. Pushing checks to
the call site is usually beneficial because

    1. the check might be hoisted out of a loop by the optimizer, or

    2. the check might be eliminated altogether, as in the example
       above, where the compiler knows that :ada:`An_Element'Access` cannot
       be :ada:`null`.

This is analogous to the situation with other run-time checks, such as
array bounds checks:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Process_Array

    package Show_Process_Array is

       type My_Index is range 1 .. 10;
       type My_Array is array (My_Index) of Integer;

       procedure Process_Array (X : in out My_Array; Index : My_Index);

    end Show_Process_Array;

    package body Show_Process_Array is

       procedure Process_Array (X : in out My_Array; Index : My_Index) is
       begin
          X (Index) := X (Index) + 1;
       end Process_Array;

    end Show_Process_Array;

If :ada:`X (Index)` occurs inside :ada:`Process_Array`, there is no need
to check that :ada:`Index` is in range, because the check is pushed to the
caller.


Access to subprograms
---------------------

Static vs. dynamic calls
~~~~~~~~~~~~~~~~~~~~~~~~

In a typical subprogram call, we indicate the subprogram we want to call
statically. For example, let's say we've implemented a procedure :ada:`Proc`
that calls a procedure :ada:`P`:

.. code:: ada compile_button main=proc.adb project=Courses.Advanced_Ada.Access_Types.Subprogram_Call

   procedure P (I : in out Integer);

   procedure P (I : in out Integer) is
   begin
      null;
   end P;

   with P;

   procedure Proc is
      I : Integer := 0;
   begin
      P (I);
   end Proc;

The call to :ada:`P` is statically dispatched: every time :ada:`Proc` runs and
calls :ada:`P`, that call is always to the same procedure. In other words, we
can determine at compilation time which procedure is called.

In contrast, an access to a subprogram allows us to dynamically indicate which
subprogram we want to call. For example, if we change :ada:`Proc` in the code
above to receive the access to a subprogram :ada:`P` as a parameter, the actual
procedure that would be called when running :ada:`Proc` would be determined at
run time, and it might be different for every call to :ada:`Proc`. In this
case, we wouldn't be able to determine at compilation time which
procedure would be called in every case. (In some cases, however, it could
still be possible to determine which procedure is called by analyzing the
argument that is passed to :ada:`Proc`.)


Access to subprogram declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We declare an access to a subprogram as a type by writing
:ada:`access procedure` or :ada:`access function` and the corresponding
prototype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Types

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

Note that, in the type declarations, we list all the parameters that we expect
in the subprogram.

We can use those types to declare access to subprograms |mdash| as subprogram
parameters, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Types

    with Access_To_Subprogram_Types; use Access_To_Subprogram_Types;

    package Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure);

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure) is
          I : Integer := 0;
       begin
          P (I);
          --  P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

In the implementation of the :ada:`Proc` procedure of the code example, we call
the :ada:`P` procedure by simply passing :ada:`I` as a parameter. In this case,
:ada:`P` is automatically dereferenced. We may, however, explicitly dereference
:ada:`P` by writing :ada:`P.all (I)`.

In addition, we can declare those subprogram parameters using anonymous types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Params

    package Access_To_Subprogram_Params is

       procedure Proc (P : access procedure (I : in out Integer));

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc (P : access procedure (I : in out Integer)) is
          I : Integer := 0;
       begin
          --  P (I);
          P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

Finally, we can get access to a subprogram using the :ada:`Access` attribute:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Params

    procedure Add_Ten (I : in out Integer);

    procedure Add_Ten (I : in out Integer) is
    begin
       I := I + 10;
    end Add_Ten;

    with Access_To_Subprogram_Params;
    use  Access_To_Subprogram_Params;

    with Add_Ten;

    procedure Show_Access_To_Subprograms is
    begin
       Proc (Add_Ten'Access);
       --            ^ Getting access to Add_Ten procedure
       --              and passing it to Proc
    end Show_Access_To_Subprograms;

Here, we get access to the :ada:`Add_Ten` procedure and pass it to the
:ada:`Proc` procedure.

.. admonition:: In the Ada Reference Manual

    - `3.10 Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10.html>`_


Selecting subprograms
~~~~~~~~~~~~~~~~~~~~~

A practical application of access to subprograms is that it enables us to
dynamically select a subprogram and pass it to another subprogram, where it can
then be called.

For example, we may have a :ada:`Process` procedure that receives a logging
procedure as a parameter (:ada:`Log_Proc`). Also, this parameter may be
:ada:`null` by default |mdash| so that no procedure is called if the parameter
isn't specified:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    package Data_Processing is

       type Data_Container is array (Positive range <>) of Float;

       type Log_Procedure is access procedure (D : Data_Container);

       procedure Process (D        : in out Data_Container;
                          Log_Proc :        Log_Procedure := null);

    end Data_Processing;

    package body Data_Processing is

       procedure Process (D        : in out Data_Container;
                          Log_Proc :        Log_Procedure := null) is
       begin
          --  missing processing part...

          if Log_Proc /= null then
             Log_Proc (D);
          end if;
       end Process;

    end Data_Processing;

In the implementation of :ada:`Process`, we check whether :ada:`Log_Proc` is
null or not. (If it's not null, we call the procedure. Otherwise, we just skip
the call.)

Now, let's implement two logging procedures that match the expected form of
the :ada:`Log_Procedure` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Csv (D : Data_Container) is
    begin
       for I in D'First .. D'Last - 1 loop
          Put (D (I)'Image & ", ");
       end loop;
       Put (D (D'Last)'Image);
       New_Line;
    end Log_Csv;

Finally, we implement a test application that selects each of the logging
procedures that we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Log_Element_Per_Line;
    with Log_Csv;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Log_Element_Per_Line ====");
       Process (D, Log_Element_Per_Line'Access);

       Put_Line ("==== Log_Csv ====");
       Process (D, Log_Csv'Access);

       Put_Line ("==== None ====");
       Process (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Log_Element_Per_Line` and :ada:`Log_Csv` procedures. Also, in the third
call, we don't pass any access as an argument, which is then :ada:`null` by
default.


Null exclusion
~~~~~~~~~~~~~~

We can use null exclusion when declaring an access to subprograms. By doing so,
we ensure that a subprogram must be specified |mdash| either as a parameter or
when initializing an access object. Otherwise, an exception is raised. Let's
adapt the previous example and introduce the :ada:`Init_Function` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    package Data_Processing is

       type Data_Container is array (Positive range <>) of Float;

       type Init_Function is not null access function return Float;

       procedure Process (D         : in out Data_Container;
                          Init_Func :        Init_Function);

    end Data_Processing;

    package body Data_Processing is

       procedure Process (D         : in out Data_Container;
                          Init_Func :        Init_Function) is
       begin
          for I in D'Range loop
             D (I) := Init_Func.all;
          end loop;
       end Process;

    end Data_Processing;


In this case, we specify that :ada:`Init_Function` is :ada:`not null access`
because we want to always be able to call this function in the :ada:`Process`
procedure (i.e. without raising an exception).

When an access to a subprogram doesn't have parameters |mdash| which is the
case for the subprograms of :ada:`Init_Function` type |mdash| we need to
explicitly dereference it by writing :ada:`.all`. (In this case, :ada:`.all`
isn't optional.) Therefore, we have to write :ada:`Init_Func.all` in the
implementation of the :ada:`Process` procedure of the code example.

Now, let's declare two simple functions |mdash| :ada:`Init_Zero` and
:ada:`Init_One` |mdash| that return 0.0 and 1.0, respectively:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    function Init_Zero return Float;
    function Init_One return Float;

    function Init_Zero return Float is
    begin
       return 0.0;
    end Init_Zero;

    function Init_One return Float is
    begin
       return 1.0;
    end Init_One;

Finally, let's see a test application where we select each of the init
functions we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Init_Zero;
    with Init_One;

    with Log_Element_Per_Line;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Init_Zero ====");
       Process (D, Init_Zero'Access);
       Log_Element_Per_Line (D);

       Put_Line ("==== Init_One ====");
       Process (D, Init_One'Access);
       Log_Element_Per_Line (D);

       --  Put_Line ("==== None ====");
       --  Process (D, null);
       --  Log_Element_Per_Line (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Init_Zero` and :ada:`Init_One` functions. Also, if we uncomment the call
to :ada:`Process` with :ada:`null` as an argument for the init function, we see
that the :ada:`Constraint_Error` exception is raised at run time |mdash| as the
argument cannot be :ada:`null` due to the null exclusion.

.. admonition:: For further reading...

    .. note::

        This example was originally written by Robert A. Duff and was part of
        the `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`_.

    Here's another example, first with :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           procedure Iterate
             (T      : Table;
              Action : access procedure (X : not null Ref_Element)
              := null);
           --  If Action is null, do nothing.

        end Show_Null_Procedure;

    and without :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           procedure Do_Nothing (X : not null Ref_Element) is null;

           procedure Iterate
             (T      : Table;
              Action : not null access procedure (X : not null Ref_Element)
              := Do_Nothing'Access);

        end Show_Null_Procedure;

    The style of the second :ada:`Iterate` is clearly better because it makes
    use of the syntax to indicate that a procedure is expected. This is a
    complete package that includes both versions of the :ada:`Iterate`
    procedure:

    .. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Complete_Not_Null_Procedure

        package Example is

           type Element is limited private;
           type Ref_Element is access all Element;

           type Table is limited private;

           procedure Iterate
             (T : Table;
              Action : access procedure (X : not null Ref_Element)
                                          := null);
           --  If Action is null, do nothing.

           procedure Do_Nothing (X : not null Ref_Element) is null;
           procedure Iterate_2
             (T : Table;
              Action : not null access procedure (X : not null Ref_Element)
                                              := Do_Nothing'Access);

        private
           type Element is limited
              record
                 Component : Integer;
              end record;
           type Table is limited null record;
        end Example;

        package body Example is

           An_Element : aliased Element;

           procedure Iterate
             (T : Table;
              Action : access procedure (X : not null Ref_Element)
                                              := null) is
           begin
              if Action /= null then
                 Action (An_Element'Access);
                 --  In a real program, this would do something more sensible.
              end if;
           end Iterate;

           procedure Iterate_2
             (T : Table;
              Action : not null access procedure (X : not null Ref_Element)
                                              := Do_Nothing'Access) is
           begin
              Action (An_Element'Access);
              --  In a real program, this would do something more sensible.
           end Iterate_2;

        end Example;

        with Example; use Example;

        procedure Show_Example is
           T : Table;
        begin
           Iterate_2 (T);
        end Show_Example;

    The :ada:`not null access procedure` is quite a mouthful, but it's
    worthwhile, and anyway, as mentioned earlier, the compatibility
    requirement requires that the :ada:`not null` be explicit, rather than the
    other way around.


Access to protected subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Up to this point, we've discussed access to *normal* Ada subprograms. In some
situations, however, we might want to have access to protected subprograms.
To do this, we can simply declare a type using :ada:`access protected`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Protected_Access

    package Simple_Protected_Access is

       type Access_Proc is
         access protected procedure;

       protected Obj is

          procedure Do_Something;

       end Obj;

       Acc : Access_Proc := Obj.Do_Something'Access;

    end Simple_Protected_Access;

    package body Simple_Protected_Access is

       protected body Obj is

          procedure Do_Something is
          begin
             --  Not doing anything
             --  for the moment...
             null;
          end Do_Something;

       end Obj;

    end Simple_Protected_Access;

Here, we declare the :ada:`Access_Proc` type as an access type to protected
procedures. Then, we declare the variable :ada:`Acc` and assign to it the
access to the :ada:`Do_Something` procedure (of the protected object
:ada:`Obj`).

Now, let's discuss a more useful example: a simple system that allows us to
register protected procedures and execute them. This is implemented in
:ada:`Work_Registry` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    package Work_Registry is

       type Work_Id is tagged limited private;

       type Work_Handler is
         access protected procedure (T : Work_Id);

       subtype Valid_Work_Handler is
         not null Work_Handler;

       type Work_Handlers is array (Positive range <>) of Work_Handler;

       protected type Work_Handler_Registry (Last : Positive) is

          procedure Register (T : Valid_Work_Handler);

          procedure Reset;

          procedure Process_All;

       private

          D    : Work_Handlers (1 .. Last);
          Curr : Natural := 0;

       end Work_Handler_Registry;

    private

       type Work_Id is tagged limited null record;

    end Work_Registry;

    package body Work_Registry is

       protected body Work_Handler_Registry is

          procedure Register (T : Valid_Work_Handler) is
          begin
             if Curr < Last then
                Curr := Curr + 1;
                D (Curr) := T;
             end if;
          end Register;

          procedure Reset is
          begin
             Curr := 0;
          end Reset;

          procedure Process_All is
             Dummy_ID : Work_Id;
          begin
             for I in D'First .. Curr loop
                D (I).all (Dummy_ID);
             end loop;
          end Process_All;

       end Work_Handler_Registry;

    end Work_Registry;

Here, we declare the protected :ada:`Work_Handler_Registry` type with the
following subprograms:

- :ada:`Register`, which we can use to register a protected procedure;

- :ada:`Reset`, which we can use to reset the system; and

- :ada:`Process_All`, which we can use to call all procedures that were
  registered in the system.

:ada:`Work_Handler` is our access to protected subprogram type. Also, we
declare the :ada:`Valid_Work_Handler` subtype, which excludes :ada:`null`. By
doing so, we can ensure that only valid procedures are passed to the
:ada:`Register` procedure. In the protected :ada:`Work_Handler_Registry` type,
we store the procedures in an array (of :ada:`Work_Handlers` type).

.. admonition:: Important

    Note that, in the type declaration :ada:`Work_Handler`, we say that the
    protected procedure must have a parameter of :ada:`Work_Id` type. In this
    example, this parameter is just used to *bind* the procedure to the
    :ada:`Work_Handler_Registry` type. The :ada:`Work_Id` type itself is
    actually declared as a null record (in the private part of the package),
    and it isn't really useful on its own.

    If we had declared :ada:`type Work_Handler is access protected procedure;`
    instead, we would be able to register *any* protected procedure into the
    system, even the ones that might not be suitable for the system. By using
    a parameter of :ada:`Work_Id` type, however, we make use of strong
    typing to ensure that only procedures that were designed for the system
    can be registered.

In the next part of the code, we declare the :ada:`Integer_Storage` type,
which is a simple protected type that we use to store an integer value:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    with Work_Registry;

    package Integer_Storage_System is

       protected type Integer_Storage is

          procedure Set (V : Integer);

          procedure Show (T : Work_Registry.Work_Id);

       private

          I : Integer := 0;

       end Integer_Storage;

       type Integer_Storage_Access is access Integer_Storage;

       type Integer_Storage_Array is
         array (Positive range <>) of Integer_Storage_Access;

    end Integer_Storage_System;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Storage_System is

       protected body Integer_Storage is

          procedure Set (V : Integer) is
          begin
             I := V;
          end Set;

          procedure Show (T : Work_Registry.Work_Id) is
             pragma Unreferenced (T);
          begin
             Put_Line ("Value: " & Integer'Image (I));
          end Show;

       end Integer_Storage;

    end Integer_Storage_System;

For the :ada:`Integer_Storage` type, we declare two procedures:

- :ada:`Set`, which we use to assign a value to the (protected) integer value;
  and

- :ada:`Show`, which we use to show the integer value that is stored in the
  protected object.

The :ada:`Show` procedure has a parameter of :ada:`Work_Id` type, which
indicates that this procedure was designed to be registered in the system of
:ada:`Work_Handler_Registry` type.

Finally, we have a test application in which we declare a registry (:ada:`WHR`)
and an array of "protected integer objects" (:ada:`Int_Stor`):

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    with Work_Registry;          use Work_Registry;
    with Integer_Storage_System; use Integer_Storage_System;

    procedure Show_Access_To_Protected_Subprograms is

       WHR      : Work_Handler_Registry (5);
       Int_Stor : Integer_Storage_Array (1 .. 3);

    begin
       --  Allocate and initialize integer storage
       --
       --  (For the initialization, we're just assigning
       --   the index here, but we could really have used
       --   any integer value.)

       for I in Int_Stor'Range loop
          Int_Stor (I) := new Integer_Storage;
          Int_Stor (I).Set (I);
       end loop;

       --  Register handlers

       for I in Int_Stor'Range loop
          WHR.Register (Int_Stor (I).all.Show'Access);
       end loop;

       --  Now, use Process_All to call the handlers
       --  (in this case, the Show procedure for
       --  each protected object from Int_Stor).

       WHR.Process_All;

    end Show_Access_To_Protected_Subprograms;

The work handler registry (:ada:`WHR`) has a maximum capacity of five
procedures, whereas the :ada:`Int_Stor` array has a capacity of three elements.
By calling :ada:`WHR.Register` and passing :ada:`Int_Stor (I).all.Show'Access`,
we register the :ada:`Show` procedure of each protected object from
:ada:`Int_Stor`.

.. admonition:: Important

    Note that the components of the :ada:`Int_Stor` array are of
    :ada:`Integer_Storage_Access` type, which is declared as an access to
    :ada:`Integer_Storage` objects. Therefore, we have to dereference the
    object (by writing :ada:`Int_Stor (I).all`) before getting access to the
    :ada:`Show` procedure (by writing :ada:`.Show'Access`).

    We have to use an access type here because we cannot pass the access (to
    the :ada:`Show` procedure) of a local object in the call to the
    :ada:`Register` procedure. Therefore, the protected objects (of
    :ada:`Integer_Storage` type) cannot be local.

    This issue becomes evident if we replace the declaration of
    :ada:`Int_Stor` with a local array (and then adapt the remaining code). If
    we do this, we get a compilation error in the call to :ada:`Register`:

    .. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function
        :class: ada-expect-compile-error

        with Work_Registry;          use Work_Registry;
        with Integer_Storage_System; use Integer_Storage_System;

        procedure Show_Access_To_Protected_Subprograms is

           WHR      : Work_Handler_Registry (5);

           Int_Stor : array (1 .. 3) of Integer_Storage;

        begin
           --  Allocate and initialize integer storage
           --
           --  (For the initialization, we're just assigning
           --   the index here, but we could really have used
           --   any integer value.)

           for I in Int_Stor'Range loop
              --  Int_Stor (I) := new Integer_Storage;
              Int_Stor (I).Set (I);
           end loop;

           --  Register handlers

           for I in Int_Stor'Range loop
              WHR.Register (Int_Stor (I).Show'Access);
              --            ^ ERROR!
           end loop;

           --  Now, call the handlers
           --  (i.e. the Show procedure of each
           --   protected object).

           WHR.Process_All;

        end Show_Access_To_Protected_Subprograms;

    As we've just discussed, this error is due to the fact that
    :ada:`Int_Stor` is now a "local" protected object, and the accessibility
    rules don't allow mixing it with non-local accesses in order to prevent the
    possibility of dangling references.

When we call :ada:`WHR.Process_All`, the registry system calls each procedure
that has been registered with the system. When looking at the values displayed
by the test application, we may notice that each call to :ada:`Show` is
referring to a different protected object. In fact, even though we're passing
just the access to a protected *procedure* in the call to :ada:`Register`, that
access is also associated to a specific protected object. (This is different
from access to non-protected subprograms we've discussed previously: in that
case, there's no object associated.) If we replace the argument to
:ada:`Register` by :ada:`Int_Stor (2).all.Show'Access`, for example, the three
:ada:`Show` procedures registered in the system will now refer to the same
protected object (stored at :ada:`Int_Stor (2)`).

Also, even though we have registered the same procedure (:ada:`Show`) of the
same type (:ada:`Integer_Storage`) in all calls to :ada:`Register`, we could
have used a different protected procedure |mdash| and of a different protected
type. As an exercise, we could, for example, create a new type called
:ada:`Float_Storage` (based on the code that we used for the
:ada:`Integer_Storage` type) and register some objects of :ada:`Float_Storage`
type into the system (with a couple of additional calls to :ada:`Register`). If
we then call :ada:`WHR.Process_All`, we'd see that the system is able to cope
with objects of both :ada:`Integer_Storage` and :ada:`Float_Storage` types. In
fact, the system implemented with the :ada:`Work_Handler_Registry` can be seen
as "type agnostic," as it doesn't care about which type the protected objects
have |mdash| as long as the subprograms we want to register are conformant to
the :ada:`Valid_Work_Handler` type.


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Accessibility checks
    --------------------

    .. note::

        This section was originally written by Ramón Fernández and Robert A. Duff, and
        published as
        `Gem #33: Accessibility Checks <https://www.adacore.com/gems/gem-33>`_,
        `Gem #41 <https://www.adacore.com/gems/gem-41>`_ and
        `Gem #44: <https://www.adacore.com/gems/gem-44>`_.
