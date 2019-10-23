More about types
================

:code-config:`reset_accumulator=True;accumulate_code=False`

.. _Aggregates:

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

.. sectionauthor:: Raphaël Amiard

Aggregates: A primer
--------------------

So far, we have talked about aggregates quite a bit and have seen a number of
examples.  Now we will revisit this feature in some more detail.

An Ada aggregate is, in effect, a literal value for a composite type. It's a
very powerful notation that helps you to avoid writing procedural code for the
initialization of your data structures in many cases.

A basic rule when writing aggregates is that *every component* of the array or
record has to be specified, even components that have a default value.

This means that the following code is incorrect:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Incorrect_Aggregate
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

There are a few shortcuts that you can use to make the notation more
convenient:

- To specify the default value for a component, you can use the
  :ada:`<>` notation.

- You can use the :ada:`|` symbol to give several components the same value.

- You can use the :ada:`others` choice to refer to every component that has not
  yet been specified, provided all those fields have the same type.

- You can use the range notation :ada:`..` to refer to specify a contiguous
  sequence of indices in an array.

However, note that as soon as you used a named association, all subsequent
components likewise need to be specified with names associations.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Points

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is array (Positive range <>) of Point;

       Origin   : Point := (X | Y => <>);   -- use the default values
       Origin_2 : Point := (others => <>);  -- likewise use the defaults

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1 => (1, 2), 2 => (3, 4), 3 .. 20 => <>);
    end Points;

Overloading and qualified expressions
-------------------------------------

Ada has a general concept of name overloading, which we saw earlier
in the section on :ref:`enumeration types <EnumTypes>`.

Let's take a simple example: it is possible in Ada to have functions that have
the same name, but different types for their parameters.

.. code:: ada no_button

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

This is a common concept in programming languages, called
`overloading <https://en.m.wikipedia.org/wiki/Function_overloading>`_, or name
overloading.

One of the novel aspects of Ada's overloading facility is the ability to
resolve overloading based on the return type of a function.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Valid, will choose the proper Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    Note that overload resolution based on the type is allowed for both
    functions and enumeration literals in Ada - which is why you can have
    multiple enumeration literals with the same name. Semantically, an
    enumeration literal is treated like a function that has no parameters.

However, sometimes an ambiguity makes it impossible to resolve which
declaration of an overloaded name a given occurrence of the name refers to.
This is where a qualified expression becomes useful.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading_Error
    :class: ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
       function Convert (Self : Integer) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Invalid, which convert should we call?

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ We specify that the type of the expression is
       --                       SSID.

       --  We could also have declared a temporary

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Syntactically the target of a qualified expression can be either any expression
in parentheses, or an aggregate:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Qual_Expr

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

This illustrates that qualified expressions are a convenient (and sometimes
necessary) way for the programmer to make the type of an expression explicit,
for the compiler of course, but also for other programmers.

.. attention::
    While they look and feel similar, type conversions and qualified
    expressions are *not* the same.

    A qualified expression specifies the exact type that the target expression
    will be resolved to, whereas a type conversion will try to convert the
    target and issue a run-time error if the target value cannot be so
    converted.

    Note that you can use a qualified expression to convert from one subtype to
    another, with an exception raised if a constraint is violated.

    .. code-block:: ada

        X : Integer := Natural'(1);

Access types (pointers)
-----------------------

Pointers are a potentially dangerous construct, which conflicts with Ada's
underlying philosophy.

There are two ways in which Ada helps shield programmers from the dangers of
pointers:

1. One approach, which we have already seen, is to provide alternative features
   so that the programmer does not need to use pointers. Parameter modes,
   arrays, and varying size types are all constructs that can replace typical
   pointer usages in C.

2. Second, Ada has made pointers as safe and restricted as possible, but allows
   "escape hatches" when the programmer explicitly requests them and presumably
   will be exercising such features with appropriate care.

.. TODO: Add paragraph and link below when advanced course is ready

..
   This course covers the basics of Ada pointers, which are known as "access
   values". There are generally better ways than to resort to the advanced
   features directly but if you need to use features that are potentially unsafe,
   you can learn more about those unsafe features
   ACCESS_TYPES_ADVANCED_LINK.

Here is how you declare a simple pointer type, or access type, in Ada:

:code-config:`reset_accumulator=True;accumulate_code=True`

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    package Dates is
       type Month_Type is (January, February, March, April, May, June, July,
                           August, September, October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Month_Type;
          Year  : Integer;
       end record;
    end Dates;

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc is access Date;
        --                      ^ "Designated type"
        --                      ^ Date_Acc values point to Date objects

        D : Date_Acc := null;
        --              ^ Literal for "access to nothing"
        --  ^ Access to date
    end Access_Types;

This illustrates how to:

- Declare an access type whose values point to ("designate") objects from a
  specific type
- Declare a variable (access value) from this access type
- Give it a value of :ada:`null`

In line with Ada's strong typing philosophy, if you declare a second access
type whose designated type is Date, the two access types will be incompatible
with each other, and you will need an explicit type conversion to convert from
one to the other:

.. code:: ada no_button
    :class: ada-expect-compile-error

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc   is access Date;
        type Date_Acc_2 is access Date;

        D  : Date_Acc   := null;
        D2 : Date_Acc_2 := D;
        --                 ^ Invalid! Different types

        D3 : Date_Acc_2 := Date_Acc_2 (D);
        --                 ^ Valid with type conversion
    end Access_Types;

.. admonition:: In other languages

    In most other languages, pointer types are structurally, not nominally
    typed, like they are in Ada, which means that two pointer types will be the
    same as long as they share the same target type and accessibility rules.

    Not so in Ada, which takes some time getting used to. A seemingly simple
    problem is, if you want to have a canonical access to a type, where should
    it be declared? A commonly used pattern is that if you need an access type
    to a specific type you "own", you will declare it along with the type:

    .. code-block:: ada

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Allocation (by type)
~~~~~~~~~~~~~~~~~~~~

Once we have declared an access type, we need a way to give variables of the
types a meaningful value! You can allocate a value of an access type
with the :ada:`new` keyword in Ada.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Allocate a new Date record
    end Access_Types;

If the type you want to allocate needs constraints, you can put them in the
subtype indication, just as you would do in a variable declaration:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type String_Acc is access String;
       --                        ^ Access to unconstrained array type
       Msg : String_Acc;
       --    ^ Default value is null

       Buffer : String_Acc := new String (1 .. 10);
       --                                ^ Constraint required
    end Access_Types;

In some cases, though, allocating just by specifiying the type is not ideal, so
Ada also allows you to initialize along with the allocation. This is done via
the qualified expression syntax:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc   := new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;

Dereferencing
~~~~~~~~~~~~~

The last important piece of Ada's access type facility is how to get from an
access value to the object that is pointed to, that is, how to dereference the
pointer. Dereferencing a pointer uses the :ada:`.all` syntax in Ada, but is
often not needed - in many cases, the access value will be implicitly
dereferenced for you:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;

       D     : Date_Acc := new Date'(30, November, 2011);

       Today : Date := D.all;
       --              ^ Access value dereference
       J     : Integer := D.Day;
       --                 ^ Implicit dereference for record and array components
       --                 Equivalent to D.all.day
    end Access_Types;

:code-config:`reset_accumulator=True;accumulate_code=False`

Other features
~~~~~~~~~~~~~~

As you might know if you have used pointers in C or C++, we are still missing
features that are considered fundamental to the use of pointers, such as:

- Pointer arithmetic (being able to increment or decrement a pointer in order
  to point to the next or previous object)

- Manual deallocation - what is called ``free`` or ``delete`` in C. This is
  a potentially unsafe operation. To keep within the realm of safe
  Ada, you need to never deallocate manually.

Those features exist in Ada, but are only available through specific standard
library APIs.

.. TODO: Add paragraph and link below when advanced course is ready

..
   You can read more about those in the
   advanced course on memory management ACCESS_TYPES_ADVANCED_LINK.

.. attention::

    The guideline in Ada is that most of the time you can avoid manual
    allocation, and you should.

    There are many ways to avoid manual allocation, some of which have been
    covered (such as parameter modes). The language also provides library
    abstractions to avoid pointers:

    1. One is the use of :ref:`containers <Containers>`. Containers help users
       avoid pointers, because container memory is automatically managed.

    2. A container to note in this context is the
       `Indefinite holder <http://www.ada-auth.org/standards/12rat/html/Rat12-8-5.html>`_.
       This container allows you to store a value of an indefinite type such as
       String.

    3. GNATCOLL has a library for smart pointers, called
       `Refcount <https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-refcount.ads>`_
       Those pointers' memory is automatically managed, so that when an
       allocated object has no more references to it, the memory is
       automatically deallocated.

Mutually recursive types
------------------------

The linked list is a common idiom in data structures; in Ada this would be most
naturally defined through two types, a record type and an access type, that are
mutually dependent.  To declare mutually dependent types, you can use an
incomplete type declaration:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Simple_List

    package Simple_List is
       type Node;
       --  This is an incomplete type declaration, which is
       --  completed in the same declarative region.

       type Node_Acc is access Node;

       type Node is record
          Content    : Natural;
          Prev, Next : Node_Acc;
       end record;
    end Simple_List;

More about records
------------------

Dynamically sized record types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have previously seen some simple examples of record types.  Let's now look
at some of the more advanced properties of this fundamental language feature.

One point to note is that object size for a record type does not need to be
known at compile time. This is illustrated in the example below:

.. ?? The example code may have elaboration order problems unless
.. ?? an elaboration pragma is used.
.. ?? Consider simplfying or restructuring the example to avoid this issue

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record

    package Runtime_Length is
       function Compute_Max_Len return Natural;
    end Runtime_Length;

    with Runtime_Length; use Runtime_Length;

    package Var_Size_Record is
        Max_Len : constant Natural := Compute_Max_Len;
        --                            ^ Not known at compile time

        type Items_Array is array (Positive range <>) of Integer;

        type Growable_Stack is record
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural;
        end record;
        --  Growable_Stack is a definite type, but size is not known at compile
        --  time.

        G : Growable_Stack;
    end Var_Size_Record;

It is completely fine to determine the size of your records at run time, but
note that all objects of this type will have the same size.

Records with discriminant
~~~~~~~~~~~~~~~~~~~~~~~~~

In the example above, the size of the Items field is determined once, at
run-time, but every ``Growable_Stack`` instance will be exactly the same size.
But maybe that's not what you want to do. We saw that arrays in general offer
this flexibility: for an unconstrained array type, different objects can have
different sizes.

You can get analogous functionality for records, too, using a special kind of
field that is called a discriminant:

:code-config:`reset_accumulator=True;accumulate_code=True`

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record_2

    package Var_Size_Record_2 is
        type Items_Array is array (Positive range <>) of Integer;

        type Growable_Stack (Max_Len : Natural) is record
        --                   ^ Discriminant. Cannot be modified once initialized.
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural := 0;
        end record;
        --  Growable_Stack is an indefinite type (like an array)
    end Var_Size_Record_2;

Discriminants, in their simple forms, are constant: You cannot modify them once
you have initialized the object. This intuitively makes sense since they
determine the size of the object.

Also, they make a type indefinite: Whether or not the discriminant is used to
specify the size of an object, a type with a discriminant will be indefinite if
the discriminant is not declared with an initialization:

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Test_Discriminants
    :class: ada-expect-compile-error

    package Test_Discriminants is
       type Point (X, Y : Natural) is record
          null;
       end record;

       P : Point;
       --  ERROR: Point is indefinite, so you need to specify the discriminants
       --  or give a default value

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  Those two declarations are equivalent.

    end Test_Discriminants;

This also means that, in the example above, you cannot declare an array of
Point values, because the size of a Point is not known.

In most other respects discriminants behave like regular fields: You have to
specify their values in aggregates, as seen above, and you can access their
values via the dot notation.

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Var_Size_Record_2
    :class: ada-run

    with Var_Size_Record_2; use Var_Size_Record_2;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Print_Stack (G : Growable_Stack) is
       begin
          Put ("<Stack, items: [");
          for I in G.Items'Range loop
             exit when I > G.Len;
             Put (" " & Integer'Image (G.Items (I)));
          end loop;
          Put_Line ("]>");
       end Print_Stack;

       S : Growable_Stack :=
         (Max_Len => 128, Items => (1, 2, 3, 4, others => <>), Len => 4);
    begin
       Print_Stack (S);
    end Main;

:code-config:`reset_accumulator=True;accumulate_code=False`

.. note:
    In the examples above, we used a discriminant to determine the size of an
    array, but it is not limited to that, and could be used, for example, to
    determine the size of a nested discriminated record.

Variant records
~~~~~~~~~~~~~~~

The examples of discriminants thus far have illustrated the declaration of
records of varying size, by having components whose size depends on the
discriminant.

However, discriminants can also be used to obtain the functionality of what are
sometimes called "variant records": records that can contain different sets of
fields.

:code-config:`reset_accumulator=True;accumulate_code=True`

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Variant_Record

    package Variant_Record is
       type Expr;                       --  Forward declaration of Expr
       type Expr_Access is access Expr; --  Access to a Expr

       type Expr_Kind_Type is (Bin_Op_Plus, Bin_Op_Minus, Num);
       --  A regular enumeration type

       type Expr (Kind : Expr_Kind_Type) is record
          --      ^ The discriminant is an enumeration value
          case Kind is
             when Bin_Op_Plus | Bin_Op_Minus =>
                Left, Right : Expr_Access;
             when Num =>
                Val : Integer;
          end case;
          --  Variant part. Only one, at the end of the record
          --  definition, but can be nested
       end record;
    end Variant_Record;

The fields that are in a :ada:`when` branch will be only available when the
value of the discriminant is covered by the branch. In the example above, you
will only be able to access the fields :ada:`Left` and :ada:`Right` when the
:ada:`Kind` is :ada:`Bin_Op_Plus` or :ada:`Bin_Op_Minus`.

If you try to access a field that is not valid for your record, a
:ada:`Constraint_Error` will be raised.

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Variant_Record
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Will compile but fail at runtime
    end Main;

Here is how you could write an evaluator for expressions:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Variant_Record
    :class: ada-run

    with Variant_Record; use Variant_Record;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       function Eval_Expr (E : Expr) return Integer is
         (case E.Kind is
          when Bin_Op_Plus => Eval_Expr (E.Left.all) + Eval_Expr (E.Right.all),
          when Bin_Op_Minus => Eval_Expr (E.Left.all) - Eval_Expr (E.Right.all),
          when Num => E.Val);

       E : Expr := (Bin_Op_Plus,
                    new Expr'(Bin_Op_Minus,
                              new Expr'(Num, 12), new Expr'(Num, 15)),
                    new Expr'(Num, 3));
    begin
       Put_Line (Integer'Image (Eval_Expr (E)));
    end Main;

:code-config:`reset_accumulator=True;accumulate_code=False`

.. admonition:: In other languages

    Ada's variant records are very similar to Sum types in functional languages
    such as OCaml or Haskell. A major difference is that the discriminant is a
    separate field in Ada, whereas the 'tag' of
    a Sum type is kind of built in, and only accessible with pattern matching.

    There are other differences (you can have several discriminants in a
    variant record in Ada). Nevertheless, they allow the same kind of type
    modeling as sum types in functional languages.

    Compared to C/C++ unions, Ada variant records are more powerful in what
    they allow, and are also checked at run time, which makes them safer.

Fixed-point types
-----------------

.. sectionauthor:: Gustavo A. Hoffmann

Decimal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~

We have already seen how to specify floating-point types.  However, in some
applications floating-point is not appropriate since, for example, the roundoff
error from binary arithmetic may be unacceptable or perhaps the hardware does
not support floating-point instructions.  Ada provides a category of types, the
decimal fixed-point types, that allows the programmer to specify the required
decimal precision (number of digits) as well as the scalaing factor (a power of
ten) and, optionally, a range.  In effect the values will be represented as
integers implicitly scaled by the specified power of 10. This is useful, for
example, for financial applications.

The syntax for a simple decimal fixed-point type is

.. code-block:: ada

    type <type-name> is delta <delta-value> digits <digits-value>;

In this case, the :ada:`delta` and the :ada:`digits` will be used by the
compiler to derive a range.

Several attributes are useful for dealing with decimal types:

+------------------------+----------------------------------------------+
| Attribute Name         | Meaning                                      |
+========================+==============================================+
| First                  | The first value of the type                  |
+------------------------+----------------------------------------------+
| Last                   | The last value of the type                   |
+------------------------+----------------------------------------------+
| Delta                  | The delta value of the type                  |
+------------------------+----------------------------------------------+

In the example below, we declare two data types: ``T3_D3`` and ``T6_D3``.
For both types, the delta value is the same: 0.001.

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Decimal_Fixed_Point_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Types is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D3 is delta 10.0 ** (-3) digits 6;
    begin
       Put_Line ("The delta    value of T3_D3 is " & T3_D3'Image (T3_D3'Delta));
       Put_Line ("The minimum  value of T3_D3 is " & T3_D3'Image (T3_D3'First));
       Put_Line ("The maximum  value of T3_D3 is " & T3_D3'Image (T3_D3'Last));
       New_Line;
       Put_Line ("The delta    value of T6_D3 is " & T6_D3'Image (T6_D3'Delta));
       Put_Line ("The minimum  value of T6_D3 is " & T6_D3'Image (T6_D3'First));
       Put_Line ("The maximum  value of T6_D3 is " & T6_D3'Image (T6_D3'Last));
    end Decimal_Fixed_Point_Types;

When running the application, we see that the delta value of both
types is indeed the same: 0.001. However, because ``T3_D3`` is restricted
to 3 digits, its range is -0.999 to 0.999. For the ``T6_D3``, we have
defined a precision of 6 digits, so the range is -999.999 to 999.999.

Similar to the type definition using the :ada:`range` syntax, because we
have an implicit range, the compiled code will check that the variables
contain values that are not out-of-range. Also, if the result of a
multiplication or division on decimal fixed-point types is smaller than
the delta value required for the context, the actual result will be
zero. For example:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Decimal_Fixed_Point_Smaller

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Decimal_Fixed_Point_Smaller is
       type T3_D3 is delta 10.0 ** (-3) digits 3;
       type T6_D6 is delta 10.0 ** (-6) digits 6;
       A : T3_D3 := T3_D3'Delta;
       B : T3_D3 := 0.5;
       C : T6_D6;
    begin
       Put_Line ("The value of A     is " & T3_D3'Image (A));
       A := A * B;
       Put_Line ("The value of A * B is " & T3_D3'Image (A));
       A := T3_D3'Delta;
       C := A * B;
       Put_Line ("The value of A * B is " & T6_D6'Image (C));
    end Decimal_Fixed_Point_Smaller;

In this example, the result of the operation :math:`0.001 * 0.5` is
0.0005. Since this value is not representable for the ``T3_D3`` type
because the delta value is 0.001, the actual value stored in variable
``A`` is zero. However, accuracy is preserved during the arithmetic
operations if the target has sufficient precision, and the value
displayed for C is 0.000500.

Fixed-point types
~~~~~~~~~~~~~~~~~

.. TODO: add link to advanced lesson that discusses 'Delta vs. 'Small

Ordinary fixed-point types are similar to decimal fixed-point types in that the
values are, in effect, scaled integers.  The difference between them is in the
scale factor: for a decimal fixed-point type, the scaling, given explicitly by
the type's ``delta``, is always a power of ten.

In contrast, for an ordinary fixed-point type, the scaling is defined by the
type's ``small``, which is derived from the specified ``delta`` and, by
default, is a power of two. Therefore, ordinary fixed-point types are sometimes
called binary fixed-point types.

.. note::
   Ordinary fixed-point types can be thought of being closer to the actual
   representation on the machine, since hardware support for decimal
   fixed-point arithmetic is not widespread (rescalings by a power of ten),
   while ordinary fixed-point types make use of the available integer shift
   instructions.

The syntax for an ordinary fixed-point type is

.. code-block:: ada

    type <type-name> is delta <delta-value> range <lower-bound> .. <upper-bound>;

By default the compiler will choose a scale factor, or ``small``, that is a
power of 2 no greater than <delta-value>.

For example, we may define a normalized range between -1.0 and 1.0 as
following:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Normalized_Fixed_Point_Type

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Normalized_Fixed_Point_Type is
       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;
    begin
       Put_Line ("TQ31 requires " & Integer'Image (TQ31'Size) & " bits");
       Put_Line ("The delta    value of TQ31 is " & TQ31'Image (TQ31'Delta));
       Put_Line ("The minimum  value of TQ31 is " & TQ31'Image (TQ31'First));
       Put_Line ("The maximum  value of TQ31 is " & TQ31'Image (TQ31'Last));
    end Normalized_Fixed_Point_Type;

In this example, we are defining a 32-bit fixed-point data type for our
normalized range. When running the application, we notice that the upper
bound is close to one, but not exact one. This is a typical effect of
fixed-point data types --- you can find more details in this discussion
about the `Q format <https://en.wikipedia.org/wiki/Q_(number_format)>`_.
We may also rewrite this code with an exact type definition:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Normalized_Adapted_Fixed_Point_Type

    procedure Normalized_Adapted_Fixed_Point_Type is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);
    begin
       null;
    end Normalized_Adapted_Fixed_Point_Type;

We may also use any other range. For example:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Custom_Fixed_Point_Range

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Fixed_Point_Range is
       type T_Inv_Trig is delta 2.0 ** (-15) * Pi range -Pi / 2.0 .. Pi / 2.0;
    begin
       Put_Line ("T_Inv_Trig requires " & Integer'Image (T_Inv_Trig'Size)
                 & " bits");
       Put_Line ("The delta    value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Delta));
       Put_Line ("The minimum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'First));
       Put_Line ("The maximum  value of T_Inv_Trig is "
                 & T_Inv_Trig'Image (T_Inv_Trig'Last));
    end Custom_Fixed_Point_Range;

In this example, we are defining a 16-bit type called ``T_Inv_Trig``,
which has a range from :math:`-\pi/2` to :math:`\pi/2`.

All standard operations are available for fixed-point types. For example:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Fixed_Point_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Fixed_Point_Op is
       type TQ31 is delta 2.0 ** (-31) range -1.0 .. 1.0 - 2.0 ** (-31);

       A, B, R : TQ31;
    begin
       A := 0.25;
       B := 0.50;
       R := A + B;
       Put_Line ("R is " & TQ31'Image (R));
    end Fixed_Point_Op;

As expected, ``R`` contains 0.75 after the addition of ``A`` and ``B``.

In fact the language is more general that these examples imply, since in
practice it is typical to need to multiply or divide values from different
fixed-point types, and obtain a result that may be of a third fixed-point type.
The details are outside the scope of this introductory course.

It is also worth noting, although again the details are outside the scope of
this course, that you can explicitly specify a value for an ordinary
fixed-point type's ``small``.  This allows non-binary scaling, for example:

.. code-block:: ada

    type Angle is delta 1.0/3600.0 range 0.0 .. 360.0 - 1.0/3600.0;
    for Angle'Small use Angle'Delta;


Character types
---------------

As noted earlier, each enumeration type is distinct and
incompatible with every other enumeration type. However, what we did not
mention previously is that character literals are permitted as
enumeration literals. This means that in addition to the language's
strongly typed character types,
user-defined character types are also permitted:

.. code:: ada project=Courses.Intro_To_Ada.More_About_Types.Character_Example
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Character_Example is
       type My_Char is ('a', 'b', 'c');
       --  Our custom character type, an enumeration type with 3 valid values.

       C : Character;
       --  ^ Built-in character type (it's an enumeration type)

       M : My_Char;
    begin
       C := '?';
       --   ^ Character literal (enumeration literal)

       M := 'a';

       C := 65;
       --   ^ Invalid: 65 is not a Character value

       C := Character'Val (65);
       --  Assign the character at position 65 in the enumeration (which is 'A')

       M := C;
       --   ^ Invalid: C is of type Character, and M is a My_Char

       M := 'd';
       --   ^ Invalid: 'd' is not a valid literal for type My_Char
    end Character_Example;
