Arrays
======

.. include:: ../../global.txt

Arrays provide another fundamental family of composite types in Ada.

Array type declaration
----------------------

Arrays in Ada are used to define contiguous collections of elements that can be
selected by indexing. Here's a simple example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;

       type My_Int_Array is
         array (Index) of My_Int;
       --                 ^ Type of elements
       --       ^ Bounds of the array
       Arr : My_Int_Array := (2, 3, 5, 7, 11);
       --                    ^ Array literal
       --                      (aggregate)

       V : My_Int;
    begin
       for I in Index loop
          V := Arr (I);
          --        ^ Take the Ith element
          Put (My_Int'Image (V));
       end loop;
       New_Line;
    end Greet;

The first point to note is that we specify the index type for the array,
rather than its size. Here we declared an integer type named :ada:`Index`
ranging from :ada:`1` to :ada:`5`, so each array instance will have 5 elements,
with the initial element at index 1 and the last element at index 5.

Although this example used an integer type for the index, Ada is more general:
any discrete type is permitted to index an array, including
:ref:`Enum types <Intro_Ada_Enum_Types>`. We will soon see what that means.

Another point to note is that querying an element of the array at a given index
uses the same syntax as for function calls: that is, the array object followed
by the index in parentheses.

Thus when you see an expression such as :ada:`A (B)`, whether it is a function
call or an array subscript depends on what :ada:`A` refers to.

Finally, notice how we initialize the array with the :ada:`(2, 3, 5, 7, 11)`
expression. This is another kind of aggregate in Ada, and is in a sense a
literal expression for an array, in the same way that :ada:`3` is a literal
expression for an integer. The notation is very powerful, with a number of
properties that we will introduce later. A detailed overview appears in the
notation of :ref:`aggregate types <Intro_Ada_Aggregates>`.

Unrelated to arrays, the example also illustrated two procedures from
:ada:`Ada.Text_IO`:

*  :ada:`Put`, which displays a string without a terminating end of line

*  :ada:`New_Line`, which outputs an end of line

Let's now delve into what it means to be able to use any discrete type
to index into the array.

.. admonition:: In other languages

    Semantically, an array object in Ada is the entire data structure, and
    not simply a handle or pointer.  Unlike C and C++, there is no implicit
    equivalence between an array and a pointer to its initial element.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Array_Bounds_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Bounds_Example is
       type My_Int is range 0 .. 1000;
       type Index is range 11 .. 15;
       --                  ^ Low bound can be any value
       type My_Int_Array is array (Index) of My_Int;
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index loop
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Bounds_Example;

One effect is that the bounds of an array can be any values. In the first
example we constructed an array type whose first index is :ada:`1`, but in the
example above we declare an array type whose first index is :ada:`11`.

That's perfectly fine in Ada, and moreover since we use the index type as a
range to iterate over the array indices, the code using the array does not need
to change.

That leads us to an important consequence with regard to code dealing with
arrays. Since the bounds can vary, you should not assume / hard-code specific
bounds when iterating / using arrays. That means the code above is good,
because it uses the index type, but a for loop as shown below is bad practice
even though it works correctly:

.. code-block:: ada

    for I in 11 .. 15 loop
       Tab (I) := Tab (I) * 2;
    end loop;

Since you can use any discrete type to index an array, enumeration types
are permitted.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Month_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Month_Example is
       type Month_Duration is range 1 .. 31;
       type Month is (Jan, Feb, Mar, Apr,
                      May, Jun, Jul, Aug,
                      Sep, Oct, Nov, Dec);

       type My_Int_Array is
         array (Month) of Month_Duration;
       --       ^ Can use an enumeration type
       --         as the index

       Tab : constant My_Int_Array :=
       --    ^ constant is like a variable but
       --      cannot be modified
         (31, 28, 31, 30, 31, 30,
          31, 31, 30, 31, 30, 31);
       --  Maps months to number of days
       --  (ignoring leap years)

       Feb_Days : Month_Duration := Tab (Feb);
       --  Number of days in February
    begin
       for M in Month loop
          Put_Line
            (Month'Image (M) & " has "
             & Month_Duration'Image (Tab (M))
             & " days.");
       --    ^ Concatenation operator
       end loop;
    end Month_Example;

In the example above, we are:

- Creating an array type mapping months to month durations in days.

- Creating an array, and instantiating it with an aggregate mapping months to
  their actual durations in days.

- Iterating over the array, printing out the months, and the number of days for
  each.

Being able to use enumeration values as indices is very helpful in creating
mappings such as shown above one, and is an often used feature in Ada.

Indexing
--------

We have already seen the syntax for selecting elements of an array. There are
however a few more points to note.

First, as is true in general in Ada, the indexing operation is strongly typed.
If you use a value of the wrong type to index the array, you will get a
compile-time error.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_2
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;

       type My_Index   is range 1 .. 5;
       type Your_Index is range 1 .. 5;

       type My_Int_Array is array (My_Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Your_Index loop
          Put (My_Int'Image (Tab (I)));
       --                         ^ Compile time error
       end loop;
       New_Line;
    end Greet;

Second, arrays in Ada are bounds checked. This means that if you try to access
an element outside of the bounds of the array, you will get a run-time error
instead of accessing random memory as in unsafe languages.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_3
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index range 2 .. 6 loop
          Put (My_Int'Image (Tab (I)));
          --                      ^ Will raise an
          --                        exception when
          --                        I = 6
       end loop;
       New_Line;
    end Greet;

Simpler array declarations
--------------------------

In the previous examples, we have always explicitly created an index type for
the array. While this can be useful for typing and readability purposes,
sometimes you simply want to express a range of values.  Ada allows you to do
that, too.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Simple_Array_Bounds

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Array_Bounds is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       --                          ^ Subtype of Integer
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in 1 .. 5 loop
       --       ^ Subtype of Integer
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Simple_Array_Bounds;

This example defines the range of the array via the range syntax, which
specifies an anonymous subtype of Integer and uses it to index the array.

This means that the type of the index is :ada:`Integer`. Similarly, when you
use an anonymous range in a for loop as in the example above, the type of the
iteration variable is also :ada:`Integer`, so you can use :ada:`I` to index
:ada:`Tab`.

You can also use a named subtype for the bounds for an array.

.. _Intro_Ada_Range_Attribute:

Range attribute
---------------

We noted earlier that hard coding bounds when iterating over an array is a bad
idea, and showed how to use the array's index type/subtype to iterate over its
range in a for loop.  That raises the question of how to write an iteration
when the array has an anonymous range for its bounds, since there is no name to
refer to the range.  Ada solves that via several attributes of array objects:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Range_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Range_Example is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'Range loop
       --          ^ Gets the range of Tab
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Range_Example;

If you want more fine grained control, you can use the separate attributes
:ada:`'First` and :ada:`'Last`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Array_Attributes_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Attributes_Example is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'First .. Tab'Last - 1 loop
       --          ^ Iterate on every index
       --            except the last
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Attributes_Example;

The :ada:`'Range`, :ada:`'First` and :ada:`'Last` attributes in these examples
could also have been applied to the array type name, and not just the array
instances.

Although not illustrated in the above examples, another useful attribute for an
array instance :ada:`A`  is :ada:`A'Length`, which is the number of elements
that :ada:`A` contains.

It is legal and sometimes useful to have a "null array", which contains no
elements.  To get this effect, define an index range whose upper bound is less
than the lower bound.

.. _Intro_Ada_Unconstrained_Array_Types:

Unconstrained arrays
--------------------

Let's now consider one of the most powerful aspects of Ada's array facility.

Every array type we have defined so far has a fixed size: every instance of
this type will have the same bounds and therefore the same number of elements
and the same size.

However, Ada also allows you to declare array types whose bounds are not fixed:
in that case, the bounds will need to be provided when creating instances of
the type.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Unconstrained_Array_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Unconstrained_Array_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Workload_Type is
         array (Days range <>) of Natural;
       --  Indefinite array type
       --       ^ Bounds are of type Days,
       --         but not known

       Workload : constant
         Workload_Type (Monday .. Friday) :=
       --               ^ Specify the bounds
       --                 when declaring
          (Friday => 7, others => 8);
       --               ^ Default value
       --  ^ Specify element by name of index
    begin
       for I in Workload'Range loop
          Put_Line (Integer'Image (Workload (I)));
       end loop;
    end Unconstrained_Array_Example;

The fact that the bounds of the array are not known is indicated by the
:ada:`Days range <>` syntax. Given a discrete type :ada:`Discrete_Type`, if we
use :ada:`Discrete_Type` for the index in an array type then
:ada:`Discrete_Type` serves as the type of the index and comprises the range of
index values for each array instance.

If we define the index as :ada:`Discrete_Type range <>` then
:ada:`Discrete_Type` serves as the type of the index, but different array
instances may have different bounds from this type

An array type that is defined with the :ada:`Discrete_Type range <>` syntax
for its index is referred to as an unconstrained array type, and, as
illustrated above, the bounds need to be provided when an instance is created.

The above example also shows other forms of the aggregate syntax. You can specify
associations by name, by giving the value of the index on the left side of an
arrow association. :ada:`1 => 2` thus means
"assign value 2 to the element at index 1 in my array". :ada:`others => 8` means
"assign value 8 to every element that wasn't previously assigned in this aggregate".

.. attention::
    The so-called "box" notation (:ada:`<>`) is commonly used as a wildcard or
    placeholder in Ada. You will often see it when the meaning is "what is
    expected here can be anything".

.. admonition:: In other languages

    While unconstrained arrays in Ada might seem similar to variable length
    arrays in C, they are in reality much more powerful, because they're truly
    first-class values in the language. You can pass them as parameters to
    subprograms or return them from functions, and they implicitly contain
    their bounds as part of their value.  This means that it is useless to pass
    the bounds or length of an array explicitly along with the array, because
    they are accessible via the :ada:`'First`, :ada:`'Last`, :ada:`'Range` and
    :ada:`'Length` attributes explained earlier.

.. _Intro_Ada_Unconstrained_Array_Type_Instance_Bound:

Although different instances of the same unconstrained array type can have different
bounds, a specific instance has the same bounds throughout its lifetime.
This allows Ada to implement unconstrained arrays efficiently; instances can be
stored on the stack and do not require heap allocation as in languages like Java.

Predefined array type: String
-----------------------------

A recurring theme in our introduction to Ada types has been the way important
built-in types like :ada:`Boolean` or :ada:`Integer` are defined through the
same facilities that are available to the user. This is also true for strings:
The :ada:`String` type in Ada is a simple array.

Here is how the string type is defined in Ada:

.. code-block:: ada

    type String is
      array (Positive range <>) of Character;

The only built-in feature Ada adds to make strings more ergonomic is custom
literals, as we can see in the example below.

.. hint::
    String literals are a syntactic sugar for aggregates, so that in the
    following example, :ada:`A` and :ada:`B` have the same value.

    .. code:: ada no_button project=Courses.Intro_To_Ada.Arrays.String_Literals

        package String_Literals is
            --  Those two declarations are equivalent
            A : String (1 .. 11) := "Hello World";
            B : String (1 .. 11) :=
                ('H', 'e', 'l', 'l', 'o', ' ',
                 'W', 'o', 'r', 'l', 'd');
        end String_Literals;

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_4

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : String (1 .. 11) := "dlroW olleH";
       --        ^ Pre-defined array type.
       --          Component type is Character
    begin
       for I in reverse Message'Range loop
          --    ^ Iterate in reverse order
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

However, specifying the bounds of the object explicitly is a bit of a hassle;
you have to manually count the number of characters in the literal.
Fortunately, Ada gives you an easier way.

You can omit the bounds when creating an instance of an unconstrained array
type if you supply an initialization, since the bounds can be deduced from the
initialization expression.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_5

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : constant String := "dlroW olleH";
       --                 ^ Bounds are automatically
       --                   computed from
       --                   initialization value
    begin
       for I in reverse Message'Range loop
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Constant_Integer_Array

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Integer_Array is array (Natural range <>) of Integer;

       My_Array : constant Integer_Array := (1, 2, 3, 4);
       --                  ^ Bounds are automatically
       --                    computed from
       --                    initialization value
    begin
        null;
    end Main;

.. attention::
    As you can see above, the standard :ada:`String` type in Ada is an array. As
    such, it shares the advantages and drawbacks of arrays: a :ada:`String`
    value is stack allocated, it is accessed efficiently, and its bounds are
    immutable.

    If you want something akin to C++'s :cpp:`std::string`, you can use
    :ref:`Unbounded Strings <Intro_Ada_Unbounded_Strings>` from Ada's standard library.
    This type is more like a mutable, automatically managed string buffer to
    which you can add content.

Restrictions
------------

A very important point about arrays: bounds *have* to be known when instances
are created. It is for example illegal to do the following.

.. code-block:: ada

    declare
       A : String;
    begin
       A := "World";
    end;

Also, while you of course can change the values of elements in an array, you
cannot change the array's bounds (and therefore its size) after it has been
initialized.  So this is also illegal:

.. code-block:: ada

    declare
       A : String := "Hello";
    begin
       A := "World";       --  OK: Same size
       A := "Hello World"; --  Not OK: Different size
    end;

Also, while you can expect a warning for this kind of error in very simple
cases like this one, it is impossible for a compiler to know in the general
case if you are assigning a value of the correct length, so this violation will
generally result in a run-time error.

.. _Intro_Ada_Indefinite_Subtype:

.. admonition:: Attention

    While we will learn more about this later, it is important to know
    that arrays are not the only types whose instances might be of unknown
    size at compile-time.

    Such objects are said to be of an *indefinite subtype*, which means that
    the subtype size is not known at compile time, but is dynamically computed
    (at run time).

    .. code:: ada no_button project=Courses.Intro_To_Ada.Arrays.Indefinite_Subtypes

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Indefinite_Subtypes is
            function Get_Number return Integer is
            begin
                return Integer'Value (Get_Line);
            end Get_Number;

           A : String := "Hello";
           --  Indefinite subtype

           B : String (1 .. 5) := "Hello";
           --  Definite subtype

           C : String (1 .. Get_Number);
           --  Indefinite subtype
           --  (Get_Number's value is computed at
           --  run-time)
        begin
           null;
        end Indefinite_Subtypes;

   Here, the :ada:`'Value` attribute converts the string to an integer.


Returning unconstrained arrays
------------------------------

The return type of a function can be any type; a function can return a value
whose size is unknown at compile time. Likewise, the parameters can be of
any type.

For example, this is a function that returns an unconstrained :ada:`String`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Day_Name_1

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       function Get_Day_Name (Day : Days := Monday)
                              return String is
       begin
          return
            (case Day is
             when Monday    => "Monday",
             when Tuesday   => "Tuesday",
             when Wednesday => "Wednesday",
             when Thursday  => "Thursday",
             when Friday    => "Friday",
             when Saturday  => "Saturday",
             when Sunday    => "Sunday");
       end Get_Day_Name;

    begin
       Put_Line ("First day is "
                 & Get_Day_Name (Days'First));
    end Main;

(This example is for illustrative purposes only.  There is a built-in mechanism,
the :ada:`'Image` attribute for scalar types, that returns the name (as a
:ada:`String`) of any element of an enumeration type.  For example
:ada:`Days'Image(Monday)` is :ada:`"MONDAY"`.)

.. admonition:: In other languages

    Returning variable size objects in languages lacking a garbage collector is
    a bit complicated implementation-wise, which is why C and C++ don't allow
    it, preferring to depend on explicit dynamic allocation / free from the user.

    The problem is that explicit storage management is unsafe as soon as you
    want to collect unused memory. Ada's ability to return variable size
    objects will remove one use case for dynamic allocation, and hence, remove
    one potential source of bugs from your programs.

    Rust follows the C/C++ model, but with safe pointer semantics.
    However, dynamic allocation is still used. Ada can benefit from
    a possible performance edge because it can use any model.

    .. amiard: TODO: say less or say more

Declaring arrays (2)
--------------------

While we can have array types whose size and bounds are determined at run time,
the array's component type needs to be of a definite and constrained type.

Thus, if you need to declare, for example, an array of strings, the
:ada:`String` subtype used as component will need to have a fixed size.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Day_Name_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Days is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Day_Name is String (1 .. 2);
       --  Subtype of string with known size

       type Days_Name_Type is
         array (Days) of Day_Name;
       --       ^ Type of the index
       --                ^ Type of the element.
       --                  Must be definite

       Names : constant Days_Name_Type :=
         ("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");
       --  Initial value given by aggregate
    begin
       for I in Names'Range loop
          Put_Line (Names (I));
       end loop;
    end Show_Days;

Array slices
------------

One last feature of Ada arrays that we're going to cover is array slices. It is
possible to take and use a slice of an array (a contiguous sequence of
elements) as a name or a value.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Slices

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
        Buf : String := "Hello ...";

        Full_Name : String := "John Smith";
    begin
        Buf (7 .. 9) := "Bob";
        --  Careful! This works because the string
        --  on the right side is the same length as
        --  the replaced slice!

        --  Prints "Hello Bob"
        Put_Line (Buf);

         --  Prints "Hi John"
        Put_Line ("Hi " & Full_Name (1 .. 4));
    end Main;

As we can see above, you can use a slice on the left side of an assignment, to
replace only part of an array.

A slice of an array is of the same type as the array, but has a different
subtype, constrained by the bounds of the slice.

.. attention::
    Ada has :arm:`multidimensional arrays <3-6>`,
    which are not covered in this course. Slices will only work on one
    dimensional arrays.

.. ?? Somewhere it should be noted that Ada allows multidimensional arrays
.. ?? The 'attention' note is the 1st implication that Ada supports more
.. ?? than one-dimensional arrays

.. _Intro_Ada_Object_Renaming:

Renaming
--------

So far, we've seen that the following elements can be renamed:
:ref:`subprograms <Intro_Ada_Subprogram_Renaming>`, :ref:`packages <Intro_Ada_Package_Renaming>`,
and :ref:`record components <Intro_Ada_Record_Comp_Renaming>`. We can also rename objects
by using the :ada:`renames` keyword. This allows for creating alternative names
for these objects. Let's look at an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Variable_Renaming

    package Measurements is

       subtype Degree_Celsius is Float;

       Current_Temperature : Degree_Celsius;

    end Measurements;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Measurements;

    procedure Main is
       subtype Degrees is Measurements.Degree_Celsius;

       T : Degrees
         renames Measurements.Current_Temperature;
    begin
        T := 5.0;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));

        T := T + 2.5;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));
    end Main;

In the example above, we declare a variable :ada:`T` by renaming the
:ada:`Current_Temperature` object from the :ada:`Measurements` package. As you
can see by running this example, both :ada:`Current_Temperature` and its
alternative name :ada:`T` have the same values:

- first, they show the value 5.0
- after the addition, they show the value 7.5.

This is because they are essentially referring to the same object, but with two
different names.

Note that, in the example above, we're using :ada:`Degrees` as an alias of
:ada:`Degree_Celsius`. We discussed this method
:ref:`earlier in the course <Intro_Ada_Subtype_Aliases>`.

Renaming can be useful for improving the readability of more complicated array
indexing. Instead of explicitly using indices every time we're accessing certain
positions of the array, we can create shorter names for these positions by
renaming them. Let's look at the following example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Reverse_Colors

    package Colors is

       type Color is (Black, Red, Green, Blue, White);

       type Color_Array is
         array (Positive range <>) of Color;

       procedure Reverse_It (X : in out Color_Array);

    end Colors;

    package body Colors is

       procedure Reverse_It (X : in out Color_Array) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : Color;
                X_Left  : Color
                  renames X (I);
                X_Right : Color
                  renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_It;

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;

    with Colors; use Colors;

    procedure Test_Reverse_Colors is

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_It (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

In the example above, package :ada:`Colors` implements the procedure
:ada:`Reverse_It` by declaring new names for two positions of the array. The
actual implementation becomes easy to read:

.. code-block:: ada

    begin
       Tmp     := X_Left;
       X_Left  := X_Right;
       X_Right := Tmp;
    end;

Compare this to the alternative version without renaming:

.. code-block:: ada

    begin
       Tmp                      := X (I);
       X (I)                    := X (X'Last + X'First - I);
       X (X'Last + X'First - I) := Tmp;
    end;
