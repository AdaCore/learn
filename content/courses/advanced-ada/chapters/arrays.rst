Arrays
======

.. include:: ../../global.txt

Unconstrained Arrays
--------------------

In the
:doc:`Introduction to Ada course </courses/intro-to-ada/chapters/arrays>`,
we've seen that we can declare array types whose bounds are not fixed: in that
case, the bounds are provided when creating objects of those types. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Unconstrained_Array_Example

    package Measurement_Defs is

       type Measurements is array (Positive range <>) of Float;
       --                          ^ Bounds are of type Positive,
       --                            but not known at this point.

    end Measurement_Defs;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Measurement_Defs; use Measurement_Defs;

    procedure Show_Measurements is
       M : Measurements (1 .. 10);
       --                ^ Providing bounds here!
    begin
       Put_Line ("First index: " & M'First'Image);
       Put_Line ("Last index:  " & M'Last'Image);
    end Show_Measurements;

In this example, the :ada:`Measurements` array type from the
:ada:`Measurement_Defs` package is unconstrained. In the
:ada:`Show_Measurements` procedure, we declare a constrained object (:ada:`M`)
of this type.

The :doc:`Introduction to Ada course </courses/intro-to-ada/chapters/arrays>`
also highlights the fact that the bounds are fixed once an object is declared:

    Although different instances of the same unconstrained array type can
    have different bounds, a specific instance has the same bounds
    throughout its lifetime. This allows Ada to implement unconstrained
    arrays efficiently; instances can be stored on the stack and do not
    require heap allocation as in languages like Java.

In the :ada:`Show_Measurements` procedure above, once we declare :ada:`M`, its
bounds are fixed for the whole lifetime of :ada:`M`. We cannot *add* another
component to this array. In other words, :ada:`M` will have 10 components for
its whole lifetime.

Unconstrained Arrays vs. Vectors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you need, however, the flexibility of increasing the length of an array, you
could use vectors instead. This is how we could rewrite the previous example
using vectors:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Unconstrained_Array_Example

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    package Measurement_Defs is

       package Vectors is new Ada.Containers.Vectors
         (Index_Type   => Positive,
          Element_Type => Float);

       subtype Measurements is Vectors.Vector;

    end Measurement_Defs;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Measurement_Defs; use Measurement_Defs;

    procedure Show_Measurements is
       use Measurement_Defs.Vectors;

       M : Measurements := To_Vector (10);
       --                  ^ Creating 10-element vector.
    begin
       Put_Line ("First index: " & M.First_Index'Image);
       Put_Line ("Last index:  " & M.Last_Index'Image);

       Put_Line ("Adding element...");
       M.Append (1.0);

       Put_Line ("First index: " & M.First_Index'Image);
       Put_Line ("Last index:  " & M.Last_Index'Image);
    end Show_Measurements;

In the declaration of :ada:`M` in this example, we're creating a 10-element
vector by calling :ada:`To_Vector` and specifying the element count. Later on,
with the call to :ada:`Append`, we're increasing the length of the :ada:`M` to
11 elements.

As you might expect, the flexibility of vectors comes with a price: every time
we add an element that doesn't fit in the current capacity of the vector, the
container has to reallocate memory in the background due to that new element.
Therefore, arrays are more efficient, as the memory allocation only happens
once for each object.


Multidimensional Arrays
-----------------------

So far, we've discussed unidimensional arrays, since they are very common in
Ada. However, Ada also supports multidimensional arrays using the same
facilities as for unidimensional arrays. For example, we can use the
:ada:`First`, :ada:`Last`, :ada:`Range` and :ada:`Length` attributes for each
dimension of a multidimensional array. This section presents more details on
this topic.

To create a multidimensional array, we simply separate the ranges of each
dimension with a comma. The following example presents the one-dimensional
array :ada:`A1`, the two-dimensional array :ada:`A2` and the three-dimensional
array :ada:`A3`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Arrays.Multidimensional_Arrays

    package Multidimensional_Arrays_Decl is

       A1 : array (1 .. 10) of Float;
       A2 : array (1 .. 5, 1 .. 10) of Float;
       --          ^ first dimension
       --                  ^ second dimension
       A3 : array (1 .. 2, 1 .. 5, 1 .. 10) of Float;
       --          ^ first dimension
       --                  ^ second dimension
       --                          ^ third dimension
    end Multidimensional_Arrays_Decl;

The two-dimensional array :ada:`A2` has 5 components in the first dimension and
10 components in the second dimension. The three-dimensional array :ada:`A3`
has 2 components in the first dimension, 5 components in the second dimension,
and 10 components in the third dimension. Note that the ranges we've selected
for :ada:`A1`, :ada:`A2` and :ada:`A3` are completely arbitrary. You may select
ranges for each dimension that are the most appropriate in the context of your
application. Also, the number of dimensions is not limited to three, so you
could declare higher-dimensional arrays if needed.

We can use the :ada:`Length` attribute to retrieve the length of each
dimension. We use an integer value in parentheses to specify which dimension
we're referring to. For example, if we write :ada:`A'Length (2)`, we're
referring to the length of the second dimension of a multidimensional array
:ada:`A`. Note that :ada:`A'Length` is equivalent to :ada:`A'Length (1)`. The
same equivalence applies to other array-related attributes such as
:ada:`First`, :ada:`Last` and :ada:`Range`.

Let's use the :ada:`Length` attribute for the arrays we declared in the
:ada:`Multidimensional_Arrays_Decl` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Multidimensional_Arrays

    with Ada.Text_IO; use Ada.Text_IO;

    with Multidimensional_Arrays_Decl;
    use Multidimensional_Arrays_Decl;

    procedure Show_Multidimensional_Arrays is
    begin
       Put_Line ("A1'Length:     " & A1'Length'Image);
       Put_Line ("A1'Length (1): " & A1'Length (1)'Image);
       Put_Line ("A2'Length (1): " & A2'Length (1)'Image);
       Put_Line ("A2'Length (2): " & A2'Length (2)'Image);
       Put_Line ("A3'Length (1): " & A3'Length (1)'Image);
       Put_Line ("A3'Length (2): " & A3'Length (2)'Image);
       Put_Line ("A3'Length (3): " & A3'Length (3)'Image);
    end Show_Multidimensional_Arrays;

As this simple example shows, we can easily retrieve the length of each
dimension. Also, as we've just mentioned, :ada:`A1'Length` is equal to
:ada:`A1'Length (1)`.

Let's consider an application where we make hourly measurements for the first
12 hours of the day, on each day of the week. We can create a two-dimensional
array type called :ada:`Measurements` to store this data. Also, we can have
three procedures for this array:

- :ada:`Show_Indices`, which presents the indices (days and hours) of the
  two-dimensional array;

- :ada:`Show_Values`, which presents the values stored in the array; and

- :ada:`Reset`, which resets each value of the array.

This is the complete code for this application:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Multidimensional_Measurements

    package Measurement_Defs is

       type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       type Hours is range 0 .. 11;

       subtype Measurement is Float;

       type Measurements is array (Days, Hours) of Measurement;

       procedure Show_Indices (M : Measurements);

       procedure Show_Values (M : Measurements);

       procedure Reset (M : out Measurements);

    end Measurement_Defs;

    with Ada.Text_IO;      use Ada.Text_IO;

    package body Measurement_Defs is

       procedure Show_Indices (M : Measurements) is
       begin
          Put_Line ("---- Indices ----");

          for D in M'Range (1) loop
             Put (D'Image & " ");

             for H in M'First (2) .. M'Last (2) - 1 loop
                Put (H'Image & " ");
             end loop;
             Put_Line (M'Last (2)'Image);
          end loop;
       end Show_Indices;

       procedure Show_Values (M : Measurements) is
          package H_IO is
            new Ada.Text_IO.Integer_IO (Hours);
          package M_IO is
            new Ada.Text_IO.Float_IO (Measurement);

            procedure Set_IO_Defaults is
            begin
               H_IO.Default_Width := 5;

               M_IO.Default_Fore  := 1;
               M_IO.Default_Aft   := 2;
               M_IO.Default_Exp   := 0;
            end Set_IO_Defaults;
       begin
          Set_IO_Defaults;

          Put_Line ("---- Values ----");
          Put ("   ");
          for H in M'Range (2) loop
             H_IO.Put (H);
          end loop;
          New_Line;

          for D in M'Range (1) loop
             Put (D'Image & " ");

             for H in M'Range (2) loop
                M_IO.Put (M (D, H));
                Put (" ");
             end loop;
             New_Line;
          end loop;
       end Show_Values;

       procedure Reset (M : out Measurements) is
       begin
          M := (others => (others => 0.0));
       end Reset;

    end Measurement_Defs;

    with Measurement_Defs; use Measurement_Defs;

    procedure Show_Measurements is
       M : Measurements;
    begin
       Reset (M);
       Show_Indices (M);
       Show_Values (M);
    end Show_Measurements;

We recommend that you spend some time analyzing this example. Also, we'd like
to highlight the following aspects:

- We access a value from a multidimensional array by using commas to separate
  the index values within the parentheses. For example:
  :ada:`M (D, H)` allows us to access the value on day :ada:`D` and hour
  :ada:`H` from the multidimensional array :ada:`M`.

- To loop over the multidimensional array :ada:`M`, we write
  :ada:`for D in M'Range (1) loop` and :ada:`for H in M'Range (2) loop` for
  the first and second dimensions, respectively.

- To reset all values of the multidimensional array, we use an aggregate with
  this form: :ada:`(others => (others => 0.0))`.

Unconstrained Multidimensional Arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, we've discussed unconstrained arrays for the unidimensional case.
It's possible to declare unconstrained multidimensional arrays as well. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Arrays.Unconstrained_Multidimensional_Arrays

    package Multidimensional_Arrays_Decl is

       type F1 is array (Positive range <>) of Float;
       type F2 is array (Positive range <>,
                         Positive range <>) of Float;
       type F3 is array (Positive range <>,
                         Positive range <>,
                         Positive range <>) of Float;

    end Multidimensional_Arrays_Decl;

Here, we're declaring the one-dimensional type :ada:`F1`, the two-dimensional
type :ada:`F2` and the three-dimensional type :ada:`F3`.

As is the case with unidimensional arrays, we must specify the bounds when
declaring objects of unconstrained multidimensional array types:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Unconstrained_Multidimensional_Arrays

    with Ada.Text_IO; use Ada.Text_IO;

    with Multidimensional_Arrays_Decl;
    use Multidimensional_Arrays_Decl;

    procedure Show_Multidimensional_Arrays is
       A1 : F1 (1 .. 2);
       A2 : F2 (1 .. 4, 10 .. 20);
       A3 : F3 (2 .. 3, 1 .. 5, 1 .. 2);
    begin
       Put_Line ("A1'Length (1): " & A1'Length (1)'Image);
       Put_Line ("A2'Length (1): " & A2'Length (1)'Image);
       Put_Line ("A2'Length (2): " & A2'Length (2)'Image);
       Put_Line ("A3'Length (1): " & A3'Length (1)'Image);
       Put_Line ("A3'Length (2): " & A3'Length (2)'Image);
       Put_Line ("A3'Length (3): " & A3'Length (3)'Image);
    end Show_Multidimensional_Arrays;

Arrays of arrays
~~~~~~~~~~~~~~~~

It's important to distinguish between multidimensional arrays and arrays of
arrays. Both are supported in Ada, but they're very distinct from each other.
We can create an array of an array by first specifying a one-dimensional array
type :ada:`T1`, and then specifying another one-dimensional array type
:ada:`T2` where each component of :ada:`T2` is of :ada:`T1` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Arrays.Array_Of_Arrays

    package Array_Of_Arrays_Decl is

       type T1 is array (Positive range <>) of Float;

       type T2 is array (Positive range <>) of T1 (1 .. 10);
       --                                          ^ bounds must be set!

    end Array_Of_Arrays_Decl;

Note that, in the declaration of :ada:`T2`, we must set the bounds for the
:ada:`T1` type. This is a major difference to multidimensional arrays, which
allow for unconstrained ranges in multiple dimensions.

We can rewrite the previous application for measurements using arrays of
arrays. This is the adapted code:

.. code:: ada run_button project=Courses.Advanced_Ada.Arrays.Multidimensional_Measurements

    package Measurement_Defs is

       type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

       type Hours is range 0 .. 11;

       subtype Measurement is Float;

       type Hourly_Measurements is array (Hours) of Measurement;

       type Measurements is array (Days) of Hourly_Measurements;

       procedure Show_Indices (M : Measurements);

       procedure Show_Values (M : Measurements);

       procedure Reset (M : out Measurements);

    end Measurement_Defs;

    with Ada.Text_IO;      use Ada.Text_IO;

    package body Measurement_Defs is

       procedure Show_Indices (M : Measurements) is
       begin
          Put_Line ("---- Indices ----");

          for D in M'Range loop
             Put (D'Image & " ");

             for H in M (D)'First .. M (D)'Last - 1 loop
                Put (H'Image & " ");
             end loop;
             Put_Line (M (D)'Last'Image);
          end loop;
       end Show_Indices;

       procedure Show_Values (M : Measurements) is
          package H_IO is
            new Ada.Text_IO.Integer_IO (Hours);
          package M_IO is
            new Ada.Text_IO.Float_IO (Measurement);

            procedure Set_IO_Defaults is
            begin
               H_IO.Default_Width := 5;

               M_IO.Default_Fore  := 1;
               M_IO.Default_Aft   := 2;
               M_IO.Default_Exp   := 0;
            end Set_IO_Defaults;
       begin
          Set_IO_Defaults;

          Put_Line ("---- Values ----");
          Put ("   ");
          for H in M (M'First)'Range loop
             H_IO.Put (H);
          end loop;
          New_Line;

          for D in M'Range loop
             Put (D'Image & " ");

             for H in M (D)'Range loop
                M_IO.Put (M (D) (H));
                Put (" ");
             end loop;
             New_Line;
          end loop;
       end Show_Values;

       procedure Reset (M : out Measurements) is
       begin
          M := (others => (others => 0.0));
       end Reset;

    end Measurement_Defs;

    with Measurement_Defs; use Measurement_Defs;

    procedure Show_Measurements is
       M : Measurements;
    begin
       Reset (M);
       Show_Indices (M);
       Show_Values (M);
    end Show_Measurements;

Again, we recommend that you spend some time analyzing this example and
comparing it to the previous version that uses multidimensional arrays. Also,
we'd like to highlight the following aspects:

- We access a value from an array of arrays by specifying the index of each
  array separately. For example: :ada:`M (D) (H)` allows us to access the value
  on day :ada:`D` and hour :ada:`H` from the array of arrays :ada:`M`.

- To loop over an array of arrays :ada:`M`, we write
  :ada:`for D in M'Range loop` for the first level of :ada:`M` and
  :ada:`for H in M (D)'Range loop` for the second level of :ada:`M`.

- Resetting all values of an array of arrays is very similar to how we do it
  for multidimensional arrays. In fact, we can still use an aggregate with this
  form: :ada:`(others => (others => 0.0))`.
