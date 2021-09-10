Arrays
======

.. include:: ../../global.txt

Unconstrained Arrays
--------------------

In the
:doc:`Introduction to Ada course <courses/intro-to-ada/chapters/arrays>`,
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

The :doc:`Introduction to Ada course <courses/intro-to-ada/chapters/arrays>`
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

.. todo::

    Complete section!
