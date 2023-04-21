Standard library: Numerics
==========================

.. include:: ../../global.txt

The standard library provides support for common numeric operations on
floating-point types as well as on complex types and matrices. In the sections
below, we present a brief introduction to these numeric operations.

Elementary Functions
--------------------

The :ada:`Ada.Numerics.Elementary_Functions` package provides common
operations for floating-point types, such as square root, logarithm,
and the trigonometric functions (e.g., sin, cos). For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Elem_Math

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Elementary_Functions;
    use  Ada.Numerics.Elementary_Functions;

    procedure Show_Elem_Math is
       X : Float;
    begin
       X := 2.0;
       Put_Line ("Square root of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Sqrt (X)));

       X := e;
       Put_Line ("Natural log of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X)));

       X := 10.0 ** 6.0;
       Put_Line ("Log_10      of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X, 10.0)));

       X := 2.0 ** 8.0;
       Put_Line ("Log_2       of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X, 2.0)));

       X := Pi;
       Put_Line ("Cos         of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Cos (X)));

       X := -1.0;
       Put_Line ("Arccos      of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Arccos (X)));
    end Show_Elem_Math;

Here we use the standard :ada:`e` and :ada:`Pi` constants from the
:ada:`Ada.Numerics` package.

The :ada:`Ada.Numerics.Elementary_Functions` package provides operations
for the :ada:`Float` type. Similar packages are available for
:ada:`Long_Float` and :ada:`Long_Long_Float` types. For example, the
:ada:`Ada.Numerics.Long_Elementary_Functions` package offers the same set
of operations for the :ada:`Long_Float` type. In addition, the
:ada:`Ada.Numerics.Generic_Elementary_Functions` package is a generic
version of the package that you can instantiate for custom floating-point
types. In fact, the :ada:`Elementary_Functions` package can be defined as
follows:

.. code-block:: ada

       package Elementary_Functions is new
         Ada.Numerics.Generic_Elementary_Functions (Float);

Random Number Generation
------------------------

The :ada:`Ada.Numerics.Float_Random` package provides a simple random
number generator for the range between 0.0 and 1.0. To use it, declare a
generator :ada:`G`, which you pass to :ada:`Random`. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Float_Random_Num

    with Ada.Text_IO;  use Ada.Text_IO;

    with Ada.Numerics.Float_Random;
    use  Ada.Numerics.Float_Random;

    procedure Show_Float_Random_Num is
       G : Generator;
       X : Uniformly_Distributed;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Float'Image
                     (Uniformly_Distributed'First)
                 & " and "
                 & Float'Image
                     (Uniformly_Distributed'Last)
                 & ":");
       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Float'Image (X));
       end loop;
    end Show_Float_Random_Num;

The standard library also includes a random number generator for discrete
numbers, which is part of the :ada:`Ada.Numerics.Discrete_Random` package.
Since it's a generic package, you have to instantiate it for the desired
discrete type. This allows you to specify a range for the generator. In the
following example, we create an application that displays random integers
between 1 and 10:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Discrete_Random_Num

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics.Discrete_Random;

    procedure Show_Discrete_Random_Num is

       subtype Random_Range is Integer range 1 .. 10;

       package R is new
         Ada.Numerics.Discrete_Random (Random_Range);
       use R;

       G : Generator;
       X : Random_Range;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Integer'Image (Random_Range'First)
                 & " and "
                 & Integer'Image (Random_Range'Last)
                 & ":");

       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Integer'Image (X));
       end loop;
    end Show_Discrete_Random_Num;

Here, package :ada:`R` is instantiated with the :ada:`Random_Range` type,
which has a constrained range between 1 and 10. This allows us to
control the range used for the random numbers. We could easily modify
the application to display random integers between 0 and 20 by
changing the specification of the :ada:`Random_Range` type.  We can also
use floating-point or fixed-point types.

Complex Types
-------------

The :ada:`Ada.Numerics.Complex_Types` package provides support for complex
number types and the :ada:`Ada.Numerics.Complex_Elementary_Functions`
package provides support for common operations on complex number types,
similar to the :ada:`Ada.Numerics.Elementary_Functions` package. Finally,
you can use the :ada:`Ada.Text_IO.Complex_IO` package to perform I/O
operations on complex numbers. In the following example, we declare
variables of the :ada:`Complex` type and initialize them using an aggregate:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Elem_Math

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Complex_Types;
    use  Ada.Numerics.Complex_Types;

    with Ada.Numerics.Complex_Elementary_Functions;
    use  Ada.Numerics.Complex_Elementary_Functions;

    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package C_IO is new
         Ada.Text_IO.Complex_IO (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;
    begin
       X := (2.0, -1.0);
       Y := (3.0,  4.0);

       Put (X);
       Put (" * ");
       Put (Y);
       Put (" is ");
       Put (X * Y);
       New_Line;
       New_Line;

       R  := 3.0;
       Th := Pi / 2.0;
       X  := Compose_From_Polar (R, Th);
       --  Alternatively:
       --  X := R * Exp ((0.0, Th));
       --  X := R * e ** Complex'(0.0, Th);

       Put ("Polar form:    "
            & Float'Image (R)  & " * e**(i * "
            & Float'Image (Th) & ")");
       New_Line;

       Put ("Modulus     of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (abs (X)));
       New_Line;

       Put ("Argument    of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (Argument (X)));
       New_Line;
       New_Line;

       Put ("Sqrt        of ");
       Put (X);
       Put (" is ");
       Put (Sqrt (X));
       New_Line;
    end Show_Elem_Math;

As we can see from this example, all the common operators, such as :ada:`*`
and :ada:`+`, are available for complex types. You also have typical
operations on complex numbers, such as :ada:`Argument` and :ada:`Exp`.  In
addition to initializing complex numbers in the cartesian form using
aggregates, you can do so from the polar form by calling the
:ada:`Compose_From_Polar` function.

The :ada:`Ada.Numerics.Complex_Types` and
:ada:`Ada.Numerics.Complex_Elementary_Functions` packages provide
operations for the :ada:`Float` type. Similar packages are available for
:ada:`Long_Float` and :ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Complex_Types` and
:ada:`Ada.Numerics.Generic_Complex_Elementary_Functions` packages are
generic versions that you can instantiate for custom or pre-defined
floating-point types. For example:

.. code-block:: ada

    with Ada.Numerics.Generic_Complex_Types;
    with Ada.Numerics.Generic_Complex_Elementary_Functions;
    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package Complex_Types is new
         Ada.Numerics.Generic_Complex_Types (Float);
       use Complex_Types;

       package Elementary_Functions is new
         Ada.Numerics.Generic_Complex_Elementary_Functions
           (Complex_Types);
       use Elementary_Functions;

       package C_IO is new Ada.Text_IO.Complex_IO
         (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;

Vector and Matrix Manipulation
------------------------------

The :ada:`Ada.Numerics.Real_Arrays` package provides support for
vectors and matrices. It includes common matrix operations such as
inverse, determinant, eigenvalues in addition to simpler operators
such as matrix addition and multiplication. You can declare vectors
and matrices using the :ada:`Real_Vector` and :ada:`Real_Matrix` types,
respectively.

The following example uses some of the operations from the
:ada:`Ada.Numerics.Real_Arrays` package:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Matrix

    with Ada.Text_IO;  use Ada.Text_IO;

    with Ada.Numerics.Real_Arrays;
    use  Ada.Numerics.Real_Arrays;

    procedure Show_Matrix is

       procedure Put_Vector (V : Real_Vector) is
       begin
          Put ("    (");
          for I in V'Range loop
             Put (Float'Image (V (I)) & " ");
          end loop;
          Put_Line (")");
       end Put_Vector;

       procedure Put_Matrix (M : Real_Matrix) is
       begin
          for I in M'Range (1) loop
             Put ("    (");
             for J in M'Range (2) loop
                Put (Float'Image (M (I, J)) & " ");
             end loop;
             Put_Line (")");
          end loop;
       end Put_Matrix;

       V1       : Real_Vector := (1.0, 3.0);
       V2       : Real_Vector := (75.0, 11.0);

       M1       : Real_Matrix :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));
       M2       : Real_Matrix :=
                    ((31.0, 11.0, 10.0),
                     (34.0, 16.0, 11.0),
                     (32.0, 12.0, 10.0),
                     (31.0, 13.0, 10.0));
       M3       : Real_Matrix := ((1.0, 2.0),
                                  (2.0, 3.0));
    begin
       Put_Line ("V1");
       Put_Vector (V1);
       Put_Line ("V2");
       Put_Vector (V2);
       Put_Line ("V1 * V2 =");
       Put_Line ("    "
                 & Float'Image (V1 * V2));
       Put_Line ("V1 * V2 =");
       Put_Matrix (V1 * V2);
       New_Line;

       Put_Line ("M1");
       Put_Matrix (M1);
       Put_Line ("M2");
       Put_Matrix (M2);
       Put_Line ("M2 * Transpose(M1) =");
       Put_Matrix (M2 * Transpose (M1));
       New_Line;

       Put_Line ("M3");
       Put_Matrix (M3);
       Put_Line ("Inverse (M3) =");
       Put_Matrix (Inverse (M3));
       Put_Line ("abs Inverse (M3) =");
       Put_Matrix (abs Inverse (M3));
       Put_Line ("Determinant (M3) =");
       Put_Line ("    "
                 & Float'Image (Determinant (M3)));
       Put_Line ("Solve (M3, V1) =");
       Put_Vector (Solve (M3, V1));
       Put_Line ("Eigenvalues (M3) =");
       Put_Vector (Eigenvalues (M3));
       New_Line;
    end Show_Matrix;

Matrix dimensions are automatically determined from the aggregate used for
initialization when you don't specify them. You can, however, also use
explicit ranges. For example:

.. code-block:: ada

       M1       : Real_Matrix (1 .. 2, 1 .. 3) :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));

The :ada:`Ada.Numerics.Real_Arrays` package implements operations for the
:ada:`Float` type. Similar packages are available for :ada:`Long_Float` and
:ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Real_Arrays` package is a generic version that
you can instantiate with custom floating-point types. For example, the
:ada:`Real_Arrays` package can be defined as follows:

.. code-block:: ada

       package Real_Arrays is new
         Ada.Numerics.Generic_Real_Arrays (Float);
