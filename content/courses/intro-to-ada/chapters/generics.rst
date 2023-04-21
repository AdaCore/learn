Generics
========

.. include:: ../../global.txt

Introduction
------------

Generics are used for metaprogramming in Ada. They are useful for
abstract algorithms that share common properties with each other.

Either a subprogram or a package can be generic. A generic is declared
by using the keyword :ada:`generic`. For example:

.. raph-amiard: We are lacking a definition/link of metaprogramming.


.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Simple_Generic

    generic
       type T is private;
       --  Declaration of formal types and objects
    --  Below, we could use one of the following:
    --  <procedure | function | package>
    procedure Operator (Dummy : in out T);

    procedure Operator (Dummy : in out T) is
    begin
       null;
    end Operator;

Formal type declaration
-----------------------

Formal types are abstractions of a specific type. For example, we may
want to create an algorithm that works on any integer type, or even on
any type at all, whether a numeric type or not. The following example
declares a formal type :ada:`T` for the :ada:`Set` procedure.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Type_Declaration

    generic
       type T is private;
       --  T is a formal type that indicates that
       --  any type can be used, possibly a numeric
       --  type or possibly even a record type.
    procedure Set (Dummy : T);

    procedure Set (Dummy : T) is
    begin
       null;
    end Set;

The declaration of :ada:`T` as :ada:`private` indicates that you can map
any definite type to it. But you can also restrict the declaration to allow
only some types to be mapped to that formal type.  Here are some
examples:

+-------------------------+---------------------------------------------+
| Formal Type             | Format                                      |
+=========================+=============================================+
| Any type                | :ada:`type T is private;`                   |
+-------------------------+---------------------------------------------+
| Any discrete type       | :ada:`type T is (<>);`                      |
+-------------------------+---------------------------------------------+
| Any floating-point type | :ada:`type T is digits <>;`                 |
+-------------------------+---------------------------------------------+

Formal object declaration
-------------------------

Formal objects are similar to subprogram parameters. They can reference
formal types declared in the formal specification. For example:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Object_Declaration

    generic
       type T is private;
       X : in out T;
       --  X can be used in the Set procedure
    procedure Set (E : T);

    procedure Set (E : T) is
       pragma Unreferenced (E, X);
    begin
       null;
    end Set;

Formal objects can be either input parameters or specified using the
:ada:`in out` mode.

Generic body definition
-----------------------

We don't repeat the :ada:`generic` keyword for the body declaration of a
generic subprogram or package.  Instead, we start with the actual
declaration and use the generic types and objects we declared. For example:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Body_Definition

    generic
       type T is private;
       X : in out T;
    procedure Set (E : T);

    procedure Set (E : T) is
    --  Body definition: "generic" keyword
    --  is not used
    begin
       X := E;
    end Set;

Generic instantiation
---------------------

Generic subprograms or packages can't be used directly. Instead, they
need to be instantiated, which we do using the :ada:`new` keyword, as
shown in the following example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Instantiation

    generic
       type T is private;
       X : in out T;
       --  X can be used in the Set procedure
    procedure Set (E : T);

    procedure Set (E : T) is
    begin
       X := E;
    end Set;

    with Ada.Text_IO; use Ada.Text_IO;
    with Set;

    procedure Show_Generic_Instantiation is

       Main    : Integer := 0;
       Current : Integer;

       procedure Set_Main is new Set (T => Integer,
                                      X => Main);
       --  Here, we map the formal parameters to
       --  actual types and objects.
       --
       --  The same approach can be used to
       --  instantiate functions or packages, e.g.:
       --
       --  function Get_Main is new ...
       --  package Integer_Queue is new ...

    begin
       Current := 10;

       Set_Main (Current);
       Put_Line ("Value of Main is "
                 & Integer'Image (Main));
    end Show_Generic_Instantiation;

In the example above, we instantiate the procedure :ada:`Set` by mapping the
formal parameters :ada:`T` and :ada:`X` to actual existing elements, in this case
the :ada:`Integer` type and the :ada:`Main` variable.


Generic packages
----------------

The previous examples focused on generic subprograms. In this section,
we look at generic packages. The syntax is similar to that used for
generic subprograms: we start with the :ada:`generic` keyword and
continue with formal declarations. The only difference is that
:ada:`package` is specified instead of a subprogram keyword.

Here's an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Package

    generic
       type T is private;
    package Element is

       procedure Set (E : T);
       procedure Reset;
       function Get return T;
       function Is_Valid return Boolean;

       Invalid_Element : exception;

    private
       Value : T;
       Valid : Boolean := False;
    end Element;

    package body Element is

       procedure Set (E : T) is
       begin
          Value := E;
          Valid := True;
       end Set;

       procedure Reset is
       begin
          Valid := False;
       end Reset;

       function Get return T is
       begin
          if not Valid then
             raise Invalid_Element;
          end if;
          return Value;
       end Get;

       function Is_Valid return Boolean is (Valid);
    end Element;

    with Ada.Text_IO; use Ada.Text_IO;
    with Element;

    procedure Show_Generic_Package is

       package I is new Element (T => Integer);

       procedure Display_Initialized is
       begin
          if I.Is_Valid then
             Put_Line ("Value is initialized");
          else
             Put_Line ("Value is not initialized");
          end if;
       end Display_Initialized;

    begin
       Display_Initialized;

       Put_Line ("Initializing...");
       I.Set (5);
       Display_Initialized;
       Put_Line ("Value is now set to "
                 & Integer'Image (I.Get));

       Put_Line ("Reseting...");
       I.Reset;
       Display_Initialized;

    end Show_Generic_Package;

In the example above, we created a simple container named :ada:`Element`,
with just one single element. This container tracks whether the
element has been initialized or not.

After writing package definition, we create the instance :ada:`I` of the
:ada:`Element`. We use the instance by calling the package subprograms
(:ada:`Set`, :ada:`Reset`, and :ada:`Get`).

.. _Intro_Ada_Formal_Subprograms:

Formal subprograms
------------------

In addition to formal types and objects, we can also declare formal
subprograms or packages. This course only describes formal subprograms;
formal packages are discussed in the advanced course.

We use the :ada:`with` keyword to declare a formal subprogram. In the
example below, we declare a formal function (:ada:`Comparison`) to be
used by the generic procedure :ada:`Check`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Subprogram

    generic
       Description : String;
       type T is private;
       with function Comparison (X, Y : T)
                                 return Boolean;
    procedure Check (X, Y : T);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Check (X, Y : T) is
       Result : Boolean;
    begin
       Result := Comparison (X, Y);
       if Result then
          Put_Line
            ("Comparison ("
             & Description
             & ") between arguments is OK!");
       else
          Put_Line
            ("Comparison ("
             & Description
             & ") between arguments is not OK!");
       end if;
    end Check;

    with Check;

    procedure Show_Formal_Subprogram is

       A, B : Integer;

       procedure Check_Is_Equal is new
         Check (Description => "equality",
                T           => Integer,
                Comparison  => Standard."=");
       --  Here, we are mapping the standard
       --  equality operator for Integer types to
       --  the Comparison formal function
    begin
       A := 0;
       B := 1;
       Check_Is_Equal (A, B);
    end Show_Formal_Subprogram;

Example: I/O instances
----------------------

Ada offers generic I/O packages that can be instantiated for standard and
derived types. One example is the generic :ada:`Float_IO` package, which
provides procedures such as :ada:`Put` and :ada:`Get`. In fact,
:ada:`Float_Text_IO` |mdash| available from the standard library |mdash| is an
instance of the :ada:`Float_IO` package, and it's defined as:

.. code-block:: ada

    with Ada.Text_IO;

    package Ada.Float_Text_IO is new Ada.Text_IO.Float_IO (Float);

You can use it directly with any object of floating-point type. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Float_Text_IO

    with Ada.Float_Text_IO;

    procedure Show_Float_Text_IO is
       X : constant Float := 2.5;

       use Ada.Float_Text_IO;
    begin
       Put (X);
    end Show_Float_Text_IO;

Instantiating generic I/O packages can be useful for derived types. For example,
let's create a new type :ada:`Price` that must be displayed with two decimal
digits after the point, and no exponent.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Float_IO_Inst

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Float_IO_Inst is

       type Price is digits 3;

       package Price_IO is new
         Ada.Text_IO.Float_IO (Price);

       P : Price;
    begin
       --  Set to zero => don't display exponent
       Price_IO.Default_Exp  := 0;

       P := 2.5;
       Price_IO.Put (P);
       New_Line;

       P := 5.75;
       Price_IO.Put (P);
       New_Line;
    end Show_Float_IO_Inst;

By adjusting :ada:`Default_Exp` from the :ada:`Price_IO` instance to *remove*
the exponent, we can control how variables of :ada:`Price` type are displayed.
Just as a side note, we could also have written:

.. code-block:: ada

    -- [...]

       type Price is new Float;

       package Price_IO is new
         Ada.Text_IO.Float_IO (Price);

    begin
       Price_IO.Default_Aft  := 2;
       Price_IO.Default_Exp  := 0;

In this case, we're ajusting :ada:`Default_Aft`, too, to get two decimal digits
after the point when calling :ada:`Put`.

In addition to the generic :ada:`Float_IO` package, the following generic
packages are available from :ada:`Ada.Text_IO`:

- :ada:`Enumeration_IO` for enumeration types;
- :ada:`Integer_IO` for integer types;
- :ada:`Modular_IO` for modular types;
- :ada:`Fixed_IO` for fixed-point types;
- :ada:`Decimal_IO` for decimal types.

In fact, we could rewrite the example above using decimal types:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Decimal_IO_Inst

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_IO_Inst is

       type Price is delta 10.0 ** (-2) digits 12;

       package Price_IO is new
         Ada.Text_IO.Decimal_IO (Price);

       P : Price;
    begin
       Price_IO.Default_Exp  := 0;

       P := 2.5;
       Price_IO.Put (P);
       New_Line;

       P := 5.75;
       Price_IO.Put (P);
       New_Line;
    end Show_Decimal_IO_Inst;

Example: ADTs
-------------

An important application of generics is to model abstract data types
(ADTs). In fact, Ada includes a library with numerous ADTs using
generics: :ada:`Ada.Containers` (described in the :ref:`containers
section <Intro_Ada_Containers>`).

A typical example of an ADT is a stack:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Stack

    generic
       Max : Positive;
       type T is private;
    package Stacks is

       type Stack is limited private;

       Stack_Underflow, Stack_Overflow : exception;

       function Is_Empty (S : Stack) return Boolean;

       function Pop (S : in out Stack) return T;

       procedure Push (S : in out Stack;
                       V :        T);

    private

       type Stack_Array is
         array (Natural range <>) of T;

       Min : constant := 1;

       type Stack is record
          Container : Stack_Array (Min .. Max);
          Top       : Natural := Min - 1;
       end record;

    end Stacks;

    package body Stacks is

       function Is_Empty (S : Stack) return Boolean is
         (S.Top < S.Container'First);

       function Is_Full (S : Stack) return Boolean is
         (S.Top >= S.Container'Last);

       function Pop (S : in out Stack) return T is
       begin
          if Is_Empty (S) then
             raise Stack_Underflow;
          else
             return X : T do
                X     := S.Container (S.Top);
                S.Top := S.Top - 1;
             end return;
          end if;
       end Pop;

       procedure Push (S : in out Stack;
                       V :        T) is
       begin
          if Is_Full (S) then
             raise Stack_Overflow;
          else
             S.Top               := S.Top + 1;
             S.Container (S.Top) := V;
          end if;
       end Push;

    end Stacks;

    with Ada.Text_IO; use Ada.Text_IO;
    with Stacks;

    procedure Show_Stack is

       package Integer_Stacks is new
         Stacks (Max => 10,
                 T   => Integer);
       use Integer_Stacks;

       Values : Integer_Stacks.Stack;

    begin
       Push (Values, 10);
       Push (Values, 20);

       Put_Line ("Last value was "
                 & Integer'Image (Pop (Values)));
    end Show_Stack;

In this example, we first create a generic stack package (:ada:`Stacks`)
and then instantiate it to create a stack of up to 10 integer values.

Example: Swap
-------------

Let's look at a simple procedure that swaps variables of type
:ada:`Color`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Non_Generic_Swap_Colors

    package Colors is
       type Color is (Black, Red, Green,
                      Blue, White);

       procedure Swap_Colors (X, Y : in out Color);
    end Colors;

    package body Colors is

       procedure Swap_Colors (X, Y : in out Color) is
          Tmp : constant Color := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap_Colors;

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Test_Non_Generic_Swap_Colors is
       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));
    end Test_Non_Generic_Swap_Colors;

In this example, :ada:`Swap_Colors` can only be used for the :ada:`Color`
type.  However, this algorithm can theoretically be used for any type,
whether an enumeration type or a complex record type with many
elements. The algorithm itself is the same: it's only the type that
differs. If, for example, we want to swap variables of :ada:`Integer`
type, we don't want to duplicate the implementation. Therefore, such
an algorithm is a perfect candidate for abstraction using generics.

In the example below, we create a generic version of :ada:`Swap_Colors`
and name it :ada:`Generic_Swap`. This generic version can operate on any
type due to the declaration of formal type :ada:`T`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Swap_Colors

    generic
       type T is private;
    procedure Generic_Swap (X, Y : in out T);

    procedure Generic_Swap (X, Y : in out T) is
       Tmp : constant T := X;
    begin
       X := Y;
       Y := Tmp;
    end Generic_Swap;

    with Generic_Swap;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       procedure Swap_Colors is new
         Generic_Swap (T => Color);

    end Colors;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Colors;       use Colors;

    procedure Test_Swap_Colors is
       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));
    end Test_Swap_Colors;

As we can see in the example, we can create the same :ada:`Swap_Colors`
procedure as we had in the non-generic version of the algorithm by
declaring it as an instance of the generic :ada:`Generic_Swap` procedure. We
specify that the generic :ada:`T` type will be mapped to the :ada:`Color` type
by passing it as an argument to the :ada:`Generic_Swap` instantiation.

Example: Reversing
------------------

The previous example, with an algorithm to swap two values, is one of the
simplest examples of using generics. Next we study an algorithm for
reversing elements of an array. First, let's start with a non-generic
version of the algorithm, one that works specifically for the :ada:`Color`
type:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Non_Generic_Reverse_Colors

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It (X : in out Color_Array);

    end Colors;

    package body Colors is

       procedure Reverse_It (X : in out Color_Array) is
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2 loop
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
    with Colors;      use Colors;

    procedure Test_Non_Generic_Reverse_Colors is

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

    end Test_Non_Generic_Reverse_Colors;

The procedure :ada:`Reverse_It` takes an array of colors, starts by
swapping the first and last elements of the array, and continues doing that
with successive elements until it reaches the middle of array. At that
point, the entire array has been reversed, as we see from the output of the
test program.

To abstract this procedure, we declare formal types for three components of
the algorithm:

    - the elements of the array (:ada:`Color` type in the example)

    - the range used for the array (:ada:`Integer` range in the example)

    - the actual array type (:ada:`Color_Array` type in the example)

.. _Intro_Ada_Generic_Reverse_Colors_Example:

This is a generic version of the algorithm:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Reverse_Colors

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
    procedure Generic_Reverse (X : in out Array_T);

    procedure Generic_Reverse (X : in out Array_T) is
    begin
       for I in X'First ..
                (X'Last + X'First) / 2 loop
          declare
             Tmp     : T;
             X_Left  : T
               renames X (I);
             X_Right : T
               renames X (X'Last + X'First - I);
          begin
             Tmp     := X_Left;
             X_Left  := X_Right;
             X_Right := Tmp;
          end;
       end loop;
    end Generic_Reverse;

    with Generic_Reverse;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It is new
         Generic_Reverse (T       => Color,
                          Index   => Integer,
                          Array_T => Color_Array);

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Test_Reverse_Colors is

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_It (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

As mentioned above, we're abstracting three components of the algorithm:

    - the :ada:`T` type abstracts the elements of the array

    - the :ada:`Index` type abstracts the range used for the array

    - the :ada:`Array_T` type abstracts the array type and uses the
      formal declarations of the :ada:`T` and :ada:`Index` types.

Example: Test application
-------------------------

In the previous example we've focused only on abstracting the reversing
algorithm itself. However, we could have decided to also abstract our small
test application. This could be useful if we, for example, decide to test
other procedures that change elements of an array.

In order to do this, we again have to choose the elements to abstract. We
therefore declare the following formal parameters:

    - :ada:`S`: the string containing the array name

    - a function :ada:`Image` that converts an element of type :ada:`T` to a
      string

    - a procedure :ada:`Test` that performs some operation on the array

Note that :ada:`Image` and :ada:`Test` are examples of formal subprograms and
:ada:`S` is an example of a formal object.

Here is a version of the test application making use of the generic
:ada:`Perform_Test` procedure:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Reverse_Colors_2

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
    procedure Generic_Reverse (X : in out Array_T);

    procedure Generic_Reverse (X : in out Array_T) is
    begin
       for I in X'First ..
                (X'Last + X'First) / 2 loop
          declare
             Tmp     : T;
             X_Left  : T
               renames X (I);
             X_Right : T
               renames X (X'Last + X'First - I);
          begin
             Tmp     := X_Left;
             X_Left  := X_Right;
             X_Right := Tmp;
          end;
       end loop;
    end Generic_Reverse;

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
       S : String;
       with function Image (E : T) return String is <>;
       with procedure Test (X : in out Array_T);
    procedure Perform_Test (X : in out Array_T);

    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Perform_Test (X : in out Array_T) is
    begin
       for C of X loop
          Put_Line (S & ": " & Image (C));
       end loop;

       New_Line;
       Put_Line ("Testing " & S & "...");
       New_Line;
       Test (X);

       for C of X loop
          Put_Line (S & ": " & Image (C));
       end loop;
    end Perform_Test;

    with Generic_Reverse;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It is new
         Generic_Reverse (T       => Color,
                          Index   => Integer,
                          Array_T => Color_Array);

    end Colors;

    with Colors;       use Colors;
    with Perform_Test;

    procedure Test_Reverse_Colors is

       procedure Perform_Test_Reverse_It is new
         Perform_Test (T       => Color,
                       Index   => Integer,
                       Array_T => Color_Array,
                       S       => "My_Color",
                       Image   => Color'Image,
                       Test    => Reverse_It);

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       Perform_Test_Reverse_It (My_Colors);
    end Test_Reverse_Colors;

In this example, we create the procedure
:ada:`Perform_Test_Reverse_It` as an instance of the generic
procedure (:ada:`Perform_Test`). Note that:

    - For the formal :ada:`Image` function, we use the :ada:`'Image` attribute
      of the :ada:`Color` type

    - For the formal :ada:`Test` procedure, we reference the
      :ada:`Reverse_Array` procedure from the package.
