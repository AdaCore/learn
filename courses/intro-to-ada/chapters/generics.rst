Generics
========
:code-config:`reset_accumulator=True`

Introduction
------------

Generics are used for metaprogramming in Ada. They are useful for abstract
algorithms that share common properties.

Generics can be used for subprograms or packages. A generic is declared
by using the keyword :ada:`generic`. For example:

.. raph-amiard: We are lacking a definition/link of metaprogramming.

.. code:: ada

    procedure Show_Simple_Generic is

       generic
          type T is private;
          --  Declaration of formal types and objects
       procedure Operator (X : in out T);
       --  This could be one of the following:
       --  <procedure | function | package>

       procedure Operator (X : in out T) is null;

    begin
       null;
    end Show_Simple_Generic;

Formal type declaration
~~~~~~~~~~~~~~~~~~~~~~~

Formal types are abstractions of a specific type. We may, for example,
want to create an algorithm that works on any integer type, or even on
any type at all, independently of being a numeric type or not. The
following example declares a formal type ``T`` for the ``Set`` procedure.

.. code:: ada

    procedure Show_Formal_Type_Declaration is

       generic
          type T is private;
          --  T is a formal type that indicates that any type can be used,
          --  be it a numeric type or, for example, a record.
       procedure Set (E : T);

       procedure Set (E : T) is null;

    begin
       null;
    end Show_Formal_Type_Declaration;

The declaration of ``T`` as :ada:`private` indicates that any type can be
mapped to it. These are some examples of formal types:

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
~~~~~~~~~~~~~~~~~~~~~~~~~

Formal objects are similar to subprogram parameters. Also, they can make
use of formal types declared in the formal specification. For example:

.. code:: ada

    procedure Show_Formal_Object_Declaration is

       generic
          type T is private;
          X : in out T;
          --  X can be used in the Set procedure
       procedure Set (E : T);

       procedure Set (E : T) is null;

    begin
       null;
    end Show_Formal_Object_Declaration;

Formal objects can be either just input parameters or use the
:ada:`in out` mode.

Generic body definition
~~~~~~~~~~~~~~~~~~~~~~~

For the body declaration of a generic subprogram or package, we don't
repeat the :ada:`generic` keyword: we simply start with the actual
declaration and make use of the generic types and objects that we
declared. For example:

.. code:: ada

    procedure Show_Generic_Body_Definition is

       generic
          type T is private;
          X : in out T;
       procedure Set (E : T);

       procedure Set (E : T) is
       --  Body definition: "generic" keyword is not used
       begin
          X := E;
       end Set;
    begin
       null;
    end Show_Generic_Body_Definition;

Generic instantiation
~~~~~~~~~~~~~~~~~~~~~

Generic subprograms or packages cannot be used directly. Instead, they
need to be instantiated. The instantiation is done by using the :ada:`new`
keyword, as illustrated in the following example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Generic_Instantiation is

       generic
          type T is private;
          X : in out T;
          --  X can be used in the Set procedure
       procedure Set (E : T);

       procedure Set (E : T) is
       begin
          X := E;
       end Set;

       Main    : Integer := 0;
       Current : Integer;

       procedure Set_Main is new Set (T => Integer,
                                      X => Main);
       --  Here, we map the formal parameters to actual types and objects.
       --
       --  The same approach can be used to instantiate functions or
       --  packages, e.g.:
       --  function Get_Main is new ...
       --  package Integer_Queue is new ...

    begin
       Current := 10;

       Set_Main (Current);
       Put_Line ("Value of Main is " & Integer'Image (Main));
    end Show_Generic_Instantiation;

In the example above, we instantiate the procedure ``Set`` by mapping the
formal parameters ``T`` and ``X`` to actual existing elements: the
:ada:`Integer` type and the ``Main`` variable.


Generic packages
~~~~~~~~~~~~~~~~

The previous examples focused on generic subprograms. In this section, we
will look into generic packages. In general, the syntax is not different
from the one used for generic subprograms: it starts with the :ada:`generic`
keyword and continues with formal declarations. The only difference is
that a :ada:`package` is specified instead of a subprogram.

This is an example:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Generic_Package is

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
       Put_Line ("Value is now set to " & Integer'Image (I.Get));

       Put_Line ("Reseting...");
       I.Reset;
       Display_Initialized;

    end Show_Generic_Package;

In the example above, we create a very simple container named ``Element``
for just a single element. This container keeps track whether the element
has been initialized or not. After the package definition, we create the
instance ``I`` of the ``Element``. We can then use the instance by calling
the package subprograms (e.g.: ``Set``, ``Get`` and ``Reset``).

Formal subprograms
~~~~~~~~~~~~~~~~~~

In addition to formal types and objects, we can also declare formal
subprograms or packages. This course only describes formal subprograms.
Formal packages are discussed in the advanced course.

In order to declare a formal subprogram, we make use of the :ada:`with`
keyword. In the example below, we declare a formal function
(``Comparison``) that is used by the generic procedure ``Check``.

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Formal_Subprogram is

       generic
          Description : String;
          type T is private;
          with function Comparison (X, Y : T) return Boolean;
       procedure Check (X, Y : T);

       procedure Check (X, Y : T) is
          Result : Boolean;
       begin
          Result := Comparison (X, Y);
          if Result then
             Put_Line ("Comparison (" & Description &
                       ") between arguments is OK!");
          else
             Put_Line ("Comparison (" & Description &
                       ") between arguments is not OK!");
          end if;
       end Check;

       A, B : Integer;

       procedure Check_Is_Equal is new Check (Description => "equality",
                                              T           => Integer,
                                              Comparison  => Standard."=");
       --  Here, we are mapping the standard equality operator for Integer
       --  types to the Comparison function
    begin
       A := 0;
       B := 1;
       Check_Is_Equal (A, B);
    end Show_Formal_Subprogram;

Examples of using generics
--------------------------

In this section, we will look into examples and strategies for abstracting
algorithms using generics.

Application: ADTs
~~~~~~~~~~~~~~~~~

An important application of generics is to model abstract data types
(ADTs). In fact, Ada includes a library with all sorts of ADTs using
generics: :ada:`Ada.Containers` (described in the
:ref:`containers section <Containers>`).

A typical example of an ADT is a stack:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Stack is

       generic
          Max : Positive;
          type T is private;
       package Stacks is

          type Stack is limited private;

          function Is_Empty (S : Stack) return Boolean;

          function Pop (S : in out Stack) return T;

          procedure Push (S : in out Stack; V : T);

       private

          type Stack_Array is array (Natural range <>) of T;

          type Stack is record
             Container : Stack_Array (1 .. Max);
             Top       : Natural := 0;
          end record;

       end Stacks;

       package body Stacks is

          function Is_Empty (S : Stack) return Boolean is
            (S.Top < S.Container'First);

          function Pop (S : in out Stack) return T is
          begin
             return X : T do
                X     := S.Container (S.Top);
                S.Top := S.Top - 1;
             end return;
          end Pop;

          procedure Push (S : in out Stack;
                          V : T) is
          begin
             S.Top               := S.Top + 1;
             S.Container (S.Top) := V;
          end Push;

       end Stacks;

       package Integer_Stacks is new Stacks (Max => 10,
                                             T   => Integer);
       use Integer_Stacks;

       Values : Integer_Stacks.Stack;

    begin
       Push (Values, 10);
       Push (Values, 20);

       Put_Line ("Last value was " & Integer'Image (Pop (Values)));
    end Show_Stack;

In this example, we first create a generic stack package (``Stacks``).
Then, we instantiate it in order to create a stack for 10 positions of
integer values.

Abstracting a swap algorithm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's first look into a simple procedure that swaps variables of the type
``Color``:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Non_Generic_Swap_Colors is
       type Color is (Black, Red, Green, Blue, White);

       procedure Swap_Colors (X, Y : in out Color);

       procedure Swap_Colors (X, Y : in out Color) is
          Tmp : constant Color := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap_Colors;

       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));
    end Test_Non_Generic_Swap_Colors;

In this example, ``Swap_Colors`` can only be used for the ``Color`` type.
However, a swapping algorithm can theoretically be used for any type, be
it an enumeration or a complex record type with many elements. The
algorithm itself is the same, just the types are different. Also, we don't
want to duplicate the implementation for swapping variables of :ada:`Integer`
type, for example. Therefore, such an algorithm is an perfect candidate
for abstraction using generics.

In the example below, we create a generic version of the ``Swap_Colors``
and name it ``Generic_Swap``. This generic version can work on any type.
This is achieved by the declaration of the formal type ``T``.

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Swap_Colors is
       generic
          type T is private;
       procedure Generic_Swap (X, Y : in out T);

       procedure Generic_Swap (X, Y : in out T) is
          Tmp : constant T := X;
       begin
          X := Y;
          Y := Tmp;
       end Generic_Swap;

       type Color is (Black, Red, Green, Blue, White);

       procedure Swap_Colors is new Generic_Swap (T => Color);

       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is " & Color'Image (A));
       Put_Line ("Value of B is " & Color'Image (B));
       Put_Line ("Value of C is " & Color'Image (C));
    end Test_Swap_Colors;

As we can see in the example, we can create the same ``Swap_Colors``
procedure as we had in the non-generic version of the algorithm by
declaring it as an instance of generic ``Generic_Swap`` procedure. As an
argument to the ``Generic_Swap`` procedure, we define that the ``T`` type
will be mapped to the ``Color`` type.

Abstracting a reversing algorithm
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The previous example using a swapping algorithm is one of the simplest
examples of generic algorithms. Now, we will look into an algorithm for
reversing elements of an array. First, let's start with a non-generic
version of the algorithm that works specifically for the ``Color`` type:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Non_Generic_Reverse_Colors is
       type Color is (Black, Red, Green, Blue, White);

       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array (X : in out Color_Array);

       procedure Reverse_Color_Array (X : in out Color_Array) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : Color;
                X_Left  : Color renames X (I);
                X_Right : Color renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_Color_Array;

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_Color_Array (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Non_Generic_Reverse_Colors;

The procedure ``Reverse_Color_Array`` takes an array of colors and starts
by swapping the first and last elements of the array, and continues doing
that with the next elements until the middle of array. At this point, the
whole array has been reversed, as we can see in the text output of the
test application.

In order to abstract this procedure, we will declare formal types for
three components of the algorithm:

    - the elements of the array (``Color`` type in the example)

    - the range used for the array (``Integer`` range in the example)

    - the actual array type (``Color_Array`` type in the example)

This is a generic version of the algorithm:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Reverse_Colors is
       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
       procedure Generic_Reverse_Array (X : in out Array_T);

       procedure Generic_Reverse_Array (X : in out Array_T) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Generic_Reverse_Array;

       type Color is (Black, Red, Green, Blue, White);
       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array is new Generic_Reverse_Array
         (T => Color, Index => Integer, Array_T => Color_Array);

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_Color_Array (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

As mentioned above, we're abstracting three components of the algorithm:

    - the ``T`` type abstracts the elements of the array

    - the ``Index`` type abstracts the range used for the array

    - the ``Array_T`` type abstracts the array type and makes use of the
      formal declarations of the ``T`` and ``Index`` types.

Abstracting the test application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, we've focused only on abstracting the reversing
algorithm. However, we could have decided to also abstract our little
test application. This could be useful if we, for example, decide to
test other procedures that change elements of an array.

In order to achieve this, we have to abstract some elements. We
will therefore declare the following formal parameters:

    - ``S``: the string containing the array name

    - a function ``Image`` that converts an element of type ``T`` to a
      string

    - a procedure ``Test`` that performs some operation on the array

Note that ``Image`` and ``Test`` are examples of formal subprograms.
Also, note that ``S`` is an example of a formal object.

This is a version of the test application that makes use of the generic
``Perform_Test`` procedure:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Reverse_Colors is

       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
       procedure Generic_Reverse_Array (X : in out Array_T);

       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
          S : String;
          with function Image (E : T) return String is <>;
          with procedure Test (X : in out Array_T);
       procedure Perform_Test (X : in out Array_T);

       procedure Generic_Reverse_Array (X : in out Array_T) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Generic_Reverse_Array;

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

       type Color is (Black, Red, Green, Blue, White);
       type Color_Array is array (Integer range <>) of Color;

       procedure Reverse_Color_Array is new
         Generic_Reverse_Array (T       => Color,
                                Index   => Integer,
                                Array_T => Color_Array);

       procedure Perform_Test_Reverse_Color_Array is new
         Perform_Test (T       => Color,
                       Index   => Integer,
                       Array_T => Color_Array,
                       S       => "My_Color",
                       Image   => Color'Image,
                       Test    => Reverse_Color_Array);

       My_Colors : Color_Array (1 .. 5) := (Black, Red, Green, Blue, White);

    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors;

In this example, we create the procedure
``Perform_Test_Reverse_Color_Array`` as an instance of the generic
procedure (``Perform_Test``). Note that:

    - For the formal ``Image`` function, we make use of the ``'Image``
      attribute of the ``Color`` type

    - For the formal ``Test`` procedure, we reference the
      ``Reverse_Array`` procedure from the package.
