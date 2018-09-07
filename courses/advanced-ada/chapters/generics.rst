:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Generics
========

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Formal packages
---------------

Abstracting definitions into packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section and in the next ones, we will reuse the generic
reversing algorithm that we discussed in the chapter about generics
from the introductory course (:doc:`../../intro-to-ada/generics`). In that
example, we were declaring three formal types for the
``Generic_Reverse_Array`` procedure. However, we could abstract the array
definition into a separate package and reuse it for the generic procedure.
This could be potentially useful in case we want to create more generic
procedures for the same array.

In order to achieve this, we start by first specifying a generic package
that contains the generic array type definition:

.. code:: ada

    generic
       type T is private;
       type Index is range <>;
    package Simple_Generic_Array_Pkg is
       type Array_T is array (Index range <>) of T;
    end Simple_Generic_Array_Pkg;

As you can see, this definition is the same that we've seen in the
previous section: we just moved it into a separate package. Now, we have a
definition of ``Array_T`` that can be reused in multiple places.

The next step is to reuse the ``Simple_Generic_Array_Pkg`` package in the
``Generic_Reverse_Array`` procedure. By doing this, we can eliminate the
declaration of the ``Index`` and ``Array_T`` types that we had before,
since the definition will come from the ``Simple_Generic_Array_Pkg``
package.

In order to reuse the ``Simple_Generic_Array_Pkg`` package in the
``Generic_Reverse_Array`` procedure, we need to use a formal package
declaration in the form:

.. code-block:: ada

    with package P is new Simple_Generic_Array_Pkg(<params>)

This will allow us to reuse definitions from the generic package.

This is the updated version of the our test application for the reversing
algorithm:

.. code:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Simple_Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Simple_Pkg is

       generic
          type T is private;
          with package P is new Simple_Generic_Array_Pkg (T => T, others => <>);
       procedure Reverse_Array (X : in out P.Array_T);

       procedure Reverse_Array (X : in out P.Array_T) is
          use P;
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
       end Reverse_Array;

       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new Simple_Generic_Array_Pkg (T => Color, Index => Integer);

       procedure Reverse_Color_Array is new Reverse_Array (T => Color, P => Color_Pkg);

       My_Colors : Color_Pkg.Array_T (1 .. 5) := (Black, Red, Green, Blue, White);
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

    end Test_Reverse_Colors_Simple_Pkg;

In this example, we're first instantiating the
``Simple_Generic_Array_Pkg`` package, thereby creating the ``Color_Pkg``
package. We then proceed to use this ``Color_Pkg`` package in the
instantiation of the generic ``Reverse_Array`` procedure. Also, in the
declaration of the ``My_Colors`` array, we make use of the array type
definition from the ``Color_Pkg`` package.

Abstracting procedures into packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, we moved the array type definition into a
separate package, but left the generic procedure (``Reverse_Array``) in
the test application. We can also move the generic procedure into the
generic package:

.. code:: ada

    generic
       type T is private;
       type Index is range <>;
    package Generic_Array_Pkg is
       type Array_T is array (Index range <>) of T;

       procedure Reverse_Array (X : in out Array_T);
    end Generic_Array_Pkg;

The advantage of this approach is that we don't need to repeat the formal
declaration for the ``Reverse_Array`` procedure. Also, this simplifies the
instantiation in the test application.

However, the disadvantage of this approach is that it also increases code
size: every instantiation of the generic package generates code for each
subprogram from the package. Also, compilation time tends to increase
significantly. Therefore, developers must be careful when considering
this approach.

Because we have a procedure declaration in the generic package, we need a
corresponding package body. Here, we can simply reuse the existing code
and move the procedure into the package body. In the test application, we
just instantiate the ``Generic_Array_Pkg`` package and make use of the
array type (``Array_T``) and the procedure (``Reverse_Array``):

.. code-block:: ada

       Color_Pkg.Reverse_Array (My_Colors);

Abstracting the test application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous examples, we've focused only on abstracting the reversing
algorithm. However, we could have decided to also abstract our little
test application. This could be useful if we, for example, decide to
test other procedures that change elements of an array.

In order to achieve this, we have to abstract quite a few elements. We
will therefore declare the following formal parameters:

    - ``S``: the string containing the array name

    - an instance of the ``Generic_Array_Pkg`` package (which was
      implemented in the previous section)

    - a function ``Image`` that converts an element of type ``T`` to a
      string

    - a procedure ``Pkg_Test`` that performs some operation on the array

Note that ``Image`` and ``Pkg_Test`` are examples of formal subprograms.
Also, note that ``S`` is an example of a formal object.

This is a version of the test application that makes use of the generic
``Perform_Test`` procedure:

.. code:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Pkg is

       generic
          S : String;
          with package Array_Pkg is new Generic_Array_Pkg (<>);
          use Array_Pkg;
          with function Image (E : in T) return String is <>;
          with procedure Pkg_Test (X : in out Array_T);
       procedure Perform_Test (X : in out Array_T);

       procedure Perform_Test (X : in out Array_T) is
       begin
          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;

          New_Line;
          Put_Line ("Performing operation on " & S & "...");
          New_Line;
          Pkg_Test (X);

          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;
       end Perform_Test;

       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new Generic_Array_Pkg (T => Color, Index => Integer);

       My_Colors : Color_Pkg.Array_T (1 .. 5) := (Black, Red, Green, Blue, White);

       procedure Perform_Test_Reverse_Color_Array is new Perform_Test
         (S         => "My_Color",
          Image     => Color'Image,
          Array_Pkg => Color_Pkg,
          Pkg_Test  => Color_Pkg.Reverse_Array);
    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors_Pkg;

In this example, we create the procedure
``Perform_Test_Reverse_Color_Array`` as an instance of the generic
procedure (``Perform_Test``). Note that:

    - For the formal ``Image`` function, we make use of the ``'Image``
      attribute of the ``Color`` type

    - For the formal ``Pkg_Test`` procedure, we reference the
      ``Reverse_Array`` procedure from the package.

Note that this example includes a formal package declaration:

.. code-block:: ada

    with package Array_Pkg is new Generic_Array_Pkg (<>);

Previously, we've seen package instantiations that define the elements.
For example:

.. code-block:: ada

    package Color_Pkg is new Generic_Array_Pkg (T => Color, Index => Integer);

In this case, however, we're using simply ``(<>)``. This means that the
generic procedure (``Perform_Test``) will accept the default definition
used for the instance of ``Generic_Array_Pkg``.

Abstracting test application by cascading generic packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the code example from the previous section, we declared four formal
parameters for the ``Perform_Test`` procedure. Two of them are directly
related to the array that we're using for the test:

    - ``S``: the string containing the array name

    - the function ``Image`` that converts an elements of the array to a
      string

We could abstract our implementation even further by moving these elements
into a separate package named ``Generic_Array_Bundle`` and reference the
``Generic_Array_Pkg`` there. This would create a chain of generic
packages:

.. code-block:: ada

    Generic_Array_Bundle <= Generic_Array_Pkg

This strategy demonstrates that, in Ada, it is really straightforward to
make use of generics in order to abstracts algorithms.

First, let us define the new ``Generic_Array_Bundle`` package, which
references the ``Generic_Array_Pkg`` package and the two formal elements
(``S`` and ``Image``) mentioned previously:

.. code:: ada

    with Generic_Array_Pkg;

    generic
       S : String;
       with package Array_Pkg is new Generic_Array_Pkg (<>);
       with function Image (E : in Array_Pkg.T) return String is <>;
    package Generic_Array_Bundle is
    end Generic_Array_Bundle;

Then, we update the definition of ``Perform_Test``:

.. code:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;
    with Generic_Array_Bundle;

    procedure Test_Reverse_Colors_Pkg is

       generic
          with package Array_Bundle is new Generic_Array_Bundle (<>);
          use Array_Bundle;
          use Array_Pkg;
          with procedure Pkg_Test (X : in out Array_T);
       procedure Perform_Test (X : in out Array_T);

       procedure Perform_Test (X : in out Array_T) is
       begin
          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;

          New_Line;
          Put_Line ("Reversing " & S & "...");
          New_Line;
          Pkg_Test (X);

          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;
       end Perform_Test;

       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new Generic_Array_Pkg (T => Color, Index => Integer);

       My_Colors : Color_Pkg.Array_T (1 .. 5) := (Black, Red, Green, Blue, White);

       package Color_Array_Bundle is new Generic_Array_Bundle
         (S         => "My_Color",
          Image     => Color'Image,
          Array_Pkg => Color_Pkg);

       procedure Perform_Test_Reverse_Color_Array is new Perform_Test
         (Array_Bundle => Color_Array_Bundle,
          Pkg_Test     => Color_Pkg.Reverse_Array);
    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors_Pkg;

Note that, in this case, we reduce the number of formal parameters to only
two:

    - ``Array_Bundle``: an instance of the new ``Generic_Array_Bundle``
      package

   - the procedure ``Pkg_Test`` that we already had before

We could go even further and move ``Perform_Test`` into a separate
package. However, this will be left as an exercise for the reader.

Formal objects
--------------

-----------------------------------------------------------------------

**Simple example**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    procedure Show_Formal_In_Out_Object is
       generic
          K : in out Integer;
       procedure Increment;

       procedure Increment is
       begin
          K := K + 1;
       end Increment;

       A : Integer := 2;

       procedure Incr_Int is new Increment (K => A);

    begin
       Put_Line ("A: " & Integer'Image (A));
       Incr_Int;
       Put_Line ("A: " & Integer'Image (A));
    end Show_Formal_In_Out_Object;


-----------------------------------------------------------------------

**Simple example without generics**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    procedure Show_Alternative_Formal_Object is
       procedure Increment (K : in out Integer);

       procedure Increment (K : in out Integer) is
       begin
          K := K + 1;
       end Increment;

       A : Integer := 2;

    begin
       Put_Line ("A: " & Integer'Image (A));
       Increment (A);
       Put_Line ("A: " & Integer'Image (A));
    end Show_Alternative_Formal_Object;


-----------------------------------------------------------------------

**Generic package with formal object**

-----------------------------------------------------------------------

.. code:: ada

    generic
       K : in out Integer;
    package Integer_Op_Cnt is

       procedure Increment;
       procedure Decrement;

       function Get_Increment_Count return Natural;
       function Get_Decrement_Count return Natural;

    end Integer_Op_Cnt;

.. code:: ada

    package body Integer_Op_Cnt is

       Incr_Cnt : Natural := 0;
       Decr_Cnt : Natural := 0;

       function Get_Increment_Count return Natural is (Incr_Cnt);
       function Get_Decrement_Count return Natural is (Decr_Cnt);

       procedure Increment is
       begin
          K := K + 1;
          Incr_Cnt := Incr_Cnt + 1;
       end Increment;

       procedure Decrement is
       begin
          K := K - 1;
          Decr_Cnt := Decr_Cnt + 1;
       end Decrement;

    end Integer_Op_Cnt;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;
    with Integer_Op_Cnt;

    procedure Show_Formal_In_Out_Object_Pkg is

       A, B : Integer := 2;

       --  A is now bound to A_Op
       package A_Op is new Integer_Op_Cnt (K => A);
       package B_Op is new Integer_Op_Cnt (K => B);

    begin
       Put_Line ("A: " & Integer'Image (A));
       Put_Line ("B: " & Integer'Image (B));
       A_Op.Increment;
       B_Op.Decrement;
       Put_Line ("A: " & Integer'Image (A));
       Put_Line ("B: " & Integer'Image (B));
       Put_Line ("# Incr A: " & Natural'Image (A_Op.Get_Increment_Count));
       Put_Line ("# Decr A: " & Natural'Image (A_Op.Get_Decrement_Count));
       Put_Line ("# Incr B: " & Natural'Image (B_Op.Get_Increment_Count));
       Put_Line ("# Decr B: " & Natural'Image (B_Op.Get_Decrement_Count));
    end Show_Formal_In_Out_Object_Pkg;


-----------------------------------------------------------------------

**Generic package with formal object: container and operations**

-----------------------------------------------------------------------

.. code:: ada

    package Data_Elements is
       type Data_Element is private;

    private
       type Data_Element is record
          Name : String (1 .. 100);
          Age  : Natural;
       end record;
    end Data_Elements;


.. code:: ada

    with Ada.Containers;
    with Ada.Containers.Vectors;

    with Data_Elements; use Data_Elements;

    package Data is

       type Data_Container is private;

       procedure Insert (C : in out Data_Container;
                         V : Data_Element);
       procedure Get (C     : Data_Container;
                      V     : out Data_Element;
                      Found : out Boolean);

    private

       package Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Data_Element);

       type Data_Container is record
          V : Vectors.Vector;
       end record;

    end Data;

.. code:: ada

    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    with Data; use Data;

    generic
       Container : in out Data_Container;
    package File_Ops is

       procedure Read (S : Stream_Access);

    end File_Ops;

.. code:: ada

    with Data; use Data;

    generic
       Container : in out Data_Container;
       Fast      : Boolean := True;
    package Proc_Ops is

       procedure Process;

    end Proc_Ops;

.. code:: ada

    with Data;     use Data;
    with File_Ops;
    with Proc_Ops;

    package App is

       C : Data_Container;

       package File is new File_Ops (Container => C);
       package Fast_Proc is new Proc_Ops (Container => C,
                                          Fast      => True);
       package Slow_Proc is new Proc_Ops (Container => C,
                                          Fast      => False);

    end App;


Generic numeric types
---------------------

Generic floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Simple formal package
^^^^^^^^^^^^^^^^^^^^^

.. code:: ada

    generic
       type F is digits <>;
    package Gen_Float_Ops is
       procedure Saturate (V : in out F);
    end Gen_Float_Ops;

.. code:: ada

    package body Gen_Float_Ops is

       procedure Saturate (V : in out F) is
       begin
          if V > 1.0 then
             V := 1.0;
          elsif V < -1.0 then
             V := -1.0;
          end if;
       end Saturate;

    end Gen_Float_Ops;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;
    with Gen_Float_Ops;

    procedure Show_Float_Ops is

       package Float_Ops is new Gen_Float_Ops (F => Float);
       use Float_Ops;

       package Long_Float_Ops is new Gen_Float_Ops (F => Long_Float);
       use Long_Float_Ops;

       F  : Float := 0.5;
       LF : Long_Float := -0.5;

    begin
       F  := F + 0.7;
       LF := LF - 0.7;

       Put_Line ("F:  " & Float'Image (F));
       Put_Line ("LF: " & Long_Float'Image (LF));

       Saturate (F);
       Saturate (LF);

       Put_Line ("F:  " & Float'Image (F));
       Put_Line ("LF: " & Long_Float'Image (LF));

    end Show_Float_Ops;


Operations in formal packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: ada

    package Float_Types is

       type My_Float is new Float;
       function "+" (A, B : My_Float) return My_Float;

    end Float_Types;

.. code:: ada

    package body Float_Types is

       procedure Saturate (V : in out My_Float) is
       begin
          if V > 1.0 then
             V := 1.0;
          elsif V < -1.0 then
             V := -1.0;
          end if;
       end Saturate;

       overriding function "+" (A, B : My_Float) return My_Float is
       begin
          return R : My_Float do
             R := My_Float (Float (A) + Float (B));
             Saturate (R);
          end return;
       end "+";

    end Float_Types;

.. code:: ada

    generic
       type F is digits <>;
       with function "+" (A, B : F) return F is <>;
    package Gen_Float_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Float_Acc;

.. code:: ada

    with Float_Types; use Float_Types;

    generic
       type F is new My_Float;
       with function "+" (A : F; B : F) return F is <>;
    package Gen_Float_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Float_Acc;

.. code:: ada

    package body Gen_Float_Acc is

       procedure Acc (V : in out F; S : F) is
       begin
          V := V + S;
       end Acc;

    end Gen_Float_Acc;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;

    with Float_Types; use Float_Types;
    with Gen_Float_Acc;

    procedure Show_Float_Overriding is

       package Float_Ops is new Gen_Float_Acc (F => My_Float);
       use Float_Ops;

       F1, F2 : My_Float := 0.5;

    begin
       Put_Line ("F1:  " & My_Float'Image (F1));
       Put_Line ("F2:  " & My_Float'Image (F2));

       Acc (F1, 3.0);
       F2 := F2 + 3.0;

       Put_Line ("F1:  " & My_Float'Image (F1));
       Put_Line ("F2:  " & My_Float'Image (F2));

    end Show_Float_Overriding;


Generic fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Simple formal package
^^^^^^^^^^^^^^^^^^^^^

.. code:: ada

    generic
       type F is delta <>;
    package Gen_Fixed_Ops is
       function Sat_Add (V1, V2 : F) return F;
    end Gen_Fixed_Ops;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;

    package body Gen_Fixed_Ops is

       Ovhd_Depth : constant Positive := 64;
       Ovhd_Bits  : constant := 32;
       Ovhd_Delta : constant := 2.0 ** Ovhd_Bits / 2.0 ** (Ovhd_Depth - 1);

       type Ovhd_Fixed is delta Ovhd_Delta range
         -2.0 ** Ovhd_Bits .. 2.0 ** Ovhd_Bits - Ovhd_Delta
         with Size => Ovhd_Depth;

       --  Ensure that 'First and 'Last have at least double amount
       --  of bits as the original type
       pragma Assert (Ovhd_Fixed'First <=
                      Ovhd_Fixed (-2.0 ** (F'Size - 1)));
       pragma Assert (Ovhd_Fixed'Last  >=
                      Ovhd_Fixed (2.0 ** (F'Size - 1) - Ovhd_Delta));

       --  Ensure that 'Size is has at least twice as many bits as
       --  the original type
       pragma Assert (Ovhd_Fixed'Size  >= F'Size * 2);

       --  Ensure that the precision is at least the same
       pragma Assert (Ovhd_Fixed'Small <= F'Small);

       procedure Saturate (V : in out Ovhd_Fixed)
          with Inline;

       procedure Saturate (V : in out Ovhd_Fixed) is
          First : constant Ovhd_Fixed := Ovhd_Fixed (F'First);
          Last  : constant Ovhd_Fixed := Ovhd_Fixed (F'Last);
       begin
          if V > Last then
             V := Last;
          elsif V < First then
             V := First;
          end if;
       end Saturate;

       function Sat_Add (V1, V2 : F) return F is
          VC1 : Ovhd_Fixed := Ovhd_Fixed (V1);
          VC2 : Ovhd_Fixed := Ovhd_Fixed (V2);
          VC  : Ovhd_Fixed;
       begin
          VC := VC1 + VC2;
          Saturate (VC);
          return F (VC);
       end Sat_Add;

    end Gen_Fixed_Ops;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;
    with Gen_Fixed_Ops;

    procedure Show_Fixed_Ops is

       Fixed_Depth      : constant Positive := 16;
       Long_Fixed_Depth : constant Positive := 32;

       Fixed_Delta      : constant := 1.0 / 2.0 ** (Fixed_Depth - 1);
       Long_Fixed_Delta : constant := 1.0 / 2.0 ** (Long_Fixed_Depth - 1);

       type Fixed is delta
         Fixed_Delta range -1.0 .. 1.0 - Fixed_Delta
         with Size => Fixed_Depth;

       type Long_Fixed is delta
         Long_Fixed_Delta range -1.0 .. 1.0 - Long_Fixed_Delta
         with Size => Long_Fixed_Depth;

       package Fixed_Ops is new Gen_Fixed_Ops (F => Fixed);
       use Fixed_Ops;

       package Long_Fixed_Ops is new Gen_Fixed_Ops (F => Long_Fixed);
       use Long_Fixed_Ops;

       F  : Fixed      :=  0.5;
       LF : Long_Fixed := -0.5;

    begin
       Put_Line ("F:  " & Fixed'Image (F));
       Put_Line ("LF: " & Long_Fixed'Image (LF));

       F  := Sat_Add (F,   0.75);
       LF := Sat_Add (LF, -0.75);

       Put_Line ("F:  " & Fixed'Image (F));
       Put_Line ("LF: " & Long_Fixed'Image (LF));

    end Show_Fixed_Ops;


Operations in formal packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: ada

    package Fixed_Types is

       Fixed_Depth      : constant Positive := 16;
       Fixed_Delta      : constant := 1.0 / 2.0 ** (Fixed_Depth - 1);

       type Fixed is delta
         Fixed_Delta range -1.0 .. 1.0 - Fixed_Delta
         with Size => Fixed_Depth;

       function "+" (A, B : Fixed) return Fixed;

    end Fixed_Types;

.. code:: ada

    with Gen_Fixed_Ops;

    package body Fixed_Types is

       package Fixed_Ops is new Gen_Fixed_Ops (F => Fixed);
       use Fixed_Ops;

       function "+" (A, B : Fixed) return Fixed is
       begin
          return R : Fixed do
             R := Sat_Add (A, B);
          end return;
       end "+";

    end Fixed_Types;

.. code:: ada

    with Fixed_Types; use Fixed_Types;

    generic
       type F is new Fixed;
    package Gen_Fixed_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Fixed_Acc;

.. code:: ada

    with Fixed_Types; use Fixed_Types;

    generic
       type F is delta <>;
       with function "+" (A : F; B : F) return F is <>;
    package Gen_Fixed_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Fixed_Acc;

.. code:: ada

    package body Gen_Fixed_Acc is

       procedure Acc (V : in out F; S : F) is
       begin
          V := V + S;
       end Acc;

    end Gen_Fixed_Acc;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;

    with Fixed_Types; use Fixed_Types;
    with Gen_Fixed_Acc;

    procedure Show_Fixed_Overriding is

       package Fixed_Ops is new Gen_Fixed_Acc (F => Fixed);
       use Fixed_Ops;

       F1 : Fixed := -0.5;

    begin
       Put_Line ("F1:  " & Fixed'Image (F1));

       Acc (F1, -0.9);

       Put_Line ("F1:  " & Fixed'Image (F1));
    end Show_Fixed_Overriding;


Formal access types
-------------------

-----------------------------------------------------------------------

**Access to objects**

**Adapted from formal object example**

-----------------------------------------------------------------------

.. code:: ada

    generic
       K : access Integer;
    package Integer_Op_Cnt is

       procedure Increment;
       procedure Decrement;

       function Get_Increment_Count return Natural;
       function Get_Decrement_Count return Natural;

    end Integer_Op_Cnt;

.. code:: ada

    package body Integer_Op_Cnt is

       Incr_Cnt : Natural := 0;
       Decr_Cnt : Natural := 0;

       function Get_Increment_Count return Natural is (Incr_Cnt);
       function Get_Decrement_Count return Natural is (Decr_Cnt);

       procedure Increment is
       begin
          K.all := K.all + 1;
          Incr_Cnt := Incr_Cnt + 1;
       end Increment;

       procedure Decrement is
       begin
          K.all := K.all - 1;
          Decr_Cnt := Decr_Cnt + 1;
       end Decrement;

    end Integer_Op_Cnt;

.. code:: ada

    with Ada.Text_IO;    use Ada.Text_IO;
    with Integer_Op_Cnt;

    procedure Show_Formal_Access_Pkg is

       A, B : Integer := 2;

       --  A is now bound to A_Op
       package A_Op is new Integer_Op_Cnt (K => A'Access);
       package B_Op is new Integer_Op_Cnt (K => B'Access);

    begin
       Put_Line ("A: " & Integer'Image (A));
       Put_Line ("B: " & Integer'Image (B));
       A_Op.Increment;
       B_Op.Decrement;
       Put_Line ("A: " & Integer'Image (A));
       Put_Line ("B: " & Integer'Image (B));
       Put_Line ("# Incr A: " & Natural'Image (A_Op.Get_Increment_Count));
       Put_Line ("# Decr A: " & Natural'Image (A_Op.Get_Decrement_Count));
       Put_Line ("# Incr B: " & Natural'Image (B_Op.Get_Increment_Count));
       Put_Line ("# Decr B: " & Natural'Image (B_Op.Get_Decrement_Count));
    end Show_Formal_Access_Pkg;

-----------------------------------------------------------------------

**Access to subprograms**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    package Read_File_Pkg is

       generic
          type Index_Type is (<>);
          type Element_Type is private;
          type Array_Type is array (Index_Type) of Element_Type;
          Sort : access procedure (A : in out Array_Type);
       procedure Gen_Read_Sorted_File (A : Array_Type; S : Stream_Access);

    end Read_File_Pkg;

.. code:: ada

    package body Read_File_Pkg is

       procedure Gen_Read_Sorted_File (A : Array_Type; S : Stream_Access) is
       begin
          --  Missing implementation
          null;
       end Gen_Read_Sorted_File;

    end Read_File_Pkg;

.. code:: ada

    with Ada.Containers.Generic_Constrained_Array_Sort;

    with Read_File_Pkg;

    package Show_Procedure_Access is

       type A_Range is range 0 .. 10;
       type A is array (A_Range) of Integer;

       procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
         (Index_Type   => A_Range,
          Element_Type => Integer,
          Array_Type   => A);

       procedure Read_Sorted_File is new Read_File_Pkg.Gen_Read_Sorted_File
         (Index_Type   => A_Range,
          Element_Type => Integer,
          Array_Type   => A,
          Sort         => Sort'Access);

    end Show_Procedure_Access;

-----------------------------------------------------------------------

**Implementation of the packages above using formal subprograms**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    package Read_File_Pkg is

       generic
          type Index_Type is (<>);
          type Element_Type is private;
          type Array_Type is array (Index_Type) of Element_Type;
          with procedure Sort (A : in out Array_Type) is <>;
       procedure Gen_Read_Sorted_File (A : Array_Type; S : Stream_Access);

    end Read_File_Pkg;

.. code:: ada

    package body Read_File_Pkg is

       procedure Gen_Read_Sorted_File (A : Array_Type; S : Stream_Access) is
       begin
          --  Missing implementation
          null;
       end Gen_Read_Sorted_File;

    end Read_File_Pkg;

.. code:: ada

    with Ada.Containers.Generic_Constrained_Array_Sort;

    with Read_File_Pkg;

    package Show_Procedure_Access is

       type A_Range is range 0 .. 10;
       type A is array (A_Range) of Integer;

       procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
         (Index_Type   => A_Range,
          Element_Type => Integer,
          Array_Type   => A);

       procedure Read_Sorted_File is new Read_File_Pkg.Gen_Read_Sorted_File
         (Index_Type   => A_Range,
          Element_Type => Integer,
          Array_Type   => A);

    end Show_Procedure_Access;



Generic tagged types
--------------------

-----------------------------------------------------------------------

**Simple example**

-----------------------------------------------------------------------

.. code:: ada

    generic
       type T is tagged private;
       Proc : access procedure (E : in out T'Class);
    package Show_Gen_Tagged_Type is

       --  Some processing on type T

    end Show_Gen_Tagged_Type;


-----------------------------------------------------------------------

**Example using "people database" and database analysis**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Containers.Vectors;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    generic
       with package P is new Ada.Containers.Vectors (<>);
       type T is new P.Vector with private;
    package Vector_Analysis is

       type Meets_Criteria is not null access
         function (E : P.Element_Type) return Boolean;

       type Analysis_Set is record
          Check       : Meets_Criteria;
          Description : Unbounded_String;
       end record;

       procedure Analyze (V : T'Class;
                          A : Analysis_Set);

       procedure Display_Results;

    end Vector_Analysis;

.. code:: ada

    with Ada.Containers;          use Ada.Containers;
    with Ada.Containers.Vectors;
    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    package body Vector_Analysis is

       package Analysis_Pkg is
          type Analysis_Entry is record
             Timestamp   : Time;
             Description : Unbounded_String;
             Total       : Natural;
             Passed      : Natural;
          end record;

          package Analysis_Vec is new Ada.Containers.Vectors
            (Index_Type   => Natural,
             Element_Type => Analysis_Entry);

          use Analysis_Vec;

          Ana_DB : Vector;
       end Analysis_Pkg;

       use Analysis_Pkg;

       function Get_Passed (V : T'Class;
                            C : Meets_Criteria) return Natural is
          Passed : Natural := 0;
       begin
          for E of V loop
             if C (E) then
                Passed := Passed + 1;
             end if;
          end loop;

          return Passed;
       end Get_Passed;

       procedure Analyze (V : T'Class;
                          A : Analysis_Set)
       is
          Passed : Natural := 0;
       begin
          Passed := Get_Passed (V, A.Check);

          Ana_DB.Append ((Timestamp   => Clock,
                          Description => A.Description,
                          Total       => Natural (V.Length),
                          Passed      => Passed));
       end Analyze;

       procedure Display_Results is
          TZ   : Time_Offset := UTC_Time_Offset;
       begin
          for A of Ana_DB loop
             Put_Line ("Date:  "  & Image (A.Timestamp, True, TZ));
             Put_Line ("Test:  "  & To_String (A.Description));
             Put_Line ("Total: "  & Natural'Image (A.Total));
             Put_Line ("Passed: " & Natural'Image (A.Passed));
             New_Line;
          end loop;
       end Display_Results;

    end Vector_Analysis;

.. code:: ada

    with Ada.Containers.Vectors;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    with Vector_Analysis;

    procedure Show_Vector_Processing is

       type Person is record
          Name : Unbounded_String;
          Age  : Natural;
       end record;

       package People_DB_Pkg is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Person);

       package People_Ana is new Vector_Analysis
         (P => People_DB_Pkg,
          T => People_DB_Pkg.Vector);
       use People_Ana;

       function Older_Than_18 (E : Person) return Boolean is
         (E.Age >= 18);

       function To_US (S : String) return Unbounded_String renames
         To_Unbounded_String;

       Older_Than_18_Test : constant Analysis_Set
         := (Check       => Older_Than_18'Access,
             Description => To_US ("Count people older than 18 years"));

       People_DB : People_DB_Pkg.Vector;

    begin
       People_DB.Append ((Name => To_US ("John"),
                          Age  => 35));
       People_DB.Append ((Name => To_US ("Bob"),
                          Age  => 24));
       People_DB.Append ((Name => To_US ("Alice"),
                          Age  => 16));

       Analyze (People_DB, Older_Than_18_Test);

       People_DB.Append ((Name => To_US ("Sara"),
                          Age  => 26));

       Analyze (People_DB, Older_Than_18_Test);

       Display_Results;

    end Show_Vector_Processing;

-----------------------------------------------------------------------

**Example using "people database" and database analysis**

**Formal subprogram**

-----------------------------------------------------------------------

.. code:: ada

    with Ada.Containers.Vectors;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    package Vector_Analysis is

       generic
          with package P is new Ada.Containers.Vectors (<>);
          type T is new P.Vector with private;
          V : access T'Class;
          with function Check (E : P.Element_Type) return Boolean;
          Description : String;
       package Gen is
          procedure Analyze;
          procedure Display_Results;
       end Gen;

    end Vector_Analysis;

.. code:: ada

    with Ada.Containers;          use Ada.Containers;
    with Ada.Containers.Vectors;
    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    package body Vector_Analysis is

       package Analysis_Pkg is
          type Analysis_Entry is record
             Timestamp   : Time;
             Description : Unbounded_String;
             Total       : Natural;
             Passed      : Natural;
          end record;

          package Analysis_Vec is new Ada.Containers.Vectors
            (Index_Type   => Natural,
             Element_Type => Analysis_Entry);

          use Analysis_Vec;

          Ana_DB : Vector;
       end Analysis_Pkg;

       package body Gen is
          use Analysis_Pkg;

          procedure Analyze
          is
             Passed : Natural := 0;
          begin
             for E of V.all loop
                if Check (E) then
                   Passed := Passed + 1;
                end if;
             end loop;

             Ana_DB.Append
               (Analysis_Entry'(Timestamp   => Clock,
                                Description => To_Unbounded_String (Description),
                                Total       => Natural (V.Length),
                                Passed      => Passed));
          end Analyze;


          procedure Display_Results is
             TZ   : Time_Offset := UTC_Time_Offset;
          begin
             for A of Ana_DB loop
                Put_Line ("Date:  "  & Image (A.Timestamp, True, TZ));
                Put_Line ("Test:  "  & To_String (A.Description));
                Put_Line ("Total: "  & Natural'Image (A.Total));
                Put_Line ("Passed: " & Natural'Image (A.Passed));
                New_Line;
             end loop;
          end Display_Results;

       end Gen;

    end Vector_Analysis;

.. code:: ada

    with Ada.Containers.Vectors;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

    with Vector_Analysis;

    procedure Show_Vector_Processing is

       type Person is record
          Name : Unbounded_String;
          Age  : Natural;
       end record;

       package People_DB_Pkg is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Person);

       function Older_Than_18 (E : Person) return Boolean is
         (E.Age >= 18);

       function To_US (S : String) return Unbounded_String renames
         To_Unbounded_String;

       People_DB : People_DB_Pkg.Vector;

       package People_Ana is new Vector_Analysis.Gen
         (P           => People_DB_Pkg,
          T           => People_DB_Pkg.Vector,
          V           => People_DB'Access,
          Check       => Older_Than_18,
          Description => "Count people older than 18 years");
       use People_Ana;

    begin
       People_DB.Append ((Name => To_US ("John"),
                          Age  => 35));
       People_DB.Append ((Name => To_US ("Bob"),
                          Age  => 24));
       People_DB.Append ((Name => To_US ("Alice"),
                          Age  => 16));

       Analyze;

       People_DB.Append ((Name => To_US ("Sara"),
                          Age  => 26));

       Analyze;

       Display_Results;

    end Show_Vector_Processing;


Generic interfaces
------------------

Generic interfaces can be used to generate a collection of pre-defined
subprograms for new types. For example, let's suppose that, for a given
type T, we need at least a pair of subprograms that set and get elements
of type T based on another type. We might want to convert back and forth
between the types T and :ada:`Integer`. In addition, we might want to
convert from and to other types (e.g., :ada:`Float`). To implement this,
we can define the following generic interface:

.. code:: ada

    package Gen_Interface is

       generic
          type TD is private;
          type TI is interface;
       package Set_Get is
          type T is interface and TI;

          procedure Set (E : in out T; D : TD) is abstract;
          function Get (E : T) return TD is abstract;
       end Set_Get;

    end Gen_Interface;

In this example, the package :ada:`Set_Get` defines subprograms that allow
converting from any definite type (:ada:`TD`) and the interface type
(:ada:`TI`).

We then proceed to declare packages for converting between :ada:`Integer`
and :ada:`Float` types and the interface type. Also, we declare an actual
tagged type that combines these conversion subprograms into a single type:

.. code:: ada

    with Gen_Interface;

    package My_Type_Pkg is

       type My_Type_Interface is interface;

       package Set_Get_Integer is new
         Gen_Interface.Set_Get (TD => Integer,
                                TI => My_Type_Interface);
       use Set_Get_Integer;

       package Set_Get_Float   is new
         Gen_Interface.Set_Get (TD => Float,
                                TI => My_Type_Interface);
       use Set_Get_Float;

       type My_Type is
         new Set_Get_Integer.T and Set_Get_Float.T with private;

       overriding procedure Set (E : in out My_Type; D : Integer);
       overriding function Get (E : My_Type) return Integer;

       overriding procedure Set (E : in out My_Type; D : Float);
       overriding function Get (E : My_Type) return Float;

    private
       type My_Type is
         new Set_Get_Integer.T and Set_Get_Float.T with record
          I : Integer;
          F : Float;
       end record;

    end My_Type_Pkg;

First, we declare the packages :ada:`Set_Get_Integer` and
:ada:`Set_Get_Float` based on the generic :ada:`Set_Get` package. Next,
we declare :ada:`My_Type` based on the interface type from these two
packages. By doing this, :ada:`My_Type` now needs to implement the actual
conversion from and to :ada:`Integer` and :ada:`Float` types.

Note that, in the private part of :ada:`My_Type`, we're storing the
floating-point and integer representations that we receive in the calls to
the :ada:`Set` procedures. However, we could have complex data as well and
just use conversion subprograms to provide a simplified representation of
the complex data.

This is just an example on how we could implement these :ada:`Set` and
:ada:`Get` subprograms:

.. code:: ada

    package body My_Type_Pkg is

       procedure Set (E : in out My_Type; D : Integer) is
       begin
          E.I := D;
          E.F := Float (D);
       end Set;

       function Get (E : My_Type) return Integer is
       begin
          return E.I;
       end;

       procedure Set (E : in out My_Type; D : Float) is
       begin
          E.F := D;
          E.I := Integer (D);
       end Set;

       function Get (E : My_Type) return Float is
       begin
          return E.F;
       end;

    end My_Type_Pkg;

As expected, declaring and using variable of :ada:`My_Type` is
straightforward:

.. code:: ada

    with My_Type_Pkg; use My_Type_Pkg;

    procedure Show_Gen_Interface is
       C : My_Type;
    begin
       C.Set (2);
       C.Set (2.1);
    end Show_Gen_Interface;

Discussion: Generic interfaces vs. other approaches
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. TODO: Add discussion about interfaces vs. types & formal subprograms

Generic synchronized interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generic synchronized interfaces are a specialized case of generic
interfaces that can be used for task types and protected types. Since
generic synchronized interfaces are similar to generic interfaces,
we can reuse the previous source-code example with minimal adaptations.

When adapting the :ada:`Gen_Interface` package, we just need to make use
of the :ada:`synchronized` keyword:

.. code:: ada

    package Gen_Sync_Interface is

       generic
          type TD is private;
          type TI is synchronized interface;
       package Set_Get is
          type T is synchronized interface and TI;

          procedure Set (E : in out T; D : TD) is abstract;
          function Get (E : T) return TD is abstract;
       end Set_Get;

    end Gen_Sync_Interface;

Note that we're also renaming some packages (e.g., renaming
:ada:`Gen_Interface` to :ada:`Gen_Sync_Interface`) to better differentiate
between them. This approach is used in the adaptations below as well.

When adapting the :ada:`My_Type_Pkg`, we again need to make use of
the :ada:`synchronized` keyword. Also, we need to declare :ada:`My_Type`
as a protected type and adapt the subprogram and component declarations.
Note that we could have used a task type instead. This is the adapted
package:

.. code:: ada

    with Gen_Sync_Interface;

    package My_Sync_Type_Pkg is

       type My_Type_Interface is synchronized interface;

       package Set_Get_Integer is
         new Gen_Sync_Interface.Set_Get (TD => Integer,
                                         TI => My_Type_Interface);
       use Set_Get_Integer;

       package Set_Get_Float is
         new Gen_Sync_Interface.Set_Get (TD => Float,
                                         TI => My_Type_Interface);
       use Set_Get_Float;

       protected type My_Type is
            new Set_Get_Integer.T and Set_Get_Float.T with

          overriding procedure Set (D : Integer);
          function Get return Integer;

          overriding procedure Set (D : Float);
          function Get return Float;
       private
          I : Integer;
          F : Float;
       end My_Type;

    end My_Sync_Type_Pkg;

In the package body, we just need to adapt the access to components in the
subprograms:

.. code:: ada

    package body My_Sync_Type_Pkg is

       protected body My_Type is
          procedure Set (D : Integer) is
          begin
             I := D;
             F := Float (D);
          end Set;

          function Get return Integer is
          begin
             return I;
          end;

          procedure Set (D : Float) is
          begin
             F := D;
             I := Integer (D);
          end Set;

          function Get return Float is
          begin
             return F;
          end;
       end My_Type;

    end My_Sync_Type_Pkg;

Finally, the main application doesn't require adaptations:

.. code:: ada

    with My_Sync_Type_Pkg; use My_Sync_Type_Pkg;

    procedure Show_Gen_Sync_Interface is
       C : My_Type;
    begin
       C.Set (2);
       C.Set (2.1);
    end Show_Gen_Sync_Interface;

