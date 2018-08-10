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

.. code-block::

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


Generic numeric types
---------------------


Formal access types
-------------------


Generic tagged types
--------------------


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

       procedure Set (E : in out My_Type; D : Integer);
       function Get (E : My_Type) return Integer;

       procedure Set (E : in out My_Type; D : Float);
       function Get (E : My_Type) return Float;

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

