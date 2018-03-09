Generics
========

Abstracting definitions into packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section and in the next ones, we will reuse the generic
reversing algorithm that we discussed in the previous section.  In that
example, we were declaring three formal types for the
``Generic_Reverse_Array`` procedure. However, we could abstract the array
definition into a separate package and reuse it for the generic procedure.
This could be potentially useful in case we want to create more generic
procedures for the same array.

In order to achieve this, we start by first specifying a generic package
that contains the generic array type definition:

.. code-block:: ada

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

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Simple_Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Simple_Pkg is

       generic
          type T is private;
          with package P is new Simple_Generic_Array_Pkg (T => T, Index => Integer);
       procedure Reverse_Array (X : in out P.Array_T);

       procedure Reverse_Array (X : in out P.Array_T) is
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
separate package, but leaved the generic procedure (``Reverse_Array``) in
the test application. Another approach would have been to also move the
generic procedure into the generic package. The advantage of this approach
is that we don't need to repeat the formal declaration for the
``Reverse_Array`` procedure. Also, this simplifies the instantiation in
the test application.

First, we need to extend the previous package by adding the declaration of
the ``Reverse_Array`` procedure. Note that we've just renamed the
``Simple_Generic_Array_Pkg`` package to ``Generic_Array_Pkg`` in order to
avoid confusion with the previous example. This is the resulting
specification:

.. code-block:: ada

    generic
       type T is private;
       type Index is range <>;
    package Generic_Array_Pkg is
       type Array_T is array (Index range <>) of T;

       procedure Reverse_Array (X : in out Array_T);
    end Generic_Array_Pkg;

Because we have a procedure declaration, we need a package body for the
procedure implementation. Here, we haven't changed the previous algorithm
-- we're simply moving the existing code into the new package body:

.. code-block:: ada

    package body Generic_Array_Pkg is
       procedure Reverse_Array (X : in out Array_T) is
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
    end Generic_Array_Pkg;

In the test application, we just need to instantiate the
``Generic_Array_Pkg`` package and make use of the array type (``Array_T``)
and the procedure (``Reverse_Array``):

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Pkg is
       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new Generic_Array_Pkg (T => Color, Index => Integer);

       My_Colors : Color_Pkg.Array_T (1 .. 5) := (Black, Red, Green, Blue, White);
    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Color_Pkg.Reverse_Array (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Reverse_Colors_Pkg;


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

.. code-block:: ada

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Pkg is

       generic
          S : in String;
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

 .. code-block:: ada

    with Generic_Array_Pkg;

    generic
       S : String;
       with package Array_Pkg is new Generic_Array_Pkg (<>);
       with function Image (E : in Array_Pkg.T) return String is <>;
    package Generic_Array_Bundle is
    end Generic_Array_Bundle;

Then, we update the definition of ``Perform_Test``:

.. code-block:: ada

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
