Generics
========

.. include:: ../../../global.txt

.. _Adv_Ada_Formal_Definite_Indefinite_Subtypes:

Mapping of Definite and Indefinite Subtypes
-------------------------------------------

Earlier on, we had a discussion about
:ref:`definite and indefinite subtypes <Adv_Ada_Definite_Indefinite_Subtypes>`. In this
section, we look into formal definite and indefinite types and how those types
are mapped.

We can distinguish between the definite and indefinite version of many formal
types. For example, consider the simple formal type :ada:`type T is private`,
which is the definite version of a formal nonlimited private type. The
indefinite version of this formal type is :ada:`type T (<>) is private`. Here,
the syntax :ada:`(<>)` is used to distinguish the indefinite form from the
definite form.

Let's create a generic package using the definite form of the formal nonlimited
private type and map an actual data type to it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Definite_Formal_Type

    generic
       type T is private;
    package Definite_Formal_Type_Example is
       Dummy : T;
    end Definite_Formal_Type_Example;

    with Definite_Formal_Type_Example;

    package Show_Map_To_Definite_Formal_Type is

       type Null_Record is null record;

       package Map_Definite_Type is new
         Definite_Formal_Type_Example
           (T => Null_Record);

    end Show_Map_To_Definite_Formal_Type;

We can map any nonlimited, definite type to the type :ada:`T` above. However,
we cannot map indefinite types to it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Definite_Formal_Type
    :class: ada-expect-compile-error

    with Definite_Formal_Type_Example;

    package Show_Map_To_Definite_Formal_Type is

       type Simple_Record (Extended : Boolean) is
       record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

       package Map_Indefinite_Type is new
         Definite_Formal_Type_Example
           (T => Simple_Record);
       --  ERROR: trying to map an indefinite type
       --         to a formal definite type!

    end Show_Map_To_Definite_Formal_Type;

When we try to compile this example, we get a compilation error at the
declaration of the :ada:`Map_Indefinite_Type` package. We could solve this
problem by changing type :ada:`T` to a formal indefinite type, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Indefinite_Formal_Type

    generic
       type T (<>) is private;
    package Indefinite_Formal_Type_Example is
       function Dummy (Unused : T)
                       return Boolean is
         (True);
    end Indefinite_Formal_Type_Example;

    with Indefinite_Formal_Type_Example;

    package Show_Map_To_Indefinite_Formal_Type is

       type Simple_Record (Extended : Boolean) is
       record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

       package Map_Indefinite_Type is new
         Indefinite_Formal_Type_Example
           (T => Simple_Record);

    end Show_Map_To_Indefinite_Formal_Type;

Now, we don't get a compilation error because type :ada:`T` allows us to map an
indefinite type. Note that we could still map a definite type to type :ada:`T`.
For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Indefinite_Formal_Type

    with Indefinite_Formal_Type_Example;

    package Show_Map_To_Indefinite_Formal_Type is

       type Null_Record is null record;

       package Map_Definite_Type is new
         Indefinite_Formal_Type_Example
           (T => Null_Record);

    end Show_Map_To_Indefinite_Formal_Type;

In other words, we can map any nonlimited type |mdash| indefinite or definite
|mdash| to a formal indefinite nonlimited private type.

Many instances of formal types have an indefinite and a definite version. Here
are a few examples:

    - Formal limited private type:

        - Definite version: :ada:`type T is limited private`
        - Indefinite version: :ada:`type T (<>) is limited private`

    - Tagged private type:

        - Definite version: :ada:`type T is tagged private`
        - Indefinite version: :ada:`type T (<>) is tagged private`

    - Abstract tagged limited private type:

        - Definite version: :ada:`type T is abstract tagged limited private`
        - Indefinite version: :ada:`type T (<>) is abstract tagged limited private`

Appendix A of the :ref:`Introduction to Ada course <Intro_Ada_Generic_Formal_Types>`
provides a detailed list of formal types and their variations.

Note that, instead of just using a formal indefinite nonlimited private type,
we could be more specific about the discriminants that we use for type
:ada:`T`. Consider the following example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Formal_Type_Discriminants

    generic
       type T (B : Boolean) is private;
    package Formal_Type_Discriminants_Example is
       function Dummy (Unused : T)
                       return Boolean is
         (True);
    end Formal_Type_Discriminants_Example;

    with Formal_Type_Discriminants_Example;

    package Show_Map_To_Formal_Type_With_Discrimants
    is
       type Simple_Record (Extended : Boolean) is
       record
          V : Integer;
          case Extended is
             when False =>
                null;
             when True  =>
                V_Float : Float;
          end case;
       end record;

       type Integer_Array is
         array (Positive range <>) of Integer;

       type Simple_Record_2 (Last : Integer) is record
          A : Integer_Array (1 .. Last);
       end record;

       type Null_Record is null record;

       package Map_Boolean_Discriminant is new
         Formal_Type_Discriminants_Example
           (T => Simple_Record);

    end Show_Map_To_Formal_Type_With_Discrimants;

Here, we replaced :ada:`(<>)` by :ada:`(B : Boolean)` in the declaration of
type :ada:`T`. This means that, now, we can only map indefinite types with that
specific list of discriminants. For example, we cannot map the following types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Mapping_Definite_Indefinite_Subtypes.Formal_Type_Discriminants
    :class: ada-expect-compile-error

    with Formal_Type_Discriminants_Example;

    package Show_Map_To_Formal_Type_With_Discrimants
    is
       type Integer_Array is
         array (Positive range <>) of Integer;

       type Simple_Record_2 (Last : Integer) is record
          A : Integer_Array (1 .. Last);
       end record;

       type Null_Record is null record;

       package Map_Type_With_Integer_Discriminant
       is new
         Formal_Type_Discriminants_Example
           (T => Simple_Record_2);

       package Map_Definite_Type is new
         Formal_Type_Discriminants_Example
           (T => Null_Record);

    end Show_Map_To_Formal_Type_With_Discrimants;

The compilation of this example fails as expected because, as soon as we
specify a list of discriminants for a formal type, we can only map actual types
that have the exact same discriminants. We cannot use a type with a different
set of discriminants |mdash| as in the declaration of
:ada:`Map_Type_With_Integer_Discriminant` |mdash| nor a definite type |mdash|
as in the declaration of :ada:`Map_Definite_Type`.

.. _Adv_Ada_Formal_Incomplete_Types:

Formal incomplete types
-----------------------

A formal incomplete type has the following syntax:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Incomplete_Types.Simple_Formal_Incomplete_Type

    generic
      type Formal_Incomplete;
    package Using_Formal_Incomplete
      with Pure is
    end Using_Formal_Incomplete;

We can use them to map incomplete types when instantiating generic packages or
subprograms. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Incomplete_Types.Simple_Formal_Incomplete_Type

    with Using_Formal_Incomplete;

    package Show_Inst_Formal_Incomplete is

       type R;

       package R_Pkg is new
         Using_Formal_Incomplete (R);

       type R is record
          I : Integer;
       end record;

    end Show_Inst_Formal_Incomplete;

As we've seen before, incomplete types are rather restricted in terms of usage.
Therefore, formal incomplete types are typically used in conjunction with other
generic packages or subprograms. We explain later how to use them to create
:ref:`signature packages <Adv_Ada_Signature_Packages>`.

A formal incomplete type can also be tagged:

.. code-block:: ada

    generic
      type Incomplete_Tagged is tagged;
    package Dummy;

Let's see an example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Incomplete_Types.Formal_Incomplete_Tagged_Types

    generic
      type Incomplete_Tagged is tagged;
      with function Test (V : Incomplete_Tagged)
                          return Boolean;
    package Formal_Incomplete_Tagged_Type_Example
    is

       procedure Perform_Test (I : Incomplete_Tagged);

    end Formal_Incomplete_Tagged_Type_Example;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Formal_Incomplete_Tagged_Type_Example
    is

       procedure Perform_Test (I : Incomplete_Tagged)
       is
       begin
          if Test (I) then
             Put_Line ("Test passed!");
          else
             Put_Line ("Test failed!");
          end if;
       end Perform_Test;

    end Formal_Incomplete_Tagged_Type_Example;

Note that this example only compiles because :ada:`Incomplete_Tagged` is
tagged. If it was an *untagged* formal incomplete type, we wouldn't be allowed
to call the :ada:`Test` function in the body of :ada:`Perform_Test`. This is
possible, however, with tagged formal incomplete types |mdash| as well as with
other kinds of formal types.

.. _Adv_Ada_Formal_Packages:

Formal packages
---------------

Abstracting definitions into packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this section and in the next ones, we will reuse the generic
reversing algorithm that we discussed in the
:ref:`chapter about generics <Intro_Ada_Generic_Reverse_Colors_Example>`
from the introductory course.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Reverse_Color

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Test_Reverse_Colors is
       generic
          type T is private;
          type Index is range <>;
          type Array_T is array (Index range <>) of T;
       procedure Generic_Reverse_Array
         (X : in out Array_T);

       procedure Generic_Reverse_Array
         (X : in out Array_T) is
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X
                              (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Generic_Reverse_Array;

       type Color is (Black, Red, Green, Blue, White);
       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_Color_Array is new
         Generic_Reverse_Array
           (T      => Color,
           Index   => Integer,
           Array_T => Color_Array);

       My_Colors : Color_Array (1 .. 5) :=
                    (Black, Red, Green, Blue, White);

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

In that example, we were declaring three formal types for the
:ada:`Generic_Reverse_Array` procedure: a type :ada:`T`, a range :ada:`Index`
and the array type :ada:`Array_T`. However, we could abstract the array
definition into a separate package and reuse it for the generic procedure.
This could be potentially useful in case we want to create more generic
procedures for the same array.

In order to achieve this, we start by first specifying a generic package
that contains the generic array type definition:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Simple_Generic_Array_Pkg

    generic
       type T is private;
       type Index is range <>;
    package Simple_Generic_Array_Pkg is
       type Array_T is array (Index range <>) of T;
    end Simple_Generic_Array_Pkg;

As you can see, this definition is the same that we've seen in the
previous section: we just moved it into a separate package. Now, we have a
definition of :ada:`Array_T` that can be reused in multiple places.

The next step is to reuse the :ada:`Simple_Generic_Array_Pkg` package in
the :ada:`Generic_Reverse_Array` procedure. By doing this, we can
eliminate the declaration of the :ada:`Index` and :ada:`Array_T` types
that we had before, since the definition will come from the
:ada:`Simple_Generic_Array_Pkg` package.

In order to reuse the :ada:`Simple_Generic_Array_Pkg` package in the
:ada:`Generic_Reverse_Array` procedure, we need to use a formal package
parameter in the form:

.. code-block:: ada

    with package P is new Simple_Generic_Array_Pkg(<params>)

This will allow us to reuse definitions from the generic package.

This is the updated version of the our test application for the reversing
algorithm:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Simple_Generic_Array_Pkg

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Simple_Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Simple_Pkg is

       generic
          type T is private;
          with package P is new
            Simple_Generic_Array_Pkg (T      => T,
                                      others => <>);
       procedure Reverse_Array (X : in out P.Array_T);

       procedure Reverse_Array (X : in out P.Array_T)
       is
          use P;
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X
                              (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_Array;

       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new
         Simple_Generic_Array_Pkg (T     => Color,
                                   Index => Integer);

       procedure Reverse_Color_Array is new
         Reverse_Array (T => Color, P => Color_Pkg);

       My_Colors : Color_Pkg.Array_T (1 .. 5) :=
                     (Black, Red, Green, Blue, White);
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
:ada:`Simple_Generic_Array_Pkg` package, thereby creating the
:ada:`Color_Pkg` package. We then proceed to use this :ada:`Color_Pkg`
package in the instantiation of the generic :ada:`Reverse_Array`
procedure. Also, in the declaration of the :ada:`My_Colors` array, we make
use of the array type definition from the :ada:`Color_Pkg` package.

.. _Adv_Ada_Formal_Package_Parametrization:

Formal package parametrization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we're using partial parametrization for the formal package
parameter :ada:`P` in the previous example. Partial parametrization makes
use of :ada:`others => <>` to indicate that the generic declaration takes
the definitions from the package argument provided in the generic
instantiation:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Simple_Generic_Array_Pkg

    with Simple_Generic_Array_Pkg;

    package Show_Partial_Parametrization is

       generic
          type T is private;
          with package P is new
            Simple_Generic_Array_Pkg (T      => T,
                                      others => <>);
       procedure Reverse_Array (X : in out P.Array_T);

    end Show_Partial_Parametrization;

For the previous example, the definitions come from the declarations of
the :ada:`Color_Pkg` package:

A complete parametrization, in constrast, contains the definition of all
types in the generic declaration. For example:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Simple_Generic_Array_Pkg

    with Simple_Generic_Array_Pkg;

    package Show_Complete_Parametrization is

       generic
          type T is private;
          type Index is range <>;
          with package P is new
            Simple_Generic_Array_Pkg (T     => T,
                                      Index => Index);
       procedure Reverse_Array (X : in out P.Array_T);

    end Show_Complete_Parametrization;

Another approach is to take all definitions from the formal package
parameter:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Simple_Generic_Array_Pkg

    with Simple_Generic_Array_Pkg;

    package Show_Box_Parameter is

       generic
          with package P is new
            Simple_Generic_Array_Pkg (<>);
       procedure Reverse_Array (X : in out P.Array_T);

    end Show_Box_Parameter;

In this case, package :ada:`P` contains all type and subprogram
definitions that are used by the generic :ada:`Reverse_Array` procedure.
By using the box syntax :ada:`(<>)`, we indicate that we make use of all
definitions from the formal package parameter.

Abstracting procedures into packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, we moved the array type definition into a
separate package, but left the generic procedure (:ada:`Reverse_Array`) in
the test application. We could also move the generic procedure into the
generic package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Generic_Array_Pkg

    generic
       type T is private;
       type Index is range <>;
    package Generic_Array_Pkg is
       type Array_T is array (Index range <>) of T;

       procedure Reverse_Array (X : in out Array_T);
    end Generic_Array_Pkg;

The advantage of this approach is that we don't need to repeat the formal
declaration for the :ada:`Reverse_Array` procedure. Also, this simplifies
the instantiation in the test application.

However, the disadvantage of this approach is that it also increases code
size: every instantiation of the generic package generates code for each
subprogram from the package. Also, compilation time tends to increase
significantly. Therefore, developers must be careful when considering
this approach.

Because we have a procedure declaration in the generic package, we need a
corresponding package body. Here, we can simply reuse the existing code
and move the procedure into the package body. In the test application, we
just instantiate the :ada:`Generic_Array_Pkg` package and make use of the
array type (:ada:`Array_T`) and the procedure (:ada:`Reverse_Array`):

.. code-block:: ada

       Color_Pkg.Reverse_Array (My_Colors);

This is the generic package body:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Generic_Array_Pkg

    package body Generic_Array_Pkg is
       procedure Reverse_Array (X : in out Array_T) is
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2 loop
             declare
                Tmp     : T;
                X_Left  : T renames X (I);
                X_Right : T renames X
                              (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_Array;
    end Generic_Array_Pkg;

Abstracting the test application
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous examples, we've focused only on abstracting the reversing
algorithm. However, we could have decided to also abstract our little
test application. This could be useful if we, for example, decide to
test other procedures that change elements of an array.

In order to achieve this, we have to abstract quite a few elements. We
will therefore declare the following formal parameters:

    - the string :ada:`S` containing the array name;

    - the formal :ada:`Generic_Array_Pkg` package parameter, which is a
      generic package implemented in the previous section;

    - the formal :ada:`Image` function that converts an element of type
      :ada:`T` to a string;

    - the formal :ada:`Pkg_Test` procedure that performs some operation on
      the array.

Note that :ada:`Image` and :ada:`Pkg_Test` are examples of formal
subprograms, which have been discussed in the introductory course. Also,
note that :ada:`S` is an example of a formal object, which we discuss in
later section.

This is a version of the test application that makes use of the generic
:ada:`Perform_Test` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Generic_Array_Pkg

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;

    procedure Test_Reverse_Colors_Pkg is

       generic
          S : String;
          with package Array_Pkg is new
            Generic_Array_Pkg (<>);
          use Array_Pkg;
          with function Image (E : T)
                              return String is <>;
          with procedure Pkg_Test
                 (X : in out Array_T);
       procedure Perform_Test (X : in out Array_T);

       procedure Perform_Test (X : in out Array_T) is
       begin
          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;

          New_Line;
          Put_Line
            ("Performing operation on " & S & "...");
          New_Line;
          Pkg_Test (X);

          for C of X loop
             Put_Line (S & ": " & Image (C));
          end loop;
       end Perform_Test;

       type Color is (Black, Red, Green, Blue, White);

       package Color_Pkg is new
         Generic_Array_Pkg (T     => Color,
                            Index => Integer);

       My_Colors : Color_Pkg.Array_T (1 .. 5) :=
                     (Black, Red, Green, Blue, White);

       procedure Perform_Test_Reverse_Color_Array
       is new
         Perform_Test
           (S         => "My_Color",
            Image     => Color'Image,
            Array_Pkg => Color_Pkg,
            Pkg_Test  => Color_Pkg.Reverse_Array);
    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors_Pkg;

In this example, we create the procedure
:ada:`Perform_Test_Reverse_Color_Array` as an instance of the generic
procedure (:ada:`Perform_Test`). Note that:

    - For the formal :ada:`Image` function, we make use of the
      :ada:`'Image` attribute of the :ada:`Color` type

    - For the formal :ada:`Pkg_Test` procedure, we reference the
      :ada:`Reverse_Array` procedure from the package.

Note that this example includes a formal package declaration:

.. code-block:: ada

    with package Array_Pkg is new
      Generic_Array_Pkg (<>);

Previously, we've seen package instantiations that define the elements.
For example:

.. code-block:: ada

    package Color_Pkg is new
      Generic_Array_Pkg (T     => Color,
                         Index => Integer);

In this case, however, we're simply using :ada:`(<>)`, as discussed in the
section on
:ref:`formal package parametrization <Adv_Ada_Formal_Package_Parametrization>`.
This means that :ada:`Perform_Test` makes use of the default definition
used for the instance of :ada:`Generic_Array_Pkg`.

Cascading generic packages
~~~~~~~~~~~~~~~~~~~~~~~~~~

In the code example from the previous section, we declared four formal
parameters for the :ada:`Perform_Test` procedure. Two of them are directly
related to the array that we're using for the test:

    - :ada:`S`: the string containing the array name

    - the function :ada:`Image` that converts an elements of the array to a
      string

We could abstract our implementation even further by moving these elements
into a separate package named :ada:`Generic_Array_Bundle` and reference
the :ada:`Generic_Array_Pkg` there. This would create a chain of generic
packages:

.. code-block:: ada

    Generic_Array_Bundle <= Generic_Array_Pkg

This strategy demonstrates that, in Ada, it is really straightforward to
make use of generics in order to abstracts algorithms.

First, let us define the new :ada:`Generic_Array_Bundle` package, which
references the :ada:`Generic_Array_Pkg` package and the two formal elements
(:ada:`S` and :ada:`Image`) mentioned previously:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Generic_Array_Pkg

    with Generic_Array_Pkg;

    generic
       S : String;
       with package Array_Pkg is new
         Generic_Array_Pkg (<>);
       with function Image (E : Array_Pkg.T)
                            return String is <>;
    package Generic_Array_Bundle is
    end Generic_Array_Bundle;

Then, we update the definition of :ada:`Perform_Test`:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Generic_Array_Pkg

    with Ada.Text_IO;
    use  Ada.Text_IO;

    with Generic_Array_Pkg;
    with Generic_Array_Bundle;

    procedure Test_Reverse_Colors_Pkg is

       generic
          with package Array_Bundle is new
            Generic_Array_Bundle (<>);
          use Array_Bundle;
          use Array_Pkg;
          with procedure Pkg_Test
                 (X : in out Array_T);
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

       package Color_Pkg is new
         Generic_Array_Pkg (T     => Color,
                            Index => Integer);

       My_Colors : Color_Pkg.Array_T (1 .. 5) :=
                     (Black, Red, Green, Blue, White);

       package Color_Array_Bundle is new
         Generic_Array_Bundle
           (S         => "My_Color",
            Image     => Color'Image,
            Array_Pkg => Color_Pkg);

       procedure Perform_Test_Reverse_Color_Array
       is new
         Perform_Test
           (Array_Bundle => Color_Array_Bundle,
            Pkg_Test     => Color_Pkg.Reverse_Array);
    begin
       Perform_Test_Reverse_Color_Array (My_Colors);
    end Test_Reverse_Colors_Pkg;

Note that, in this case, we reduce the number of formal parameters to only
two:

    - :ada:`Array_Bundle`: an instance of the new
      :ada:`Generic_Array_Bundle` package

   - the procedure :ada:`Pkg_Test` that we already had before

We could go even further and move :ada:`Perform_Test` into a separate
package. However, this will be left as an exercise for the reader.

.. _Adv_Ada_Signature_Packages:

Signature Packages
~~~~~~~~~~~~~~~~~~

Signature packages are used to group a set of types and subprograms that
serve as a formal package parameter in another generic package. In the
source-code examples of the previous section, we've seen the
package :ada:`Generic_Array_Bundle`, which was used as a formal package
for the generic procedure :ada:`Perform_Test`. :ada:`Generic_Array_Bundle`
is an example of a signature package.

In this simple example, we define the signature package :ada:`Sig_Pkg`:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Sig_Pkg

    generic
       type T is private;
       with function Image (E : T)
                            return String is <>;
    package Sig_Pkg is
    end Sig_Pkg;

As a standalone package, :ada:`Sig_Pkg` is not really useful. However, it
becomes useful when used as a formal package in other generic declarations.
For example, let's use this signature package for the generic procedure
:ada:`Show` of a package :ada:`P`:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Sig_Pkg

    with Sig_Pkg;

    package P is
       generic
          with package SP is new Sig_Pkg (<>);
       procedure Show (V : SP.T);
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is
       procedure Show (V : SP.T) is
       begin
          Put_Line ("Value: " & SP.Image (V));
       end Show;
    end P;

Finally, we can use this package in an application:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Sig_Pkg

    with Sig_Pkg;
    with P;

    procedure Main is
       package Int_P is new Sig_Pkg (Integer,
                                     Integer'Image);
       procedure Show_Int is new P.Show (Int_P);

       V : Integer;
    begin
       V := 42;
       Show_Int (V);
    end Main;

We can also use formal incomplete types for signature packages. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Signature_Package_Formal_Incomplete_Type

    generic
      type Incomplete;
      with function "+" (V1, V2 : Incomplete)
                         return Incomplete;
      with function "-" (V1, V2 : Incomplete)
                         return Incomplete;
    package Formal_Incomplete_Type_Example
      with Pure is
    end Formal_Incomplete_Type_Example;

This allows us to map other formal incomplete types, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Packages.Signature_Package_Formal_Incomplete_Type

    with Formal_Incomplete_Type_Example;

    generic
       type T;
       with package P is new
         Formal_Incomplete_Type_Example
           (T,
            others => <>);
    package Map_Incomplete_Type_Example
      with Pure is
    end Map_Incomplete_Type_Example;

In general, signature packages aren't used in isolation, but in
combination with other generic packages. Also, they don't define anything
themselves. In this sense, signature packages don't have an associated
package body.

Using signature packages is an useful approach to clean-up the declaration
of generic packages or subprograms that contain many formal parameters.
You may move these formal parameters into multiple signature packages,
each one containing a group of formal parameters that belong together.
Also, multiple signature packages can be cascaded to create more complex
generic implementations.

Formal objects
--------------

Formal objects are used to bind objects to a generic specification. They
are similar to parameters in subprograms and can have :ada:`in` or
:ada:`in out` modes.

One of the simplest applications of formal objects is to use them to
configure a generic subprogram or package during instantiation. For
example, we can implement a generic function that processes an array of
floating-point values and calculates an output value. This calculation is
implemented in two versions:

- a standard version;

- a faster version that is less accurate than the standard version.

While the generic implementation offers both variants, developers can
select the version that is more appropriate for their system during
instantiation.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.Formal_Object

    with Ada.Text_IO;
    use  Ada.Text_IO;

    procedure Show_Formal_Object is

       type Array_Float is
         array (Positive range <>) of Float;

       generic
          Use_Fast_Version : Boolean;
       function Gen_Calc (A : Array_Float)
                          return Float;

       function Gen_Calc (A : Array_Float)
                          return Float is
       begin
          if Use_Fast_Version then
             Put_Line ("Using fast version");
          else
             Put_Line ("Using standard version");
          end if;

          --  Implementation missing here...
          return 0.0;
       end Gen_Calc;

       function Calc is new
         Gen_Calc (Use_Fast_Version => True);

       Vals : Array_Float (1 .. 2) := (0.5, 0.3);
       X    : Float;

    begin
       X := Calc (Vals);
    end Show_Formal_Object;

In this example, we instantiate the *fast* version of :ada:`Gen_Calc`.

Input-output formal objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Formal objects with :ada:`in out` mode are used to bind objects in an
instance of a generic specification. For example, we may bind a global
object from a package to the instantiation of a generic procedure, so that
all calls to this instance make use of that object internally.

In the application below, we create a database using a container and bind
it to procedures that display information from the database in a specific
format.

The :ada:`Data_Elements` package describes the data fields of the data
container. It also includes an :ada:`Image` function that returns a string
based on the specified field.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.In_Out_Formal_Object

    with Ada.Calendar; use Ada.Calendar;

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package Data_Elements is

       type Data_Element is record
          First_Name : Unbounded_String;
          Last_Name  : Unbounded_String;
          Birthday   : Time;
       end record;

       type Data_Fields is
         (First_Name_F, Last_Name_F,
          Birthday_F, Age_F);

       function Image (D : Data_Element;
                       F : Data_Fields) return String;

    end Data_Elements;

This is the corresponding package body:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.In_Out_Formal_Object

    with Ada.Calendar.Formatting;
    use  Ada.Calendar.Formatting;

    with Ada.Calendar.Time_Zones;
    use  Ada.Calendar.Time_Zones;

    package body Data_Elements is
       TZ   : Time_Offset := UTC_Time_Offset;

       function To_Year (D : Duration)
                         return Natural is
         (Natural (D) / 86_400 / 365);

       function Image (D : Data_Element;
                       F : Data_Fields)
                       return String is
          Now : Time := Clock;
          Age : Natural := To_Year (Now - D.Birthday);
       begin
          case F is
             when First_Name_F =>
               return To_String (D.First_Name);

             when Last_Name_F  =>
               return To_String (D.Last_Name);

             when Birthday_F   =>
               return Image (D.Birthday, True, TZ);

             when Age_F        =>
               return Natural'Image (Age);
          end case;
       end Image;

    end Data_Elements;

Note that the age field in the :ada:`Image` function (represented by
:ada:`Age_F`) isn't a field from the data container, but a calculated
value instead.

The :ada:`Data` package below implements the data container using a
vector. It includes the generic procedure :ada:`Display` that exhibits the
information from the data container based on the fields specified by the
developer at the procedure instantiation.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.In_Out_Formal_Object

    with Ada.Containers;
    with Ada.Containers.Vectors;

    with Data_Elements; use Data_Elements;

    package Data is

       type Data_Container is private;

       procedure Insert (C : in out Data_Container;
                         V : Data_Element);

       type Data_Fields_Array is
         array (Positive range <>) of Data_Fields;

       generic
          Container : in out Data_Container;
          Fields    : Data_Fields_Array;
          Header    : String := "";
       procedure Display;

    private

       package Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Data_Element);

       type Data_Container is record
          V : Vectors.Vector;
       end record;

    end Data;

Note that, in addition to :ada:`Container`, which is a formal input-output
object, we make use of the :ada:`Fields` and :ada:`Header` objects, which
are formal input objects. Also, note that we could have declared
:ada:`Container` as a parameter of :ada:`Display` instead of declaring it
as a formal object:

.. code-block:: ada

    generic
       Fields    : Data_Fields_Array;
       Header    : String := "";
    procedure Display (Container : in out Data_Container);

In this case, we wouldn't be able to bind a local :ada:`Container` object
to the instantiation of the :ada:`Display` procedure. Instead, we would
always have to pass the container as an argument. Potentially, we could
pass the wrong container to the procedure. By using a formal input-output
object, we make sure that a specific object is bound to the procedure.
This design decision ensures that we always have the same object being
used in all calls to an instance of the :ada:`Display` procedure.

This is the corresponding body of the :ada:`Data` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.In_Out_Formal_Object

    with Ada.Text_IO; use Ada.Text_IO;

    package body Data is

       procedure Insert (C : in out Data_Container;
                         V : Data_Element) is
       begin
          C.V.Append (V);
       end Insert;

       procedure Display is
       begin
          if Header /= "" then
             Put_Line (Header);
             New_Line;
          end if;

          for E of Container.V loop
             for F of Fields loop
                Put (Image (E, F) & " ");
             end loop;
             New_Line;
          end loop;

          New_Line;
       end Display;

    end Data;

Finally, we implement the :ada:`Test_Data_Container` procedure, which
makes use of the data container:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Objects.In_Out_Formal_Object

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    with Ada.Calendar.Formatting;

    with Data;          use Data;
    with Data_Elements; use Data_Elements;

    procedure Test_Data_Container is

       package App_Data_Container is

          --
          --  Data container for all operations.
          --
          C : Data_Container;

          --
          --  Display procedures are specific for
          --  the data container.
          --

          procedure Display_First_Name_Age is new
            Display (Container => C,
                     Fields    => (1 => First_Name_F,
                                   2 => Age_F),
                     Header    => "FIRST_NAME AGE");

          procedure Display_Name_Birthday is new
            Display (Container => C,
                     Fields    => (1 => First_Name_F,
                                   2 => Last_Name_F,
                                   3 => Birthday_F),
                     Header    => "NAME BIRTHDAY");
       end App_Data_Container;

       use App_Data_Container;

       --
       --  Data container initialization
       --

       procedure Init_Container is
          function To_US (S : String)
                          return Unbounded_String
            renames
              To_Unbounded_String;
       begin
          Insert
            (C, (First_Name => To_US ("John"),
                 Last_Name  => To_US ("Smith"),
                 Birthday   =>
                   Ada.Calendar.Formatting.Time_Of
                     (Year        => 1951,
                      Month       => 5,
                      Day         => 1)));

          Insert
            (C, (First_Name => To_US ("Alice"),
                 Last_Name  => To_US ("Williams"),
                 Birthday   =>
                   Ada.Calendar.Formatting.Time_Of
                     (Year        => 1968,
                      Month       => 10,
                      Day         => 12)));
       end Init_Container;

    begin
       Init_Container;

       Display_First_Name_Age;
       Display_Name_Birthday;

    end Test_Data_Container;

In this example, we declare the data container :ada:`C` and bind it to
two instantiations of the :ada:`Display` procedure:

- :ada:`Display_First_Name_Age`, which displays the first name and age of
  each person from the database;

- :ada:`Display_Name_Birthday`, which displays the full name and birthday
  of each person.

Formal definite and indefinite types
------------------------------------

.. admonition:: Relevant topics

    - definite and indefinite (sub)types in the context of generics
    - discriminants
        - :ada:`A'Constrained`

.. todo::

    Complete section!


Formal incomplete type
----------------------

.. admonition:: Relevant topics

    - Formal incomplete type mentioned in
      :arm:`Formal Types <12-5>`

.. todo::

    Complete section!


Default subtype mark
--------------------

.. admonition:: Relevant topics

    - Default subtype mark (:ada:`or use`) mentioned in
      :arm:`Formal Types <12-5>`

.. todo::

    Complete section!


Formal private and derived types
--------------------------------

.. admonition:: Relevant topics

    - :arm:`Formal Private and Derived Types <12-5-1>`

.. todo::

    Complete section!


Formal interfaces
-----------------

Generating subprogram specifications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Formal interfaces can be used to generate a collection of pre-defined
subprograms for new types. For example, let's suppose that, for a given
type :ada:`T`, we need at least a pair of subprograms that set and get
elements of type :ada:`T` based on another type. We might want to convert
back and forth between the types :ada:`T` and :ada:`Integer`. In addition,
we might want to convert from and to other types (e.g., :ada:`Float`). To
implement this, we can define the following generic interface:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Gen_Interface

    package Gen_Interface is

       generic
          type TD is private;
          type TI is interface;
       package Set_Get is
          type T is interface and TI;

          procedure Set (E : in out T;
                         D :        TD) is abstract;
          function Get (E : T)
                        return TD is abstract;
       end Set_Get;

    end Gen_Interface;

In this example, the package :ada:`Set_Get` defines subprograms that allow
converting from any definite type (:ada:`TD`) and the interface type
(:ada:`TI`).

We then proceed to declare packages for converting between :ada:`Integer`
and :ada:`Float` types and the interface type. Also, we declare an actual
tagged type that combines these conversion subprograms into a single type:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Gen_Interface

    with Gen_Interface;

    package My_Type_Pkg is

       type My_Type_Interface is interface;

       package Set_Get_Integer is new
         Gen_Interface.Set_Get
           (TD => Integer,
            TI => My_Type_Interface);
       use Set_Get_Integer;

       package Set_Get_Float   is new
         Gen_Interface.Set_Get
           (TD => Float,
            TI => My_Type_Interface);
       use Set_Get_Float;

       type My_Type is
         new Set_Get_Integer.T and
             Set_Get_Float.T
         with private;

       overriding procedure Set (E : in out My_Type;
                                 D :        Integer);
       overriding function Get (E : My_Type)
                                return Integer;

       overriding procedure Set (E : in out My_Type;
                                 D :        Float);
       overriding function Get (E : My_Type)
                                return Float;

    private
       type My_Type is
         new Set_Get_Integer.T and
             Set_Get_Float.T
         with record
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

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Gen_Interface

    package body My_Type_Pkg is

       procedure Set (E : in out My_Type;
                      D :        Integer) is
       begin
          E.I := D;
          E.F := Float (D);
       end Set;

       function Get (E : My_Type)
                     return Integer is
       begin
          return E.I;
       end Get;

       procedure Set (E : in out My_Type;
                      D :        Float) is
       begin
          E.F := D;
          E.I := Integer (D);
       end Set;

       function Get (E : My_Type)
                     return Float is
       begin
          return E.F;
       end Get;

    end My_Type_Pkg;

As expected, declaring and using variable of :ada:`My_Type` is
straightforward:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Gen_Interface

    with My_Type_Pkg; use My_Type_Pkg;

    procedure Show_Gen_Interface is
       C : My_Type;
    begin
       C.Set (2);
       C.Set (2.1);
    end Show_Gen_Interface;

Facilitating arrays of interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Formal interfaces can facilitate the handling of arrays of interface
types. Let's consider an interface type :ada:`TI` and the derived tagged
types :ada:`T` and :ada:`T2`. We may declare arrays containing elements
that access the :ada:`TI` class. These arrays can be initialized with
elements that access types :ada:`T` or :ada:`T2`. Also, we may process
these arrays with an operation :ada:`Op` using the API of the :ada:`TI`
interface.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Array

    package TI_Pkg is

       type TI is interface;

       procedure Op (E : in out TI) is abstract;

       type TI_Class_Access is
         access all TI'Class;

       type TI_Array is
         array (Positive range <>) of TI_Class_Access;

       procedure Op (A : in out TI_Array);

    end TI_Pkg;

    package body TI_Pkg is

       procedure Op (A : in out TI_Array) is
       begin
          for E of A loop
             E.Op;
          end loop;
       end Op;

    end TI_Pkg;

    with TI_Pkg; use TI_Pkg;

    package T_Pkg is

       type T is new
         TI with null record;

       type T_Class_Access is
         access all T'Class;

       type T_Array is
         array (Positive range <>) of T_Class_Access;

       --  Missing implementation
       procedure Op (E : in out T) is null;

       type T2 is new T with null record;

       --  Missing implementation
       procedure Op (E : in out T2) is null;

    end T_Pkg;

This is a test application that declares an array :ada:`A` of the
interface type :ada:`TI` and calls :ada:`Op` for :ada:`A`:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Array

    with TI_Pkg; use TI_Pkg;
    with T_Pkg;  use T_Pkg;

    procedure Test_T is

       A : TI_Array (1 .. 3) :=
             (1 => new T,
              2 => new T2,
              3 => new T);

    begin

       Op (TI_Array (A));

    end Test_T;

This example doesn't work if we use an array of the derived type :ada:`T`:

.. code-block:: ada

    with TI_Pkg; use TI_Pkg;
    with T_Pkg;  use T_Pkg;

    procedure Test_T is

       A : T_Array (1 .. 3) :=
             (1 => new T,
              2 => new T2,
              3 => new T);

    begin

       Op (A);

    end Test_T;

This is incorrect because :ada:`Op` expects an array of type :ada:`TI`,
not :ada:`T`. Even if the type :ada:`T` is derived from :ada:`TI`, the
corresponding array type is not. Formal interfaces can be used to create
a generic version of :ada:`Op` that operates directly on an array of
type :ada:`T`. Let's look at an example.

The example below calculates the average of interface types that are
*convertible* to floating-point values. We consider that a type is
convertible to floating-point if it provides a :ada:`To_Float` function.
This is implemented with the :ada:`Float_Cnvt_Type` interface. We also
declare a generic package containing the :ada:`Average` function, which
calculates the average of an array containing elements of a
*convertible type* (i.e. any type derived from the :ada:`Float_Cnvt_Type`
interface).

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Float_Interface_Pkg

    package Float_Interface_Pkg is

       type Float_Cnvt_Type is interface;
       function To_Float (E : Float_Cnvt_Type)
                          return Float is abstract;

    end Float_Interface_Pkg;

    generic
       type Float_Cnvt_T is new
         Float_Cnvt_Type with private;
       type Float_Cnvt_Class_Access is
         access all Float_Cnvt_T'Class;
       type Float_Cnvt_Array is
         array (Positive range <>) of
           Float_Cnvt_Class_Access;
    package Float_Interface_Pkg.Ops is

       function Average (A : Float_Cnvt_Array)
                         return Float;

    end Float_Interface_Pkg.Ops;

This is the corresponding package body containing the implementation of
the generic :ada:`Average` function:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Float_Interface_Pkg

    package body Float_Interface_Pkg.Ops is

       function Average (A : Float_Cnvt_Array)
                         return Float is
       begin
          return Acc : Float do
             Acc := 0.0;
             for E of A loop
                Acc := Acc + E.To_Float;
             end loop;
             Acc := Acc /
                    Float (A'Last - A'First + 1);
          end return;
       end Average;

    end Float_Interface_Pkg.Ops;

In the :ada:`App_Data` package, we declare two types derived from
:ada:`Float_Cnvt_Type`: :ada:`T` and :ada:`T2`. We also declare the
corresponding :ada:`To_Float` functions.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Float_Interface_Pkg

    with Float_Interface_Pkg; use Float_Interface_Pkg;

    package App_Data is

       type T is new Float_Cnvt_Type with private;
       type T_Class_Access is access all T'Class;
       type T_Array is
         array (Positive range <>) of T_Class_Access;

       procedure Set (E : in out T; F : Float);
       function To_Float (E : T) return Float;

       type T2 is new T with private;
       type T2_Class_Access is access all T2'Class;

       procedure Set_Ext (E : in out T2;
                          F :        Float);
       overriding function To_Float (E : T2)
                                     return Float;

    private

       type T is new Float_Cnvt_Type with record
          F : Float := 0.0;
       end record;

       type T2 is new T with record
          F2 : Float := 0.0;
       end record;

    end App_Data;

This is the corresponding package body:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Float_Interface_Pkg

    package body App_Data is

       procedure Set (E : in out T; F : Float) is
       begin
          E.F := F;
       end Set;

       function To_Float (E : T) return Float is
         (E.F);

       procedure Set_Ext (E : in out T2; F : Float) is
       begin
          E.F2 := F;
       end Set_Ext;

       function To_Float (E : T2) return Float is
         (E.F + E.F2);

    end App_Data;

Finally, this is a test application that declares an array of
*convertible* types and calls the :ada:`Average` function to calculate
the average of all elements.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Float_Interface_Pkg

    with App_Data;                use App_Data;
    with Float_Interface_Pkg.Ops;

    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Show_Average is

       package Ops is new Float_Interface_Pkg.Ops
         (Float_Cnvt_T            => T,
          Float_Cnvt_Class_Access => T_Class_Access,
          Float_Cnvt_Array        => T_Array);

       A : T_Array (1 .. 3) :=
             (1 => new T,
              2 => new T2,
              3 => new T);

       Avg : Float;
    begin
       for I in A'Range loop
          A (I).Set (1.0);

          if A (I).all in T2'Class then
             declare
                A_I : T2_Class_Access :=
                        T2_Class_Access (A (I));
             begin
                A_I.Set_Ext (3.0);
             end;
          end if;
       end loop;

       Avg := Ops.Average (A);

       Put_Line ("Avg: " & Float'Image (Avg));

    end Show_Average;

In this example, we declare the array :ada:`A` with elements of both
:ada:`T` and :ada:`T2` types. After initializing the elements of :ada:`A`,
we call the :ada:`Average` function from :ada:`Ops`, an instance of the
generic package :ada:`Float_Interface_Pkg.Ops`.

Discussion: formal interfaces vs. other approaches
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Ada, we basically have three approaches to describe interfaces for
generic types. In addition to the approach using formal interfaces that
we've just seen above, we also have these approaches:

- Formal subprograms, which we've presented in the introductory course
  (in the
  :ref:`chapter about generics <Intro_Ada_Formal_Subprograms>` of the
  Introduction to Ada course).

- Signature packages, which we've discussed in a
  :ref:`previous section <Adv_Ada_Signature_Packages>`.

Let's briefly recapitulate these approaches:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Approaches

    package Interface_Approaches is

       -------------------------------
       --  Using Formal Subprograms --
       -------------------------------
       package Using_Formal_Subprograms is

          generic
             type T is private;
             with procedure P (E : T) is <>;
          package Pkg is
          end Pkg;

       end Using_Formal_Subprograms;

       -------------------------------
       --  Using Signature Packages --
       -------------------------------
       package Using_Signature_Packages is

          generic
             type T2;
             with procedure P (E : T2) is <>;
          package Sig_Pkg is
          end Sig_Pkg;

          generic
             type T is private;
             with package SP is new Sig_Pkg (T, <>);
          package Pkg is
          end Pkg;

       end Using_Signature_Packages;

       -------------------------
       --  Using Tagged Types --
       -------------------------
       package Using_Tagged_Types is

          type I is interface;
          procedure P (E : I) is abstract;

          generic
             type T is new I with private;
          package Pkg is
          end Pkg;

       end Using_Tagged_Types;

    end Interface_Approaches;

The following subsections discuss the pros and cons of each approach.
For the source-code examples, we'll implement a generic hash table.

Interfaces using formal subprograms
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Formal subprograms, combined with a formal type, can be used to define
an implicit interface. Let's look at the implementation of a generic hash
table:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Formal_Function

    with Ada.Containers; use Ada.Containers;

    package Interface_Using_Formal_Function is

       generic
          type T is private;
          with function Hash (Self : T)
                              return Hash_Type is <>;
       package Hash_Tables is
          --  Missing implementation
       end Hash_Tables;

    end Interface_Using_Formal_Function;

In contrast to formal interfaces, the interface described with formal
subprograms is implicit: we don't have an explicit :ada:`interface` type
defined here. However, the combination of type :ada:`T` and the function
:ada:`Hash` represent an interface.

The fact that we don't declare an explicit interface has the disadvantage
of not being as obvious as when the :ada:`interface` keyword is used in
the code. Developers are forced to recognize the design pattern: they have
to deduce that the intention of declaring :ada:`T` and :ada:`Hash` is to
define an interface. However, this approach has the advantage of not
requiring the use of tagged types in the package instantiation.

This is an example of a package instantiating the generic hash table:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Formal_Function

    with Ada.Containers; use Ada.Containers;
    with Ada.Strings.Hash;

    with Interface_Using_Formal_Function;
    use  Interface_Using_Formal_Function;

    package Instantiation_Using_Formal_Function is

       type My_Type is record
          Key   : String (1 .. 100);
          Key_2 : String (1 .. 100);
       end record;

       function Hash (Self : My_Type)
                      return Hash_Type is
         (Ada.Strings.Hash (Self.Key));

       function Alt_Hash (Self : My_Type)
                          return Hash_Type is
         (Ada.Strings.Hash (Self.Key_2));

       package My_Type_Hash_Tables is new
         Hash_Tables (My_Type);

       package My_Type_Alt_Hash_Tables is new
         Hash_Tables (T    => My_Type,
                      Hash => Alt_Hash);

    end Instantiation_Using_Formal_Function;

Note that, in the declaration of the :ada:`My_Type_Hash_Tables`, we're
not specifying the :ada:`Hash` function for the instantiation of the
generic :ada:`Hash_Tables` package. This is possible for two reasons:

- In the declaration of the formal function parameter, we're using
  :ada:`is <>`, which automatically selects a function with the same name
  and a compatible signature in the package instantiation if available.

- For :ada:`My_Type`, we've declared a function that has the same name as
  the formal function and the expected signature.

If the above-mentioned conditions are not met, we have to provide an
argument for the formal function parameter in the package instantiation.

We may also instantiate the formal package using alternative versions of
the function associated with the formal package. This is what we're doing
in the declaration of the :ada:`My_Type_Alt_Hash_Tables` package. In this
case, we're using :ada:`Alt_Hash` instead of :ada:`Hash` for the formal
function parameter. Note that, because the name of the actual function
doesn't match the name of the formal function, we need to indicate it
explicitly.

Interfaces using signature packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The basic form of signature packages is similar to the approach we've just
seen using formal subprograms: a signature package defines an interface
using a formal type and formal subprograms.

Signature packages make it more explicit that the types and subprograms
defined in the package represent an interface. This is an advantage over
the approach using formal subprograms directly. However, using signature
package isn't as explicit as using the :ada:`interface` keyword.

As mentioned before, signature packages aren't used in isolation, but in
combination with other generic packages. Also, they don't define anything
themselves. These features might provide a hint that a package is used to
represent an interface.

Let's look at the implementation of a generic hash table using a signature
package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Signature_Package

    with Ada.Containers; use Ada.Containers;

    package Interface_Using_Signature_Package is

       generic
          type Element;
          with function Hash (Self : Element)
                              return Hash_Type is <>;
       package Hashable_Signature is
       end Hashable_Signature;

       generic
          type T is private;
          with package T_Hashable is new
            Hashable_Signature (T, <>);
       package Hash_Tables is
          --  Missing implementation
       end Hash_Tables;

    end Interface_Using_Signature_Package;

Note that this approach is more verbose than the previous one using formal
subprograms directly. In this case, we have to declare two generic
packages instead of one.

This is an example of a package instantiating a signature package and the
generic hash table:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Signature_Package

    with Ada.Containers; use Ada.Containers;
    with Ada.Strings.Hash;

    with Interface_Using_Signature_Package;
    use  Interface_Using_Signature_Package;

    package Instantiation_Using_Signature_Package is

       type My_Type is record
          Key   : String (1 .. 100);
          Key_2 : String (1 .. 100);
       end record;

       function Hash (Self : My_Type)
                      return Hash_Type is
         (Ada.Strings.Hash (Self.Key));

       function Alt_Hash (Self : My_Type)
                          return Hash_Type is
         (Ada.Strings.Hash (Self.Key_2));

       package My_Type_Hashable is new
         Hashable_Signature (My_Type, Hash);

       package My_Type_Hash_Tables is new
         Hash_Tables (My_Type, My_Type_Hashable);

       package My_Type_Alt_Hashable is new
         Hashable_Signature (My_Type, Alt_Hash);

       package My_Type_Alt_Hash_Tables is new
         Hash_Tables (My_Type, My_Type_Alt_Hashable);

    end Instantiation_Using_Signature_Package;

This approach shares the same advantage listed for the previous approach:
we may use any type, not only tagged types for instantiating the generic
package. However, when using signature packages, the generic package
instantiation also becomes more verbose: we have to instantiate two
packages instead of one to achieve the same result. For the example above,
we first declare the :ada:`My_Type_Hashable` package and use it in the
declaration of the :ada:`My_Type_Hash_Tables` package.

The advantage of this approach is that the instantiation of the actual
package (the hash table in our example) is simplified: instead of passing
all formal subprograms as parameters to :ada:`My_Type_Hash_Tables`, we
only need to specify the signature package which contains the complete
interface. When implementing complex interfaces, this approach might lead
to a cleaner design than the previous approach using formal subprograms
directly.

Similar to the previous approach, we may also instantiate the formal
package using alternative versions of the function associated with the
formal package. This is what we're doing in the declaration of the
:ada:`My_Type_Alt_Hash_Tables` package.

Interfaces using tagged types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Finally, let's discuss the design of generic packages using formal
interfaces and tagged types. In contrast to the two approaches mentioned
above, formal interfaces explicitly indicate what's the interface in the
implementation through the :ada:`interface` keyword. No interpretation of
design patterns is needed in this case.

For the approaches we've discussed earlier (using formal subprograms and
signature packages), we were free to use any type in the instantiation of
the generic package. However, for generic packages using formal
interfaces, we can only use tagged types in the instantiation. This may
be a serious restriction, especially if we have to deal with existing code
containing types that are *not* tagged. Fortunately, in this case, we can
use the previous approaches to implement interfaces.

Let's look at the implementation of a generic hash table using a formal
interface:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Tagged_Types

    with Ada.Containers; use Ada.Containers;

    package Interface_Using_Tagged_Types is

       type Hashable is interface;
       function Hash (Self : Hashable)
                      return Hash_Type is abstract;

       generic
          type T is new Hashable with private;
       package Hash_Tables is
          --  Missing implementation
       end Hash_Tables;

    end Interface_Using_Tagged_Types;

This is an example of a package instantiating the generic hash table
using a tagged type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Tagged_Types

    with Ada.Containers; use Ada.Containers;
    with Ada.Strings.Hash;

    with Interface_Using_Tagged_Types;
    use  Interface_Using_Tagged_Types;

    package Instantiation_Using_Tagged_Types is

       type My_Type is new Hashable with record
          Key   : String (1 .. 100);
          Key_2 : String (1 .. 100);
       end record;

       function Hash (Self : My_Type)
                      return Hash_Type is
         (Ada.Strings.Hash (Self.Key));

       package My_Type_Hash_Tables is new
         Hash_Tables (My_Type);

    end Instantiation_Using_Tagged_Types;

The instantiation of generic packages is much simpler in this case: we
don't have to pass operations as parameters in the package instantiation.
In this example, the declaration of :ada:`My_Type_Hash_Tables` is very
straightforward: we just have to specify the tagged type (:ada:`My_Type`).
All operations are *implicitly defined* in the tagged type, so we don't
have to specify them. Conversely, we're bound to use the implementation
associated with the type. We cannot easily replace :ada:`Hash` by
:ada:`Alt_Hash` as in the previous approaches. In order to do that, we
have to declare a derived type and override the :ada:`Hash` function. This
is how we may create the :ada:`My_Type_Alt_Hash_Tables` package using the
alternative hashing function, as we did in the previous approaches:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Interface_Using_Tagged_Types

    with Ada.Containers;   use  Ada.Containers;
    with Ada.Strings.Hash;

    with Interface_Using_Tagged_Types;
    use  Interface_Using_Tagged_Types;

    with Instantiation_Using_Tagged_Types;
    use  Instantiation_Using_Tagged_Types;

    package Instantiation_Using_Alt_Tagged_Types is

       type My_Alt_Type is new
         My_Type with null record;

       overriding function Hash (Self : My_Alt_Type)
                                 return Hash_Type is
         (Ada.Strings.Hash (Self.Key_2));

       package My_Type_Alt_Hash_Tables is new
         Hash_Tables (My_Alt_Type);

    end Instantiation_Using_Alt_Tagged_Types;

In this example, the :ada:`Hash` function of the :ada:`My_Alt_Type` type
corresponds to the :ada:`Alt_Hash` function that we implemented in the
previous approaches.

Formal synchronized interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Formal synchronized interfaces are a specialized case of formal
interfaces that can be used for task types and protected types. Since
formal synchronized interfaces are similar to formal interfaces,
we can reuse the previous source-code example with minimal adaptations.

When adapting the :ada:`Gen_Interface` package, we just need to make use
of the :ada:`synchronized` keyword:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Formal_Synchronized_Interfaces

    package Gen_Sync_Interface is

       generic
          type TD is private;
          type TI is synchronized interface;
       package Set_Get is
          type T is synchronized interface and TI;

          procedure Set (E : in out T;
                         D :        TD) is abstract;
          function Get (E : T)
                        return TD is abstract;
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

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Formal_Synchronized_Interfaces

    with Gen_Sync_Interface;

    package My_Sync_Type_Pkg is

       type My_Type_Interface is
         synchronized interface;

       package Set_Get_Integer is new
         Gen_Sync_Interface.Set_Get
           (TD => Integer,
            TI => My_Type_Interface);
       use Set_Get_Integer;

       package Set_Get_Float is new
         Gen_Sync_Interface.Set_Get
           (TD => Float,
            TI => My_Type_Interface);
       use Set_Get_Float;

       protected type My_Type is new
         Set_Get_Integer.T and Set_Get_Float.T with

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Formal_Synchronized_Interfaces

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
          end Get;

          procedure Set (D : Float) is
          begin
             F := D;
             I := Integer (D);
          end Set;

          function Get return Float is
          begin
             return F;
          end Get;
       end My_Type;

    end My_Sync_Type_Pkg;

Finally, the main application doesn't require adaptations:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Interfaces.Formal_Synchronized_Interfaces

    with My_Sync_Type_Pkg; use My_Sync_Type_Pkg;

    procedure Show_Gen_Sync_Interface is
       C : My_Type;
    begin
       C.Set (2);
       C.Set (2.1);
    end Show_Gen_Sync_Interface;


Formal numeric types
---------------------

Ada supports the use of numeric types for generics. This can be used to
describe a numeric algorithm independently of the actual data type. We'll
see examples below.

This is the corresponding syntax:

- For floating-point types:  :ada:`type T is digits <>;`

- For binary fixed-point type: :ada:`type T is delta <>;`

- For decimal fixed-point types: :ada:`type T is delta <> digits <>;`

In this section, we discuss generic floating-point and binary fixed-point
types.

Formal floating-point types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Simple generic package
^^^^^^^^^^^^^^^^^^^^^^

Let's look at an example of a generic package containing a procedure that
*saturates* floating-point numbers. In this code, we work with a
normalized range between -1.0 and 1.0. Due to the fact that some
calculations might lead to results outside this range, we use the
:ada:`Saturate`  procedure to put values back into the normalized range.

This is the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Float

    generic
       type F is digits <>;
    package Gen_Float_Ops is
       procedure Saturate (V : in out F);
    end Gen_Float_Ops;

This is the package body:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Float

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

Finally, we create a test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Float

    with Ada.Text_IO;    use Ada.Text_IO;
    with Gen_Float_Ops;

    procedure Show_Float_Ops is

       package Float_Ops is new
         Gen_Float_Ops (F => Float);
       use Float_Ops;

       package Long_Float_Ops is new
         Gen_Float_Ops (F => Long_Float);
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

In this application, we create two instances of the :ada:`Gen_Float_Ops`
package: one for the :ada:`Float` type and one for the :ada:`Long_Float`
type. We then make use of computations whose results are outside the
normalized range. By calling the :ada:`Saturate` procedure, we ensure that
the values are inside the range again.

Operations in generic packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we discuss how to declare operations associated with
floating-point types in generic packages.

Let's first define a package that implements a new type :ada:`My_Float`
based on the standard :ada:`Float` type. For this type, we override the
addition operator with an implementation that saturates the value after
the actual addition.

This is the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    package Float_Types is

       type My_Float is new Float;
       function "+" (A, B : My_Float) return My_Float;

    end Float_Types;

This is the corresponding package body:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    package body Float_Types is

       procedure Saturate (V : in out My_Float) is
       begin
          if V > 1.0 then
             V := 1.0;
          elsif V < -1.0 then
             V := -1.0;
          end if;
       end Saturate;

       overriding function "+" (A, B : My_Float)
                                return My_Float is
       begin
          return R : My_Float do
             R := My_Float (Float (A) + Float (B));
             Saturate (R);
          end return;
       end "+";

    end Float_Types;

Next, we create a package containing a procedure that accumulates
floating-point values. This is the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    generic
       type F is digits <>;
       with function "+" (A, B : F) return F is <>;
    package Gen_Float_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Float_Acc;

In this specification, we declare a formal function for the addition
operator using :ada:`with function`. This operator is used by the
:ada:`Acc` procedure in the package body. Also, because we use :ada:`<>`
in the specification, the corresponding addition operator for type
:ada:`F` is selected.

This is the package body:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    package body Gen_Float_Acc is

       procedure Acc (V : in out F; S : F) is
       begin
          V := V + S;
       end Acc;

    end Gen_Float_Acc;

This is a test application that makes use of the :ada:`Float_Types` and
:ada:`Gen_Float_Acc` packages.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    with Ada.Text_IO;    use Ada.Text_IO;

    with Float_Types; use Float_Types;
    with Gen_Float_Acc;

    procedure Show_Float_Overriding is

       package Float_Ops is new
         Gen_Float_Acc (F => My_Float);
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

We create an instance of the :ada:`Gen_Float_Acc` by using the
:ada:`My_Float` type declared in the :ada:`Float_Types` package. Because
we used :ada:`<>` in the specification of :ada:`function "+"` (in the
:ada:`Gen_Float_Acc` package), the compiler will automatically select
the addition operator that we've overriden in the :ada:`Float_Types`
package, so that we don't need to specify it in the package instantiation.

The main reason for the formal subprogram in the specification of the
:ada:`Gen_Float_Acc` package is that it prevents the compiler from
selecting the standard operator. We could have removed the
:ada:`function "+"` from the specification, as illustrated in the
example below, where we modified the :ada:`Gen_Float_Acc` package:

.. code-block:: ada

    generic
       type F is digits <>;
       --  no "with function" here!
    package Gen_Float_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Float_Acc;

    package body Gen_Float_Acc is

       procedure Acc (V : in out F; S : F) is
       begin
          --  Using standard addition for universal
          --  floating-point type (digits <>) here:
          V := V + S;
       end Acc;

    end Gen_Float_Acc;

In this case, however, even though we declared a custom addition operator
for the :ada:`My_Float` type in the :ada:`Float_Types` package, an
instantiation of the modified :ada:`Gen_Float_Acc` package would always
make use of the standard addition:

.. code-block:: ada

    --  This makes use of the type definition of
    --  My_Float, but not its overridden operators.
    package Float_Ops is new
      Gen_Float_Acc (F => My_Float);

Because the type :ada:`F` is declared as :ada:`digits <>`, which
corresponds to the universal floating-point data type, the compiler
selects operators associated with the universal floating-point data type
in the package body. By specifying the formal subprogram, we make sure
that the operator associated with the actual type is used.

Alternatively, we could make use of the :ada:`Float_Types` package
directly in the generic package. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Float_Types

    with Float_Types; use Float_Types;

    generic
       type F is new My_Float;
    package Gen_Float_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Float_Acc;

In this case, because the formal type is now based on :ada:`My_Float`, the
corresponding operator for :ada:`My_Float` is used in the :ada:`Acc`
procedure.

Formal fixed-point types
~~~~~~~~~~~~~~~~~~~~~~~~

Simple generic package
^^^^^^^^^^^^^^^^^^^^^^

In the previous section, we looked into an example of saturation for
generic floating-point types. Let's adapt this example for fixed-point
types. This is the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    generic
       type F is delta <>;
    package Gen_Fixed_Ops is
       function Sat_Add (V1, V2 : F) return F;
    end Gen_Fixed_Ops;

For the fixed-point version, we specify the normalized range in the
definition of the data type. Therefore, any computation that leads to
values out of the normalized range will raise a :ada:`Constraint_Error`
exception. In order to circumvent this, we can declare a fixed-point data
type with a wider range and use it in combination with the actual
operation that we want to perform -- an addition, in this case. This
approach  can be seen in the implementation of :ada:`Sat_Add`, which
computes the addition using the local :ada:`Ovhd_Fixed` type with wider
range, calls the :ada:`Saturate` procedure and converts the data type back
into the original range.

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    with Ada.Text_IO;    use Ada.Text_IO;

    package body Gen_Fixed_Ops is

       Ovhd_Depth : constant Positive := 64;
       Ovhd_Bits  : constant := 32;
       Ovhd_Delta : constant :=
                      2.0 ** Ovhd_Bits /
                      2.0 ** (Ovhd_Depth - 1);

       type Ovhd_Fixed is delta Ovhd_Delta range
         -2.0 ** Ovhd_Bits ..
          2.0 ** Ovhd_Bits - Ovhd_Delta
            with Size => Ovhd_Depth;

       --  Ensure that Ovhd_Fixed has enough headroom
       pragma Assert (Ovhd_Fixed'First <=
                      2.0 * Ovhd_Fixed (F'First));
       pragma Assert (Ovhd_Fixed'Last  >=
                      2.0 * Ovhd_Fixed (F'Last));

       --  Ensure that the precision is at least
       --  the same
       pragma Assert (Ovhd_Fixed'Small <= F'Small);

       procedure Saturate (V : in out Ovhd_Fixed)
          with Inline;

       procedure Saturate (V : in out Ovhd_Fixed) is
          First : constant Ovhd_Fixed :=
                    Ovhd_Fixed (F'First);
          Last  : constant Ovhd_Fixed :=
                    Ovhd_Fixed (F'Last);
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

:ada:`Ovhd_Fixed` is a 64-bit fixed-point data type. By using
:ada:`Assert`\s in the package body that compare this data type to the
formal :ada:`F` type from the package specification, we ensure that the
local fixed-point data type has enough overhead to cope with any
fixed-point operation that we want to implement. Also, we ensure that we
don't lose precision when converting back-and-forth between the local type
and the original type.

We then use the :ada:`Gen_Fixed_Ops` package in a test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    with Ada.Text_IO;    use Ada.Text_IO;
    with Gen_Fixed_Ops;

    procedure Show_Fixed_Ops is

       F_Depth  : constant Positive := 16;
       LF_Depth : constant Positive := 32;

       F_Delta  : constant :=
                    1.0 / 2.0 ** (F_Depth - 1);
       LF_Delta : constant :=
                    1.0 / 2.0 ** (LF_Depth - 1);

       type Fixed is
         delta F_Delta
         range -1.0 .. 1.0 - F_Delta
           with Size => F_Depth;

       type Long_Fixed is
         delta LF_Delta
         range -1.0 .. 1.0 - LF_Delta
           with Size => LF_Depth;

       package Fixed_Ops is new
         Gen_Fixed_Ops (F => Fixed);
       use Fixed_Ops;

       package Long_Fixed_Ops is new
         Gen_Fixed_Ops (F => Long_Fixed);
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

In this test application, we declare two fixed-point data types:
the 16-bit type :ada:`Fixed` and the 32-bit type :ada:`Long_Fixed`.
These types are used to create instances of the :ada:`Gen_Fixed_Ops`. By
calling :ada:`Sat_Add`, we ensure that the result of adding fixed-point
values will always be in the allowed range and the computation will never
raise an exception.

Operations in generic packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this section, we discuss how to declare operations associated with
fixed-point types in generic packages. We start by adapting the examples
used for floating-point in the previous section, so that fixed-point types
are used instead.

First, we define a package that implements a new fixed-point type called
:ada:`Fixed`. For this type, we override the addition operator with an
implementation that saturates the value after the actual addition. This is
the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    package Fixed_Types is

       F_Depth : constant Positive := 16;
       F_Delta     : constant :=
                       1.0 / 2.0 ** (F_Depth - 1);

       type Fixed is
         delta F_Delta
         range -1.0 .. 1.0 - F_Delta
           with Size => F_Depth;

       function "+" (A, B : Fixed) return Fixed;

    end Fixed_Types;

In the package body, we make use of the :ada:`Gen_Fixed_Ops` package that
we discussed earlier in the previous section. By instantiating the
:ada:`Gen_Fixed_Ops` package, we can use the :ada:`Sat_Add` function in
the implementation of the saturating addition operator.

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    with Gen_Fixed_Ops;

    package body Fixed_Types is

       package Fixed_Ops is new
         Gen_Fixed_Ops (F => Fixed);
       use Fixed_Ops;

       function "+" (A, B : Fixed) return Fixed is
       begin
          return R : Fixed do
             R := Sat_Add (A, B);
          end return;
       end "+";

    end Fixed_Types;

Next, we create a package containing a procedure that accumulates
fixed-point values. This is the package specification:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    generic
       type F is delta <>;
       with function "+" (A : F; B : F)
                          return F is <>;
    package Gen_Fixed_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Fixed_Acc;

In this specification, we declare a formal function for the addition
operator using :ada:`with function`. This operator is used by the
:ada:`Acc` procedure in the package body, which we show next.

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    package body Gen_Fixed_Acc is

       procedure Acc (V : in out F; S : F) is
       begin
          V := V + S;
       end Acc;

    end Gen_Fixed_Acc;

This is a test application that makes use of the :ada:`Fixed_Types` and
:ada:`Gen_Fixed_Acc` packages.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Generics.Formal_Numeric_Types.Generic_Fixed

    with Ada.Text_IO; use Ada.Text_IO;

    with Fixed_Types; use Fixed_Types;
    with Gen_Fixed_Acc;

    procedure Show_Fixed_Overriding is

       package Fixed_Ops is new
         Gen_Fixed_Acc (F => Fixed);
       use Fixed_Ops;

       F1 : Fixed := -0.5;

    begin
       Put_Line ("F1:  " & Fixed'Image (F1));

       Acc (F1, -0.9);

       Put_Line ("F1:  " & Fixed'Image (F1));
    end Show_Fixed_Overriding;

We create an instance of the :ada:`Gen_Fixed_Acc` by using the
:ada:`Fixed` type declared in the :ada:`Fixed_Types` package. We then
call :ada:`Acc` to accumulate and saturate a fixed-point variable.

As mentioned earlier in the section on generic floating-point types, the
main reason for the formal subprogram in the specification of the
:ada:`Gen_Fixed_Acc` package is that it prevents the compiler from
selecting the standard operator. Alternatively, we could make use of the
:ada:`Fixed_Types` package directly in the generic package:

.. code-block:: ada

    with Fixed_Types; use Fixed_Types;

    generic
       type F is new Fixed;
    package Gen_Fixed_Acc is
       procedure Acc (V : in out F; S : F);
    end Gen_Fixed_Acc;


Generic Renaming
----------------

.. admonition:: Relevant topics

    - :arm:`Generic Renaming Declarations <8-5-5>`

.. todo::

    Complete section!

