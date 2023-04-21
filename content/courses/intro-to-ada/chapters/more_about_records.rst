More about records
==================

.. include:: ../../global.txt

Dynamically sized record types
------------------------------

We have previously seen
:ref:`some simple examples of record types <Intro_Ada_Record_Type_Declaration>`.
Let's now look at some of the more advanced properties of this fundamental
language feature.

One point to note is that object size for a record type does not need to be
known at compile time. This is illustrated in the example below:

.. ?? The example code may have elaboration order problems unless
.. ?? an elaboration pragma is used.
.. ?? Consider simplifying or restructuring the example to avoid this issue

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record
    :class: ada-syntax-only

    package Runtime_Length is
       function Compute_Max_Len return Natural;
    end Runtime_Length;

    with Runtime_Length; use Runtime_Length;

    package Var_Size_Record is
        Max_Len : constant Natural :=
                    Compute_Max_Len;
        --          ^ Not known at compile time

        type Items_Array is
          array (Positive range <>) of Integer;

        type Growable_Stack is record
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural;
        end record;
        --  Growable_Stack is a definite type, but
        --  size is not known at compile time.

        G : Growable_Stack;
    end Var_Size_Record;

It is completely fine to determine the size of your records at run time, but
note that all objects of this type will have the same size.

Records with discriminant
-------------------------

In the example above, the size of the Items field is determined once, at
run-time, but every :ada:`Growable_Stack` instance will be exactly the same size.
But maybe that's not what you want to do. We saw that arrays in general offer
this flexibility: for an unconstrained array type, different objects can have
different sizes.

You can get analogous functionality for records, too, using a special kind of
field that is called a discriminant:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record_2

    package Var_Size_Record_2 is
        type Items_Array is
          array (Positive range <>) of Integer;

        type Growable_Stack (Max_Len : Natural) is
        record
        --                   ^ Discriminant. Cannot be
        --                     modified once
        --                     initialized.
           Items : Items_Array (1 .. Max_Len);
           Len   : Natural := 0;
        end record;
        --  Growable_Stack is an indefinite type
        --  (like an array)
    end Var_Size_Record_2;

Discriminants, in their simple forms, are constant: You cannot modify them once
you have initialized the object. This intuitively makes sense since they
determine the size of the object.

Also, they make a type indefinite: Whether or not the discriminant is used to
specify the size of an object, a type with a discriminant will be indefinite if
the discriminant is not declared with an initialization:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Test_Discriminants
    :class: ada-expect-compile-error

    package Test_Discriminants is
       type Point (X, Y : Natural) is record
          null;
       end record;

       P : Point;
       --  ERROR: Point is indefinite, so you
       --  need to specify the discriminants
       --  or give a default value

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  Those two declarations are equivalent.

    end Test_Discriminants;

This also means that, in the example above, you cannot declare an array of
Point values, because the size of a Point is not known.

As mentioned in the example above, we could provide a default value for the
discriminants, so that we could legally declare :ada:`Point` values without
specifying the discriminants. For the example above, this is how it would look:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Test_Discriminants

    package Test_Discriminants is
       type Point (X, Y : Natural := 0) is record
          null;
       end record;

       P : Point;
       --  We can now simply declare a "Point"
       --  without further ado. In this case,
       --  we're using the default values (0)
       --  for X and Y.

       P2 : Point (1, 2);
       P3 : Point := (1, 2);
       --  We can still specify discriminants.

    end Test_Discriminants;

Also note that, even though the :ada:`Point` type now has default
discriminants, we can still specify discriminants, as we're doing in the
declarations of :ada:`P2` and :ada:`P3`.

In most other respects discriminants behave like regular fields: You have to
specify their values in aggregates, as seen above, and you can access their
values via the dot notation.

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Var_Size_Record_2

    with Ada.Text_IO;       use Ada.Text_IO;

    with Var_Size_Record_2; use Var_Size_Record_2;

    procedure Main is
       procedure Print_Stack (G : Growable_Stack) is
       begin
          Put ("<Stack, items: [");
          for I in G.Items'Range loop
             exit when I > G.Len;
             Put (" " & Integer'Image (G.Items (I)));
          end loop;
          Put_Line ("]>");
       end Print_Stack;

       S : Growable_Stack :=
         (Max_Len => 128,
          Items   => (1, 2, 3, 4, others => <>),
          Len     => 4);
    begin
       Print_Stack (S);
    end Main;

.. note::
    In the examples above, we used a discriminant to determine the size of an
    array, but it is not limited to that, and could be used, for example, to
    determine the size of a nested discriminated record.

.. _Intro_Ada_Variant_Records:

Variant records
---------------

The examples of discriminants thus far have illustrated the declaration of
records of varying size, by having components whose size depends on the
discriminant.

However, discriminants can also be used to obtain the functionality of what are
sometimes called "variant records": records that can contain different sets of
fields.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record

    package Variant_Record is
       --  Forward declaration of Expr
       type Expr;

       --  Access to a Expr
       type Expr_Access is access Expr;

       type Expr_Kind_Type is (Bin_Op_Plus,
                               Bin_Op_Minus,
                               Num);
       --  A regular enumeration type

       type Expr (Kind : Expr_Kind_Type) is record
          --      ^ The discriminant is an
          --        enumeration value
          case Kind is
             when Bin_Op_Plus | Bin_Op_Minus =>
                Left, Right : Expr_Access;
             when Num =>
                Val : Integer;
          end case;
          --  Variant part. Only one, at the end of
          --  the record definition, but can be
          --  nested
       end record;
    end Variant_Record;

The fields that are in a :ada:`when` branch will be only available when the
value of the discriminant is covered by the branch. In the example above, you
will only be able to access the fields :ada:`Left` and :ada:`Right` when the
:ada:`Kind` is :ada:`Bin_Op_Plus` or :ada:`Bin_Op_Minus`.

If you try to access a field that is not valid for your record, a
:ada:`Constraint_Error` will be raised.

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record
    :class: ada-run-expect-failure

    with Variant_Record; use Variant_Record;

    procedure Main is
       E : Expr := (Num, 12);
    begin
       E.Left := new Expr'(Num, 15);
       --  Will compile but fail at runtime
    end Main;

Here is how you could write an evaluator for expressions:

.. code:: ada run_button project=Courses.Intro_To_Ada.More_About_Records.Variant_Record

    with Ada.Text_IO;    use Ada.Text_IO;

    with Variant_Record; use Variant_Record;

    procedure Main is
       function Eval_Expr (E : Expr) return Integer is
         (case E.Kind is
          when Bin_Op_Plus  =>
                 Eval_Expr (E.Left.all)
                 + Eval_Expr (E.Right.all),
          when Bin_Op_Minus =>
                 Eval_Expr (E.Left.all)
                 - Eval_Expr (E.Right.all),
          when Num => E.Val);

       E : Expr := (Bin_Op_Plus,
                    new Expr'(Bin_Op_Minus,
                              new Expr'(Num, 12),
                              new Expr'(Num, 15)),
                    new Expr'(Num, 3));
    begin
       Put_Line (Integer'Image (Eval_Expr (E)));
    end Main;

.. admonition:: In other languages

    Ada's variant records are very similar to Sum types in functional languages
    such as OCaml or Haskell. A major difference is that the discriminant is a
    separate field in Ada, whereas the 'tag' of
    a Sum type is kind of built in, and only accessible with pattern matching.

    There are other differences (you can have several discriminants in a
    variant record in Ada). Nevertheless, they allow the same kind of type
    modeling as sum types in functional languages.

    Compared to C/C++ unions, Ada variant records are more powerful in what
    they allow, and are also checked at run time, which makes them safer.
