Strong typing
=============

In this chapter, we discuss the advantages of strong typing and how it can
be used to avoid common implementation and maintenance issues.

Table access
------------

In this section, we discuss an application that accesses a two-dimensional
table. We first look into a typical implementation, and then discuss how
to improve it with better use of strong typing.

Typical implementation
~~~~~~~~~~~~~~~~~~~~~~

Let's look at an application that declares a two-dimensional lookup table,
retrieves a value from it an displays this value.

.. code:: ada

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Tab_Access is

       Tab : array (1 .. 5, 1 .. 10) of Float
         := ((0.50, 0.73, 0.22, 0.66, 0.64, 0.20, 0.73, 0.22, 0.66, 0.64),
             (0.60, 0.23, 0.56, 0.27, 0.72, 0.36, 0.27, 0.18, 0.18, 0.08),
             (0.20, 0.56, 0.74, 0.43, 0.72, 0.19, 0.46, 0.45, 0.25, 0.49),
             (0.75, 0.88, 0.29, 0.08, 0.17, 0.96, 0.23, 0.83, 0.89, 0.97),
             (0.18, 0.97, 0.82, 0.86, 0.96, 0.24, 0.84, 0.83, 0.14, 0.26));

       X, Y : Positive;
       V    : Float;

    begin
       X := 1;
       Y := 5;
       V := Tab (X, Y);

       Put_Line (Float'Image (V));
    end Show_Tab_Access;

In this application, we use :ada:`X` and :ada:`Y` as indices to access the
:ada:`Tab` table. We store the value in :ada:`V` and display it.

In principle, there is nothing wrong with this implementation. Also, we're
already making use of strong typing here, since accessing an invalid
position of the array (say :ada:`Tab (6, 25)`) raises an exception.
However, in this application, we're assuming that :ada:`X` always refers
to the first dimension, while :ada:`Y` refers to the second dimension.
What happens, however, if we write :ada:`Tab (Y, X)`? In the application
above, this would still work because :ada:`Tab (5, 1)` is in the table's
range. Even though this works fine here, it's not the expected behavior.
In the next section, we'll look into strategies to make better use of
strong typing to avoid this problem.

One could argue that the problem we've just described doesn't happen to
competent developers, who are expected to be careful. While this might be
true for the simple application we're discussing here, complex systems
can be much more complicated to understand: they might include multiple
tables and multiple indices for example. In this case, even competent
developers might make use of wrong indices to access tables. Fortunately,
Ada provides means to avoid this problem.


Using stronger typing
~~~~~~~~~~~~~~~~~~~~~

In the example above, we make use of the :ada:`Positive` type, which is
already a constrained type: we're avoiding accessing the :ada:`Tab` table
using an index with negative values or zero. But we still may use indices
that are out-of-range in the positive range, or switch the indices, as in
the :ada:`Tab (Y, X)` access we mentioned previously. These problems can
be avoided by defining range types for each dimension. This is the updated
implementation:

.. code:: ada

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Tab_Access is

       type X_Range is range 1 .. 5;
       type Y_Range is range 1 .. 10;

       Tab : array (X_Range, Y_Range) of Float
         := ((0.50, 0.73, 0.22, 0.66, 0.64, 0.20, 0.73, 0.22, 0.66, 0.64),
             (0.60, 0.23, 0.56, 0.27, 0.72, 0.36, 0.27, 0.18, 0.18, 0.08),
             (0.20, 0.56, 0.74, 0.43, 0.72, 0.19, 0.46, 0.45, 0.25, 0.49),
             (0.75, 0.88, 0.29, 0.08, 0.17, 0.96, 0.23, 0.83, 0.89, 0.97),
             (0.18, 0.97, 0.82, 0.86, 0.96, 0.24, 0.84, 0.83, 0.14, 0.26));

       X : X_Range;
       Y : Y_Range;
       V : Float;

    begin
       X := 1;
       Y := 5;
       V := Tab (X, Y);

       Put_Line (Float'Image (V));
    end Show_Tab_Access;

Now, we not only avoid mistakes like :ada:`Tab (Y, X)`, but we also detect
them at compile time! This might decrease development time, since we don't
need to run the application in order to check for those issues.

Also, maintenance becomes easier as well. Because we're explicitly stating
the allowed ranges for :ada:`X` and :ada:`Y`, developers can know how to
avoid constraint issues when accessing the :ada:`Tab` table. We're also
formally indicating the expected behavior. For example, because we declare
:ada:`X` to be of :ada:`X_Range` type, and that type is used in the first
dimension of :ada:`Tab`, we're documenting --- using the syntax of the Ada
language --- that :ada:`X` is supposed to be used to access the first
dimension of :ada:`Tab`. Using this approach, developers that need to
maintain this application can immediately identify the purpose of
:ada:`X` and use the variable accordingly.


Multiple indices
----------------

Typical implementation
~~~~~~~~~~~~~~~~~~~~~~

.. code:: ada

    package Indirect_Ordering is

       type Chunk is record
          V1  : Float;
          V2  : Float;
          Idx : Positive;
       end record;

       type Selector is array (1 .. 2) of Positive;

       type Mapping is array (Positive range <>) of Positive;

       type Chunks is array (Positive range <>) of Chunk;

       function Get_Ordered_Chunks (C : Chunks) return Chunks;

       function Get_Mapping (C : Chunks) return Mapping;

    end Indirect_Ordering;


.. code:: ada

    package body Indirect_Ordering is

       function Get_Mapping (C : Chunks) return Mapping is
       begin
          declare
             Map : Mapping (C'Range);
          begin
             for I in Map'Range loop
                for J in C'Range loop
                   if C (J).Idx = I then
                      Map (I) := J;
                   end if;
                end loop;
             end loop;

             return Map;
          end;
       end Get_Mapping;

       function Get_Ordered_Chunks (C : Chunks) return Chunks is
          Map : constant Mapping := Get_Mapping (C);
          OC  : Chunks (C'Range);
       begin
          for I in OC'Range loop
             OC (I) := C (Map (I));
          end loop;

          return OC;
       end Get_Ordered_Chunks;

    end Indirect_Ordering;


.. code:: ada

    with Indirect_Ordering; use Indirect_Ordering;

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Indirect_Ordering is

       function Init_Chunks return Chunks;

       function Init_Chunks return Chunks is
          C : Chunks (1 .. 4);
       begin
          C (1) := (V1  => 0.70, V2  => 0.72, Idx => 3);
          C (2) := (V1  => 0.20, V2  => 0.15, Idx => 1);
          C (3) := (V1  => 0.40, V2  => 0.74, Idx => 2);
          C (4) := (V1  => 0.80, V2  => 0.26, Idx => 4);

          return C;
       end Init_Chunks;

       C  : Chunks            := Init_Chunks;
       S  : constant Selector := (3, 1);
       M  : constant Mapping  := Get_Mapping (C);

    begin
       --  Loop over selector using original chunks
       for I in S'Range loop
          declare
             C1 : Chunk := C (M (S (I)));
          begin
             Put_Line ("Selector #" & Positive'Image (I)
                       & ": V1 = " & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;

       Display_Ordered_Chunk (C, S);
    end Show_Indirect_Ordering;


.. code:: ada

    procedure Display_Ordered_Chunk (C : Chunks;
                                     S : Selector) is
       OC : Chunks := Get_Ordered_Chunks (C);
    begin
       --  Loop over selector using ordered chunks
       for I in S'Range loop
          declare
             C1 : Chunk := OC (S (I));
          begin
             Put_Line ("Selector #" & Positive'Image (I)
                       & ": V1 = " & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;
    end Display_Ordered_Chunk;


Using stronger typing
~~~~~~~~~~~~~~~~~~~~~

.. code:: ada

    package Indirect_Ordering is

       type Chunk_Index     is new Positive;
       type Ord_Chunk_Index is new Positive;

       type Chunk is record
          V1  : Float;
          V2  : Float;
          Idx : Ord_Chunk_Index;
       end record;

       type Selector_Index is range 1 .. 2;

       type Selector is array (Selector_Index) of Ord_Chunk_Index;

       type Mapping is array (Ord_Chunk_Index range <>) of Chunk_Index;

       type Chunks is array (Chunk_Index range <>) of Chunk;

       type Ord_Chunks is array (Ord_Chunk_Index range <>) of Chunk;

       function Get_Ordered_Chunks (C : Chunks) return Ord_Chunks;

       function Get_Mapping (C : Chunks) return Mapping;

    end Indirect_Ordering;


.. code:: ada

    package body Indirect_Ordering is

       type Ord_Chunk_Range is record
          First : Ord_Chunk_Index;
          Last  : Ord_Chunk_Index;
       end record;

       function Get_Ord_Chunk_Range (C : Chunks)
           return Ord_Chunk_Range is
         ((Ord_Chunk_Index (C'First), Ord_Chunk_Index (C'Last)));

       function Get_Mapping (C : Chunks) return Mapping is
          R : constant Ord_Chunk_Range := Get_Ord_Chunk_Range (C);
       begin
          declare
             Map : Mapping (R.First .. R.Last);
          begin
             for I in Map'Range loop
                for J in C'Range loop
                   if C (J).Idx = I then
                      Map (I) := J;
                   end if;
                end loop;
             end loop;

             return Map;
          end;
       end Get_Mapping;

       function Get_Ordered_Chunks (C : Chunks) return Ord_Chunks is
          Map : constant Mapping := Get_Mapping (C);
          R   : constant Ord_Chunk_Range := Get_Ord_Chunk_Range (C);
          OC  : Ord_Chunks (R.First .. R.Last);
       begin
          for I in OC'Range loop
             OC (I) := C (Map (I));
          end loop;

          return OC;
       end Get_Ordered_Chunks;

    end Indirect_Ordering;


.. code:: ada

    with Indirect_Ordering; use Indirect_Ordering;

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Indirect_Ordering is

       function Init_Chunks return Chunks;

       function Init_Chunks return Chunks is
          C : Chunks (1 .. 4);
       begin
          C (1) := (V1  => 0.70, V2  => 0.72, Idx => 3);
          C (2) := (V1  => 0.20, V2  => 0.15, Idx => 1);
          C (3) := (V1  => 0.40, V2  => 0.74, Idx => 2);
          C (4) := (V1  => 0.80, V2  => 0.26, Idx => 4);

          return C;
       end Init_Chunks;

       C  : Chunks            := Init_Chunks;
       S  : constant Selector := (3, 1);
       M  : constant Mapping  := Get_Mapping (C);

    begin
       --  Loop over selector using original chunks
       for I in S'Range loop
          declare
             C1 : Chunk := C (M (S (I)));
          begin
             Put_Line ("Selector #" & Selector_Index'Image (I)
                       & ": V1 = " & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;

    end Show_Indirect_Ordering;


.. code:: ada

    procedure Display_Ordered_Chunk (C : Chunks;
                                     S : Selector) is
       OC : Ord_Chunks := Get_Ordered_Chunks (C);
    begin
       --  Loop over selector using ordered chunks
       for I in S'Range loop
          declare
             C1 : Chunk := OC (S (I));
          begin
             Put_Line ("Selector #" & Selector_Index'Image (I)
                       & ": V1 = " & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;
    end Display_Ordered_Chunk;
