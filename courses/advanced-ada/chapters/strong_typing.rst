Strong typing
=============

Table access
------------

Typical implementation
~~~~~~~~~~~~~~~~~~~~~~

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


Using stronger typing
~~~~~~~~~~~~~~~~~~~~~

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

       type Selector is array (1 .. 2) of Ord_Chunk_Index;

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
             Put_Line ("Selector #" & Positive'Image (I)
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
             Put_Line ("Selector #" & Positive'Image (I)
                       & ": V1 = " & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;
    end Display_Ordered_Chunk;
