:orphan:

Privacy
=======

:code-config:`reset_accumulator=True;accumulate_code=False`

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Directions
----------

This exercise is based on the *Directions* exercise from the :doc:`records`
labs. In this version, however, :ada:`Ext_Angle` is a private type.

In the implementation of the :ada:`Test_Directions` procedure below, the
Ada developer tried to initialize :ada:`All_Directions` |mdash| an array
of :ada:`Ext_Angle` type |mdash| with aggregates. Since we now have a
private type, the compiler complains about this initialization. Your job
for this exercise is to fix the compilation error by using the appropriate
API function from the :ada:`Directions` package.

**Note**: the initialization of :ada:`All_Directions` in the code below
contains a consistency error where the angle doesn't match the assessed
direction. See if you can spot this error! This kind of errors can happen
when record components that have correlated information are initialized
individually without consistency checks |mdash| using private types helps
avoiding the problem.

.. code:: ada lab=Privacy.Directions

    --  START LAB IO BLOCK
    in 0:Direction_Chk
    out 0:Angle:  0 => EAST. Angle:  45 => NORTHWEST. Angle:  90 => NORTH. Angle:  91 => NORTHWEST. Angle:  180 => WEST. Angle:  270 => SOUTH.
    --  END LAB IO BLOCK

    package Directions is

       type Angle_Mod is mod 360;

       type Direction is
         (North,
          Northwest,
          West,
          Southwest,
          South,
          Southeast,
          East);

       function To_Direction (N : Angle_Mod) return Direction;

       type Ext_Angle is private;

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle;

       procedure Display (N : Ext_Angle);

    private

       type Ext_Angle is record
          Angle_Elem     : Angle_Mod;
          Direction_Elem : Direction;
       end record;

    end Directions;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Directions is

       procedure Display (N : Ext_Angle) is
       begin
          Put_Line ("Angle: "
                    & Angle_Mod'Image (N.Angle_Elem)
                    & " => "
                    & Direction'Image (N.Direction_Elem)
                    & ".");
       end Display;

       function To_Direction (N : Angle_Mod) return Direction is
       begin
          case N is
             when   0        => return East;
             when   1 ..  89 => return Northwest;
             when  90        => return North;
             when  91 .. 179 => return Northwest;
             when 180        => return West;
             when 181 .. 269 => return Southwest;
             when 270        => return South;
             when 271 .. 359 => return Southeast;
          end case;
       end To_Direction;

       function To_Ext_Angle (N : Angle_Mod) return Ext_Angle is
       begin
          return (Angle_Elem     => N,
                  Direction_Elem => To_Direction (N));
       end To_Ext_Angle;

    end Directions;

    with Directions; use Directions;

    procedure Test_Directions is
       type Ext_Angle_Array is array (Positive range <>) of Ext_Angle;

       All_Directions : constant Ext_Angle_Array (1 .. 6)
         := ((0,   East),
             (45,  Northwest),
             (90,  North),
             (91,  North),
             (180, West),
             (270, South));

    begin
       for I in All_Directions'Range loop
          Display (All_Directions (I));
       end loop;

    end Test_Directions;

    with Ada.Command_Line;  use Ada.Command_Line;
    with Ada.Text_IO;       use Ada.Text_IO;

    with Test_Directions;

    procedure Main is
         type Test_Case_Index is
         (Direction_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
          when Direction_Chk =>
             Test_Directions;
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

Limited Strings
---------------

In this exercise, we'll use :ada:`limited private` types. The code below
shows the :ada:`Lim_String` type (in the :ada:`Limited_Strings` package
specification), which acts as a container for strings. In the *background*
(i.e. in the private part), it's actually declared as an access type to a
:ada:`String`. The :ada:`Lim_String` type is initialized with :ada:`Init`
functions |mdash| either by taking another string, or by indicating the
number of characters for a string *container* |mdash| and displayed with
the :ada:`Put_Line` procedure.

As we've discussed in the course, variables of limited types cannot be
assigned to. Also, they don't have an equality operator (:ada:`=`). We
can, however, define our own, custom subprograms to circumvent this
limitation:

    - In order to copy instances of a limited type, we can define a
      custom :ada:`Copy` procedure.

    - In order to compare instances of a limited type, we can define an
      :ada:`=` operator.

Your job for this exercise is to complete the implementation of :ada:`Copy`
and :ada:`=` in the package body of :ada:`Limited_Strings`.

These are further requirements for the implementation:

    #. For both subprograms, the two parameters may refer to strings with
       different lengths. We'll limit the implementation to just take the
       minimum length:

        - In case of copying the string "Hello World" to a string with 5
          characters, the copied string is "Hello":

        .. code-block:: ada

               S1 : constant Lim_String := Init ("Hello World");
               S2 :          Lim_String := Init (5);
            begin
               Copy (From => S1, To => S2);
               Put_Line (S2);     --  This displays "Hello".

        - When comparing "Hello World" to "Hello", the :ada:`=` operator
          indicates that these strings are equivalent:

        .. code-block:: ada

               S1 : constant Lim_String := Init ("Hello World");
               S2 : constant Lim_String := Init ("Hello");
            begin
               if S1 = S2 then
                  --  True => This branch gets selected.

        - You can use the :ada:`Min_Last` constant |mdash| which is already
          declared in the implementation of these subprograms |mdash| in
          the code you write.

    #. When copying from a short string to a longer string, the remaining
       characters of the longer string must be initialized with underscores
       (``_``). For example:

    .. code-block:: ada

           S1 : constant Lim_String := Init ("Hello");
           S2 :          Lim_String := Init (10);
        begin
           Copy (From => S1, To => S2);
           Put_Line (S2);     --  This displays "Hello_____".

**Note**: the design and implementation of the :ada:`Limited_Strings`
package is very simplistic. A good design would have better handling of
access types, for example.

.. code:: ada lab=Privacy.Limited_Strings

    --  START LAB IO BLOCK
    in 0:Lim_String_Chk
    out 0:S1 => Hello World S2 => ______________________________ S1 isn't equal to S2. S3 => Hello S1 is equal to S3. S4 => Hello World___________________ S1 is equal to S4.
    --  END LAB IO BLOCK

    package Limited_Strings is

       type Lim_String is limited private;

       function Init (S : String) return Lim_String;

       function Init (Max : Positive) return Lim_String;

       procedure Put_Line (LS : Lim_String);

       procedure Copy (From :        Lim_String;
                       To   : in out Lim_String);

       function "=" (Ref, Dut : Lim_String) return Boolean;

    private

       type Lim_String is access String;

    end Limited_Strings;

    with Ada.Text_IO;

    package body Limited_Strings
    is

       function Init (S : String) return Lim_String is
          LS : constant Lim_String := new String'(S);
       begin
          return Ls;
       end Init;

       function Init (Max : Positive) return Lim_String is
          LS : constant Lim_String := new String (1 .. Max);
       begin
          LS.all := (others => '_');
          return LS;
       end Init;

       procedure Put_Line (LS : Lim_String) is
       begin
          Ada.Text_IO.Put_Line (LS.all);
       end Put_Line;

       function Get_Min_Last (A, B : Lim_String) return Positive is
       begin
          return Positive'Min (A'Last, B'Last);
       end Get_Min_Last;

       procedure Copy (From :        Lim_String;
                       To   : in out Lim_String) is
          Min_Last : constant Positive := Get_Min_Last (From, To);
       begin
          --  Complete the implementation!
          null;
       end;

       function "=" (Ref, Dut : Lim_String) return Boolean is
          Min_Last : constant Positive := Get_Min_Last (Ref, Dut);
       begin
          --  Complete the implementation!
          return True;
       end;

    end Limited_Strings;

    with Ada.Text_IO;     use Ada.Text_IO;

    with Limited_Strings; use Limited_Strings;

    procedure Check_Lim_String is
       S1 : constant Lim_String := Init ("Hello World");
       S2 : constant Lim_String := Init (30);
       S3 : Lim_String := Init (5);
       S4 : Lim_String := Init (30);
    begin
       Put ("S1 => ");
       Put_Line (S1);
       Put ("S2 => ");
       Put_Line (S2);

       if S1 = S2 then
          Put_Line ("S1 is equal to S2.");
       else
          Put_Line ("S1 isn't equal to S2.");
       end if;

       Copy (From => S1, To => S3);
       Put ("S3 => ");
       Put_Line (S3);

       if S1 = S3 then
          Put_Line ("S1 is equal to S3.");
       else
          Put_Line ("S1 isn't equal to S3.");
       end if;

       Copy (From => S1, To => S4);
       Put ("S4 => ");
       Put_Line (S4);

       if S1 = S4 then
          Put_Line ("S1 is equal to S4.");
       else
          Put_Line ("S1 isn't equal to S4.");
       end if;
    end Check_Lim_String;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Check_Lim_String;

    procedure Main is
       type Test_Case_Index is
         (Lim_String_Chk);

       procedure Check (TC : Test_Case_Index) is
       begin
          case TC is
          when Lim_String_Chk =>
             Check_Lim_String;
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

Bonus exercise
--------------

In previous labs, we had many source-code snippets containing records that
could be declared private. The source-code for the exercise above
(*Directions*) is an example: we've modified the type declaration of
:ada:`Ext_Angle`, so that the record is now private. Encapsulating the
record components |mdash| by declaring record components in the private
part |mdash| makes the code safer. Also, because many of the code snippets
weren't making use of record components directly (but handling record
types via the API instead), they continue to work fine after these
modifications.

This exercise doesn't contain any source-code. In fact, the goal here is
to modify previous labs, so that the record declarations are made private.
You can look into those labs, modify the type declarations, and recompile
the code. The corresponding test-cases must still pass.

In case you don't have a working version of the source-code of previous
labs, you can look into the corresponding solutions.

This is a list of selected labs that you can work on, including changes
that you can make:

#. :doc:`records`:

    #. Exercise: *Colors*:

        - Change :ada:`RGB` type to :ada:`private`.

#. *Arrays* labs:

    #. Exercise: *List of Names*:

        - Change :ada:`Person` and :ada:`People` types to
          :ada:`limited private`.

        - Move :ada:`People_Array` type declaration to private part.

#. *More About Types* labs:

    #. Exercise: *Price List*:

        - Change :ada:`Price_List` type to :ada:`limited private`.

    #. Exercise: *Inventory*:

        - Change :ada:`Item` and :ada:`Inventory` types to
          :ada:`limited private`.
