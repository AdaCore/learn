.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Packages.bodies
   :class: ada-run

   package Body_Not_Allowed is
      type Real is digits 12;
      type Device_Coordinates is record
         X, Y : Integer;
      end record;
      type Normalized_Coordinates is record
         X, Y : Real range 0.0 .. 1.0;
      end record;
      -- nothing to implement, so no body allowed
   end Body_Not_Allowed;

   package Body_Required is
      subtype Rows is Integer range 1 .. 24;
      subtype Columns is Integer range 1 .. 80;
      type Position is record
         Row : Rows    := Rows'First;
         Col : Columns := Columns'First;
      end record;
      -- The following need to be defined in the body
      procedure Move_Cursor (To : in Position);
      procedure Home;
   end Body_Required;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Body_Required is
      -- This function is not visible outside this package
      function Unsigned (Input : Integer) return String is
         Str : constant String := Integer'Image (Input);
      begin
         return Str
             (2 .. Str'Length);
      end Unsigned;
      procedure Move_Cursor (To : in Position) is
      begin
         Put
           (ASCII.ESC & "I" & Unsigned (To.Row) & ";" & Unsigned (To.Col) & "H");
      end Move_Cursor;
      procedure Home is null; -- not yet implemented
   end Body_Required;
