.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Private_Types.private_part_construction
   :class: ada-run

   package Sets is
      type Set_T is private;
      Null_Set : constant Set_T;
      type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      procedure Add (This : in out Set_T; Day : Days_T);
      procedure Remove (This : in out Set_T; Day : Days_T);
      function Str (This : Set_T) return String;
   private
      function Length (This : Set_T) return Natural;
      type Set_T is array (Days_T) of Boolean;
      Null_Set : constant Set_T := (others => False);
   end Sets;

   package body Sets is
      procedure Add (This : in out Set_T; Day : Days_T) is
      begin
         This (Day) := True;
      end Add;
      procedure Remove (This : in out Set_T; Day : Days_T) is
      begin
         This (Day) := False;
      end Remove;
      function Str (This : Set_T) return String is
         Ret_Val : String (1 .. Length (This) * 4) := (others => ' ');
         Pos     : Natural                         := 1;
      begin
         for D in This'Range loop
            if This (D) then
               Ret_Val
                 (Pos .. Pos + 2) := D'Image;
               Pos                := Pos + 4;
            end if;
         end loop;
         return Ret_Val;
      end Str;
      function Length (This : Set_T) return Natural is
         Ret_Val : Natural := 0;
      begin
         for D in This'Range loop
            Ret_Val := Ret_Val + (if This (D) then 1 else 0);
         end loop;
         return Ret_Val;
      end Length;
   end Sets;

   with Ada.Text_IO; use Ada.Text_IO;
   with Sets;        use Sets;
   procedure Main is
      Set : Set_T := Null_Set;
   begin
      Add (Set, Sun);
      Add (Set, Sat);
      Add (Set, Mon);
      Put_Line (Str (Set));
   end Main;
