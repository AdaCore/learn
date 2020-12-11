.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Exceptions.implicitly_and_explicitly_raised_exceptions
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   package body Implicit_Exceptions is
      Array_Object : array (1 .. 100) of Integer;
   
      procedure Raise_Constraint_Error (X : Integer) is
      begin
         Put_Line ("* Raise_Constraint_Error: " & X'Image);
         Array_Object (X) := X - 10;
      end Raise_Constraint_Error;
   
      function Raise_Program_Error (X : Integer) return Boolean is
      begin
         Put_Line ("* Raise_Program_Error: " & X'Image);
         if X in Array_Object'Range then
            return Array_Object (X) > 0;
         end if;
      end Raise_Program_Error;
   
   end Implicit_Exceptions;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Explicit_Exceptions is
      procedure Raise_Storage_Error (X : Integer) is
      begin
         Put_Line ("* Raise_Storage_Error: " & X'Image);
         if X < 0 then
            raise Storage_Error;
         end if;
      end Raise_Storage_Error;
   end Explicit_Exceptions;

   with Ada.Text_IO;         use Ada.Text_IO;
   with Implicit_Exceptions; use Implicit_Exceptions;
   with Explicit_Exceptions; use Explicit_Exceptions;
   procedure Test_Exceptions is
   
      procedure Test_Constraint_Error (X : Integer) is
      begin
         Raise_Constraint_Error (X);
         Put_Line ("Test_Constraint_Error success");
      exception
         when Constraint_Error =>
            Put_Line ("Test_Constraint_Error caught exception");
      end Test_Constraint_Error;
   
      procedure Test_Program_Error (X : Integer) is
      begin
         if Raise_Program_Error (X) then
            Put_Line ("Test_Program_Error true");
         else
            Put_Line ("Test_Program_Error false");
         end if;
      exception
         when Program_Error =>
            Put_Line ("Test_Program_Error caught exception");
      end Test_Program_Error;
   
      procedure Test_Storage_Error (X : Integer) is
      begin
         Raise_Storage_Error (X);
         Put_Line ("Test_Storage_Error success");
      exception
         when Storage_Error =>
            Put_Line ("Test_Storage_Error caught exception");
      end Test_Storage_Error;
   
   begin
   
      Test_Constraint_Error (20);
      Test_Constraint_Error (0);
      Test_Constraint_Error (Integer'Last);
      Test_Program_Error (Integer'First);
      Test_Program_Error (Integer'Last);
      Test_Storage_Error (Integer'First);
      Test_Storage_Error (Integer'Last);
   
   end Test_Exceptions;

   package Implicit_Exceptions is
      procedure Raise_Constraint_Error (X : Integer);
      function Raise_Program_Error (X : Integer) return Boolean;
   end Implicit_Exceptions;

   package Explicit_Exceptions is
      procedure Raise_Storage_Error (X : Integer);
   end Explicit_Exceptions;
