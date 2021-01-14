.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Subprograms.declarations_and_bodies
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Declarations_And_Bodies is
   
      -- declarations
      procedure Swap (A, B : in out Integer);
      function Triple (X : Float) return Float;
   
      -- bodies
      procedure Swap (A : in out Integer; B : in out Integer) is
         C : constant Integer := A;
      begin
         A := B;
         B := C;
      end Swap;
      function Triple (X : Float) return Float is
      begin
         return X * 3.0;
      end Triple;
   
      function Factorial (Counter : Natural) return Natural;
      function Factorial (Counter : Natural) return Natural is
      begin
         if Counter = 1 then
            return 1;
         else
            return Counter * Factorial (Counter - 1);
         end if;
      end Factorial;
   
      I1, I2 : Integer := 123;
   
   begin
      Swap (I1, I2);
      Put_Line (Float'Image (Triple (12.3)));
      Put_Line (Natural'Image (Factorial (5)));
   
   end Declarations_And_Bodies;
