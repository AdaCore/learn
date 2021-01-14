.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.use_all_type_clauses
   :class: ada-run

   package Complex is
      type Number is private;
      function "*" (Left, Right : Number) return Number;
      function "/" (Left, Right : Number) return Number;
      function "+" (Left, Right : Number) return Number;
      procedure Put (C : Number);
      function Make (Real_Part, Imaginary_Part : Float) return Number;
      procedure Non_Primitive (X : Integer);
   private
      type Number is record
         Real_Part      : Float;
         Imaginary_Part : Float;
      end record;
   end Complex;

   with Complex;
   use all type Complex.Number;
   procedure Demo_Use_All_Type is
      A, B, C : Complex.Number;
   begin
      -- "use all type" makes these available
      A := Make
          (Real_Part      => 1.0,
           Imaginary_Part => 0.0);
      B := Make
          (Real_Part      => 1.0,
           Imaginary_Part => 0.0);
      C := A + B;
      Put (C);
      -- but not this one
      -- Non_Primitive (0);
   end Demo_Use_All_Type;

   with Complex;
   use type Complex.Number;
   procedure Demo_Use_Type is
      A, B, C : Complex.Number;
   begin
      -- "use type" makes this available
      C := A + B;
      -- but not these
      -- A := Make
      --     (Real_Part      => 1.0,
      --      Imaginary_Part => 0.0);
      -- B := Make
      --     (Real_Part      => 1.0,
      --      Imaginary_Part => 0.0);
      -- Put (C);
      -- Non_Primitive (0);
   end Demo_Use_Type;

   with Complex; use Complex;
   procedure Demo_Use is
      A, B, C : Complex.Number := (Complex.Make (1.1, 2.2));
   begin
      -- "use" makes all these available
      C := A + B;
      A := Make
          (Real_Part      => 1.0,
           Imaginary_Part => 0.0);
      B := Make
          (Real_Part      => 1.0,
           Imaginary_Part => 0.0);
      Put (C);
      Non_Primitive (0);
   end Demo_Use;

   package body Complex is
      function "*" (Left, Right : Number) return Number is (Left);
      function "/" (Left, Right : Number) return Number is (Left);
      function "+" (Left, Right : Number) return Number is (Left);
      procedure Put (C : Number) is null;
      function Make (Real_Part, Imaginary_Part : Float) return Number is
        ((Real_Part, Imaginary_Part));
      procedure Non_Primitive (X : Integer) is null;
   end Complex;
