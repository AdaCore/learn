.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Private_Types.idioms
   :class: ada-run

   package Complex is
      type Number_T is private;
      function Constructor (Real_Part      : Float;
                            Imaginary_Part : Float)
                            return Number_T;
      procedure Constructor (This           : out Number_T;
                             Real_Part      :     Float;
                             Imaginary_Part :     Float);
      function Real_Part (This : Number_T) return Float;
      function Imaginary_Part (This : Number_T) return Float;
      function Str (This : Number_T) return String;
   
   private
      type Number_T is record
         Real_Part      : Float;
         Imaginary_Part : Float;
      end record;
   
      function Constructor (Real_Part      : Float;
                            Imaginary_Part : Float)
                            return Number_T is
         (Real_Part, Imaginary_Part);
   
      function Real_Part (This : Number_T) return Float is
        (This.Real_Part);
      function Imaginary_Part (This : Number_T) return Float is
        (This.Imaginary_Part);
   
   end Complex;

   package body Complex is
      procedure Constructor (This           : out Number_T;
                             Real_Part      :     Float;
                             Imaginary_Part :     Float) is
      begin
         This := Constructor (Real_Part, Imaginary_Part);
      end Constructor;
   
      function Str (This : Number_T) return String is
      begin
         return Float'Image (Real_Part (This)) & " " &
           Float'Image (Imaginary_Part (This)) & "i";
      end Str;
   
   end Complex;

   with Ada.Text_IO; use Ada.Text_IO;
   with Complex;     use Complex;
   procedure Main is
      Number : Number_T := Constructor (1.2, 3.4);
   begin
      Put_Line (Str (Number));
      Constructor (Number, 56.7, 8.9);
      Put_Line (Str (Number));
   end Main;
