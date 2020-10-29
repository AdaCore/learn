.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Record_Types.default_values
    :class: ada-run
   
   with Ada.Calendar; use Ada.Calendar;
   with Ada.Text_IO;  use Ada.Text_IO;
   procedure Default_Values is
   
      type Complex is record
         Real      : Float := -1.0;
         Imaginary : Float := -1.0;
      end record;
   
      Phasor : Complex;
      I      : constant Complex := (0.0, 1.0);
   
   begin
      Put_Line
        (Float'Image (Phasor.Real) & " " & Float'Image (Phasor.Imaginary) & "i");
      Put_Line (Float'Image (I.Real) & " " & Float'Image (I.Imaginary) & "i");
      Phasor := (12.34, others => <>);
      Put_Line
        (Float'Image (Phasor.Real) & " " & Float'Image (Phasor.Imaginary) & "i");
   end Default_Values;
