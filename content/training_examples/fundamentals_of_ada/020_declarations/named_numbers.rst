.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Declarations.named_numbers
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Named_Numbers is
      Universal_Third       : constant       := 1.0 / 3.0;
      Float_Third           : constant Float := 1.0 / 3.0;
      Float_Value           : Float;
      Long_Float_Value      : Long_Float;
      Long_Long_Float_Value : Long_Long_Float;
   begin
      Float_Value           := Universal_Third;
      Long_Float_Value      := Universal_Third;
      Long_Long_Float_Value := Universal_Third;
      Put_Line (Float'Image (Float_Value));
      Put_Line (Long_Float'Image (Long_Float_Value));
      Put_Line (Long_Long_Float'Image (Long_Long_Float_Value));
      Float_Value           := Float_Third;
      Long_Float_Value      := Long_Float (Float_Third);
      Long_Long_Float_Value := Long_Long_Float (Float_Third);
      Put_Line (Float'Image (Float_Value));
      Put_Line (Long_Float'Image (Long_Float_Value));
      Put_Line (Long_Long_Float'Image (Long_Long_Float_Value));
   end Named_Numbers;
