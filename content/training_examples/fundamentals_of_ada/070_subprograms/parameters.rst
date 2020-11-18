.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Subprograms.parameters
    :class: ada-run

   procedure Parameters is
   
      procedure Do_Something (Formal_I : in Integer; Formal_B : out Boolean);
      procedure Do_Something (Formal_I : in Integer; Formal_B : out Boolean) is
      begin
         Formal_B := Formal_I > 0;
      end Do_Something;
   
      procedure All_Modes
        (Number : in     Integer;
         Value  : in out Integer;
         Result :    out Boolean) is
      begin
         Value  := Value * Number;
         Result := Value > 0;
      end All_Modes;
   
      procedure Defaults
        (A : Integer := 1;
         B : Integer := 2;
         C : Boolean := True;
         D : Boolean := False) is null;
   
      type Vector is array (Positive range <>) of Float;
      procedure Add (Left : in out Vector; Right : Vector) is
      begin
         for I in Left'First .. Left'Last loop
            Left (I) := Left (I) + Right (I);
         end loop;
      end Add;
   
      Actual_I1, Actual_I2 : Integer := 0;
      Actual_B             : Boolean;
      Actual_V             : Vector (1 .. 100);
   
   begin
      Do_Something
        (Actual_I1,
         Formal_B => Actual_B);
      All_Modes (Actual_I1 + 100, Actual_I2, Actual_B);
      -- All_Modes (Actual_I1, Actual_I2 + 100, Actual_B); -- compile error
      Defaults (1, 2, True, False);
      Defaults;
      -- Defaults (1, True); -- compile error
      Defaults
        (A => 1,
         D => True);
      Add (Actual_V (1 .. 10), Actual_V (11 .. 20));
   end Parameters;
