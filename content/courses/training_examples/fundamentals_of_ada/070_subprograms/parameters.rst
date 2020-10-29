.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Subprograms.parameters
    :class: ada-run

   procedure Parameters is
   
      procedure Do_Something (Formal1 : in     Integer;
                              Formal2 :    out Boolean);
      procedure Do_Something (Formal1 : in     Integer;
                              Formal2 :    out Boolean) is
      begin
         Formal2 := Formal1 > 0;
      end Do_Something;
   
      procedure All_Modes (Number : in     Integer;
                           Value  : in out Integer;
                           Result :    out Boolean) is
      begin
         Value  := Value * Number;
         Result := Value > 0;
      end All_Modes;
   
      procedure Defaults (A : Integer := 1;
                          B : Integer := 2;
                          C : Boolean := True;
                          D : Boolean := False) is null;
   
      type Vector is array (Positive range <>) of Float;
      procedure Add (Left  : in out Vector;
                     Right :        Vector) is
      begin
         for I in Left'First .. Left'Last loop
            Left (I) := Left (I) + Right (I);
         end loop;
      end Add;
   
      Actual_X, Actual_Y : Integer;
      Actual_Y : Boolean;
      Actual_V : Vector (1 .. 100);
   
   begin
      Do_Something (Actual_X, Formal2 => Actual_Y);
      All_Modes (Actual_X + 100, Actual_Y, Actual_Z);
      All_Modes (Actual_X, Actual_Y + 100, Actual_Z); -- compile error
      Defaults (1, 2, True, False);
      Defaults;
      Defaults (1, True); -- compile error
      Defaults (A => 1, D => True);
      Add (Actual_V (1 .. 10), Actual_V (11 .. 20));
   end Parameters;
