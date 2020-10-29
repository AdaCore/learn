.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Packages.executable_parts
   :class: ada-run

   package Executable_Part is
      Visible_Seed : Integer;
      function Number return Float;
   end Executable_Part;
   
   package body Executable_Part is
      Hidden_Seed : Integer;
      procedure Initialize (Seed1 : out Integer;
                            Seed2 : out Integer) is
      begin
         Seed1 := Integer'First;
         Seed2 := Integer'Last;
      end Initialize;
      function Number return Float is (0.0); -- not yet implemented
   begin
      Initialize (Visible_Seed, Hidden_Seed);
   end Executable_Part;
   
   package Force_Body is
      pragma Elaborate_Body;
      Global_Data : array (1 .. 10) of Integer;
   end Force_Body;
   
   -- without elaborate_body, this is illegal
   package body Force_Body is
   begin
      for I in Global_Data'Range loop
         Global_Data (I) := I * 100;
      end loop;
   end Force_Body;
