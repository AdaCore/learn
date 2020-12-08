.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Record_Types.components_rules
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Components_Rules is
      type File_T is record
         Name    : String (1 .. 12);
         Mode    : File_Mode;
         Size    : Integer range 0 .. 1_024;
         Is_Open : Boolean;
         -- Anonymous_Component : array (1 .. 3) of Integer;
         -- Constant_Component  : constant Integer := 123;
         -- Self_Reference      : File_T;
      end record;
      File : File_T;
   begin
      File.Name    := "Filename.txt";
      File.Mode    := In_File;
      File.Size    := 0;
      File.Is_Open := False;
      Put_Line (File.Name);
   end Components_Rules;
