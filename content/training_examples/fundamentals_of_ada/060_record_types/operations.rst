.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Record_Types.operations
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Operations is
      type Date_T is record
         Day   : Integer range 1 .. 31;
         Month : Positive range 1 .. 12;
         Year  : Natural range 0 .. 2_099;
      end record;
      type Personal_Information_T is record
         Name      : String (1 .. 10);
         Birthdate : Date_T;
      end record;
      type Employee_Information_T is record
         Number               : Positive;
         Personal_Information : Personal_Information_T;
      end record;
      Employee : Employee_Information_T;
   begin
      Employee.Number                              := 1_234;
      Employee.Personal_Information.Name           := "Fred Smith";
      Employee.Personal_Information.Birthdate.Year := 2_020;
      Put_Line (Employee.Number'Image);
   end Operations;
