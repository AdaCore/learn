.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Record_Types.aggregates
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Aggregates is
   
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
      Birthdate            : Date_T;
      Personal_Information : Personal_Information_T;
      Employee             : Employee_Information_T;
   begin
      Birthdate := (25, 12, 2_001);
      Put_Line
        (Birthdate.Year'Image & Birthdate.Month'Image & Birthdate.Day'Image);
      Personal_Information := (Name => "Jane Smith", Birthdate => (14, 2, 2_002));
      Put_Line
        (Personal_Information.Birthdate.Year'Image &
         Personal_Information.Birthdate.Month'Image &
         Personal_Information.Birthdate.Day'Image);
      Employee := (1_234, Personal_Information => Personal_Information);
      Put_Line
        (Employee.Personal_Information.Birthdate.Year'Image &
         Employee.Personal_Information.Birthdate.Month'Image &
         Employee.Personal_Information.Birthdate.Day'Image);
      Birthdate := (Month => 1, others => 2);
      Put_Line
        (Birthdate.Year'Image & Birthdate.Month'Image & Birthdate.Day'Image);
   end Aggregates;
