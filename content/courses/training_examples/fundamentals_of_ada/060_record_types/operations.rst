.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Record_Types.operations
    :class: ada-run
   
   procedure Operations is
      type Months_T is
        (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
      type Date_T is record
         Day   : Integer range 1 .. 31;
         Month : Months_T;
         Year  : Integer range 0 .. 2_099;
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
   end Operations;
