.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Record_Types.aggregates
    :class: ada-run
   
   procedure Aggregates is
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
      Birthdate            : Date_T;
      Personal_Information : Personal_Information_T;
      Employee             : Employee_Information_T;
   begin
      Birthdate            := (25, Dec, 2_001);
      Personal_Information :=
        (Name => "Jane Smith", Birthdate => (14, Feb, 2_002));
      Employee  := (1_234, Personal_Information => Personal_Information);
      Birthdate := (Month => Jan, others => 1);
   end Aggregates;
