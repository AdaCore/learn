.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Record_Types.components_rules
    :class: ada-run
   
   procedure Components_Rules is
      type Months_T is
        (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
      type Date_T is record
         Day                   : Integer range 1 .. 31;
         Month                 : Months_T;
         Year                  : Integer range 0 .. 2_099;
         Leap_Year, Valid_Date : Boolean;
         -- the following are all illegal
         Anonymous_Component : array (1 .. 3) of Integer;
         Constant_Component  : constant Integer := 123;
         Self_Reference      : Date_T;
      end record;
      Arrival : Date_T;
   begin
      Arrival.Day   := 27; -- components referenced by name
      Arrival.Month := Nov;
      Arrival.Year  := 1_990;
   end Components_Rules;
