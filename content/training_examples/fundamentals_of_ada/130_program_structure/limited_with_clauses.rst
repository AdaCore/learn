.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.limited_with_clauses
   :class: ada-run

   limited with Department;
   package Personnel is
      type Employee_T is private;
      procedure Assign
        (This : in out Employee_T; Section : in Department.Section_T);
   private
      type Employee_T is record
         Name        : String (1 .. 10);
         Assigned_To : access Department.Section_T;
      end record;
   end Personnel;

   limited with Personnel;
   package Department is
      type Section_T is private;
      procedure Set_Manager
        (This : in out Section_T; Who : in Personnel.Employee_T);
   private
      type Section_T is record
         Name    : String (1 .. 10);
         Manager : access Personnel.Employee_T;
      end record;
   end Department;

   with Department;
   package body Personnel is
      procedure Assign
        (This : in out Employee_T; Section : in Department.Section_T) is
      begin
         This.Assigned_To.all := Section;
      end Assign;
   end Personnel;

   with Personnel;
   package body Department is
      procedure Set_Manager
        (This : in out Section_T; Who : in Personnel.Employee_T) is
      begin
         This.Manager.all := Who;
      end Set_Manager;
   end Department;
