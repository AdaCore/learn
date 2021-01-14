.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.with_clauses
   :class: ada-run

   with Ada.Text_IO;
   package Base_Types is
      type Position_T is record
         Line   : Ada.Text_IO.Positive_Count;
         Column : Ada.Text_IO.Positive_Count;
      end record;
   end Base_Types;

   -- no need to "with" ada.text_io
   with Base_Types;
   package Files is
      subtype Name_T is String (1 .. 12);
      type File_T is record
         Name     : Name_T                := (others => ' ');
         Position : Base_Types.Position_T := (Line => 1, Column => 1);
      end record;
      function Create (Name : Name_T) return File_T;
   end Files;

   package body Files is
      -- "with" of base_types inherited from spec
      Default_Position : constant Base_Types.Position_T := (1, 1);
      function Create (Name : Name_T) return File_T is
        (Name => Name, Position => Default_Position);
   end Files;
