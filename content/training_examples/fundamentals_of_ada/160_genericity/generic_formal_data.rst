.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Genericity.generic_formal_data
   :class: ada-run

   package Generic_Formal_Data is
      generic
         type Variable_T is range <>;
         Variable : in out Variable_T;
         Increment : Variable_T;
      package Constants_And_Variables is
         procedure Add;
         procedure Subtract;
         function Value return Variable_T is (Variable);
      end Constants_And_Variables;
   
      generic
         type Type_T is (<>);
         with procedure Print_One (Prompt : String; Value : Type_T);
         with procedure Print_Two (Prompt : String; Value : Type_T) is null;
         with procedure Print_Three (Prompt : String; Value : Type_T) is <>;
      package Subprogram_Parameters is
         procedure Print (Prompt : String; Param : Type_T);
      end Subprogram_Parameters;
   end Generic_Formal_Data;

   with Ada.Text_IO;         use Ada.Text_IO;
   with Generic_Formal_Data; use Generic_Formal_Data;
   procedure Test_Generic_Formal_Data is
      procedure Print_One (Prompt : String; Param : Integer) is
      begin
         Put_Line (Prompt & " - Print_One" & Param'Image);
      end Print_One;
      procedure Print_Two (Prompt : String; Param : Integer) is
      begin
         Put_Line (Prompt & " - Print_Two" & Param'Image);
      end Print_Two;
      procedure Print_Three (Prompt : String; Param : Integer) is
      begin
         Put_Line (Prompt & " - Print_Three" & Param'Image);
      end Print_Three;
      procedure Print_Three_Prime (Prompt : String; Param : Integer) is
      begin
         Put_Line (Prompt & " - Print_Three_Prime" & Param'Image);
      end Print_Three_Prime;
   
      Global_Object : Integer := 0;
      package Global_Data is new Constants_And_Variables
        (Integer, Global_Object, 111);
   
      package Print_1 is new Subprogram_Parameters (Integer, Print_One);
      package Print_2 is new Subprogram_Parameters
        (Integer, Print_One, Print_Two);
      package Print_3 is new Subprogram_Parameters
        (Integer, Print_One, Print_Two, Print_Three_Prime);
   
   begin
      Print_1.Print ("print_1", Global_Data.Value);
      Global_Data.Add;
      Print_2.Print ("print_2", Global_Data.Value);
      Global_Data.Add;
      Print_3.Print ("print_3", Global_Data.Value);
   end Test_Generic_Formal_Data;

   package body Generic_Formal_Data is
      package body Constants_And_Variables is
         procedure Add is
         begin
            if (Variable + Increment) in Variable_T'Range then
               Variable := Variable + Increment;
            end if;
         end Add;
         procedure Subtract is
         begin
            if (Variable - Increment) in Variable_T'Range then
               Variable := Variable - Increment;
            end if;
         end Subtract;
      end Constants_And_Variables;
   
      package body Subprogram_Parameters is
         procedure Print (Prompt : String; Param : Type_T) is
         begin
            Print_One (Prompt, Param);
            Print_Two (Prompt, Param);
            Print_Three (Prompt, Param);
         end Print;
      end Subprogram_Parameters;
   end Generic_Formal_Data;
