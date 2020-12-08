.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Statements.loop_statements
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Loop_Statements is
      File    : File_Type;
      Counter : Integer := 0;
      type Light_T is (Red, Yellow, Green);
   begin
      loop
         if not Is_Open (File) then
            exit;
         end if;
         Counter := Counter + 1;
         exit when Is_Open (File);
      end loop;
   
      while Is_Open (File) loop
         Counter := Counter - 1;
      end loop;
   
      for Light in Light_T loop
         Put_Line (Light_T'Image (Light));
      end loop;
   
      for Counter in reverse 1 .. 10 loop
         Put_Line (Integer'Image (Counter));
         exit when Is_Open (File);
      end loop;
   end Loop_Statements;
