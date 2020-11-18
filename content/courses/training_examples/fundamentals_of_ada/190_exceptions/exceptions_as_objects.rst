.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Exceptions.exceptions_as_objects
   :class: ada-run

   package Exception_Objects_Example is
   
      Public_Exception : exception;
   
      procedure Do_Something (X : Integer);
   
   end Exception_Objects_Example;

   package body Exception_Objects_Example is
      Hidden_Exception : exception;
   
      procedure Do_Something (X : Integer) is
      begin
         if X < 0 then
            raise Public_Exception;
         elsif X = 0 then
            raise Hidden_Exception;
         end if;
      end Do_Something;
   
   end Exception_Objects_Example;

   with Ada.Exceptions;            use Ada.Exceptions;
   with Ada.Text_IO;               use Ada.Text_IO;
   with Exception_Objects_Example; use Exception_Objects_Example;
   procedure Test_Exception_Objects_Example is
   begin
   
      for I in -1 .. 1 loop
         begin
            Put_Line ("Try " & I'Image);
            Do_Something (I);
            Put_Line ("   success");
         exception
            when Public_Exception =>
               Put_Line ("   Expected exception");
            when The_Err : others =>
               Put_Line ("   Unexpected exception");
               Put_Line ("      Name: " & Exception_Name (The_Err));
               Put_Line ("      Information: " & Exception_Information (The_Err));
               Put_Line ("      Message: " & Exception_Message (The_Err));
         end;
      end loop;
   
   end Test_Exception_Objects_Example;
