.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Exceptions.user_defined_exceptions
   :class: ada-run

   with Ada.Text_IO;         use Ada.Text_IO;
   with GNAT.Random_Numbers; use GNAT.Random_Numbers;
   procedure Propagation is
      Error1 : exception;
      Error2 : exception;
   
      Gen : Generator;
      procedure Maybe_Raise is
         Test : constant Float := Random (Gen);
      begin
         if Test > 0.666 then
            raise Error1;
         end if;
      exception
         when Error1 =>
            if Test > 0.95 then
               raise Error2;
            else
               raise;
            end if;
      end Maybe_Raise;
   
      procedure One is
      begin
         Maybe_Raise;
      end One;
   
      procedure Two is
      begin
         One;
         Maybe_Raise;
      exception
         when Error1 =>
            Put_Line ("Exception from 1 or 2");
      end Two;
   
   begin
      Reset (Gen);
      Maybe_Raise;
      Two;
   exception
      when Error1 =>
         Put_Line ("Exception from 3");
   end Propagation;
