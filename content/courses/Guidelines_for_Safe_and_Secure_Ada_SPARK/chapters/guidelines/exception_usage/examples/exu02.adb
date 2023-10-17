with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
procedure Exu02 (X : Integer) is
   Application_Exception : exception;

   procedure Start_Application (Param : String) is null;

   package Noncompliant is
      Cli_Exception : exception;
      procedure Main;
   end Noncompliant;

   package Compliant is
      Cli_Exception : exception;
      procedure Main;
   end Compliant;

   package body Noncompliant is
      procedure Main is
      begin
         if Argument_Count = 0 then
            raise Cli_Exception;
         else
            begin
               Start_Application (Argument (1));
            exception
               when Application_Exception =>
                  Put_Line ("Application failed");
            end;
         end if;
      end Main;
   end Noncompliant;

   package body Compliant is
      procedure Main is
      begin
         if Argument_Count = 0 then
            raise Cli_Exception;
         else
            begin
               Start_Application (Argument (1));
            exception
               when Application_Exception =>
                  Put_Line ("Application failed");
            end;
         end if;
      exception
         when Cli_Exception =>
            Put_Line ("Failure");
      end Main;
   end Compliant;

begin
   null;

end Exu02;
