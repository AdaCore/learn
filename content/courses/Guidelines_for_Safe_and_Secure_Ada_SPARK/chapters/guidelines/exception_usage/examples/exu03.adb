with Ada.Text_IO; use Ada.Text_IO;
procedure Exu03 (X : in out Integer) is

   procedure Noncompliant (Param : in out Integer) is
      Noncompliant_Exception : exception;
   begin
      Param := Param * Param;
   exception
      when others =>
         raise Noncompliant_Exception;
   end Noncompliant;

   procedure Bad_Call (Param : in out Integer) is
   begin
      Noncompliant (Param);
   exception
      when Noncompliant_Exception =>  -- compile error
         null;
   end Bad_Call;

   Compliant_Exception : exception;
   procedure Compliant (Param : in out Integer) is
   begin
      Param := Param * Param;
   exception
      when others =>
         raise Compliant_Exception;
   end Compliant;

   procedure Good_Call (Param : in out Integer) is
   begin
      Compliant (Param);
   exception
      when Compliant_Exception =>
         null;
   end Good_Call;

begin
   null;
exception
   when others =>
      null;
end Exu03;
