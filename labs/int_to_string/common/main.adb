with Int_To_String;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   pragma Assert (Int_To_String (12) = "12");
   pragma Assert (Int_To_String (-12) = "-12");
end Main;
