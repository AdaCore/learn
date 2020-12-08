.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Overloading.user_defined_equality
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure User_Defined_Equality is
      type Array_T is array (1 .. 10) of Integer;
      type List_T is record
         List  : Array_T;
         Count : Integer := 0;
      end record;
   
      function "=" (L, R : List_T) return Boolean is
      begin
         if L.Count /= R.Count then
            Put_Line ("Count is off");
            return False;
         else
            for I in 1 .. L.Count loop
               if L.List (I) /= R.List (I) then
                  Put_Line ("elements don't match");
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end "=";
      L, R : List_T := (List => (others => 1), Count => 3);
   begin
      Put_Line (Boolean'Image (L = R));
      L.List (2) := 0;
      Put_Line (Boolean'Image (L = R));
      R.Count := 1;
      Put_Line (Boolean'Image (L = R));
   end User_Defined_Equality;
