.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Overloading.user_defined_equality
   :class: ada-run

   procedure User_Defined_Equality is
      type Array_T is array (1 .. 10) of Integer;
      type List_T is record
         List  : Array_T;
         Count : Integer := 0;
      end record;
   
      function "=" (L, R : List_T) return Boolean is
      begin
         if L.Count /= R.Count then
            return False;
         else
            for I in 1 .. L.Count loop
               if L.List (I) /= R.List (I) then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end "=";
   begin
      null;
   end User_Defined_Equality;
