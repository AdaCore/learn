procedure Oop06 is

   package Non_Compliant is
      type Root_T is tagged null record;
      procedure Set_Name (X    : Root_T;
                          Name : String)
        with Pre => Name'length > 0;
      function Get_Name (X : Root_T) return String
        with Post => Get_Name'result'length > 0;
   end Non_Compliant;

   package Compliant is
      type Root_T is tagged null record;
      procedure Set_Name (X    : Root_T;
                          Name : String)
        with Pre'class => Name'length > 0;
      function Get_Name (X : Root_T) return String
        with Post'class => Get_Name'result'length > 0;
   end Compliant;

   package body Non_Compliant is
      procedure Set_Name
        (X    : Root_T;
         Name : String) is null;
      function Get_Name
        (X : Root_T)
         return String is ("***");
   end Non_Compliant;

   package body Compliant is
      procedure Set_Name
        (X    : Root_T;
         Name : String) is null;
      function Get_Name
        (X : Root_T)
         return String is ("***");
   end Compliant;

begin
   null;
end Oop06;
