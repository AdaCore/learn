with System.Storage_Elements; use System.Storage_Elements;
package Con03 is

   package Noncompliant is
      Global_Object : Integer
        with Volatile;
      function Get return Integer is (Global_Object);
   end Noncompliant;

   package Compliant is
      Global_Object : Integer
        with Volatile,
             Address => To_Address (16#1234_5678#);
      function Get return Integer is (Global_Object);
   end Compliant;

end Con03;
