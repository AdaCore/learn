.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Array_Types.constrained_array_types
    :class: ada-run

   package Constrained_Array_Types is
   
      type Array_Of_Integers_T is array (1 .. 10) of Integer;
      type Array_Of_Bits_T is array (Natural range 0 .. 31) of Boolean;
   
      type Color_T is (Red, Green, Blue);
      type Color_Range_T is mod 256;
      type Rgb_T is array (Color_T) of Color_Range_T;
   
      Ten_Integers : Array_Of_Integers_T;
      One_Word     : Array_Of_Bits_T;
      Color        : Rgb_T;
   
   end Constrained_Array_Types;
