.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Genericity.generic_data

   package Generic_Data is
      generic
         type Discrete_T is (<>);
         type Integer_T is range <>;
         type Float_T is digits <>;
         type Indefinite_T;
         type Tagged_T is tagged;
         type Array_T is array (Boolean) of Integer;
         type Access_T is access all Integer;
         type Private_T is private;
         type Unconstrained_T (<>) is private;
      package Parameter_Properties is
         procedure Do_Something ( Discrete_Param      : Discrete_T;
                                  Integer_Param       : Integer_T;
                                  Float_Param         : Float_T;
                                  Indefinite_Param    : access Indefinite_T;
                                  Tagged_Param        : Tagged_T;
                                  Array_Param         : Array_T;
                                  Access_Param        : Access_T;
                                  Private_Param       : Private_T;
                                  Unconstrained_Param : Unconstrained_T);
      end Parameter_Properties;
   
      generic
         type Item_T is private;
         type Access_Item_T is access all Item_T;
         type Index_T is (<>);
         type Array_T is array (Index_T range <>) of Access_Item_T;
      package Combination is
         procedure Add (List  : in out Array_T;
                        Index : in     Index_T;
                        Item  : in     Item_T);
      end Combination;
   end Generic_Data;

   with Types; use Types;
   with Generic_Data;
   package Generic_Instances is
      package Parameter_Properties_Instance is new Generic_Data
        .Parameter_Properties
        (Boolean, Integer, Float, Indefinite_T => Hidden_T,
         Tagged_T => Tagged_Record_T, Array_T => Boolean_Array_Of_Integers_T,
         Access_T => Access_Integer_T, Private_T => Some_Private_T,
         Unconstrained_T                       => String);
   
      type Item_T is (Red, White, Blue);
      type Access_T is access all Item_T;
      type Index_T is range 1 .. 100;
      type Array_T is array (Index_T range <>) of Access_T;
      package Combination_Instance is new Generic_Data.Combination
        (Item_T, Access_T, Index_T, Array_T);
   end Generic_Instances;

   package body Generic_Data is
      package body Parameter_Properties is
         procedure Do_Something ( Discrete_Param      : Discrete_T;
                                  Integer_Param       : Integer_T;
                                  Float_Param         : Float_T;
                                  Indefinite_Param    : access Indefinite_T;
                                  Tagged_Param        : Tagged_T;
                                  Array_Param         : Array_T;
                                  Access_Param        : Access_T;
                                  Private_Param       : Private_T;
                                  Unconstrained_Param : Unconstrained_T) is null;
      end Parameter_Properties;
   
      package body Combination is
         procedure Add (List  : in out Array_T;
                        Index : in     Index_T;
                        Item  : in     Item_T) is
         begin
            List (Index) := new Item_T'(Item);
         end Add;
      end Combination;
   end Generic_Data;

   package Types is
      type Hidden_T;
      type Tagged_Record_T is tagged record
         Field : access Hidden_T;
      end record;
      type Hidden_T is private;
      type Boolean_Array_Of_Integers_T is array (Boolean) of Integer;
      type Access_Integer_T is access all Integer;
      type Some_Private_T is private;
   private
      type Hidden_T is new Integer;
      type Some_Private_T is new Integer;
   end Types;
