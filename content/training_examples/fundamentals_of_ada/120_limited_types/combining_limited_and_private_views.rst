.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Limited_Types.combining_limited_and_private_views
   :class: ada-run

   with Interfaces; use Interfaces;
   package Multiprocessor_Mutex is
      type Limited_T is limited private;
      procedure Lock (This : in out Limited_T);
      procedure Unlock (This : in out Limited_T);
      function Create return Limited_T;
   private
      type Limited_T is limited -- no internal copying allowed
      record
         -- users cannot see this field
         Flag : Interfaces.Unsigned_8;
      end record;
   end Multiprocessor_Mutex;

   package body Multiprocessor_Mutex is
      procedure Lock (This : in out Limited_T) is null;
      procedure Unlock (This : in out Limited_T) is null;
   
      Global_Lock_Counter : Interfaces.Unsigned_8 := 0;
      function Create return Limited_T is
      begin
         return Ret_Val : Limited_T do
            Global_Lock_Counter := Global_Lock_Counter + 1;
            Ret_Val.Flag        := Global_Lock_Counter;
         end return;
      end Create;
   end Multiprocessor_Mutex;

   with Multiprocessor_Mutex; use Multiprocessor_Mutex;
   package Use_Limited_Type is
      type Legal is limited private;
      type Also_Legal is limited private;
      -- type Not_Legal is private;
      -- type Also_Not_Legal is private;
   private
      type Legal is record
         S : Limited_T;
      end record;
      type Also_Legal is limited record
         S : Limited_T;
      end record;
      -- type Not_Legal is limited record
      --    S : Limited_T;
      -- end record;
      -- type Also_Not_Legal is record
      --    S : Limited_T;
      -- end record;
   end Use_Limited_Type;
