.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Limited_Types.declarations
   :class: ada-run

   with Interfaces;
   package Multiprocessor_Mutex is
      subtype Id_T is String (1 .. 4);
      -- prevent copying of a lock
      type Limited_T is limited record
         Flag : Interfaces.Unsigned_8;
      end record;
      type Also_Limited_T is record
         Lock : Limited_T;
         Id   : Id_T;
      end record;
      procedure Lock (This : in out Also_Limited_T) is null;
      procedure Unlock (This : in out Also_Limited_T) is null;
   end Multiprocessor_Mutex;

