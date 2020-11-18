.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Limited_Types.creating_values
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
      procedure Lock (This : in out Also_Limited_T);
      procedure Unlock (This : in out Also_Limited_T);
      function Create
        (Flag : Interfaces.Unsigned_8;
         Id   : Id_T)
         return Also_Limited_T;
   end Multiprocessor_Mutex;

   package body Multiprocessor_Mutex is
      procedure Lock (This : in out Also_Limited_T) is null;
      procedure Unlock (This : in out Also_Limited_T) is null;
      Global_Lock : Also_Limited_T := (Lock => (Flag => 0), Id => "GLOB");
      function Create
        (Flag : Interfaces.Unsigned_8;
         Id   : Id_T)
         return Also_Limited_T is
         Local_Lock : Also_Limited_T := (Lock => (Flag => 1), Id => "LOCA");
      begin
         Global_Lock.Lock.Flag := Flag;
         Local_Lock.Id         := Id;
         -- return Local_Lock; -- compile error return Global_Lock; -- compile --
         -- error
         return (Lock => (Flag => Flag), Id => Id);
      end Create;
   end Multiprocessor_Mutex;

   with Ada.Text_IO;          use Ada.Text_IO;
   with Multiprocessor_Mutex; use Multiprocessor_Mutex;
   procedure Perform_Lock is
      Lock1 : Also_Limited_T := (Lock => (Flag => 2), Id => "LOCK");
      Lock2 : Also_Limited_T;
   begin
      -- Lock2 := Create ( 3, "CREA" ); -- illegal
      Put_Line (Lock1.Id & Lock1.Lock.Flag'Image);
   end Perform_Lock;
