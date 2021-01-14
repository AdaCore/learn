.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Limited_Types.creating_values
   :class: ada-run

   with Interfaces; use Interfaces;
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
      function Create (Id : Id_T) return Also_Limited_T;
   end Multiprocessor_Mutex;

   package body Multiprocessor_Mutex is
      procedure Lock (This : in out Also_Limited_T) is null;
      procedure Unlock (This : in out Also_Limited_T) is null;
   
      Global_Lock_Counter : Interfaces.Unsigned_8 := 0;
      function Create (Id : Id_T) return Also_Limited_T is
      begin
         return Ret_Val : Also_Limited_T do
            if Global_Lock_Counter = Interfaces.Unsigned_8'Last then
               return;
            end if;
            Global_Lock_Counter := Global_Lock_Counter + 1;
            Ret_Val.Id          := Id;
            Ret_Val.Lock.Flag   := Global_Lock_Counter;
         end return;
      end Create;
   end Multiprocessor_Mutex;

   with Ada.Text_IO;          use Ada.Text_IO;
   with Multiprocessor_Mutex; use Multiprocessor_Mutex;
   procedure Perform_Lock is
      Lock1 : constant Also_Limited_T := Create ("One ");
      Lock2 : constant Also_Limited_T := Create ("Two ");
   begin
      Put_Line (Lock1.Id & Lock1.Lock.Flag'Image);
      Put_Line (Lock2.Id & Lock2.Lock.Flag'Image);
   end Perform_Lock;
