.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Access_Types.memory_management
   :class: ada-run

   with Ada.Unchecked_Deallocation;
   package Memory_Management_Types is
      type Integer_Access_T is access all Integer;
      procedure Free is new Ada.Unchecked_Deallocation
        (Integer, Integer_Access_T);
   end Memory_Management_Types;

   with Memory_Management_Types; use Memory_Management_Types;
   with Ada.Exceptions;          use Ada.Exceptions;
   with Ada.Text_IO;             use Ada.Text_IO;
   procedure Memory_Management_Test is
   
      procedure Uninitialized_Pointer is
         Object : Integer_Access_T;
      begin
         Object.all := 123;
         Put_Line ("Object = " & Integer'Image (Object.all));
      exception
         when Err : others =>
            Put_Line ("Uninitialized_Pointer error: " & Exception_Name (Err));
      end Uninitialized_Pointer;
   
      procedure Double_Deallocation is
         Object : Integer_Access_T;
      begin
         Object := new Integer'(123);
         Put_Line ("Object = " & Integer'Image (Object.all));
         Free (Object);
         Free (Object);
      exception
         when Err : others =>
            Put_Line ("double_deallocation error: " & Exception_Name (Err));
      end Double_Deallocation;
   
      procedure Accessing_Deallocated_Memory is
         Object : Integer_Access_T;
      begin
         Object := new Integer'(123);
         Put_Line ("Object = " & Integer'Image (Object.all));
         Free (Object);
         Put_Line ("Object = " & Integer'Image (Object.all));
      exception
         when Err : others =>
            Put_Line
              ("accessing_deallocated_memory error: " & Exception_Name (Err));
      end Accessing_Deallocated_Memory;
   
      procedure Memory_Leak is
         Object  : Integer_Access_T;
         Counter : Integer := 1;
      begin
         while Counter < Integer'Last loop
            Object  := new Integer'(Counter);
            Counter := Counter + 1;
         end loop;
         Put_Line ("Counter = " & Counter'Image);
      exception
         when Err : others =>
            Put_Line
              ("memory_leak error: " & Counter'Image & " " &
               Exception_Name (Err));
      end Memory_Leak;
   
   begin
      Uninitialized_Pointer;
      Double_Deallocation;
      Accessing_Deallocated_Memory;
      Memory_Leak;
   end Memory_Management_Test;
