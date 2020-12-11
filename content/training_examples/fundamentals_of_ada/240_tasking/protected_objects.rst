.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Tasking.protected_objects
   :class: ada-run

   package Protected_Objects is
      protected Object is
         procedure Set (Prompt : String; V : Integer);
         function Get (Prompt : String) return Integer;
      private
         Local : Integer := 0;
      end Object;
   end Protected_Objects;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Protected_Objects is
      protected body Object is
         procedure Set (Prompt : String; V : Integer) is
            Str : constant String := "Set " & Prompt & V'Image;
         begin
            Local := V;
            Put_Line (Str);
         end Set;
         function Get (Prompt : String) return Integer is
            Str : constant String := "Get " & Prompt & Local'Image;
         begin
            Put_Line (Str);
            return Local;
         end Get;
      end Object;
   end Protected_Objects;

   package Tasks is
   
      task T1 is
         entry Start;
         entry Receive_Message;
      end T1;
   
      task T2 is
         entry Start;
         entry Receive_Message;
      end T2;
   
   end Tasks;

   with Tasks; use Tasks;
   procedure Test_Protected_Objects is
   begin
      T1.Start;
      T1.Receive_Message;
      T2.Start;
      T2.Receive_Message;
      T2.Receive_Message;
      T2.Receive_Message;
      T1.Receive_Message;
   end Test_Protected_Objects;

   with Protected_Objects; use Protected_Objects;
   package body Tasks is
   
      task body T1 is
      begin
         accept Start do
            Object.Set ("T1 Start", 0);
         end Start;
         loop
            accept Receive_Message do
               Object.Set ("T1 Receive", Object.Get ("T1 Receive") + 1);
            end Receive_Message;
         end loop;
      end T1;
   
      task body T2 is
      begin
         accept Start do
            Object.Set ("T2 Start", 0);
         end Start;
         loop
            accept Receive_Message do
               Object.Set ("T2 Receive", Object.Get ("T2 Receive") + 1);
            end Receive_Message;
         end loop;
      end T2;
   
   end Tasks;
