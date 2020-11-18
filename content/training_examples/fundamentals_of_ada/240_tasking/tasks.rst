.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Tasking.tasks
   :class: ada-run

   package Tasks is
   
      task T is
         entry Start;
         entry Receive_Message (V : String);
      end T;
   
   end Tasks;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Tasks is
   
      task body T is
      begin
         loop
            accept Start do
               Put_Line ("Start");
            end Start;
            accept Receive_Message (V : String) do
               Put_Line ("Receive " & V);
            end Receive_Message;
         end loop;
      end T;
   
   end Tasks;

   with Ada.Text_IO; use Ada.Text_IO;
   with Tasks;       use Tasks;
   procedure Test_Tasks is
   begin
      Put_Line ("calling start");
      T.Start;
      Put_Line ("calling receive 1");
      T.Receive_Message ("1");
      Put_Line ("calling receive 2");
      --  Locks until somebody calls Start
      T.Receive_Message ("2");
   end Test_Tasks;
