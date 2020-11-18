.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Tasking.some_advanced_concepts
   :class: ada-run

   package Some_Advanced_Concepts is
   
      Termination_Flag : Boolean := False;
   
      task Select_Loop_Task is
         entry Start;
         entry Receive_Message (V : String);
         entry Send_Message (V : String);
         entry Stop;
      end Select_Loop_Task;
   
   end Some_Advanced_Concepts;

   with Ada.Calendar; use Ada.Calendar;
   with Ada.Text_IO;  use Ada.Text_IO;
   package body Some_Advanced_Concepts is
   
      task body Select_Loop_Task is
      begin
         accept Start do
            Put_Line
              ("Select_Loop_Task started at" &
               Day_Duration'Image (Seconds (Clock)));
         end Start;
         loop
            select
               accept Receive_Message (V : String) do
                  Put_Line ("Select_Loop_Task Receive: " & V);
               end Receive_Message;
            or
               accept Send_Message (V : String) do
                  Put_Line ("Select_Loop_Task Send: " & V);
               end Send_Message;
            or when Termination_Flag =>
               accept Stop;
            or
               delay 5.0;
               Put_Line
                 ("No more waiting at" & Day_Duration'Image (Seconds (Clock)));
               exit;
            end select;
         end loop;
      end Select_Loop_Task;
   
   end Some_Advanced_Concepts;

   with Ada.Text_IO;            use Ada.Text_IO;
   with Some_Advanced_Concepts; use Some_Advanced_Concepts;
   procedure Test_Some_Advanced_Concepts is
   begin
      Put_Line ("calling start");
      Select_Loop_Task.Start;
      Select_Loop_Task.Receive_Message ("1");
      Select_Loop_Task.Send_Message ("A");
      Select_Loop_Task.Send_Message ("B");
      Select_Loop_Task.Receive_Message ("2");
      Select_Loop_Task.Stop;
   exception
      when Tasking_Error =>
         Put_Line ("Expected exception: Entry not reached");
   end Test_Some_Advanced_Concepts;
