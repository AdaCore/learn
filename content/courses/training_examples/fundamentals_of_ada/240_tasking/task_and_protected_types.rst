.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Tasking.task_and_protected_types
   :class: ada-run

   package Tasks is
      task type T is
         entry Start (Id : Character; Initial_1, Initial_2 : Integer);
         entry Receive_Message (Delta_1, Delta_2 : Integer);
      end T;
      T1, T2 : T;
   end Tasks;

   package Protected_Objects is
      protected type Object is
         procedure Set (Caller : Character; V : Integer);
         function Get return Integer;
         procedure Initialize (My_Id : Character);
      private
         Local : Integer   := 0;
         Id    : Character := ' ';
      end Object;
      O1, O2 : Object;
   end Protected_Objects;

   with Tasks;             use Tasks;
   with Protected_Objects; use Protected_Objects;
   procedure Test_Types is
   begin
      O1.Initialize ('X');
      O2.Initialize ('Y');
      T1.Start ('A', 1, 2);
      T2.Start ('B', 1_000, 2_000);
      T1.Receive_Message (1, 2);
      T2.Receive_Message (10, 20);
   end Test_Types;

   with Protected_Objects; use Protected_Objects;
   package body Tasks is
      task body T is
         My_Id : Character := ' ';
      begin
         accept Start (Id : Character; Initial_1, Initial_2 : Integer) do
            My_Id := Id;
            O1.Set (My_Id, Initial_1);
            O2.Set (My_Id, Initial_2);
         end Start;
         loop
            accept Receive_Message (Delta_1, Delta_2 : Integer) do
               declare
                  New_1 : constant Integer := O1.Get + Delta_1;
                  New_2 : constant Integer := O2.Get + Delta_2;
               begin
                  O1.Set (My_Id, New_1);
                  O2.Set (My_Id, New_2);
               end;
            end Receive_Message;
         end loop;
      end T;
   end Tasks;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Protected_Objects is
      protected body Object is
         procedure Initialize (My_Id : Character) is
         begin
            Id := My_Id;
         end Initialize;
         procedure Set (Caller : Character; V : Integer) is
            Str : constant String :=
              "Task-" & Caller & " Object-" & Id & " => " & V'Image;
         begin
            Local := V;
            Put_Line (Str);
         end Set;
         function Get return Integer is
         begin
            return Local;
         end Get;
      end Object;
   end Protected_Objects;
