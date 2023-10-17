pragma Profile (Ravenscar);
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;   use Ada.Text_IO;
package body Con01 is

   package Noncompliant is
      task Task_T is
      end Task_T;
   end Noncompliant;

   package body Noncompliant is
      task body Task_T is
      begin
         loop
            -- Error: No_Relative_Delay
            delay 1.0;
            Put_Line ("Hello World");
         end loop;
      end Task_T;
   end Noncompliant;

   package Compliant is
      task Task_T is
      end Task_T;
   end Compliant;

   package body Compliant is
      task body Task_T is
         Period     : constant Time_Span := Milliseconds (10);
         Activation : Time               := Clock;
      begin
         loop
            delay until Activation;
            Put_Line ("Hello World");
            Activation := Activation + Period;
         end loop;
      end Task_T;
   end Compliant;

   procedure Example is
   begin
      null;
   end Example;

end Con01;
