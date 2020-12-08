.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Exceptions.handlers
   :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   with Automotive;  use Automotive;
   procedure Joy_Ride is
      Hot_Rod : Vehicle_T;
      Bored   : Boolean := False;
   begin
      while not Bored loop
         Steer_Aimlessly (Bored);
         Consume_Fuel (Hot_Rod);
      end loop;
      Put_Line ("Driving Home");
      Drive_Home;
   exception
      when Fuel_Exhausted =>
         Put_Line ("Pushing Home");
         Push_Home;
   end Joy_Ride;

   package Automotive is
   
      Fuel_Exhausted : exception;
   
      type Vehicle_T is record
         Fuel_Quantity : Float := 10.0;
         Fuel_Minimum  : Float := 1.0;
      end record;
   
      procedure Consume_Fuel (Car : in out Vehicle_T);
      procedure Steer_Aimlessly (Flag : out Boolean);
      procedure Drive_Home;
      procedure Push_Home;
   
   end Automotive;

   with GNAT.Random_Numbers; use GNAT.Random_Numbers;
   package body Automotive is
      Gen : Generator;
   
      function Current_Consumption is new Random_Float (Float);
      function Random_Number is new Random_Discrete (Integer);
   
      procedure Consume_Fuel (Car : in out Vehicle_T) is
      begin
         if Car.Fuel_Quantity <= Car.Fuel_Minimum then
            raise Fuel_Exhausted;
         else
            Car.Fuel_Quantity := Car.Fuel_Quantity - Current_Consumption (Gen);
         end if;
      end Consume_Fuel;
   
      procedure Steer_Aimlessly (Flag : out Boolean) is
      begin
         Flag := Random_Number (Gen, 1, 50) = 1;
         if Random_Number (Gen, 1, 50) = 2 then
            raise Constraint_Error;
         end if;
      end Steer_Aimlessly;
   
      procedure Drive_Home is null;
      procedure Push_Home is null;
   
   begin
   
      Reset (Gen);
   
   end Automotive;
