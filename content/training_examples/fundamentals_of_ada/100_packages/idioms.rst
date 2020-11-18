.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Packages.idioms
   :class: ada-run

   package Constants is
      Polar_Radius      : constant := 20_856_010.51;
      Equatorial_Radius : constant := 20_926_469.20;
      Earth_Diameter    : constant :=
        2.0 * ((Polar_Radius + Equatorial_Radius) / 2.0);
   end Constants;

   package Global_Data is
      Longitudinal_Velocity     : Float := 0.0;
      Longitudinal_Acceleration : Float := 0.0;
      Lateral_Velocity          : Float := 0.0;
      Lateral_Acceleration      : Float := 0.0;
      Vertical_Velocity         : Float := 0.0;
      Vertical_Acceleration     : Float := 0.0;
   end Global_Data;

   package Related_Units is
      type Vector is array (Positive range <>) of Float;
      function "+" (L, R : Vector) return Vector;
      function "-" (L, R : Vector) return Vector;
   end Related_Units;

   package body Related_Units is
      -- nothing is implemented yet!
      function "+" (L, R : Vector) return Vector is (L);
      function "-" (L, R : Vector) return Vector is (L);
   end Related_Units;

   package Stack_Abstract_Data_Machine is
      procedure Push (X : in Float);
      procedure Pop (X : out Float);
      function Empty return Boolean;
      function Full return Boolean;
   end Stack_Abstract_Data_Machine;

   package body Stack_Abstract_Data_Machine is
      -- nothing is implemented yet!
      procedure Push (X : in Float) is null;
      procedure Pop (X : out Float) is null;
      function Empty return Boolean is (True);
      function Full return Boolean is (True);
   end Stack_Abstract_Data_Machine;
