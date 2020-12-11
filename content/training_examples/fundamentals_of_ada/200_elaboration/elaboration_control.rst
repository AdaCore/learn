.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.elab.elab
   :class: ada-run

   package Pure_P is
      pragma Pure;
      Some_Constant : constant Integer := Integer'Size;
      function Call (I : Integer) return Integer is (I);
   end Pure_P;

   with Pure_P;
   package Preelaborate_P is
      pragma Preelaborate;
      Global_Object : Integer := Pure_P.Some_Constant;
   end Preelaborate_P;

   package Elaborate_Body_P is
      pragma Elaborate_Body;
      function Call (I : Integer) return Integer;
   end Elaborate_Body_P;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Elaborate_Body_P is
      function Call (I : Integer) return Integer is
      begin
         Put_Line ("Call with " & Integer'Image (I));
         return I;
      end Call;
   begin
      Put_Line ("Elaborate_Body_P package execution");
   end Elaborate_Body_P;

   with Elaborate_Body_P; use Elaborate_Body_P;
   pragma Elaborate (Elaborate_Body_P);
   package Elab_1 is
      Spec_Object : Integer := Call (101);
      procedure Proc;
   end Elab_1;

   with Elab_2;
   package body Elab_1 is
      Body_Object : Integer := Call (102);
      procedure Proc is null;
   begin
      Body_Object := Body_Object + Call (103);
      Elab_2.Proc;
   end Elab_1;

   with Elaborate_Body_P; use Elaborate_Body_P;
   package Elab_2 is
      Spec_Object : Integer := Call (201);
      procedure Proc;
   end Elab_2;

   package body Elab_2 is
      Body_Object : Integer := Call (202);
      procedure Proc is null;
   begin
      Body_Object := Body_Object + Call (203);
   end Elab_2;

   with Elab_2;
   with Elab_1;
   pragma Elaborate_All (Elab_2);
   procedure Test_Elab_Control is
   begin
      Elab_1.Proc;
      Elab_2.Proc;
   end Test_Elab_Control;
