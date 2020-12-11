.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.elab.elab
   :class: ada-run

   with Initializer; use Initializer;
   package Elab_1 is
      Spec_Object : Integer := Call (101);
      procedure Proc;
   end Elab_1;

   package body Elab_1 is
      Body_Object : Integer := Call (102);
      procedure Proc is null;
   begin
      Body_Object := Body_Object + Call (103);
   end Elab_1;

   with Initializer; use Initializer;
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
   procedure Test_Elab is
   begin
      Elab_2.Proc;
      Elab_1.Proc;
   end Test_Elab;

   package Initializer is
      function Call (I : Integer) return Integer;
   end Initializer;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Initializer is
      function Call (I : Integer) return Integer is
      begin
         Put_Line ("Call with " & Integer'Image (I));
         return I;
      end Call;
   end Initializer;
