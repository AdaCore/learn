.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Statements.conditional_statements
    :class: ada-run

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Conditional_Statements is
      type Light_T is (Red, Yellow, Green);
      A, B  : Integer          := Integer (Line);
      Speed : Integer;
      Light : constant Light_T := Light_T'Val (Line);
   
   begin
      if Light = Red then
         Speed := 0;
      elsif Light = Green then
         Speed := 25;
      else
         Speed := 50;
      end if;
   
      case Light is
         when Red =>
            Speed := 0;
         when Green =>
            Speed := 25;
         when Yellow =>
            Speed := 50;
      end case;
   
      case A is
         when 1 .. 100 =>
            B := A;
         when -100 .. -1 =>
            B := -A;
         when others =>
            A := B;
      end case;
   
      Put_Line ("Speed = " & Speed'Image);
      Put_Line ("Light = " & Light'Image);
   
   end Conditional_Statements;
