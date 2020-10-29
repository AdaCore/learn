.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Statements.conditional_statements
    :class: ada-run

   procedure Conditional_Statements is
      type Light_T is (Red, Yellow, Green);
      A, B  : Integer := 0;
      Speed : Integer;
      Light : Light_T := Red;
   
   begin
      if Light = Red then
         Speed := 0;
      elsif Light = Green then
         Speed := 25;
      else
         Speed := 50;
      end if;
   
      case Light is
         when Red    => Speed := 0;
         when Green  => Speed := 25;
         when Yellow => Speed := 50;
      end case;
   
      case A is
         when 1 .. 100   => B := A;
         when -100 .. -1 => B := -A;
         when others     => null;
      end case;
   
   end Conditional_Statements;
