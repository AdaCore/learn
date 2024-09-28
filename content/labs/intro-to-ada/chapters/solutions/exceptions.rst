Exceptions
----------

Uninitialized Value
~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Uninitialized_Value

    --  START LAB IO BLOCK
    in 0:Options_Chk
    out 0:Uninitialized value detected! OPTION_1 OPTION_2 OPTION_3
    --  END LAB IO BLOCK

    package Options is

       type Option is (Uninitialized,
                       Option_1,
                       Option_2,
                       Option_3);

       Uninitialized_Value : exception;

       function Image (O : Option) return String;

    end Options;

    package body Options is

       function Image (O : Option) return String is
       begin
          case O is
             when Uninitialized =>
                raise Uninitialized_Value with "Uninitialized value detected!";
             when others =>
                return Option'Image (O);
          end case;
       end Image;

    end Options;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Options;          use Options;

    procedure Main is
       type Test_Case_Index is
         (Options_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check (O : Option) is
          begin
             Put_Line (Image (O));
          exception
             when E : Uninitialized_Value =>
                Put_Line (Exception_Message (E));
          end Check;

       begin
          case TC is
          when Options_Chk =>
             for O in Option loop
                Check (O);
             end loop;
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

Numerical Exception
~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Numerical_Exception

    --  START LAB IO BLOCK
    in 0:Exception_1_Chk
    out 0:Constraint_Error detected!
    in 1:Exception_2_Chk
    out 1:Custom_Exception raised!
    --  END LAB IO BLOCK

    package Tests is

       type Test_ID is (Test_1, Test_2);

       Custom_Exception : exception;

       procedure Num_Exception_Test (ID : Test_ID);

    end Tests;

    package body Tests is

       pragma Warnings (Off, "variable ""C"" is assigned but never read");

       procedure Num_Exception_Test (ID : Test_ID) is
          A, B, C : Integer;
       begin
          case ID is
             when Test_1 =>
                A := Integer'Last;
                B := Integer'Last;
                C := A + B;
             when Test_2 =>
                raise Custom_Exception with "Custom_Exception raised!";
          end case;
       end Num_Exception_Test;

       pragma Warnings (On, "variable ""C"" is assigned but never read");

    end Tests;

    with Tests;          use Tests;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
    exception
       when Constraint_Error =>
          Put_Line ("Constraint_Error detected!");
       when E : others =>
          Put_Line (Exception_Message (E));
    end Check_Exception;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Tests;            use Tests;
    with Check_Exception;

    procedure Main is
       type Test_Case_Index is
         (Exception_1_Chk,
          Exception_2_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Handle_Exception (ID : Test_ID) is
          begin
             Check_Exception (ID);
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error"
                          & " (raised by Check_Exception) detected!");
             when E : others =>
                Put_Line (Exception_Name (E)
                          & " (raised by Check_Exception) detected!");
          end Check_Handle_Exception;

       begin
          case TC is
          when Exception_1_Chk =>
             Check_Handle_Exception (Test_1);
          when Exception_2_Chk =>
             Check_Handle_Exception (Test_2);
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;

Re-raising Exceptions
~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Exceptions.Exception_Reraise

    --  START LAB IO BLOCK
    in 0:Exception_1_Chk
    out 0:Constraint_Error detected! Constraint_Error (raised by Check_Exception) detected!
    in 1:Exception_2_Chk
    out 1:Custom_Exception raised! TESTS.ANOTHER_EXCEPTION (raised by Check_Exception) detected!
    --  END LAB IO BLOCK

    package Tests is

       type Test_ID is (Test_1, Test_2);

       Custom_Exception, Another_Exception : exception;

       procedure Num_Exception_Test (ID : Test_ID);

    end Tests;

    package body Tests is

       pragma Warnings (Off, "variable ""C"" is assigned but never read");

       procedure Num_Exception_Test (ID : Test_ID) is
          A, B, C : Integer;
       begin
          case ID is
             when Test_1 =>
                A := Integer'Last;
                B := Integer'Last;
                C := A + B;
             when Test_2 =>
                raise Custom_Exception with "Custom_Exception raised!";
          end case;
       end Num_Exception_Test;

       pragma Warnings (On, "variable ""C"" is assigned but never read");

    end Tests;

    with Tests; use Tests;

    procedure Check_Exception (ID : Test_ID);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
    exception
       when Constraint_Error =>
          Put_Line ("Constraint_Error detected!");
          raise;
       when E : others =>
          Put_Line (Exception_Message (E));
          raise Another_Exception;
    end Check_Exception;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;
    with Ada.Exceptions;   use Ada.Exceptions;

    with Tests;            use Tests;
    with Check_Exception;

    procedure Main is
       type Test_Case_Index is
         (Exception_1_Chk,
          Exception_2_Chk);

       procedure Check (TC : Test_Case_Index) is

          procedure Check_Handle_Exception (ID : Test_ID) is
          begin
             Check_Exception (ID);
          exception
             when Constraint_Error =>
                Put_Line ("Constraint_Error"
                          & " (raised by Check_Exception) detected!");
             when E : others =>
                Put_Line (Exception_Name (E)
                          & " (raised by Check_Exception) detected!");
          end Check_Handle_Exception;

       begin
          case TC is
          when Exception_1_Chk =>
             Check_Handle_Exception (Test_1);
          when Exception_2_Chk =>
             Check_Handle_Exception (Test_2);
          end case;
       end Check;

    begin
       if Argument_Count < 1 then
          Put_Line ("ERROR: missing arguments! Exiting...");
          return;
       elsif Argument_Count > 1 then
          Put_Line ("Ignoring additional arguments...");
       end if;

       Check (Test_Case_Index'Value (Argument (1)));
    end Main;
