Exceptions
==========

.. include:: ../../../global.txt

Uninitialized Value
-------------------

**Goal**: implement an enumeration to avoid the use of uninitialized values.

**Steps**:

    #. Implement the :ada:`Options` package.

        #. Declare the :ada:`Option` enumeration type.

        #. Declare the :ada:`Uninitialized_Value` exception.

        #. Implement the :ada:`Image` function.

**Requirements**:

    #. Enumeration :ada:`Option` contains:

        #. the :ada:`Uninitialized` value, and

        #. the actual options:

            - :ada:`Option_1`,
            - :ada:`Option_2`,
            - :ada:`Option_3`.

    #. Function :ada:`Image` returns a string for the :ada:`Option` type.

        #. In case the argument to :ada:`Image` is :ada:`Uninitialized`, the
           function must raise the :ada:`Uninitialized_Value` exception.

**Remarks**:

    #. In this exercise, we employ exceptions as a mechanism to avoid the use
       of uninitialized values for a certain type.

.. code:: ada lab=Exceptions.Uninitialized_Value

    --  START LAB IO BLOCK
    in 0:Options_Chk
    out 0:Uninitialized value detected! OPTION_1 OPTION_2 OPTION_3
    --  END LAB IO BLOCK

    package Options is

       --  Declare the Option enumeration type!
       type Option is null record;

       function Image (O : Option) return String;

    end Options;

    package body Options is

       function Image (O : Option) return String is
       begin
          return "";
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
-------------------

**Goal**: handle numerical exceptions in a test procedure.

**Steps**:

    #. Add exception handling to the :ada:`Check_Exception` procedure.

**Requirements**:

    #. The test procedure :ada:`Num_Exception_Test` from the :ada:`Tests`
       package below must be used in the implementation of
       :ada:`Check_Exception`.

    #. The :ada:`Check_Exception` procedure must be extended to handle
       exceptions as follows:

        #. If the exception raised by :ada:`Num_Exception_Test` is
           :ada:`Constraint_Error`, the procedure must display the message
           "Constraint_Error detected!" to the user.

        #. Otherwise, it must display the message associated with the
           exception.

**Remarks**:

    #. You can use the :ada:`Exception_Message` function to retrieve the
       message associated with an exception.

.. code:: ada lab=Exceptions.Numerical_Exception

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

    with Tests; use Tests;

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
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
---------------------

**Goal**: make use of exception re-raising in a test procedure.

**Steps**:

    #. Declare new exception: :ada:`Another_Exception`.

    #. Add exception re-raise to the :ada:`Check_Exception` procedure.

**Requirements**:

    #. Exception :ada:`Another_Exception` must be declared in the :ada:`Tests`
       package.

    #. Procedure :ada:`Check_Exception` must be extended to *re-raise* any
       exception. When an exception is detected, the procedure must:

        #. display a user message (as implemented in the previous exercise),
           and then

        #. Raise or *re-raise* exception depending on the exception that is
           being handled:

            #. In case of :ada:`Constraint_Error` exception, *re-raise* the
               exception.

            #. In all other cases, raise :ada:`Another_Exception`.

**Remarks**:

    #. In this exercise, you should extend the implementation of the
       :ada:`Check_Exception` procedure from the previous exercise.

        #. Naturally, you can use the code for the :ada:`Check_Exception`
           procedure from the previous exercise as a starting point.

.. code:: ada lab=Exceptions.Exception_Reraise

    --  START LAB IO BLOCK
    in 0:Exception_1_Chk
    out 0:Constraint_Error detected! Constraint_Error (raised by Check_Exception) detected!
    in 1:Exception_2_Chk
    out 1:Custom_Exception raised! TESTS.ANOTHER_EXCEPTION (raised by Check_Exception) detected!
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

    with Tests; use Tests;

    procedure Check_Exception (ID : Test_ID);

    procedure Check_Exception (ID : Test_ID) is
    begin
       Num_Exception_Test (ID);
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
