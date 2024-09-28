Object-oriented programming
---------------------------

Simple type extension
~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Object_Oriented_Programming.Simple_Type_Extension

    --  START LAB IO BLOCK
    in 0:Type_Extension_Chk
    out 0:T_Mixed is in T_Float'Class as expected F1: { F =>  2.00000E+00 } F2: { F =>  3.00000E+00 } M1: { F =>  4.00000E+00, I =>  4 } M2: { F =>  5.00000E+00, I =>  5 }
    --  END LAB IO BLOCK

    package Type_Extensions is

       type T_Float is tagged record
          F : Float;
       end record;

       function Init (F : Float) return T_Float;

       function Init (I : Integer) return T_Float;

       function Image (T : T_Float) return String;

       type T_Mixed is new T_Float with record
          I : Integer;
       end record;

       function Init (F : Float) return T_Mixed;

       function Init (I : Integer) return T_Mixed;

       function Image (T : T_Mixed) return String;

    end Type_Extensions;

    package body Type_Extensions is

       function Init (F : Float) return T_Float is
       begin
          return ((F => F));
       end Init;

       function Init (I : Integer) return T_Float is
       begin
          return ((F => Float (I)));
       end Init;

       function Init (F : Float) return T_Mixed is
       begin
          return ((F => F,
                   I => Integer (F)));
       end Init;

       function Init (I : Integer) return T_Mixed is
       begin
          return ((F => Float (I),
                   I => I));
       end Init;

       function Image (T : T_Float) return String is
       begin
          return "{ F => " & Float'Image (T.F) & " }";
       end Image;

       function Image (T : T_Mixed) return String is
       begin
          return "{ F => " & Float'Image (T.F)
            & ", I => " & Integer'Image (T.I) & " }";
       end Image;

    end Type_Extensions;

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Text_IO;      use Ada.Text_IO;

    with Type_Extensions;  use Type_Extensions;

    procedure Main is

       type Test_Case_Index is
         (Type_Extension_Chk);

       procedure Check (TC : Test_Case_Index) is
          F1, F2 : T_Float;
          M1, M2 : T_Mixed;
       begin
          case TC is
          when Type_Extension_Chk =>
             F1 := Init (2.0);
             F2 := Init (3);
             M1 := Init (4.0);
             M2 := Init (5);

             if M2 in T_Float'Class then
               Put_Line ("T_Mixed is in T_Float'Class as expected");
             end if;

             Put_Line ("F1: " & Image (F1));
             Put_Line ("F2: " & Image (F2));
             Put_Line ("M1: " & Image (M1));
             Put_Line ("M2: " & Image (M2));
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

Online Store
~~~~~~~~~~~~~~~~~~~~~

.. code:: ada lab=Solutions.Object_Oriented_Programming.Online_Store

    --  START LAB IO BLOCK
    in 0:Type_Chk
    out 0:Testing Status of Associate Member Type => OK Testing Status of Full Member Type => OK Testing Discount of Associate Member Type => OK Testing Discount of Full Member Type => OK
    in 1:Unit_Test_Chk
    out 1:Member # 1 Status: Associate Member Since:  2010 Due Amount:  250.00 -------- Member # 2 Status: Full Member Since:  1998 Due Amount:  144.00 -------- Member # 3 Status: Full Member Since:  1987 Due Amount:  320.00 -------- Member # 4 Status: Associate Member Since:  2013 Due Amount:  110.00 --------
    --  END LAB IO BLOCK

    with Ada.Calendar; use Ada.Calendar;

    package Online_Store is

       type Amount is delta 10.0**(-2) digits 10;

       subtype Percentage is Amount range 0.0 .. 1.0;

       type Member is tagged record
          Start : Year_Number;
       end record;

       type Member_Access is access Member'Class;

       function Get_Status (M : Member) return String;

       function Get_Price (M : Member;
                           P : Amount) return Amount;

       type Full_Member is new Member with record
          Discount : Percentage;
       end record;

       function Get_Status (M : Full_Member) return String;

       function Get_Price (M : Full_Member;
                           P : Amount) return Amount;

    end Online_Store;

    package body Online_Store is

       function Get_Status (M : Member) return String is
         ("Associate Member");

       function Get_Status (M : Full_Member) return String is
         ("Full Member");

       function Get_Price (M : Member;
                           P : Amount) return Amount is (P);

       function Get_Price (M : Full_Member;
                           P : Amount) return Amount is
         (P * (1.0 - M.Discount));

    end Online_Store;

    package Online_Store.Tests is

       procedure Simple_Test;

    end Online_Store.Tests;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Online_Store.Tests is

       procedure Simple_Test is

          type Member_Due_Amount is record
             Member     : Member_Access;
             Due_Amount : Amount;
          end record;

          function Get_Price (MA : Member_Due_Amount) return Amount is
          begin
             return MA.Member.Get_Price (MA.Due_Amount);
          end Get_Price;

          type Member_Due_Amounts is array (Positive range <>) of Member_Due_Amount;

          DB : constant Member_Due_Amounts (1 .. 4)
            := ((Member     => new Member'(Start => 2010),
                 Due_Amount => 250.0),
                (Member     => new Full_Member'(Start    => 1998,
                                                Discount => 0.1),
                 Due_Amount => 160.0),
                (Member     => new Full_Member'(Start    => 1987,
                                                Discount => 0.2),
                 Due_Amount => 400.0),
                (Member     => new Member'(Start => 2013),
                 Due_Amount => 110.0));
       begin
          for I in DB'Range loop
             Put_Line ("Member #" & Positive'Image (I));
             Put_Line ("Status: " & DB (I).Member.Get_Status);
             Put_Line ("Since: " & Year_Number'Image (DB (I).Member.Start));
             Put_Line ("Due Amount: " & Amount'Image (Get_Price (DB (I))));
             Put_Line ("--------");
          end loop;
       end Simple_Test;

    end Online_Store.Tests;

    with Ada.Command_Line;   use Ada.Command_Line;
    with Ada.Text_IO;        use Ada.Text_IO;

    with Online_Store;       use Online_Store;
    with Online_Store.Tests; use Online_Store.Tests;

    procedure Main is

       type Test_Case_Index is
         (Type_Chk,
          Unit_Test_Chk);

       procedure Check (TC : Test_Case_Index) is

          function Result_Image (Result : Boolean) return String is
            (if Result then "OK" else "not OK");

       begin
          case TC is
          when Type_Chk =>
             declare
                AM : constant Member      := (Start    => 2002);
                FM : constant Full_Member := (Start    => 1990,
                                              Discount => 0.2);
             begin
                Put_Line ("Testing Status of Associate Member Type => "
                          & Result_Image (AM.Get_Status = "Associate Member"));
                Put_Line ("Testing Status of Full Member Type => "
                          & Result_Image (FM.Get_Status = "Full Member"));
                Put_Line ("Testing Discount of Associate Member Type => "
                          & Result_Image (AM.Get_Price (100.0) = 100.0));
                Put_Line ("Testing Discount of Full Member Type => "
                          & Result_Image (FM.Get_Price (100.0) = 80.0));
             end;
             when Unit_Test_Chk =>
                Simple_Test;
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
