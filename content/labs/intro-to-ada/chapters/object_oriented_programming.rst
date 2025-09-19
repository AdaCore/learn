:orphan:

Object-oriented programming
===========================

.. include:: ../../../global.txt

Simple type extension
---------------------

**Goal**: work with type extensions using record types containing numeric
components.

**Steps**:

    #. Implement the :ada:`Type_Extensions` package.

        #. Declare the record type :ada:`T_Float`.

        #. Declare the record type :ada:`T_Mixed`

        #. Implement the :ada:`Init` function for the :ada:`T_Float` type with
           a floating-point input parameter.

        #. Implement the :ada:`Init` function for the :ada:`T_Float` type with
           an integer input parameter.

        #. Implement the :ada:`Image` function for the :ada:`T_Float` type.

        #. Implement the :ada:`Init` function for the :ada:`T_Mixed` type with
           a floating-point input parameter.

        #. Implement the :ada:`Init` function for the :ada:`T_Mixed` type with
           an integer input parameter.

        #. Implement the :ada:`Image` function for the :ada:`T_Mixed` type.

**Requirements**:

    #. Record type :ada:`T_Float` contains the following component:

        #. :ada:`F`, a floating-point type.

    #. Record type :ada:`T_Mixed` is derived from the :ada:`T_Float` type.

        #. :ada:`T_Mixed` extends :ada:`T_Float` with the following component:

            #. :ada:`I`, an integer component.

        #. Both components must be numerically *synchronized*:

            - For example, if the floating-point component contains the value
              2.0, the value of the integer component must be 2.

            - In order to simplify the implementation, you can simply use
              :ada:`Integer (F)` to convert a floating-point variable :ada:`F`
              to integer.

    #. Function :ada:`Init` returns an object of the corresponding type
       (:ada:`T_Float` or :ada:`T_Mixed`).

        #. For each type, two versions of :ada:`Init` must be declared:

            #. one with a floating-point input parameter,

            #. another with an integer input parameter.

        #. The parameter to :ada:`Init` is used to initialize the record
           components.

    #. Function :ada:`Image` returns a string for the components of the
       record type.

        #. In case of the :ada:`Image` function for the :ada:`T_Float`
           type, the string must have the format
           :ada:`"{ F =>  <float value> }"`.

            - For example, the call :ada:`Image (T_Float'(Init (8.0))))`
              should return the string :ada:`"{ F =>  8.00000E+00 }"`.

        #. In case of the :ada:`Image` function for the :ada:`T_Mixed`
           type, the string must have the format
           :ada:`"{ F =>  <float value>, I => <integer value> }"`.

            - For example, the call :ada:`Image (T_Mixed'(Init (8.0))))`
              should return the string
              :ada:`"{ F =>  8.00000E+00, I =>  8 }"`.

.. code:: ada lab=Object_Oriented_Programming.Simple_Type_Extension

    --  START LAB IO BLOCK
    in 0:Type_Extension_Chk
    out 0:T_Mixed is in T_Float'Class as expected F1: { F =>  2.00000E+00 } F2: { F =>  3.00000E+00 } M1: { F =>  4.00000E+00, I =>  4 } M2: { F =>  5.00000E+00, I =>  5 }
    --  END LAB IO BLOCK

    package Type_Extensions is

       --  Create declaration of T_Float type!
       type T_Float is null record;

       --  function Init ...

       --  function Image ...

       --  Create declaration of T_Mixed type!
       type T_Mixed is null record;

    end Type_Extensions;

    package body Type_Extensions is

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
------------

**Goal**: create an online store for the members of an association.

**Steps**:

    #. Implement the :ada:`Online_Store` package.

        #. Declare the :ada:`Member` type.

        #. Declare the :ada:`Full_Member` type.

        #. Implement the :ada:`Get_Status` function for the :ada:`Member` type.

        #. Implement the :ada:`Get_Price` function for the :ada:`Member` type.

        #. Implement the :ada:`Get_Status` function for the :ada:`Full_Member`
           type.

        #. Implement the :ada:`Get_Price` function for the :ada:`Full_Member`
           type.

    #. Implement the :ada:`Online_Store.Tests` child package.

        #. Implement the :ada:`Simple_Test` procedure.

**Requirements**:

    #. Package :ada:`Online_Store` implements an online store application for
       the members of an association.

        #. In this association, members can have one of the following status:

            - associate member, or

            - full member.

    #. Function :ada:`Get_Price` returns the correct price of an item.

        #. Associate members must pay the full price when they buy items
           from the online store.

        #. Full members can get a discount.

            #. The discount rate can be different for each full member |mdash|
               depending on factors that are irrelevant for this exercise.

    #. Package :ada:`Online_Store` has following types:

        #. :ada:`Percentage` type, which represents a percentage ranging from
           0.0 to 1.0.

        #. :ada:`Member` type for associate members containing following
           components:

            - :ada:`Start`, which indicates the starting year of the
              membership.

                - This information is common for both associate and full
                  members.

                - You can use the :ada:`Year_Number` type from the standard
                  :ada:`Ada.Calendar` package for this component.

        #. :ada:`Full_Member` type for full members.

            #. This type must extend the :ada:`Member` type above.

            #. It contains the following additional component:

                - :ada:`Discount`, which indicates the discount rate that the
                  full member gets in the online store.

                    - This component must be of :ada:`Percentage` type.

    #. For the :ada:`Member` and :ada:`Full_Member` types, you must implement
       the following functions:

        #. :ada:`Get_Status`, which returns a string with the membership
           status.

            - The string must be :ada:`"Associate Member"` or
              :ada:`"Full Member"`, respectively.

        #. :ada:`Get_Price`, which returns the *adapted price* of an item
           |mdash| indicating the actual due amount.

            - For example, for a full member with a 10% discount rate, the
              actual due amount of an item with a price of 100.00 is 90.00.

            - Associated members don't get a discount, so they always pay the
              full price.

    #. Procedure :ada:`Simple_Test` (from  the :ada:`Online_Store.Tests`
       package) is used for testing.

        #. Based on a list of members that bought on the online store and the
           corresponding full price of the item, :ada:`Simple_Test` must
           display information about each member and the actual due amount
           after discounts.

        #. Information about the members must be displayed in the following
           format:

            .. code-block:: none

                Member # <number>
                Status: <status>
                Since:  <year>
                Due Amount:  <value>
                --------

        #. For this exercise, :ada:`Simple_Test` must use the following list:

            +---+-------------------+--------------+----------+------------+
            | # | Membership status | Start (year) | Discount | Full Price |
            +===+===================+==============+==========+============+
            | 1 | Associate         | 2010         | *N/A*    |     250.00 |
            +---+-------------------+--------------+----------+------------+
            | 2 | Full              | 1998         | 10.0 %   |     160.00 |
            +---+-------------------+--------------+----------+------------+
            | 3 | Full              | 1987         | 20.0 %   |     400.00 |
            +---+-------------------+--------------+----------+------------+
            | 4 | Associate         | 2013         | *N/A*    |     110.00 |
            +---+-------------------+--------------+----------+------------+

        #. In order to pass the tests, the information displayed by a call to
           :ada:`Simple_Test` must conform to the format described above.

            - You can find another example in the remarks below.

**Remarks**:

    #. In previous labs, we could have implemented a simplified version of the
       system described above by simply using an enumeration type to specify
       the membership status. For example:

        .. code-block:: ada

            type Member_Status is (Associate_Member, Full_Member);

        #. In this case, the :ada:`Get_Price` function would then evaluate the
           membership status and adapt the item price |mdash| assuming a fixed
           discount rate for all full members. This could be the corresponding
           function declaration:

            .. code-block:: ada

                type Amount is delta 10.0**(-2) digits 10;

                function Get_Price (M : Member_Status;
                                    P : Amount) return Amount;

        #. In this exercise, however, we'll use type extension to represent the
           membership status in our application.

    #. For the procedure :ada:`Simple_Test`, let's consider the following list
       of members as an example:

        +---+-------------------+--------------+----------+------------+
        | # | Membership status | Start (year) | Discount | Full Price |
        +===+===================+==============+==========+============+
        | 1 | Associate         | 2002         | *N/A*    |     100.00 |
        +---+-------------------+--------------+----------+------------+
        | 2 | Full              | 2005         | 10.0 %   |     100.00 |
        +---+-------------------+--------------+----------+------------+

        - For this list, the test procedure displays the following information
          (in this exact format):

            .. code-block:: none

                Member # 1
                Status: Associate Member
                Since:  2002
                Due Amount:  100.00
                --------
                Member # 2
                Status: Full Member
                Since:  2005
                Due Amount:  90.00
                --------

        - Here, although both members had the same full price (as indicated by
          the last column), member #2 gets a reduced due amount of 90.00
          because of the full membership status.

.. code:: ada lab=Object_Oriented_Programming.Online_Store

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

       --  Create declaration of Member type!
       --
       --  You can use Year_Number from Ada.Calendar for the membership
       --  starting year.
       --
       type Member is null record;

       function Get_Status (M : Member) return String;

       function Get_Price (M : Member;
                           P : Amount) return Amount;

       --  Create declaration of Full_Member type!
       --
       --  Use the Percentage type for storing the membership discount.
       --
       type Full_Member is null record;

       function Get_Status (M : Full_Member) return String;

       function Get_Price (M : Full_Member;
                           P : Amount) return Amount;

    end Online_Store;

    package body Online_Store is

       function Get_Status (M : Member) return String is
         ("");

       function Get_Status (M : Full_Member) return String is
         ("");

       function Get_Price (M : Member;
                           P : Amount) return Amount is (0.0);

       function Get_Price (M : Full_Member;
                           P : Amount) return Amount is
         (0.0);

    end Online_Store;

    package Online_Store.Tests is

       procedure Simple_Test;

    end Online_Store.Tests;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Online_Store.Tests is

       procedure Simple_Test is
       begin
          null;
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
