.. code:: ada project=Training_Material.Fundamentals_Of_Ada.Expressions.quantified_expression
    :class: ada-run

   with GNAT.Random_Numbers; use GNAT.Random_Numbers;
   with Ada.Text_IO;         use Ada.Text_IO;
   procedure Quantified_Expressions is
      Gen    : Generator;
      Values : array (1 .. 10) of Integer := (others => Random (Gen));
   
      Any_Even : Boolean := (for some N of Values => N mod 2 = 0);
      All_Odd  : Boolean := (for all N of reverse Values => N mod 2 = 1);
   
      function Is_Sorted return Boolean is
        (for all K in Values'Range =>
           K = Values'First or else Values (K - 1) <= Values (K));
   
      function Duplicate return Boolean is
        (for some I in Values'Range =>
           (for some J in I + 1 .. Values'Last => Values (I) = Values (J)));
   
   begin
      Put_Line ("Any Even: " & Boolean'Image (Any_Even));
      Put_Line ("All Odd: " & Boolean'Image (All_Odd));
      Put_Line ("Is_Sorted " & Boolean'Image (Is_Sorted));
      Put_Line ("Duplicate " & Boolean'Image (Duplicate));
   end Quantified_Expressions;
