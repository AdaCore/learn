.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Type_Contracts.subtype_predicates

   with Ada.Exceptions; use Ada.Exceptions;
   with Ada.Text_IO;    use Ada.Text_IO;
   procedure Predicates is
   
      subtype Even_T is Integer with Dynamic_Predicate => Even_T mod 2 = 0;
      type Serial_Baud_Rate_T is range 110 .. 115_200 with
         Static_Predicate => Serial_Baud_Rate_T in -- Non-contiguous range
             2_400 | 4_800 | 9_600 | 14_400 | 19_200 | 28_800 | 38_400 | 56_000;
   
      -- This must be dynamic because "others" will be evaluated at run-time
      subtype Vowel_T is Character with Dynamic_Predicate =>
            (case Vowel_T is when 'A' | 'E' | 'I' | 'O' | 'U' => True, when others => False);
   
      type Table_T is array (Integer range <>) of Integer;
      subtype Sorted_Table_T is Table_T (1 .. 5) with
           Dynamic_Predicate =>
           (for all K in Sorted_Table_T'Range =>
              (K = Sorted_Table_T'First or else Sorted_Table_T (K - 1) <= Sorted_Table_T (K)));
   
      J      : Even_T;
      Values : Sorted_Table_T := (1, 3, 5, 7, 9);
   
   begin
      begin
         Put_Line ("J is" & J'Img);
         J := Integer'Succ (J); -- assertion failure here
         Put_Line ("J is" & J'Img);
         J := Integer'Succ (J); -- or maybe here
         Put_Line ("J is" & J'Img);
      exception
         when The_Err : others =>
            Put_Line (Exception_Message (The_Err));
      end;
   
      for Baud in Serial_Baud_Rate_T loop
         Put_Line (Baud'Image);
      end loop;
   
      Put_Line (Vowel_T'Image (Vowel_T'Succ ('A')));
      Put_Line (Vowel_T'Image (Vowel_T'Pred ('Z')));
   
      begin
         Values (3) := 0; -- not an exception
         Values     := (1, 3, 0, 7, 9); -- exception
      exception
         when The_Err : others =>
            Put_Line (Exception_Message (The_Err));
      end;
   end Predicates;
