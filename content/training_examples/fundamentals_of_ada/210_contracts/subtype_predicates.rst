.. code:: ada run_button project=Training_Material.Fundamentals_Of_Ada.Contracts.subtype_predicates
   :class: ada-run

   with Ada.Exceptions; use Ada.Exceptions;
   with Ada.Text_IO;    use Ada.Text_IO;
   with Stack;
   procedure Predicates is
   
      subtype Even_T is Integer with
           Dynamic_Predicate => Even_T mod 2 = 0;
      type Serial_Baud_Rate_T is range 110 .. 115_200 with
         Static_Predicate => Serial_Baud_Rate_T in
         -- Non-contiguous range
         110 | 300 | 600 | 1_200 |
             2_400 | 4_800 | 9_600 | 14_400 | 19_200 | 28_800 | 38_400 | 56_000 |
             57_600 | 115_200;
   
      type Days_T is (Sun, Mon, Tues, We, Thu, Fri, Sat);
      -- subtype containing non-contiguous values
      subtype Weekend_T is Days_T with
           Static_Predicate => Weekend_T in Sat | Sun;
      subtype Case_T is Days_T with
           Static_Predicate =>
           (case Case_T is when Sat | Sun => True, when Mon .. Fri => False);
         -- subtype Illegal_T is Days_T with static_predicate => (if Illegal_T in
         -- Mon .. Fri then False else True);
   
         -- This must be dynamic because "others" will be evaluated at run-time
      subtype Vowel_T is Character with
           Dynamic_Predicate =>
           (case Vowel_T is when 'A' | 'E' | 'I' | 'O' | 'U' => True,
              when others => False);
   
      type Table_T is array (Integer range <>) of Integer;
      subtype Sorted_Table_T is Table_T (1 .. 5) with
           Dynamic_Predicate =>
           (for all K in Sorted_Table_T'Range =>
              (K = Sorted_Table_T'First
               or else Sorted_Table_T (K - 1) <= Sorted_Table_T (K)));
   
      J      : Even_T;
      Values : Sorted_Table_T := (1, 3, 5, 7, 9);
   
   begin
   
      begin
         Put_Line ("J is" & J'Img);
         -- predicate is checked here
         J := Integer'Succ (J); -- assertion failure here
         Put_Line ("J is" & J'Img);
         J := Integer'Succ (J); -- or maybe here
         Put_Line ("J is" & J'Img);
      exception
         when The_Err : others =>
            Put_Line (Exception_Message (The_Err));
      end;
   
      for Counter in Weekend_T loop
         Put_Line (Counter'Image);
      end loop;
   
      Put_Line
        (Serial_Baud_Rate_T'Image
           (Serial_Baud_Rate_T'Pred (Serial_Baud_Rate_T'Last_Valid)));
      Put_Line (Vowel_T'Image (Vowel_T'Succ ('A')));
      Put_Line (Vowel_T'Image (Vowel_T'Pred ('Z')));
   
      begin
         Values (3) := 0; -- not an exception
         Values     := (1, 3, 0, 7, 9); -- exception
      exception
         when The_Err : others =>
            Put_Line (Exception_Message (The_Err));
      end;
   
      declare
         I : Integer;
      begin
         Stack.Normal_Pop (I);
      exception
         when The_Err : others =>
            Put_Line ("Normal_Pop exception: " & Exception_Name (The_Err));
      end;
   
      declare
         I : Integer;
      begin
         Stack.Underflow_Pop (I);
      exception
         when The_Err : others =>
            Put_Line ("Underflow_Pop exception: " & Exception_Name (The_Err));
      end;
   
   end Predicates;

   package Stack is
      Overflow, Underflow : exception;
      procedure Normal_Push (I : Integer) with
         Pre => not Full;
      procedure Normal_Pop (I : out Integer) with
         Pre => not Empty;
      function Full return Boolean;
      function Empty return Boolean;
      procedure Overflow_Push (I : Integer) with
         Pre => not Full or else raise Overflow;
      procedure Underflow_Pop (I : out Integer) with
         Pre => not Empty or else raise Underflow;
   end Stack;

   package body Stack is
      Values : array (1 .. 100) of Integer;
      Top    : Integer := 0;
      procedure Normal_Push (I : Integer) is
      begin
         Top          := Top + 1;
         Values (Top) := I;
      end Normal_Push;
      procedure Normal_Pop (I : out Integer) is
      begin
         I   := Values (Top);
         Top := Top - 1;
      end Normal_Pop;
      function Full return Boolean is (Top >= Values'Last);
      function Empty return Boolean is (Top not in Values'Range);
      procedure Overflow_Push (I : Integer) is
      begin
         Top          := Top + 1;
         Values (Top) := I;
      end Overflow_Push;
      procedure Underflow_Pop (I : out Integer) is
      begin
         I   := Values (Top);
         Top := Top - 1;
      end Underflow_Pop;
   end Stack;
