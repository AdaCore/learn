.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Contracts.type_invariants
   :class: ada-run

   package Bank is
      type Account_T is private with
         Type_Invariant => Consistent_Balance (Account_T);
      type Currency_T is delta 0.01 digits 12;
      function Consistent_Balance (This : Account_T) return Boolean;
      procedure Open (This : in out Account_T; Initial_Deposit : Currency_T);
   private
      type List_T is array (1 .. 100) of Currency_T;
      type Transaction_List_T is record
         Values : List_T;
         Count  : Natural := 0;
      end record;
      -- initial state MUST satisfy invariant
      type Account_T is record
         Current_Balance : Currency_T := 0.0;
         Withdrawals     : Transaction_List_T;
         Deposits        : Transaction_List_T;
      end record;
      function Total (This : Transaction_List_T) return Currency_T;
   end Bank;

   package body Bank is
      function Total (This : Transaction_List_T) return Currency_T is
         Result : Currency_T := 0.0;
      begin
         for I in 1 .. This.Count loop -- no iteration if list empty
            Result := Result + This.Values (I);
         end loop;
         return Result;
      end Total;
      function Consistent_Balance (This : Account_T) return Boolean is
      begin
         return Total (This.Deposits) - Total (This.Withdrawals) =
           This.Current_Balance;
      end Consistent_Balance;
      procedure Open (This : in out Account_T; Initial_Deposit : Currency_T) is
      begin
         This.Current_Balance := Initial_Deposit;
         -- if we checked, the invariant would be false here!
         This.Withdrawals.Count   := 0;
         This.Deposits.Count      := 1;
         This.Deposits.Values (1) := Initial_Deposit;
         -- invariant is now true
      end Open;
   end Bank;
