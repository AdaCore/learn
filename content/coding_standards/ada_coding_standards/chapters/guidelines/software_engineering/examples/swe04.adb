package body Swe04 is

   package body Noncompliant is

      procedure Push
        (Stack : in out Stack_T;
         Item  :        Integer) is
      begin
         Stack.Top                 := Stack.Top + 1;
         Stack.Content (Stack.Top) := Item;
      end Push;

      procedure Pop
        (Stack : in out Stack_T;
         Item  :    out Integer) is
      begin
         Item      := Stack.Content (Stack.Top);
         Stack.Top := Stack.Top - 1;
      end Pop;

   end Noncompliant;

   package body Compliant is

      procedure Push
        (Stack : in out Stack_T;
         Item  :        Integer) is
      begin
         Stack.Top                 := Stack.Top + 1;
         Stack.Content (Stack.Top) := Item;
      end Push;

      procedure Pop
        (Stack : in out Stack_T;
         Item  :    out Integer) is
      begin
         Item      := Stack.Content (Stack.Top);
         Stack.Top := Stack.Top - 1;
      end Pop;

   end Compliant;

end Swe04;
