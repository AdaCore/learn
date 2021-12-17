Subprograms
===========

.. include:: ../../global.txt

.. _Operators:

Operators
---------

Operators are commonly used for variables of scalar types such as
:ada:`Integer` and :ada:`Float`. In these cases, they replace *usual* function
calls. (To be more precise, operators are function calls, but written in a
different format.) For example, we simply write :ada:`A := A + B + C;` when we
want to add three integer variables. A hypothetical, non-intuitive version of
this operation could be :ada:`A := Add (Add (A, B), C);`. In such cases,
operators allow for expressing function calls in a more intuitive way.

Many pre-defined operators exist for scalar types. Obviously, for record types,
most operators are not defined, as it wouldn't make sense to expect a compiler
to include an addition operator for a record type with multiple components, for
example. An exception to this rule are the equality and inequality operators
(:ada:`=` and :ada:`/=`), which are defined for both scalar and record types.

We can, however, define *custom* operators for record types. For example, we
could declare two :ada:`+` operators for a record containing the name and
address of a person:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Rec_Operator

    package Addresses is

       type Person is private;

       function "+" (Name    : String;
                     Address : String) return Person;
       function "+" (Left, Right : Person) return Person;

       procedure Display (P : Person);

    private

       subtype Name_String    is String (1 .. 40);
       subtype Address_String is String (1 .. 100);

       type Person is record
          Name    : Name_String;
          Address : Address_String;
       end record;

    end Addresses;

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    package body Addresses is

       function "+" (Name    : String;
                     Address : String) return Person is
       begin
          return (Name    => Head (Name,
                                   Name_String'Length),
                  Address => Head (Address,
                                   Address_String'Length));
       end "+";

       function "+" (Left, Right : Person) return Person is
       begin
          return (Name    => Left.Name,
                  Address => Right.Address);
       end "+";

       procedure Display (P : Person) is
       begin
          Put_Line ("Name:    " & P.Name);
          Put_Line ("Address: " & P.Address);
          New_Line;
       end Display;

    end Addresses;

    with Ada.Text_IO; use Ada.Text_IO;
    with Addresses;   use Addresses;

    procedure Show_Address_Addition is
       John : Person := "John" + "4 Main Street";
       Jane : Person := "Jane" + "7 High Street";
    begin
       Display (John);
       Display (Jane);
       Put_Line("----------------");

       Jane := Jane + John;
       Display (Jane);
    end Show_Address_Addition;

In this example, the first :ada:`+` operator takes two strings |mdash| with the
name and address of a person |mdash| and returns an object of :ada:`Person`
type. We use this operator to initialize the :ada:`John` and :ada:`Jane`
variables.

The second :ada:`+` operator in this example brings two people together. Here,
the person on the left side of the :ada:`+` operator moves to the home of the
person on the right side. In this specific case, Jane is moving to John's
house.

As a small remark, we usually expect that the :ada:`+` operator is commutative.
In other words, changing the order of the elements in the operation doesn't
change the result. However, in our definition above, this is *not* the case, as
we can confirm by comparing the operation in both orders:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Rec_Operator

    with Ada.Text_IO; use Ada.Text_IO;
    with Addresses;   use Addresses;

    procedure Show_Address_Addition is
       John : Person := "John" + "4 Main Street";
       Jane : Person := "Jane" + "7 High Street";
    begin
       if Jane + John = John + Jane then
          Put_Line("It's commutative!");
       else
          Put_Line("It's not commutative!");
       end if;
    end Show_Address_Addition;

In the Ada standard library, operators are defined for the :ada:`Complex` type
of the :ada:`Ada.Numerics.Generic_Complex_Types` package, for example. This
package contains not only the definition of the :ada:`+` operator for two
objects of :ada:`Complex` type, but also for combination of :ada:`Complex` and
other types. For example:

.. code-block:: ada

    function "+" (Left, Right : Complex) return Complex;
    function "+" (Left : Complex;   Right : Real'Base) return Complex;
    function "+" (Left : Real'Base; Right : Complex)   return Complex;

.. admonition:: In the Ada Reference Manual

    - `4.5 Operators and Expression Evaluation <http://www.ada-auth.org/standards/12rm/html/RM-4-5.html>`_
    - `6.1 Subprogram Declarations <http://www.ada-auth.org/standards/12rm/html/RM-6-1.html>`_
    - `G.1.1 Complex Types <http://www.ada-auth.org/standards/12rm/html/RM-G-1-1.html>`_

Expression functions
--------------------

Usually, we implement Ada functions with a construct like this:
:ada:`begin return X; end;`. In other words, we create a :ada:`begin ... end;`
block and we have at least one :ada:`return` statement in that block. An
expression function, in contrast, is a function that is implemented with a
simple expression in parentheses, such as :ada:`(X);`. In this case, we don't
use a :ada:`begin ... end;` block or a :ada:`return` statement.

As an example of an expression, let's say we want to implement a function
named :ada:`Is_Zero` that checks if the value of the integer parameter :ada:`I`
is zero. We can implement this function with the expression :ada:`I = 0`. In
the usual approach, we would create the implementation by writing
:ada:`is begin return I = 0; end Is_Zero;`. When using expression functions,
however, we can simplify the implementation by just writing
:ada:`is (I = 0);`. This is the complete code of :ada:`Is_Zero` using an
expression function:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Simple_Expression_Function_1

    package Expr_Func is

       function Is_Zero (I : Integer) return Boolean is
         (I = 0);

    end Expr_Func;

An expression function has the same effect as the usual version using a block.
In fact, the code above is similar to this implementation of the :ada:`Is_Zero`
function using a block:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Simple_Expression_Function_2

    package Expr_Func is

       function Is_Zero (I : Integer) return Boolean;

    end Expr_Func;

    package body Expr_Func is

       function Is_Zero (I : Integer) return Boolean is
       begin
          return I = 0;
       end Is_Zero;

    end Expr_Func;

The only difference between these two versions of the :ada:`Expr_Func` packages
is that, in the first version, the package specification contains the
implementation of the :ada:`Is_Zero` function, while, in the second version,
the implementation is in the body of the :ada:`Expr_Func` package.

An expression function can be, at same time, the specification and the
implementation of a function. Therefore, in the first version of the
:ada:`Expr_Func` package above, we don't have a separate implementation of the
:ada:`Is_Zero` function because :ada:`(I = 0)` is the actual implementation of
the function. Note that this is only possible for expression functions; you
cannot have a function implemented with a block in a package specification. For
example, the following code is wrong and won't compile:

.. code:: ada manual_chop compile_button project=Courses.Advanced_Ada.Numerics.Simple_Expression_Function_3
    :class: ada-nocheck

    !expr_func.ads
    package Expr_Func is

       function Is_Zero (I : Integer) return Boolean is
       begin
          return I = 0;
       end Is_Zero;

    end Expr_Func;

We can, of course, separate the function declaration from its implementation as
an expression function. For example, we can rewrite the first version of the
:ada:`Expr_Func` package and move the expression function to the body of the
package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Simple_Expression_Function_4

    package Expr_Func is

       function Is_Zero (I : Integer) return Boolean;

    end Expr_Func;

    package body Expr_Func is

       function Is_Zero (I : Integer) return Boolean is
         (I = 0);

    end Expr_Func;

In addition, we can use expression functions in the private part of a
package specification. For example, the following code declares the
:ada:`Is_Valid` function in the specification of the :ada:`My_Data` package,
while its implementation is an expression function in the private part of the
package specification:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Private_Expression_Function_1

    package My_Data is

       type Data is private;

       function Is_Valid (D : Data) return Boolean;

    private

       type Data is record
          Valid : Boolean;
       end record;

       function Is_Valid (D : Data) return Boolean is
          (D.Valid);

    end My_Data;

Naturally, we could write the function implementation in the package body
instead:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Private_Expression_Function_2

    package My_Data is

       type Data is private;

       function Is_Valid (D : Data) return Boolean;

    private

       type Data is record
          Valid : Boolean;
       end record;

    end My_Data;

    package body My_Data is

       function Is_Valid (D : Data) return Boolean is
          (D.Valid);

    end My_Data;

.. admonition:: In the Ada Reference Manual

    - `6.8 Expression functions <http://www.ada-auth.org/standards/12rm/html/RM-6-8.html>`_


Conditional Expressions
-----------------------

In the previous section, we've seen simple expressions such as :ada:`I = 0` or
:ada:`D.Valid`. A conditional expression, as the name implies, is an
expression that contains a condition. This might be an "if-expression" (in the
:ada:`if ... then ... else` form) or a "case-expression" (in the
:ada:`case ... is when =>` form).

The :ada:`Max` function in the following code example is an expression function
implemented with a conditional expression |mdash| an if-expression, to be more
precise:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Simple_Conditional_If_Expressions

    package Expr_Func is

       function Max (A, B : Integer) return Integer is
         (if A >= B then A else B);

    end Expr_Func;


Let's say we have a system with four states :ada:`Off`, :ada:`On`,
:ada:`Waiting`, and :ada:`Invalid`. For this system, we want to implement a
function named :ada:`Toggled` that returns the *toggled* value of a state
:ada:`S`. If the current value of :ada:`S` is either :ada:`Off` or :ada:`On`,
the function toggles from :ada:`Off` to :ada:`On` (or from :ada:`On`
to :ada:`Off`). For other values, the state remains unchanged |mdash| i.e. the
returned value is the same as the input value. This is the implementation using
a conditional expression:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_If_Expressions_1

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State is
         (if S = Off then On elsif S = On then Off else S);

    end Expr_Func;

As you can see, if-expressions may contain an :ada:`elsif` branch (and
therefore be more complicated).

The code above corresponds to this more verbose version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_If_Expressions_2

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State;

    end Expr_Func;

    package body Expr_Func is

       function Toggled (S : State) return State is
       begin
          if S = Off then
             return On;
          elsif S = On then
             return Off;
          else
             return S;
          end if;
       end Toggled;

    end Expr_Func;

If we compare the if-block of this code example to the if-expression of the
previous example, we notice that the if-expression is just a simplified version
without the :ada:`return` keyword and the :ada:`end if;`. In fact, converting
an if-block to an if-expression is quite straightforward.

We could also replace the if-expression used in the :ada:`Toggled` function
above with a case-expression. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_Case_Expressions_1

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State is
         (case S is
           when Off    => On,
           when On     => Off,
           when others => S);

    end Expr_Func;

Note that we use commas in case-expressions to separate the alternatives (the
:ada:`when` expressions). The code above corresponds to this more verbose
version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Conditional_Case_Expressions_2

    package Expr_Func is

       type State is (Off, On, Waiting, Invalid);

       function Toggled (S : State) return State;

    end Expr_Func;

    package body Expr_Func is

       function Toggled (S : State) return State is
       begin
          case S is
             when Off    => return On;
             when On     => return Off;
             when others => return S;
          end case;
       end Toggled;

    end Expr_Func;

If we compare the case block of this code example to the case-expression of the
previous example, we notice that the case-expression is just a simplified
version of the case block without the :ada:`return` keyword and the
:ada:`end case;`, and with alternatives separated by commas instead of
semicolons.

.. admonition:: In the Ada Reference Manual

    - `4.5.7 Conditional Expressions <http://www.ada-auth.org/standards/12rm/html/RM-4-5-7.html>`_


Quantified Expressions
----------------------

Quantified expressions are :ada:`for` expressions using a quantifier |mdash|
which can be either :ada:`all` or :ada:`some` |mdash| and a predicate. This
kind of expressions let us formalize statements such as:

- "all values of array :ada:`A` must be zero" into
  :ada:`for all I in A'Range => A (I) = 0`, and

- "at least one value of array :ada:`A` must be zero" into
  :ada:`for some I in A'Range => A (I) = 0`.

In the quantified expression :ada:`for all I in A'Range => A (I) = 0`, the
quantifier is :ada:`all` and the predicate is :ada:`A (I) = 0`. In the second
expression, the quantifier is :ada:`some`. The result of a quantified
expression is always a Boolean value.

For example, we could use the quantified expressions above and implement these
two functions:

- :ada:`Is_Zero`, which checks whether all components of an array :ada:`A` are
  zero, and

- :ada:`Has_Zero`, which checks whether array :ada:`A` has at least one
  component of the array :ada:`A` is zero.

This is the complete code:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_1

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean is
          (for all I in A'Range => A (I) = 0);

       function Has_Zero (A : Integer_Arr) return Boolean is
          (for some I in A'Range => A (I) = 0);

       procedure Display_Array (A : Integer_Arr; Name : String);

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Arrays is

       procedure Display_Array (A    : Integer_Arr;
                                Name : String) is
       begin
          Put (Name & ": ");
          for E of A loop
             Put (E'Image & " ");
          end loop;
          New_Line;
       end Display_Array;

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Int_Arrays;  use Int_Arrays;

    procedure Test_Int_Arrays is
       A : Integer_Arr := (0, 0, 1);
    begin
       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

As you might have expected, we can rewrite a quantified expression as a loop
in the :ada:`for I in A'Range loop if ... return ...` form. In the code below,
we're implementing :ada:`Is_Zero` and :ada:`Has_Zero` using loops and
conditions instead of quantified expressions:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_2

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean;

       function Has_Zero (A : Integer_Arr) return Boolean;

       procedure Display_Array (A : Integer_Arr; Name : String);

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Arrays is

       function Is_Zero (A : Integer_Arr) return Boolean is
       begin
          for I in A'Range loop
             if A (I) /= 0 then
                return False;
             end if;
          end loop;

          return True;
       end Is_Zero;

       function Has_Zero (A : Integer_Arr) return Boolean is
       begin
          for I in A'Range loop
            if A (I) = 0 then
               return True;
            end if;
          end loop;

          return False;
       end Has_Zero;

       procedure Display_Array (A    : Integer_Arr;
                                Name : String) is
       begin
          Put (Name & ": ");
          for E of A loop
             Put (E'Image & " ");
          end loop;
          New_Line;
       end Display_Array;

    end Int_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Int_Arrays;  use Int_Arrays;

    procedure Test_Int_Arrays is
       A : Integer_Arr := (0, 0, 1);
    begin
       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));

       A := (0, 0, 0);

       Display_Array (A, "A");
       Put_Line ("Is_Zero: "  & Boolean'Image (Is_Zero (A)));
       Put_Line ("Has_Zero: " & Boolean'Image (Has_Zero (A)));
    end Test_Int_Arrays;

So far, we've seen quantified expressions using indices |mdash| e.g.
:ada:`for all I in A'Range => ...`. We could avoid indices in quantified
expressions by simply using the :ada:`E of A` form. In this case, we can just
write :ada:`for all E of A => ...`. Let's adapt the implementation of
:ada:`Is_Zero` and :ada:`Has_Zero` using this form:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Quantified_Expression_3

    package Int_Arrays is

       type Integer_Arr is array (Positive range <>) of Integer;

       function Is_Zero (A : Integer_Arr) return Boolean is
          (for all E of A => E = 0);

       function Has_Zero (A : Integer_Arr) return Boolean is
          (for some E of A => E = 0);

    end Int_Arrays;

Here, we're checking the components :ada:`E` of the array :ada:`A` and
comparing them against zero.

.. admonition:: In the Ada Reference Manual

    - `4.5.8 Quantified Expressions <http://www.ada-auth.org/standards/12rm/html/RM-4-5-8.html>`_


Declare Expressions
-------------------

.. admonition:: Relevant topics

    - `Declare Expressions <http://www.ada-auth.org/standards/2xrm/html/RM-4-5-9.html>`_

.. todo::

    Complete section!


Reduction Expressions
---------------------

.. admonition:: Relevant topics

    - `Reduction Expressions <http://www.ada-auth.org/standards/2xrm/html/RM-4-5-10.html>`_

.. todo::

    Complete section!


Overloading
-----------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #50: Overload Resolution <https://www.adacore.com/gems/gem-50>`_.

Ada allows overloading of subprograms, which means that two or more
subprogram declarations with the same name can be visible at the same
place. Here, "name" can refer to operator symbols, like :ada:`"+"`. Ada
also allows overloading of various other notations, such as literals and
aggregates.

In most languages that support overloading, overload resolution is done
"bottom up" |mdash| that is, information flows from inner constructs to outer
constructs. As usual, computer folks draw their trees upside-down, with
the root at the top. For example, if we have two procedures :ada:`Print`:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading

    procedure Show_Overloading is

       package Types is
          type Sequence is null record;
          type Set is null record;

          procedure Print (S : Sequence) is null;
          procedure Print (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin

       --  Compiler selects Print (S : Sequence)
       Print (X);
    end Show_Overloading;

the type of :ada:`X` determines which :ada:`Print` is meant in the call.

Ada is unusual in that it supports top-down overload resolution as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading

    procedure Show_Top_Down_Overloading is

       package Types is
          type Sequence is null record;
          type Set is null record;

          function Empty return Sequence is ((others => <>));
          function Empty return Set  is ((others => <>));

          procedure Print_Sequence (S : Sequence) is null;
          procedure Print_Set (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin
       --  Compiler selects function Empty return Sequence
       Print_Sequence (Empty);
    end Show_Top_Down_Overloading;

The type of the formal parameter :ada:`S` of :ada:`Print_Sequence`
determines which :ada:`Empty` is meant in the call. In C++, for example,
the equivalent of the :ada:`Print (X)` example would resolve, but the
:ada:`Print_Sequence (Empty)` would be illegal, because C++ does not use
top-down information.

If we overload things too heavily, we can cause ambiguities:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading
    :class: ada-expect-compile-error

    procedure Show_Overloading_Error is

       package Types is
          type Sequence is null record;
          type Set is null record;

          function Empty return Sequence is ((others => <>));
          function Empty return Set  is ((others => <>));

          procedure Print (S : Sequence) is null;
          procedure Print (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin
       Print (Empty);  -- Illegal!
    end Show_Overloading_Error;

The call is ambiguous, and therefore illegal, because there are two
possible meanings. One way to resolve the ambiguity is to use a qualified
expression to say which type we mean:

.. code-block:: ada

    Print (Sequence'(Empty));

Note that we're now using both bottom-up and top-down overload resolution:
:ada:`Sequence'` determines which :ada:`Empty` is meant (top down) and
which :ada:`Print` is meant (bottom up). You can qualify an expression,
even if it is not ambiguous according to Ada rules |mdash| you might want
to clarify the type because it might be ambiguous for human readers.

Of course, you could instead resolve the :ada:`Print (Empty)` example by
modifying the source code so the names are unique, as in the earlier
examples. That might well be the best solution, assuming you can modify
the relevant sources. Too much overloading can be confusing. How much is
"too much" is in part a matter of taste.

Ada really needs to have top-down overload resolution, in order to resolve
literals. In some languages, you can tell the type of a literal by looking
at it, for example appending ``L`` (letter el) means "the type of this
literal is long int". That sort of kludge won't work in Ada, because we
have an open-ended set of integer types:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Literal_Resolution

    procedure Show_Literal_Resolution is

       type Apple_Count is range 0 .. 100;

       procedure Peel (Count : Apple_Count) is null;
    begin
       Peel (20);
    end Show_Literal_Resolution;

You can't tell by looking at the literal :ada:`20` what its type is. The
type of formal parameter :ada:`Count` tells us that :ada:`20` is an
:ada:`Apple_Count`, as opposed to some other type, such as
:ada:`Standard.Long_Integer`.

Technically, the type of :ada:`20` is :ada:`universal_integer`, which is
implicitly converted to :ada:`Apple_Count` |mdash| it's really the result
type of that implicit conversion that is at issue. But that's an obscure
point |mdash| you won't go *too* far wrong if you think of the integer
literal notation as being overloaded on all integer types.

Developers sometimes wonder why the compiler can't resolve something that
seems obvious. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Literal_Resolution_Error
    :class: ada-expect-compile-error

    procedure Show_Literal_Resolution_Error is

       type Apple_Count is range 0 .. 100;
       procedure Slice (Count : Apple_Count) is null;

       type Orange_Count is range 0 .. 10_000;
       procedure Slice (Count : Orange_Count) is null;
    begin
       Slice (Count => (10_000));  --  Illegal!
    end Show_Literal_Resolution_Error;

This call is ambiguous, and therefore illegal. But why? Clearly the
developer must have meant the :ada:`Orange_Count` one, because
:ada:`10_000` is out of range for :ada:`Apple_Count`. And all the relevant
expressions happen to be static.

Well, a good rule of thumb in language design (for languages with
overloading) is that the overload resolution rules should not be
"too smart". We want this example to be illegal to avoid confusion on the
part of developers reading the code. As usual, a qualified expression
fixes it:

.. code-block:: ada

    Slice (Count => Orange_Count'(10_000));

Another example, similar to the literal, is the aggregate. Ada uses a
simple rule: the type of an aggregate is determined top down (i.e., from
the context in which the aggregate appears). Bottom-up information is not
used; that is, the compiler does not look inside the aggregate in order to
determine its type.

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Record_Resolution_Error
    :class: ada-expect-compile-error

    procedure Show_Record_Resolution_Error is

       type Complex is record
          Re, Im : Float;
       end record;

       procedure Grind (X : Complex) is null;
       procedure Grind (X : String) is null;
    begin
       Grind (X => (Re => 1.0, Im => 1.0));  --  Illegal!
    end Show_Record_Resolution_Error;

There are two :ada:`Grind` procedures visible, so the type of the
aggregate could be :ada:`Complex` or :ada:`String`, so it is ambiguous and
therefore illegal. The compiler is not required to notice that there is
only one type with components :ada:`Re` and :ada:`Im`, of some real type
|mdash| in fact, the compiler is not *allowed* to notice that, for
overloading purposes.

We can qualify as usual:

.. code-block:: ada

    Grind (X => Complex'(Re => 1.0, Im => 1.0));

Only after resolving that the type of the aggregate is :ada:`Complex` can
the compiler look inside and make sure :ada:`Re` and :ada:`Im` make sense.

This not-too-smart rule for aggregates helps prevent confusion on the part
of developers reading the code. It also simplifies the compiler, and
makes the overload resolution algorithm reasonably efficient.


Operator Overloading
--------------------

We've seen :ref:`previously <Operators>` that we can define custom operators
for record types. We can also overload operators of derived types. This allows
for modifying the behavior of operators for certain types.

To overload an operator of a derived type, we simply implement a function for
that operator. This is the same as how we implement custom operators (as we've
seen previously).

As an example, when adding two fixed-point values, the result might be out of
range, which causes an exception to be raised. A common strategy to avoid
exceptions in this case is to saturate the resulting value. This strategy is
typically employed in signal processing algorithms, for example.

In this example, we declare and use the 32-bit fixed-point type :ada:`TQ31`:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Fixed_Point_Exception

    package Fixed_Point is

       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;

    end Fixed_Point;

    with Ada.Text_IO; use Ada.Text_IO;
    with Fixed_Point; use Fixed_Point;

    procedure Show_Sat_Op is
       A, B, C : TQ31;
    begin
       A := TQ31'Last;
       B := TQ31'Last;
       C := A + B;

       Put_Line (A'Image   & " + "
                 & B'Image & " = "
                 & C'Image);

       A := TQ31'First;
       B := TQ31'First;
       C := A + B;

       Put_Line (A'Image   & " + "
                 & B'Image & " = "
                 & C'Image);

    end Show_Sat_Op;

Here, we're using the standard :ada:`+` operator, which raises a
:ada:`Constraint_Error` exception in the :ada:`C := A + B;` statement due to an
overflow. Let's now overload the addition operator and enforce saturation when
the result is out of range:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Fixed_Point_Operator_Overloading

    package Fixed_Point is

       D : constant := 2.0 ** (-31);
       type TQ31 is delta D range -1.0 .. 1.0 - D;

       function "+" (Left, Right : TQ31) return TQ31;

    end Fixed_Point;

    package body Fixed_Point is

       function "+" (Left, Right : TQ31) return TQ31 is
          type TQ31_2 is delta TQ31'Delta
            range TQ31'First * 2.0 .. TQ31'Last * 2.0;

          L   : constant TQ31_2 := TQ31_2 (Left);
          R   : constant TQ31_2 := TQ31_2 (Right);
          Res : TQ31_2;
       begin
          Res := L + R;

          if Res > TQ31_2 (TQ31'Last) then
             return TQ31'Last;
          elsif Res < TQ31_2 (TQ31'First) then
             return TQ31'First;
          else
             return TQ31 (Res);
          end if;
       end "+";

    end Fixed_Point;

    with Ada.Text_IO; use Ada.Text_IO;
    with Fixed_Point; use Fixed_Point;

    procedure Show_Sat_Op is
       A, B, C : TQ31;
    begin
       A := TQ31'Last;
       B := TQ31'Last;
       C := A + B;

       Put_Line (A'Image   & " + "
                 & B'Image & " = "
                 & C'Image);

       A := TQ31'First;
       B := TQ31'First;
       C := A + B;

       Put_Line (A'Image   & " + "
                 & B'Image & " = "
                 & C'Image);

    end Show_Sat_Op;

In the implementation of the overloaded :ada:`+` operator of the :ada:`TQ31`
type, we declare another type (:ada:`TQ31_2`) with a wider range than
:ada:`TQ31`. We use variables of the :ada:`TQ31_2` type to perform the actual
addition, and then we verify whether the result is still in :ada:`TQ31`\'s
range. If it is, we simply convert the result *back* to the :ada:`TQ31` type.
Otherwise, we saturate it |mdash| using either the first or last value of the
:ada:`TQ31` type.

When overloading operators, the overloaded operator replaces the original
one. For example, in the :ada:`A + B` operation of the :ada:`Show_Sat_Op`
procedure above, we're using the overloaded version of the :ada:`+` operator,
which performs saturation. Therefore, this operation doesn't raise an
exception (as it was the case with the original :ada:`+` operator).

.. admonition:: In the Ada Reference Manual

    - `6.6 Overloading of Operators <http://www.ada-auth.org/standards/12rm/html/RM-6-6.html>`_

Nonreturning procedures
-----------------------

.. admonition:: Relevant topics

    - `Nonreturning Procedures <http://www.ada-auth.org/standards/2xrm/html/RM-6-5-1.html>`_

.. todo::

    Complete section!


Inline subprograms
------------------

.. admonition:: Relevant topics

    - **Briefly** mention :ada:`Inline` aspect mentioned in
      `Inline Expansion of Subprograms <http://www.ada-auth.org/standards/2xrm/html/RM-6-3-2.html>`_

.. todo::

    Complete section!


.. _Null_Procedures:

Null Procedures
---------------

.. admonition:: Relevant topics

    - `Null Procedures <http://www.ada-auth.org/standards/2xrm/html/RM-6-7.html>`_
    - Mention application for (non-tagged) type extensions

.. todo::

    Complete section!
