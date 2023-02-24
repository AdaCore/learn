Subprograms
===========

.. include:: ../../global.txt

.. _Adv_Ada_Parameter_Modes_Associations:

Parameter Modes and Associations
--------------------------------

In this section, we discuss some details about parameter modes and associations.
First of all, as we know, parameters can be either formal or actual:

- Formal parameters are the ones we see in a subprogram declaration and
  implementation;

- Actual parameters are the ones we see in a subprogram call.

   - Note that actual parameters are also called *subprogram arguments* in other
     languages.

We define parameter associations as the connection between an actual parameter
in a subprogram call and its declaration as a formal parameter in a subprogram
specification or body.

.. admonition:: In the Ada Reference Manual

   - :arm:`6.2 Formal Parameter Modes <6-2>`
   - :arm:`6.4.1 Parameter Associations <6-4-1>`


Formal Parameter Modes
~~~~~~~~~~~~~~~~~~~~~~

We already discussed formal parameter modes in the
:ref:`Introduction to Ada <Intro_Ada_Parameter_Modes>` course:

+---------------+--------------------------------------------+
| :ada:`in`     | Parameter can only be read, not written    |
+---------------+--------------------------------------------+
| :ada:`out`    | Parameter can be written to, then read     |
+---------------+--------------------------------------------+
| :ada:`in out` | Parameter can be both read and written     |
+---------------+--------------------------------------------+

As this topic was already discussed in that course |mdash| and we used parameter
modes extensively in all code examples from that course |mdash|, we won't
introduce the topic again here. Instead, we'll look into some of the more
advanced details that have been left.


By-copy and by-reference
~~~~~~~~~~~~~~~~~~~~~~~~

In the :ref:`Introduction to Ada <Intro_Ada_Parameter_Modes>` course, we saw
that parameter modes don't correspond directly to how parameters are
actually passed. In fact, an :ada:`in out` parameter could be passed by copy.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.By_Copy_By_Ref_Params

    with System;

    procedure Check_Param_Passing (Formal : System.Address;
                                   Actual : System.Address);

    with Ada.Text_IO;          use Ada.Text_IO;
    with System.Address_Image;

    procedure Check_Param_Passing (Formal : System.Address;
                                   Actual : System.Address) is
    begin
       Put_Line ("Formal parameter at "
                 & System.Address_Image (Formal));
       Put_Line ("Actual parameter at "
                 & System.Address_Image (Actual));
       if System.Address_Image (Formal) =
          System.Address_Image (Actual)
       then
          Put_Line ("Parameter is passed by reference.");
       else
          Put_Line ("Parameter is passed by copy.");
       end if;
    end Check_Param_Passing;

    with System;

    package Machine_X is

       procedure Update_Value (V  : in out Integer;
                               AV :        System.Address);

    end Machine_X;

    with Check_Param_Passing;

    package body Machine_X is

       procedure Update_Value (V  : in out Integer;
                               AV :        System.Address) is
       begin
          V := V + 1;
          Check_Param_Passing (Formal => V'Address,
                               Actual => AV);
       end Update_Value;

    end Machine_X;

    with Machine_X; use Machine_X;

    procedure Show_By_Copy_By_Ref_Params is
       A : Integer := 5;
    begin
       Update_Value (A, A'Address);
    end Show_By_Copy_By_Ref_Params;

As we can see by running this example,

- the integer variable :ada:`A` in the :ada:`Show_By_Copy_By_Ref_Params`
  procedure

and

- the :ada:`V` parameter in the :ada:`Update_Value` procedure

have different addresses, so they are different objects. Therefore, we conclude
that this parameter is being passed by value, even though it has the
:ada:`in out` mode.

As we know, when a parameter is passed by copy, it is first copied to a
temporary object. In the case of a parameter with :ada:`in out` mode, the
temporary object is copied back to the original (actual) parameter at the end of
the subprogram call. In our example, the temporary object indicated by :ada:`V`
is copied back to :ada:`A` at the end of the call to :ada:`Update_Value`.

In Ada, it's not the parameter mode that determines whether a parameter is
passed by copy or by reference, but rather its type. We can distinguish between
three categories:

1. By-copy types;

2. By-reference types;

3. *Unspecified* types.

Obviously, parameters of by-copy types are passed by copy and parameters of
by-reference type are passed by reference. However, if a category isn't
specified |mdash| i.e. when the type is neither a by-copy nor a by-reference
type |mdash|, the decision is essentially left to the compiler.

As a rule of thumb, we can say that;

- elementary types |mdash| and any type that is essentially elementary, such as
  a private type whose full view is an elementary type |mdash| are passed by
  copy;

- tagged and explicitly limited types |mdash| and other types that are
  essentially tagged, such as task types |mdash| are passed by reference.

The following table provides more details:

+--------------------+-------------------+------------------------------------+
| Type category      | Parameter passing | List of types                      |
+====================+===================+====================================+
| By copy            | By copy           | - Elementary types                 |
|                    |                   | - Descendant of a private type     |
|                    |                   |   whose full type is a by-copy     |
|                    |                   |   type                             |
+--------------------+-------------------+------------------------------------+
| By reference       | By reference      | - Tagged types                     |
|                    |                   | - Task and protected types         |
|                    |                   | - Explicitly limited record types  |
|                    |                   | - Composite types with at least    |
|                    |                   |   one subcomponent of a            |
|                    |                   |   by-reference type                |
|                    |                   | - Private types whose full type    |
|                    |                   |   is a by-reference type           |
|                    |                   | - Any descendant of the types      |
|                    |                   |   mentioned above                  |
+--------------------+-------------------+------------------------------------+
| Unspecified        | Either by copy or | - Any type not mentioned above     |
|                    | by reference      |                                    |
+--------------------+-------------------+------------------------------------+

Note that, for parameters of limited types, only those parameters whose type is
*explicitly* limited are always passed by reference. We discuss this topic in
more details :ref:`in another chapter <Adv_Ada_Limited_Types_As_Parameters>`.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.By_Copy_By_Ref_Params

    with System;

    package Machine_X is

       type Integer_Array is array (Positive range <>) of Integer;

       type Rec is record
          A : Integer;
       end record;

       type Rec_Array is record
          A   : Integer;
          Arr : Integer_Array (1 .. 100);
       end record;

       type Tagged_Rec is tagged record
          A : Integer;
       end record;

       procedure Update_Value (R  : in out Rec;
                               AR :        System.Address);

       procedure Update_Value (RA  : in out Rec_Array;
                               ARA :        System.Address);

       procedure Update_Value (R  : in out Tagged_Rec;
                               AR :        System.Address);

    end Machine_X;

    with Check_Param_Passing;

    package body Machine_X is

       procedure Update_Value (R  : in out Rec;
                               AR :        System.Address) is
       begin
          R.A := R.A + 1;
          Check_Param_Passing (Formal => R'Address,
                               Actual => AR);
       end Update_Value;

       procedure Update_Value (RA  : in out Rec_Array;
                               ARA :        System.Address) is
       begin
          RA.A := RA.A + 1;
          Check_Param_Passing (Formal => RA'Address,
                               Actual => ARA);
       end Update_Value;

       procedure Update_Value (R  : in out Tagged_Rec;
                               AR :        System.Address) is
       begin
          R.A := R.A + 1;
          Check_Param_Passing (Formal => R'Address,
                               Actual => AR);
       end Update_Value;

    end Machine_X;

    with Ada.Text_IO; use Ada.Text_IO;
    with Machine_X;   use Machine_X;

    procedure Show_By_Copy_By_Ref_Params is
       TR : Tagged_Rec := (A   => 5);
       R  : Rec        := (A   => 5);
       RA : Rec_Array  := (A   => 5,
                           Arr => (others => 0));
    begin
       Put_Line ("Tagged record");
       Update_Value (TR, TR'Address);

       Put_Line ("Untagged record");
       Update_Value (R,  R'Address);

       Put_Line ("Untagged record with array");
       Update_Value (RA, RA'Address);
    end Show_By_Copy_By_Ref_Params;

When we run this example, we see that the object of tagged type
(:ada:`Tagged_Rec`) is passed by reference to the :ada:`Update_Value` procedure.
In the case of the objects of untagged record types, you might see this:

- the parameter of :ada:`Rec` type |mdash| which is an untagged record with a
  single component of integer type |mdash|, the parameter is passed by copy;

- the parameter of :ada:`Rec_Array` type |mdash| which is an untagged record
  with a large array of 100 components |mdash|, the parameter is passed by
  reference.

Because :ada:`Rec` and :ada:`Rec_Array` are neither by-copy nor by-reference
types, the decision about how to pass them to the :ada:`Update_Value` procedure
is made by the compiler. (Thus, it is possible that you see different results
when running the code above.)


Bounded errors
~~~~~~~~~~~~~~

When we use parameters of types that are neither by-copy nor by-reference types,
we might encounter the situation where we have the same object bounded to
different names in a subprogram. For example, if:

- we use a global object :ada:`Global_R` of a record type :ada:`Rec`

and

- we have a subprogram with an in-out parameter of the same record type
  :ada:`Rec`

and

- we pass :ada:`Global_R` as the actual parameter for the in-out parameter of
  this subprogram,

then we have two access paths to this object: one of them using the global
variable directly, and the other one using it indirectly via the in-out
parameter. This situation could lead to undefined behavior or to a program
error. Consider the following code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.By_Copy_By_Ref_Params

    with System;

    package Machine_X is

       type Rec is record
          A : Integer;
       end record;

       Global_R : Rec := (A => 0);

       procedure Update_Value (R  : in out Rec;
                               AR :        System.Address);

    end Machine_X;

    with Ada.Text_IO;         use Ada.Text_IO;

    with Check_Param_Passing;

    package body Machine_X is

       procedure Update_Value (R  : in out Rec;
                               AR :        System.Address) is

          procedure Show_Vars is
          begin
             Put_Line ("Global_R.A: "
                       & Integer'Image (Global_R.A));
             Put_Line ("R.A:        "
                       & Integer'Image (R.A));
          end Show_Vars;
       begin
          Check_Param_Passing (Formal => R'Address,
                               Actual => AR);

          Put_Line ("Incrementing Global_R.A...");
          Global_R.A := Global_R.A + 1;
          Show_Vars;

          Put_Line ("Incrementing R.A...");
          R.A := R.A + 5;
          Show_Vars;
       end Update_Value;

    end Machine_X;

    with Ada.Text_IO; use Ada.Text_IO;
    with Machine_X;   use Machine_X;

    procedure Show_By_Copy_By_Ref_Params is
    begin
       Put_Line ("Calling Update_Value...");
       Update_Value (Global_R,  Global_R'Address);

       Put_Line ("After call to Update_Value...");
       Put_Line ("Global_R.A: "
                 & Integer'Image (Global_R.A));
    end Show_By_Copy_By_Ref_Params;

In the :ada:`Update_Value` procedure, because :ada:`Global_R` and :ada:`R`
have a type that is neither a by-pass nor a by-reference type, the language does
not specify whether the old or the new value would be read in the calls to
:ada:`Put_Line`. In other words, the actual behavior is undefined. Also, this
situation might raise the :ada:`Program_Error` exception.

.. admonition:: Important

   As a general advice:

   - you should be very careful when using global variables and

   - you should avoid passing them as parameters in situations such as the one
     illustrated in the code example above.


.. _Adv_Ada_Aliased_Parameters:

Aliased parameters
~~~~~~~~~~~~~~~~~~

When a parameter is specified as *aliased*, it is always passed by
reference, independently of the type we're using. In this sense, we can use this
keyword to circumvent the rules mentioned so far.

Let's rewrite a previous code example that has a parameter of elementary type
and change it to *aliased*:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.By_Copy_By_Ref_Params

    with System;

    package Machine_X is

       procedure Update_Value (V  : aliased in out Integer;
                               AV :                System.Address);

    end Machine_X;

    with Check_Param_Passing;

    package body Machine_X is

       procedure Update_Value (V  : aliased in out Integer;
                               AV :                System.Address) is
       begin
          V := V + 1;
          Check_Param_Passing (Formal => V'Address,
                               Actual => AV);
       end Update_Value;

    end Machine_X;

    with Machine_X; use Machine_X;

    procedure Show_By_Copy_By_Ref_Params is
       A : aliased Integer := 5;
    begin
       Update_Value (A, A'Address);
    end Show_By_Copy_By_Ref_Params;

As we can see, :ada:`A` is now passed by reference. (Note that we can only pass
aliased objects to aliased parameters. We discuss aliased objects later
:ref:`in another chapter <Adv_Ada_Aliased_Objects>`.)


Parameter Associations
~~~~~~~~~~~~~~~~~~~~~~

When actual parameters are associated with formal parameters, some rules are
checked. As a typical example, the type of each actual parameter must match the
type of the corresponding actual parameter. In this section, we see some details
about how this association is made and some of the potential errors.

.. admonition:: In the Ada Reference Manual

   - :arm:`6.4.1 Parameter Associations <6-4-1>`


Parameter order and association
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As we already know, when calling subprograms, we can use positional or named
parameter association |mdash| or a mixture of both. Also, parameters can have
default values. Let's see some examples:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Param_Association_1

    package Operations is

       procedure Add (Left  : in out Integer;
                      Right :        Float := 1.0);

    end Operations;

    package body Operations is

       procedure Add (Left  : in out Integer;
                      Right :        Float := 1.0) is
       begin
          Left := Left + Integer (Right);
       end Add;

    end Operations;

    with Operations; use Operations;

    procedure Show_Param_Association is
       A : Integer := 5;
    begin
       --  Positional association
       Add (A, 2.0);

       --  Positional association
       --  (using default value)
       Add (A);

       --  Named association
       Add (Left  => A,
            Right => 2.0);

       --  Named association (inversed order)
       Add (Right => 2.0,
            Left  => A);

       --  Mixed positional / named association
       Add (A, Right => 2.0);
    end Show_Param_Association;

This code snippet has examples of positional and name parameter association.
Also, it has an example of mixed positional / named parameter association. In
most cases, the actual :ada:`A` parameter is associated with the formal
:ada:`Left` parameter, and the actual 2.0 parameter is associated with the
formal :ada:`Right` parameter.

In addition to that, parameters can have default values, so, when we write
:ada:`Add (A)`, the :ada:`A` variable is associated with the :ada:`Left`
parameter and the default value (1.0) is associated with the :ada:`Right`
parameter.

Also, when we use named parameter association, the parameter order is
irrelevant: we can, for example, write the last parameter as the first one.
Therefore, we can write :ada:`Add (Right => 2.0, Left  => A)` instead of
:ada:`Add (Left  => A, Right => 2.0)`.


Ambiguous calls
^^^^^^^^^^^^^^^

Ambiguous calls can be detected by the compiler during parameter association.
For example, when we have both default values in parameters and subprogram
overloading, the compiler might be unable to decide which subprogram we're
calling:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Param_Association_1
    :class: ada-expect-compile-error

    package Operations is

       procedure Add (Left  : in out Integer);

       procedure Add (Left  : in out Integer;
                      Right :        Float := 1.0);

    end Operations;

    package body Operations is

       procedure Add (Left  : in out Integer) is
       begin
          Left := Left + 1;
       end Add;

       procedure Add (Left  : in out Integer;
                      Right :        Float := 1.0) is
       begin
          Left := Left + Integer (Right);
       end Add;

    end Operations;

    with Operations; use Operations;

    procedure Show_Param_Association is
       A : Integer := 5;
    begin
       Add (A);
       --  ERROR: cannot decide which
       --         procedure to take
    end Show_Param_Association;

As we see in this example, the :ada:`Add` procedure is overloaded. The first
instance has one parameter, and the second instance has two parameters, where
the second parameter has a default value. When we call :ada:`Add` with just one
parameter, the compiler cannot decide whether we intend to call

- the first instance of :ada:`Add` with one parameter

or

- the second instance of :ada:`Add` using the default value for the second
  parameter.

In this specific case, there are multiple options to solve the issue, but all of
them involve redesigning the package specification:

- we could just rename one of :ada:`Add` procedures (thereby eliminating the
  subprogram overloading);

- we could rename the first parameter of one of the :ada:`Add` procedures and
  use named parameter association in the call to the procedure;

   - For example, we could rename the parameter to :ada:`Value` and call
     :ada:`Add (Value => A)`.

- remove the default value from the second parameter of the second instance of
  :ada:`Add`.


Overlapping actual parameters
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When we have more than one :ada:`out` or :ada:`in out` parameters in a
subprogram, we might run into the situation where the actual parameter overlaps
with another parameter. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Illegal_Calls
    :class: ada-expect-compile-error

    package Machine_X is

       procedure Update_Value (V1 : in out Integer;
                               V2 :    out Integer);

    end Machine_X;

    package body Machine_X is

       procedure Update_Value (V1 : in out Integer;
                               V2 :    out Integer) is
       begin
          V1 := V1 + 1;
          V2 := V2 + 1;
       end Update_Value;

    end Machine_X;

    with Machine_X; use Machine_X;

    procedure Show_By_Copy_By_Ref_Params is
       A : Integer := 5;
    begin
       Update_Value (A, A);
    end Show_By_Copy_By_Ref_Params;

In this case, we're using :ada:`A` for for output parameters in the call to
:ada:`Update_Value`. Having one variable in more than one output parameter is
forbidden in Ada, so this triggers a compilation error. Depending on the
specific context, you could solve this issue by using temporary variables for
the other output parameters.


.. _Adv_Ada_Operators:

Operators
---------

Operators are commonly used for variables of scalar types such as
:ada:`Integer` and :ada:`Float`. In these cases, they replace *usual* function
calls. (To be more precise, operators are function calls, but written in a
different format.) For example, we simply write :ada:`A := A + B + C;` when we
want to add three integer variables. A hypothetical, non-intuitive version of
this operation could be :ada:`A := Add (Add (A, B), C);`. In such cases,
operators allow for expressing function calls in a more intuitive way.

Many primitive operators exist for scalar types. We classify them as follows:

+--------------------+--------------------------------------------------------+
| Category           | Operators                                              |
+====================+========================================================+
| Logical            | :ada:`and`, :ada:`or`, :ada:`xor`                      |
+--------------------+--------------------------------------------------------+
| Relational         | :ada:`=`, :ada:`/=`, :ada:`<`, :ada:`<=`, :ada:`>`,    |
|                    | :ada:`>=`                                              |
+--------------------+--------------------------------------------------------+
| Unary adding       | :ada:`+`, :ada:`-`                                     |
+--------------------+--------------------------------------------------------+
| Binary adding      | :ada:`+`, :ada:`-`, :ada:`&`                           |
+--------------------+--------------------------------------------------------+
| Multiplying        | :ada:`*`, :ada:`/`, :ada:`mod`, :ada:`rem`             |
+--------------------+--------------------------------------------------------+
| Highest precedence | :ada:`**`, :ada:`abs`, :ada:`not`                      |
+--------------------+--------------------------------------------------------+

.. admonition:: In the Ada Reference Manual

    - :arm:`4.5 Operators and Expression Evaluation <4-5>`

User-defined operators
~~~~~~~~~~~~~~~~~~~~~~

For non-scalar types, not all operators are defined. For example, it wouldn't
make sense to expect a compiler to include an addition operator for a record
type with multiple components. Exceptions to this rule are the
equality and inequality operators (:ada:`=` and :ada:`/=`), which are defined
for any type (be it scalar, record types, and array types).

For array types, the concatenation operator (:ada:`&`) is a primitive operator:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Integer_Arrays_Concat

    package Integer_Arrays is

       type Integer_Array is array (Positive range <>) of Integer;

    end Integer_Arrays;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Concatenation is
       A, B : Integer_Array (1 .. 5);
       R : Integer_Array (1 .. 10);
    begin
       A := (1 & 2 & 3 & 4 & 5);
       B := (6 & 7 & 8 & 9 & 10);
       R := A & B;

       for E of R loop
          Put (E'Image & ' ');
       end loop;
       New_Line;
    end Show_Array_Concatenation;

In this example, we're using the primitive :ada:`&` operator to concatenate the
:ada:`A` and :ada:`B` arrays in the assignment to :ada:`R`. Similarly, we're
concatenating individual components (integer values) to create an aggregate
that we assign to :ada:`A` and :ada:`B`.

In contrast to this, the addition operator is not available for arrays:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Integer_Arrays_Addition
    :class: ada-expect-compile-error

    package Integer_Arrays is

       type Integer_Array is array (Positive range <>) of Integer;

    end Integer_Arrays;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Addition is
       A, B, R : Integer_Array (1 .. 5);
    begin
       A := (1 & 2 & 3 & 4 & 5);
       B := (6 & 7 & 8 & 9 & 10);
       R := A + B;

       for E of R loop
          Put (E'Image & ' ');
       end loop;
       New_Line;

    end Show_Array_Addition;

We can, however, define *custom* operators for any type. For example, if a
specific type doesn't have a predefined addition operator, we can define our
own :ada:`+` operator for it.

Note that we're limited to the operator symbols that are already defined by the
Ada language (see the previous table for the complete list of operators). In
other words, the operator we define must be selected from one of those existing
symbols; we cannot use new symbols for custom operators.

.. admonition:: In other languages

    Some programming languages |mdash| such as Haskell |mdash| allow you to
    define and use custom operator symbols. For example, in Haskell, you can
    create a new "broken bar" (`¦`) operator for integer values:

    .. code-block::

        (¦) :: Int -> Int -> Int
        a ¦ b = a + a + b

        main = putStrLn $ show (2 ¦ 3)

    This is not possible in Ada.

Let's define a custom addition operator that adds individual components of the
:ada:`Integer_Array` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Integer_Arrays_Addition

    package Integer_Arrays is

       type Integer_Array is array (Positive range <>) of Integer;

       function "+" (Left, Right : Integer_Array) return Integer_Array
         with Post => (for all I in "+"'Result'Range =>
                         "+"'Result (I) = Left (I) + Right (I));

    end Integer_Arrays;

    package body Integer_Arrays is

       function "+" (Left, Right : Integer_Array) return Integer_Array is
          R : Integer_Array (Left'Range);
       begin
          for I in Left'Range loop
             R (I) := Left (I) + Right (I);
          end loop;

          return R;
       end "+";

    end Integer_Arrays;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Integer_Arrays; use Integer_Arrays;

    procedure Show_Array_Addition is
       A, B, R : Integer_Array (1 .. 5);
    begin
       A := (1 & 2 & 3 & 4 & 5);
       B := (6 & 7 & 8 & 9 & 10);
       R := A + B;

       for E of R loop
          Put (E'Image & ' ');
       end loop;
       New_Line;

    end Show_Array_Addition;

Now, the :ada:`R := A + B` line doesn't trigger a compilation error anymore
because the :ada:`+` operator is defined for the :ada:`Integer_Array` type.

In the implementation of the :ada:`+`, we return an array with the range of the
:ada:`Left` array where each component is the sum of the :ada:`Left` and
:ada:`Right` arrays. In the declaration of the :ada:`+` operator, we're
defining the expected behavior in the postcondition. Here, we're saying that,
for each index of the resulting array (:ada:`for all I in "+"'Result'Range`),
the value of each component of the resulting array at that specific index is
the sum of the components from the :ada:`Left` and :ada:`Right` arrays at the
same index (:ada:`"+"'Result (I) = Left (I) + Right (I)`). (:ada:`for all`
denotes a :ref:`quantified expression <Adv_Ada_Quantified_Expressions>`.)

Note that, in this implementation, we assume that the range of :ada:`Right` is
a subset of the range of :ada:`Left`. If that is not the case, the
:ada:`Constraint_Error` exception will be raised at runtime in the loop. (You
can test this by declaring :ada:`B` as :ada:`Integer_Array (5 .. 10)`, for
example.)

We can also define custom operators for record types. For example, we
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
       Put_Line ("----------------");

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
       John : constant Person := "John" + "4 Main Street";
       Jane : constant Person := "Jane" + "7 High Street";
    begin
       if Jane + John = John + Jane then
          Put_Line ("It's commutative!");
       else
          Put_Line ("It's not commutative!");
       end if;
    end Show_Address_Addition;

In this example, we're using the primitive :ada:`=` operator for the
:ada:`Person` to assess whether the result of the addition is commutative.

.. admonition:: In the Ada Reference Manual

    - :arm:`6.1 Subprogram Declarations <6-1>`

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

    - :arm:`6.8 Expression functions <6-8>`


.. _Adv_Ada_Overloading:

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

We've seen :ref:`previously <Adv_Ada_Operators>` that we can define custom
operators for any type. We've also seen that subprograms can be
:ref:`overloaded <Adv_Ada_Overloading>`. Since operators are functions, we're
essentially talking about operator overloading, as we're defining the same
operator (say :ada:`+` or :ada:`-`) for different types.

As another example of operator overloading, in the Ada standard library,
operators are defined for the :ada:`Complex` type of the
:ada:`Ada.Numerics.Generic_Complex_Types` package. This package contains not
only the definition of the :ada:`+` operator for two objects of :ada:`Complex`
type, but also for combination of :ada:`Complex` and other types. For instance,
we can find these declarations:

.. code-block:: ada

    function "+" (Left, Right : Complex) return Complex;
    function "+" (Left : Complex;   Right : Real'Base) return Complex;
    function "+" (Left : Real'Base; Right : Complex)   return Complex;

This example shows that the :ada:`+` operator |mdash| as well as other
operators |mdash| are being overloaded in the :ada:`Generic_Complex_Types`
package.

.. admonition:: In the Ada Reference Manual

    - :arm:`6.6 Overloading of Operators <6-6>`
    - :arm:`G.1.1 Complex Types <G-1-1>`

Operator Overriding
-------------------

We can also override operators of derived types. This allows for modifying the
behavior of operators for the corresponding derived types.

To override an operator of a derived type, we simply implement a function for
that operator. This is the same as how we implement custom operators (as we've
seen previously).

As an example, when adding two fixed-point values, the result might be out of
range, which causes an exception to be raised. A common strategy to avoid
exceptions in this case is to saturate the resulting value. This strategy is
typically employed in signal processing algorithms, for example.

In this example, we declare and use the 32-bit fixed-point type :ada:`TQ31`:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Fixed_Point_Exception
    :class: ada-run-expect-failure

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
overflow. Let's now override the addition operator and enforce saturation when
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

In the implementation of the overridden :ada:`+` operator of the :ada:`TQ31`
type, we declare another type (:ada:`TQ31_2`) with a wider range than
:ada:`TQ31`. We use variables of the :ada:`TQ31_2` type to perform the actual
addition, and then we verify whether the result is still in :ada:`TQ31`\'s
range. If it is, we simply convert the result *back* to the :ada:`TQ31` type.
Otherwise, we saturate it |mdash| using either the first or last value of the
:ada:`TQ31` type.

When overriding operators, the overridden operator replaces the original
one. For example, in the :ada:`A + B` operation of the :ada:`Show_Sat_Op`
procedure above, we're using the overridden version of the :ada:`+` operator,
which performs saturation. Therefore, this operation doesn't raise an
exception (as it was the case with the original :ada:`+` operator).

Nonreturning procedures
-----------------------

Usually, when calling a procedure :ada:`P`, we expect that it returns to the
caller's *thread of control* after performing some action in the body of
:ada:`P`. However, there are situations where a procedure never returns. We can
indicate this fact by using the :ada:`No_Return` aspect in the subprogram
declaration.

A typical example is that of a server that is designed to run forever until the
process is killed or the machine where the server runs is switched off. This
server can be implemented as an endless loop. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Server_Proc
    :class: ada-norun

    package Servers is

       procedure Run_Server
         with No_Return;

    end Servers;

    package body Servers is

       procedure Run_Server is
       begin
          pragma Warnings (Off, "implied return after this statement");
          while True loop
             --  Processing happens here...
             null;
          end loop;
       end Run_Server;

    end Servers;

    with Servers; use Servers;

    procedure Show_Endless_Loop is
    begin
       Run_Server;
    end Show_Endless_Loop;

In this example, :ada:`Run_Server` doesn't exit from the :ada:`while True`
loop, so it never returns to the :ada:`Show_Endless_Loop` procedure.

The same situation happens when we call a procedure that raises an exception
unconditionally. In that case, exception handling is triggered, so that the
procedure never returns to the caller. An example is that of a logging
procedure that writes a message before raising an exception internally:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Log_Exception
    :class: ada-norun

    package Loggers is

       Logged_Failure : exception;

       procedure Log_And_Raise (Msg : String)
         with No_Return;

    end Loggers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Loggers is

       procedure Log_And_Raise (Msg : String) is
       begin
          Put_Line (Msg);
          raise Logged_Failure;
       end Log_And_Raise;

    end Loggers;

    with Ada.Text_IO; use Ada.Text_IO;
    with Loggers;     use Loggers;

    procedure Show_No_Return_Exception is
       Check_Passed : constant Boolean := False;
    begin
       if not Check_Passed then
          Log_And_Raise ("Check failed!");
          Put_Line ("This line will not be reached!");
       end if;
    end Show_No_Return_Exception;

In this example, :ada:`Log_And_Raise` writes a message to the user and raises
the :ada:`Logged_Failure`, so it never returns to the
:ada:`Show_No_Return_Exception` procedure.

We could implement exception handling in the :ada:`Show_No_Return_Exception`
procedure, so that the :ada:`Logged_Failure` exception could be handled there
after it's raised in :ada:`Log_And_Raise`. However,  this wouldn't be
considered a *normal* return to the procedure because it wouldn't return to the
point where it should (i.e. to the point where :ada:`Put_Line` is about to be
called, right after the call to the :ada:`Log_And_Raise` procedure).

If a nonreturning procedure returns nevertheless, this is considered a program
error, so that the :ada:`Program_Error` exception is raised. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Erroneous_Log_Exception
    :class: ada-run-expect-failure

    package Loggers is

       Logged_Failure : exception;

       procedure Log_And_Raise (Msg : String)
         with No_Return;

    end Loggers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Loggers is

       procedure Log_And_Raise (Msg : String) is
       begin
          Put_Line (Msg);
       end Log_And_Raise;

    end Loggers;

    with Ada.Text_IO; use Ada.Text_IO;
    with Loggers;     use Loggers;

    procedure Show_No_Return_Exception is
       Check_Passed : constant Boolean := False;
    begin
       if not Check_Passed then
          Log_And_Raise ("Check failed!");
          Put_Line ("This line will not be reached!");
       end if;
    end Show_No_Return_Exception;

Here, :ada:`Program_Error` is raised when :ada:`Log_And_Raise` returns to the
:ada:`Show_No_Return_Exception` procedure.

.. admonition:: In the Ada Reference Manual

    - :arm:`6.5.1 Nonreturning Subprograms <6-5-1>`


Inline subprograms
------------------

:wikipedia:`Inlining <Inline_expansion>` refers to a kind
of optimization where the code of a subprogram is expanded at the point of
the call in place of the call itself.

In modern compilers, inlining depends on the optimization level selected by the
user. For example, if we select the higher optimization level, the compiler
will perform automatic inlining agressively.

.. admonition:: In the GNAT toolchain

    The highest optimization level (``-O3``) of GNAT performs aggressive
    automatic inlining. This could mean that this level inlines too much rather
    than not enough. As a result, the cache may become an issue and the overall
    performance may be worse than the one we would achieve by compiling the
    same code with optimization level 2 (``-O2``). Therefore, the general
    recommendation is to not *just* select ``-O3`` for the optimized version of
    an application, but instead compare it the optimized version built with
    ``-O2``.

It's important to highlight that the inlining we're referring above happens
automatically, so the decision about which subprogram is inlined depends
entirely on the compiler. However, in some cases, it's better to reduce the
optimization level and perform manual inlining instead of automatic inlining.
We do that by using the :ada:`Inline` aspect.

Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Inlining_Float_Arrays

    package Float_Arrays is

       type Float_Array is array (Positive range <>) of Float;

       function Average (Data : Float_Array) return Float
         with Inline;

    end Float_Arrays;

    package body Float_Arrays is

       function Average (Data : Float_Array) return Float is
          Total : Float := 0.0;
       begin
          for Value of Data loop
             Total := Total + Value;
          end loop;
          return Total / Float (Data'Length);
       end Average;

    end Float_Arrays;

    with Ada.Text_IO; use Ada.Text_IO;

    with Float_Arrays; use Float_Arrays;

    procedure Compute_Average is
       Values        : constant Float_Array := (10.0, 11.0, 12.0, 13.0);
       Average_Value : Float;
    begin
       Average_Value := Average (Values);
       Put_Line ("Average = " & Float'Image (Average_Value));
    end Compute_Average;

When compiling this example, the compiler will most probably inline
:ada:`Average` in the :ada:`Compute_Average` procedure. Note, however, that the
:ada:`Inline` aspect is just a *recommendation* to the compiler. Sometimes, the
compiler might not be able to follow this recommendation, so it won't inline
the subprogram.

These are some examples of situations where the compiler might not be able to
inline a subprogram:

- when the code is too large,

- when it's too complicated |mdash| for example, when it involves exception
  handling |mdash|, or

- when it contains tasks, etc.

.. admonition:: In the GNAT toolchain

    In order to effectively use the :ada:`Inline` aspect, we need to set the
    optimization level to at least ``-O1`` and use the ``-gnatn`` switch, which
    instructs the compiler to take the :ada:`Inline` aspect into account.

    In addition to the :ada:`Inline` aspect, in GNAT, we also have the
    (implementation-defined) :ada:`Inline_Always` aspect. In contrast to the
    former aspect, however, the :ada:`Inline_Always` aspect isn't primarily
    related to performance. Instead, it should be used when the functionality
    would be incorrect if inlining was not performed by the compiler. Examples
    of this are procedures that insert Assembly instructions that only make
    sense when the procedure is inlined, such as memory barriers.

    Similar to the :ada:`Inline` aspect, there might be situations where a
    subprogram has the :ada:`Inline_Always` aspect, but the compiler is unable
    to inline it. In this case, we get a compilation error from GNAT.

Note that we can use the :ada:`Inline` aspect for generic subprograms as well.
When we do this, we indicate to the compiler that we wish it inlines all
instances of that generic subprogram.

.. admonition:: In the Ada Reference Manual

    - :arm:`6.3.2 Inline Expansion of Subprograms <6-3-2>`


.. _Adv_Ada_Null_Procedures:

Null Procedures
---------------

Null procedures are procedures that don't have any effect, as their body is
empty. We declare a null procedure by simply writing :ada:`is null` in its
declaration. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Subprograms.Null_Proc_1

    package Null_Procs is

       procedure Do_Nothing (Msg : String) is null;

    end Null_Procs;

As expected, calling a null procedure doesn't have any effect. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Null_Proc_1

    with Null_Procs; use Null_Procs;

    procedure Show_Null_Proc is
    begin
       Do_Nothing ("Hello");
    end Show_Null_Proc;

Null procedures are equivalent to implementing a procedure with a body that
only contains :ada:`null`. Therefore, the :ada:`Do_Nothing` procedure above is
equivalent to this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Subprograms.Null_Proc_1

    package Null_Procs is

       procedure Do_Nothing (Msg : String);

    end Null_Procs;

    package body Null_Procs is

       procedure Do_Nothing (Msg : String) is
       begin
          null;
       end Do_Nothing;

    end Null_Procs;

Null procedures and overriding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use null procedures as a way to simulate interfaces for non-tagged
types |mdash| similar to what actual interfaces do for tagged types. For
example, we may start by declaring a type and null procedures that operate on
that type. For example, let's model a very simple API:

.. code:: ada compile_button project=Courses.Advanced_Ada.Subprograms.Simple_Storage_Model

    package Simple_Storage is

       type Storage_Model is null record;

       procedure Set (S : in out Storage_Model;
                      V :        String) is null;
       procedure Display (S : Storage_Model) is null;

    end Simple_Storage;

Here, the API of the :ada:`Storage_Model` type consists of the :ada:`Set` and
:ada:`Display` procedures. Naturally, we can use objects of the
:ada:`Storage_Model` type in an application, but this won't have any effect:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Simple_Storage_Model

    with Ada.Text_IO;    use Ada.Text_IO;
    with Simple_Storage; use Simple_Storage;

    procedure Show_Null_Proc is
       S : Storage_Model;
    begin
       Put_Line ("Setting 24...");
       Set (S, "24");
       Display (S);
    end Show_Null_Proc;

By itself, the :ada:`Storage_Model` type is not very useful. However, we can
derive other types from it and override the null procedures. Let's say we want
to implement the :ada:`Integer_Storage` type to store an integer value:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Simple_Storage_Model

    package Simple_Storage is

       type Storage_Model is null record;

       procedure Set (S : in out Storage_Model;
                      V :        String) is null;
       procedure Display (S : Storage_Model) is null;

       type Integer_Storage is private;

       procedure Set (S : in out Integer_Storage;
                      V :        String);
       procedure Display (S : Integer_Storage);

    private

       type Integer_Storage is record
          V : Integer := 0;
       end record;

    end Simple_Storage;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Storage is

       procedure Set (S : in out Integer_Storage;
                      V :        String) is
       begin
          S.V := Integer'Value (V);
       end Set;

       procedure Display (S : Integer_Storage) is
       begin
          Put_Line ("Value: " & S.V'Image);
       end Display;

    end Simple_Storage;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Simple_Storage; use Simple_Storage;

    procedure Show_Null_Proc is
       S : Integer_Storage;
    begin
       Put_Line ("Setting 24...");
       Set (S, "24");
       Display (S);
    end Show_Null_Proc;

In this example, we can view :ada:`Storage_Model` as a sort of interface for
derived non-tagged types, while the derived types |mdash| such as
:ada:`Integer_Storage` |mdash| provide the actual implementation.

The section on :ref:`null records <Adv_Ada_Null_Records>` contains an extended example
that makes use of null procedures.

.. admonition:: In the Ada Reference Manual

    - :arm:`6.7 Null Procedures <6-7>`
