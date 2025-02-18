Exceptions
==========

.. include:: ../../../global.txt


.. _Adv_Ada_Classification_Of_Errors:

Classification of Errors
------------------------

When we talk about errors and erroneous behavior in Ada, we can classify them
in one of the four categories:

- compilation errors |mdash| i.e. errors that an Ada compiler must detect at
  compilation time;

- runtime errors |mdash| i.e. errors that are detected by an Ada-based
  application using checks at runtime;

- bounded errors;

- erroneous execution.

In this section, we discuss each of these categories.

.. admonition:: In the Ada Reference Manual

    - :arm:`1.1.5 Classification of Errors <1-1-5>`


.. _Adv_Ada_Compilation_Errors:

Compilation errors
~~~~~~~~~~~~~~~~~~

In the category of compilation errors, the goal is to prevent compilers to
accept illegal programs. Here, any program that doesn't follow the rules
described in the Ada Reference Manual is considered illegal. Those rules
include not only simple syntax errors, but also more complicated rules, such as
the ones concerning
:ref:`accessibility levels <Adv_Ada_Accessibility_Levels_Intro>` for access
types.

Note that Ada |mdash| in contrast to many programming languages, which can be
quite permissive |mdash| tries to prevent as many errors as possible at
compilation time because of its focus on safety. However, even though a wide
range of errors can be detected at compilation time, this doesn't mean that a
legal Ada program is free of errors. Therefore, using methods such as static
analysis or unit testing is important.


.. _Adv_Ada_Runtime_Errors:

Runtime errors
~~~~~~~~~~~~~~

When a rule cannot be verified at compilation time, a common strategy is to
have the compiler insert runtime checks into the resulting application. We see
details about these checks later on when we discuss
:ref:`checks and exceptions <Adv_Ada_Checks_And_Exceptions>`.

A typical example is an :ref:`overflow check <Adv_Ada_Overflow_Check>`.
Consider a calculation using variables: if this calculation leads to a result
that isn't representable with the underlying data types, we cannot possibly
store a value  into a register or memory that can be considered correct |mdash|
so we have to detect this situation. Unfortunately, because we're using
variables, we obviously cannot verify the result of he calculation at
compilation time, so we have to verify it at runtime.

As we've mentioned before, Ada strives for detecting as many erroneous
conditions as possible, while other programming language would allow errors
such as overflow errors to remain undetected |mdash| which would likely lead
the application to misbehave. Those checks raise an exception if an erroneous
condition is detected, so the programmer has the means |mdash| and the
responsibility |mdash| to catch that exception and handle the situation
properly (Note, however, that some of the runtime checks can be deactivated.
We will discuss this topic later on.)


.. _Adv_Ada_Bounded_Errors:

Bounded errors
~~~~~~~~~~~~~~

For certain kinds of errors, the compiler might not be able to detect the error
|mdash| neither at compilation time, nor with checks at runtime. Such errors
are called bounded errors if their possible effects are *bounded*. In fact, the
Ada Reference Manual describes each bounded error and its possible effects
|mdash| one of those effects is raising the :ada:`Program_Error` exception.

Just as an example, consider the bounded error described in section
:arm:`13.9.1 Data Validity <13-9-1>`, paragraphs 9:

    If the representation of a scalar object does not represent a value of the
    object's subtype (perhaps because the object was not initialized), the
    object is said to have an invalid representation. It is a bounded error to
    evaluate the value of such an object. If the error is detected, either
    :ada:`Constraint_Error` or :ada:`Program_Error` is raised. Otherwise,
    execution continues using the invalid representation. The rules of the
    language outside this subclause assume that all objects have valid
    representations.

Let's see a code example:

.. code:: ada no_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Classification_Of_Errors.Data_Validity_Bounded_Error
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Bounded_Error is
       subtype Int_1_10 is Integer range 1 .. 10;

       I1         : Int_1_10;
       I1_Overlay : Integer
         with Address => I1'Address, Import, Volatile;
    begin
       I1_Overlay := 0;
       --  ^^^^^^^^^^^
       --  We use this overlay to write an invalid
       --  value to I1.

       Put_Line ("I1 = " & I1'Image);
       --                  ^^^^^^^^
       --  Bounded error: value in
       --  I1 is out of range.

       I1 := I1 + 1;
       --    ^^
       --  Bounded error: using value
       --  in operation that is out of
       --  range.

       Put_Line ("I1 = " & I1'Image);
    end Show_Bounded_Error;

In this example, we simulate a missing initialization by using an overlay
(:ada:`I1_Overlay`). As a consequence, :ada:`I1` has an invalid value that is
out of the allowed range of the :ada:`Int_1_10` subtype. This situation causes
two bounded errors:

- a bounded error when :ada:`I1` is evaluated in the call to :ada:`Image`; and

- a bounded error when the value of the right-sided :ada:`I1` is evaluated
  |mdash| in the increment :ada:`I1 := I1 + 1`.

.. admonition:: In the Ada Reference Manual

    - :arm:`13.9.1 Data Validity <13-9-1>`


.. _Adv_Ada_Erroneous_Execution:

Erroneous execution
~~~~~~~~~~~~~~~~~~~

Erroneous execution is similar to bounded errors in the sense that having the
compiler detect the erroneous condition at compilation time or at runtime isn't
possible. However, unlike bounded errors, the effects are usually
nondeterministic: a bound on possible effects is not described by the language.

Again, as an example of erroneous execution, consider the description from
section :arm:`13.9.1 Data Validity <13-9-1>`, paragraph 12/3, which discusses
the implications of using the :ada:`Unchecked_Conversion` function. Let's see a
code example:

.. code:: ada no_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Classification_Of_Errors.Data_Validity_Erroneous_Execution
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Unchecked_Conversion;

    procedure Show_Erroneous_Execution is
       subtype Int_1_10 is Integer range 1 .. 10;

       function To_Int_1_10 is new
         Ada.Unchecked_Conversion (Source => Integer,
                                   Target => Int_1_10);

       I1 : Int_1_10 := To_Int_1_10 (0);
       --               ^^^^^^^^^^^^^^^
       --  Bounded error
    begin
       Put_Line ("I1 = " & I1'Image);

       I1 := I1 + 1;
       --    ^^^^^^
       --  Erroneous execution: using value
       --  in operation that is out of range.

       Put_Line ("I1 = " & I1'Image);
    end Show_Erroneous_Execution;

It is consider to be a bounded error to use the :ada:`To_Int_1_10` function
(based on :ada:`Unchecked_Conversion`) with a value that is invalid for the
target data type. However, if we use the invalid value of :ada:`I1` in an
operation such as the :ada:`I1 := I1 + 1` assignment, this leads to erroneous
execution, and the effects are unpredictable: they aren't described in the Ada
Reference Manual, as they are nondeterministic.

.. admonition:: In the Ada Reference Manual

    - :arm:`13.9.1 Data Validity <13-9-1>`


Asserts
-------

When we want to indicate a condition in the code that must always be valid, we
can use the pragma :ada:`Assert`. As the name implies, when we use this pragma,
we're *asserting* some truth about the source-code. (We can also use the
procedural form, as we'll see later.)

.. admonition:: Important

    Another method to assert the truth about the source-code is to use
    :doc:`pre and post-conditions <../design_by_contracts/contracts>`.

A simple assert has this form:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Asserts.Pragma_Assert_1

    procedure Show_Pragma_Assert is
       I : constant Integer := 10;

       pragma Assert (I = 10);
    begin
       null;
    end Show_Pragma_Assert;

In this example, we're asserting that the value of :ada:`I` is always 10. We
could also display a message if the assertion is false:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Asserts.Pragma_Assert_2
    :class: ada-run-expect-failure

    procedure Show_Pragma_Assert is
       I : constant Integer := 11;

       pragma Assert (I = 10, "I is not 10");
    begin
       null;
    end Show_Pragma_Assert;

Similarly, we can use the procedural form of :ada:`Assert`. For example, the
code above can implemented as follows:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Asserts.Procedure_Assert
    :class: ada-run-expect-failure

    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Procedure_Assert is
       I : constant Integer := 11;

    begin
       Assert (I = 10, "I is not 10");
    end Show_Procedure_Assert;

Note that a call to :ada:`Assert` is simply translated to a check |mdash| and
the :ada:`Assertion_Error` exception from the :ada:`Ada.Assertions` package
being raised in the case that the check fails. For example, the code above
roughly corresponds to this:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Asserts.Assertion_Error
    :class: ada-run-expect-failure

    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Assertion_Error is
       I : constant Integer := 11;

    begin
       if I /= 10 then
          raise Assertion_Error with "I is not 10";
       end if;

    end Show_Assertion_Error;

.. admonition:: In the Ada Reference Manual

    - :arm22:`11.4.2 Pragmas Assert and Assertion_Policy <11-4-2>`


Assertion policies
------------------

We can activate and deactivate assertions based on assertion policies. We can do
that by using the pragma :ada:`Assertion_Policy`. As an argument to this pragma,
we indicate whether a specific policy must be checked or ignored.

For example, we can deactivate assertion checks by specifying
:ada:`Assert => Ignore`. Similarly, we can activate assertion checks by
specifying :ada:`Assert => Check`. Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Assertion_Policies.Pragma_Assertion_Policy_1

    procedure Show_Pragma_Assertion_Policy is
       I : constant Integer := 11;

       pragma Assertion_Policy (Assert => Ignore);
    begin
       pragma Assert (I = 10);
    end Show_Pragma_Assertion_Policy;

Here, we're specifying that asserts shall be ignored. Therefore, the call to the
pragma :ada:`Assert` doesn't raise an exception. If we replace :ada:`Ignore`
with :ada:`Check` in the call to :ada:`Assertion_Policy`, the assert will raise
the :ada:`Assertion_Error` exception.

The following table presents all policies that we can set:

+----------------------------------+-------------------------------------------+
| Policy                           | Descripton                                |
+==================================+===========================================+
| :ada:`Assert`                    | Check assertions                          |
+----------------------------------+-------------------------------------------+
| :ada:`Static_Predicate`          | Check static predicates                   |
+----------------------------------+-------------------------------------------+
| :ada:`Dynamic_Predicate`         | Check dynamic predicates                  |
+----------------------------------+-------------------------------------------+
| :ada:`Pre`                       | Check pre-conditions                      |
+----------------------------------+-------------------------------------------+
| :ada:`Pre'Class`                 | Check pre-condition of classes of tagged  |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+
| :ada:`Post`                      | Check post-conditions                     |
+----------------------------------+-------------------------------------------+
| :ada:`Post'Class`                | Check post-condition of classes of tagged |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+
| :ada:`Type_Invariant`            | Check type invariants                     |
+----------------------------------+-------------------------------------------+
| :ada:`Type_Invariant'Class`      | Check type invariant of classes of tagged |
|                                  | types                                     |
+----------------------------------+-------------------------------------------+

.. admonition:: In the GNAT toolchain

    Compilers are free to include policies that go beyond the ones listed above.
    For example, GNAT includes the following policies |mdash| called
    *assertion kinds* in this context:

    - :ada:`Assertions`
    - :ada:`Assert_And_Cut`
    - :ada:`Assume`
    - :ada:`Contract_Cases`
    - :ada:`Debug`
    - :ada:`Ghost`
    - :ada:`Initial_Condition`
    - :ada:`Invariant`
    - :ada:`Invariant'Class`
    - :ada:`Loop_Invariant`
    - :ada:`Loop_Variant`
    - :ada:`Postcondition`
    - :ada:`Precondition`
    - :ada:`Predicate`
    - :ada:`Refined_Post`
    - :ada:`Statement_Assertions`
    - :ada:`Subprogram_Variant`

    Also, in addtion to :ada:`Check` and :ada:`Ignore`, GNAT allows you to set
    a policy to :ada:`Disable` and :ada:`Suppressible`.

    You can read more about them in the
    `GNAT Reference Manual <https://gcc.gnu.org/onlinedocs/gnat_rm/Pragma-Assertion_005fPolicy>`_.

You can specify multiple policies in a single call to :ada:`Assertion_Policy`.
For example, you can activate all policies by writing:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Assertion_Policies.Multiple_Assertion_Policies

    procedure Show_Multiple_Assertion_Policies is
       pragma Assertion_Policy
         (Assert               => Check,
          Static_Predicate     => Check,
          Dynamic_Predicate    => Check,
          Pre                  => Check,
          Pre'Class            => Check,
          Post                 => Check,
          Post'Class           => Check,
          Type_Invariant       => Check,
          Type_Invariant'Class => Check);
    begin
       null;
    end Show_Multiple_Assertion_Policies;

.. admonition:: In the GNAT toolchain

    With GNAT, policies can be specified in multiple ways. In addition to calls
    to :ada:`Assertion_Policy`, you can use
    `configuration pragmas files <https://gcc.gnu.org/onlinedocs/gnat_ugn/The-Configuration-Pragmas-Files#The-Configuration-Pragmas-Files>`_.
    You can use these files to specify all pragmas that are relevant to your
    application, including :ada:`Assertion_Policy`. In addition, you can manage
    the granularity for those pragmas. For example, you can use a global
    configuration pragmas file for your complete application, or even different
    files for each source-code file you have.

    Also, by default, all policies listed in the table above are deactivated,
    i.e. they're all set to :ada:`Ignore`. You can use the command-line switch
    :ada:`-gnata` to activate them.

Note that the :ada:`Assert` procedure raises an exception independently of the
assertion policy (:ada:`Assertion_Policy (Assert => Ignore)`). For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Assertion_Policies.Assert_Procedure_Policy
    :class: ada-run-expect-failure

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Assertions; use Ada.Assertions;

    procedure Show_Assert_Procedure_Policy is
       pragma Assertion_Policy (Assert => Ignore);

       I : constant Integer := 1;
    begin
       Put_Line ("------ Pragma Assert -----");
       pragma Assert (I = 0);

       Put_Line ("---- Procedure Assert ----");
       Assert (I = 0);

       Put_Line ("Finished.");
    end Show_Assert_Procedure_Policy;

Here, the :ada:`pragma Assert` is ignored due to the assertion policy. However,
the call to :ada:`Assert` is not ignored.

.. admonition:: In the Ada Reference Manual

    - :arm22:`11.4.2 Pragmas Assert and Assertion_Policy <11-4-2>`


..
    TO BE DONE:

    :ada:`Default_Initial_Condition` policy
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    .. admonition:: In the Ada Reference Manual

        - :arm22:`7.3.3 Default Initial Conditions <11-4-2>`
        - :arm22:`11.4.2 Pragmas Assert and Assertion_Policy <11-4-2>`

    .. todo::

        Complete section!


.. _Adv_Ada_Checks_And_Exceptions:

Checks and exceptions
---------------------

This table shows all language-defined checks and the associated exceptions:

+-----------------------------+-------------------------+
| Check                       | Exception               |
+=============================+=========================+
| :ada:`Access_Check`         | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Discriminant_Check`   | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Division_Check`       | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Index_Check`          | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Length_Check`         | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Overflow_Check`       | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Range_Check`          | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Tag_Check`            | :ada:`Constraint_Error` |
+-----------------------------+-------------------------+
| :ada:`Accessibility_Check`  | :ada:`Program_Error`    |
+-----------------------------+-------------------------+
| :ada:`Allocation_Check`     | :ada:`Program_Error`    |
+-----------------------------+-------------------------+
| :ada:`Elaboration_Check`    | :ada:`Program_Error`    |
+-----------------------------+-------------------------+
| :ada:`Program_Error_Check`  | :ada:`Program_Error`    |
+-----------------------------+-------------------------+
| :ada:`Storage_Check`        | :ada:`Storage_Error`    |
+-----------------------------+-------------------------+
| :ada:`Tasking_Check`        | :ada:`Tasking_Error`    |
+-----------------------------+-------------------------+

In addition, we can use :ada:`All_Checks` to refer to all those checks above at
once.

Let's discuss each check and see code examples where those checks are
performed. Note that all examples are erroneous, so please avoid reusing them
elsewhere.


.. _Adv_Ada_Access_Check:

Access Check
~~~~~~~~~~~~

As you know, an object of an access type might be null. It would be an error to
dereference this object, as it doesn't indicate a valid position in memory.
Therefore, the access check verifies that an access object is not null when
dereferencing it. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Access_Check
    :class: ada-run-expect-failure

    procedure Show_Access_Check is

       type Integer_Access is access Integer;

       AI : Integer_Access;
    begin
       AI.all := 10;
    end Show_Access_Check;

Here, the value of :ada:`AI` is null by default, so we cannot dereference it.

The access check also performs this verification when assigning to a subtype
that excludes null (:ada:`not null access`). (You can find more information
about this topic in the section about
:ref:`not null access <Adv_Ada_Not_Null_Access>`.) For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Access_Check_2
    :class: ada-run-expect-failure

    procedure Show_Access_Check is

       type Integer_Access is
         access all Integer;

       type Safe_Integer_Access is
         not null access all Integer;

       AI  : Integer_Access;
       SAI : Safe_Integer_Access := new Integer;

    begin
       SAI := Safe_Integer_Access (AI);
    end Show_Access_Check;

Here, the value of :ada:`AI` is null (by default), so we cannot assign it to
:ada:`SAI` because its type excludes null.

Note that, if we remove the :ada:`:= new Integer` assignment from the
declaration of :ada:`SAI`, the null exclusion fails in the declaration
itself (because the default value of the access type is :ada:`null`).


.. _Adv_Ada_Discriminant_Check:

Discriminant Check
~~~~~~~~~~~~~~~~~~

As we've seen earlier, a variant record is a record with discriminants that
allows for changing its structure. In operations such as an assignment, it's
important to ensure that the discriminants of the objects match |mdash| i.e. to
ensure that the structure of the objects matches. The discriminant check
verifies whether this is the case. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Discriminant_Check
    :class: ada-run-expect-failure

    procedure Show_Discriminant_Check is

       type Rec (Valid : Boolean) is record
          case Valid is
             when True =>
                Counter : Integer;
             when False =>
                null;
          end case;
       end record;

       R : Rec (Valid => False);
    begin
       R := (Valid  => True,
             Counter => 10);
    end Show_Discriminant_Check;

Here, :ada:`R`\ 's discriminant (:ada:`Valid`) is :ada:`False`, so we cannot
assign an object whose :ada:`Valid` discriminant is :ada:`True`.

Also, when accessing a component, the discriminant check ensures that this
component exists for the current discriminant value:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Discriminant_Check_2
    :class: ada-run-expect-failure

    procedure Show_Discriminant_Check is

       type Rec (Valid : Boolean) is record
          case Valid is
             when True =>
                Counter : Integer;
             when False =>
                null;
          end case;
       end record;

       R : Rec (Valid => False);
       I : Integer;
    begin
       I := R.Counter;
    end Show_Discriminant_Check;

Here, :ada:`R`\ 's discriminant (:ada:`Valid`) is :ada:`False`, so we cannot
access the :ada:`Counter` component, for it only exists when the :ada:`Valid`
discriminant is :ada:`True`.


Division Check
~~~~~~~~~~~~~~

The division check verifies that we're not trying to divide a value by zero
when using the :ada:`/`, :ada:`rem` and :ada:`mod` operators. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Division_Check
    :class: ada-run-expect-failure

    package Ops is
       function Div_Op (A, B : Integer)
                        return Integer is
         (A / B);

       function Rem_Op (A, B : Integer)
                        return Integer is
         (A rem B);

       function Mod_Op (A, B : Integer)
                        return Integer is
         (A mod B);
    end Ops;

    with Ops; use Ops;

    procedure Show_Division_Check is
       I : Integer;
    begin
       I := Div_Op (10, 0);
       I := Rem_Op (10, 0);
       I := Mod_Op (10, 0);
    end Show_Division_Check;

All three calls in the :ada:`Show_Division_Check` procedure |mdash| to
the :ada:`Div_Op`, :ada:`Rem_Op` and :ada:`Mod_Op` functions |mdash| can raise
an exception because we're using 0 as the second argument, which makes the
division check in those functions fail.


Index Check
~~~~~~~~~~~

We use indices to access components of an array. An index check verifies that
the index we're using to access a specific component is within the array's
bounds. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Index_Check
    :class: ada-run-expect-failure

    procedure Show_Index_Check is

       type Integer_Array is
         array (Positive range <>) of Integer;

       function Value_Of (A : Integer_Array;
                          I : Integer)
                          return Integer
       is
          type Half_Integer_Array is new
            Integer_Array (A'First ..
                           A'First + A'Length / 2);

          A_2 : Half_Integer_Array := (others => 0);
       begin
          return A_2 (I);
       end Value_Of;

       Arr_1 : Integer_Array (1 .. 10) :=
                 (others => 1);

    begin
       Arr_1 (10) := Value_Of (Arr_1, 10);

    end Show_Index_Check;

The range of :ada:`A_2` |mdash| which is passed as an argument to the
:ada:`Value_Of` function |mdash| is 1 to 6. However, in that function call,
we're trying to access position 10, which is outside :ada:`A_2` \'s bounds.

Length Check
~~~~~~~~~~~~

In array assignments, both arrays must have the same length. To ensure that
this is the case, a length check is performed. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Length_Check
    :class: ada-run-expect-failure

    procedure Show_Length_Check is

       type Integer_Array is
         array (Positive range <>) of Integer;

       procedure Assign (To   : out Integer_Array;
                         From :     Integer_Array) is
       begin
          To := From;
       end Assign;

       Arr_1 : Integer_Array (1 .. 10);
       Arr_2 : Integer_Array (1 .. 9) :=
                 (others => 1);

    begin
       Assign (Arr_1, Arr_2);
    end Show_Length_Check;

Here, the length of :ada:`Arr_1` is 10, while the length of :ada:`Arr_2` is 9,
so we cannot assign :ada:`Arr_2` (:ada:`From` parameter) to :ada:`Arr_1`
(:ada:`To` parameter) in the :ada:`Assign` procedure.


.. _Adv_Ada_Overflow_Check:

Overflow Check
~~~~~~~~~~~~~~

Operations on scalar objects might lead to overflow, which, if not checked,
lead to wrong information being computed and stored. Therefore, an overflow
check verifies that the value of a scalar object is within the base range of
its type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Overflow_Check
    :class: ada-run-expect-failure

    procedure Show_Overflow_Check is
       A, B : Integer;
    begin
       A := Integer'Last;
       B := 1;

       A := A + B;
    end Show_Overflow_Check;

In this example, :ada:`A` already has the last possible value of the
:ada:`Integer'Base` range, so increasing it by one causes an overflow error.


Range Check
~~~~~~~~~~~

The range check verifies that a scalar value is within a specific range |mdash|
for instance, the range of a subtype. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Range_Check
    :class: ada-run-expect-failure

    procedure Show_Range_Check is

       subtype Int_1_10 is Integer range 1 .. 10;

       I : Int_1_10;

    begin
       I := 11;
    end Show_Range_Check;

In this example, we're trying to assign 11 to the variable :ada:`I` of the
:ada:`Int_1_10` subtype, which has a range from 1 to 10. Since 11 is outside
that range, the range check fails.


Tag Check
~~~~~~~~~

The tag check ensures that the tag of a tagged object matches the expected tag
in a dispatching operation. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Tag_Check
    :class: ada-run-expect-failure

    package P is

       type T is tagged null record;
       type T1 is new T with null record;
       type T2 is new T with null record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Tags;

    with P;           use P;

    procedure Show_Tag_Check is

       A1 : T'Class := T1'(null record);
       A2 : T'Class := T2'(null record);

    begin
       Put_Line ("A1'Tag: "
                 & Ada.Tags.Expanded_Name (A1'Tag));
       Put_Line ("A2'Tag: "
                 & Ada.Tags.Expanded_Name (A2'Tag));

       A2 := A1;

    end Show_Tag_Check;

Here, :ada:`A1` and :ada:`A2` have different tags:

- :ada:`A1'Tag = T1'Tag`, while
- :ada:`A2'Tag = T2'Tag`.

Since the tags don't match, the tag check fails in the assignment of :ada:`A1`
to :ada:`A2`.


.. _Adv_Ada_Accessibility_Check:

Accessibility Check
~~~~~~~~~~~~~~~~~~~

The accessibility check verifies that the accessibility level of an entity
matches the expected level. We discuss accessibility levels
:ref:`in a later chapter <Adv_Ada_Accessibility_Levels_Intro>`.

Let's look at an example that mixes access types and anonymous access types.
Here, we use an anonymous access type in the declaration of :ada:`A1` and a
named access type in the declaration of :ada:`A2`:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Accessibility_Check
    :class: ada-run-expect-failure

    package P is

       type T is tagged null record;
       type T_Class is access all T'Class;

    end P;

    with P; use P;

    procedure Show_Accessibility_Check is

       A1 : access T'Class := new T;
       A2 : T_Class;

    begin
       A2 := T_Class (A1);

    end Show_Accessibility_Check;

The anonymous type (:ada:`access T'Class`), which is used in the declaration of
:ada:`A1`, doesn't have the same accessibility level as the :ada:`T_Class`
type. Therefore, the accessibility check fails during the :ada:`T_Class (A1)`
conversion.

We can see the accessibility check failing in this example as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Accessibility_Check
    :class: ada-run-expect-failure

    with P; use P;

    procedure Show_Accessibility_Check is

       A : access T'Class := new T;

       procedure P (A : T_Class) is null;

    begin
       P (T_Class (A));

    end Show_Accessibility_Check;

Again, the check fails in the :ada:`T_Class (A)` conversion and raises a
:ada:`Program_Error` exception.


.. _Adv_Ada_Allocation_Check:

Allocation Check
~~~~~~~~~~~~~~~~

The allocation check ensures, when a task is about to be created, that its
master has not been completed. Also, it ensures that the finalization has not
started.

This is an example adapted from
`AI-00280 <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00280.txt?rev=1.12&raw=N>`_:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Allocation_Check
    :class: ada-run-expect-failure

    with Ada.Finalization;
    with Ada.Unchecked_Deallocation;

    package P is
       type T1 is new
         Ada.Finalization.Controlled with null record;
       procedure Finalize (X : in out T1);

       type T2 is new
         Ada.Finalization.Controlled with null record;
       procedure Finalize (X : in out T2);

       X1 : T1;

       type T2_Ref is access T2;
       procedure Free is new
         Ada.Unchecked_Deallocation (T2, T2_Ref);
    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       procedure Finalize (X : in out T1) is
          X2 : T2_Ref := new T2;
       begin
          Put_Line ("Finalizing T1...");
          Free (X2);
       end Finalize;

       procedure Finalize (X : in out T2) is
       begin
          Put_Line ("Finalizing T2...");
       end Finalize;

    end P;

    with P; use P;

    procedure Show_Allocation_Check is
       X2 : T2_Ref := new T2;
    begin
       Free (X2);
    end Show_Allocation_Check;

Here, in the finalization of the :ada:`X1` object of :ada:`T1` type, we're
trying to create an object of :ada:`T2` type while the finalization of the
master has already started. (Note that :ada:`X1` was declared in the :ada:`P`
package.) This is forbidden, so the allocation check raises a
:ada:`Program_Error` exception.


Elaboration Check
~~~~~~~~~~~~~~~~~

The elaboration check verifies that subprograms |mdash| or protected entries,
or task activations |mdash| have been elaborated before being called.

This is an example adapted from
`AI-00064 <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00064.txt?rev=1.12&raw=N>`_:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Elaboration_Check
    :class: ada-run-expect-failure

    function P return Integer;

    function P return Integer is
    begin
       return 1;
    end P;

    with P;

    procedure Show_Elaboration_Check is

       function F return Integer;

       type Pointer_To_Func is
         access function return Integer;

       X : constant Pointer_To_Func := P'Access;

       Y : constant Integer := F;
       Z : constant Pointer_To_Func := X;

       --  Renaming-as-body
       function F return Integer renames Z.all;
    begin
       null;
    end Show_Elaboration_Check;

This is a curious example: first, we declare a function :ada:`F` and assign the
value returned by this function to constant :ada:`Y` in its declaration. Then,
we declare :ada:`F` as a renamed function, thereby providing a body to :ada:`F`
|mdash| this is called renaming-as-body. Consequently, the compiler doesn't
complain that a body is missing for function :ada:`F`. (If you comment out the
function renaming, you'll see that the compiler can then detect the missing
body.) Therefore,  at runtime, the elaboration check fails because the body of
the first declaration of the :ada:`F` function is actually missing.


.. _Adv_Ada_Program_Error_Check:

:ada:`Program_Error_Check`
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

   This concept was introduced in Ada 2022.

As we've seen before, there are three checks that may raise a
:ada:`Program_Error` exception: the :ada:`Accessibility_Check`, the
:ada:`Allocation_Check` and the :ada:`Elaboration_Check`. In addition to that,
we have the :ada:`Program_Error_Check`, which is actually a collection of
various different checks that may raise a :ada:`Program_Error`, but don't have
a category for themselves.

For completeness, these are the error conditions checked by the
:ada:`Program_Error_Check` (listed in the
`Action Item (AI) 12-0309 document <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0309-1.txt?rev=1.5&raw=N>`_),
according to their definition in the Ada Reference Manual:

+---------------------------------------------------------------+-----------+-------------------------------------------------+
| Ada Reference Manual                                          | Paragraph | Description                                     |
+===============================================================+===========+=================================================+
| :arm22:`3.2.4 Subtype Predicates <3-2-4>`                     | (29.1/4)  | It checks that subtypes with predicates are     |
|                                                               |           | not used to index an array in generic units.    |
+---------------------------------------------------------------+-----------+-------------------------------------------------+
| :arm22:`5.5 Loop Statements <5-5>`                            | (8.1/5)   | It checks that the maximum number of chunks for |
|                                                               |           | statement-level parallelism is                  |
|                                                               |           | greater than zero.                              |
+---------------------------------------------------------------+-----------+-------------------------------------------------+
| :arm22:`6.4.1 Parameter Associations <6-4-1>`                 | (13.4/4)  | It checks that the default value of an out      |
|                                                               |           | parameter is convertible: an error occurs when  |
|                                                               |           | we have an out parameter with                   |
|                                                               |           | :ada:`Default_Value`, and the actual is a       |
|                                                               |           | view conversion of an unrelated type that does  |
|                                                               |           | not have :ada:`Default_Value`.                  |
+---------------------------------------------------------------+-----------+-------------------------------------------------+
| :arm22:`12.5.1 Formal Private and Derived Types <12-5-1>`     | (23.3/2)  | It checks that there is no misuse of functions  |
|                                                               |           | in a generic with a class-wide actual type.     |
+---------------------------------------------------------------+-----------+-------------------------------------------------+
| :arm22:`13.3 Operational and Representation Attributes <13-3>`| (75.1/3)  | It checks that there are no colliding           |
|                                                               |           | :ada:`External_Tag` values.                     |
+---------------------------------------------------------------+-----------+-------------------------------------------------+
| :arm22:`B.3.3 Unchecked Union Types <B-3-3>`                  | (22/2)    | It checks that there is no misuse of            |
|                                                               |           | operations of :ada:`Unchecked_Unions` without   |
|                                                               |           | inferable discriminants.                        |
+---------------------------------------------------------------+-----------+-------------------------------------------------+

.. admonition:: In the Ada Reference Manual

    - :arm22:`11.5 Suppressing Checks <11-5>`
    - :arm22:`3.2.4 Subtype Predicates <3-2-4>`
    - :arm22:`5.5 Loop Statements <5-5>`
    - :arm22:`6.4.1 Parameter Associations <6-4-1>`
    - :arm22:`12.5.1 Formal Private and Derived Types <12-5-1>`
    - :arm22:`13.3 Operational and Representation Attributes <13-3>`
    - :arm22:`B.3.3 Unchecked Union Types <B-3-3>`


Example of a :ada:`Program_Error_Check`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Just as an example, let's look at the check for subtype predicates in generic
units:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Subtype_Predicate_Programm_Error
    :class: ada-run-expect-failure

    generic
       type R is (<>);
    package Some_Generic_Package is
       procedure Process;
    end Some_Generic_Package;

    package body Some_Generic_Package is

       procedure Process is
          type Arr is
            array (R) of Integer;

          Dummy : Arr := (others => 0);
       begin
          null;
       end Process;

    end Some_Generic_Package;

    with Some_Generic_Package;

    procedure Show_Subtype_Predicate_Programm_Error is

       type Custom_Range is range 1 .. 5
         with Dynamic_Predicate =>
                4 not in Custom_Range;

       package P is new
         Some_Generic_Package (Custom_Range);
       use P;
    begin
       Process;
    end Show_Subtype_Predicate_Programm_Error;

Here, we're using the :ada:`Custom_Range` type for the formal type :ada:`R`
in the instantiation of the generic package :ada:`Some_Generic_Package`. Since
we use :ada:`R` as an index for the array type :ada:`Arr` (in the procedure
:ada:`Process`), we cannot map a type to :ada:`R` that uses a predicate.
Therefore, because :ada:`Custom_Range` type has a dynamic predicate, the
:ada:`Program_Error` exception is raised.


.. _Adv_Ada_Storage_Check:

Storage Check
~~~~~~~~~~~~~

The storage check ensures that the storage pool has enough space when
allocating memory. Let's revisit an example that we
:ref:`discussed earlier <Adv_Ada_Types_Storage_Size_Error>`:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Storage_Check
    :class: ada-run-expect-failure

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Reserved_Access is access UInt_7
         with Storage_Size => 8;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Storage_Check is

       RAV1, RAV2 : UInt_7_Reserved_Access;

    begin
       Put_Line ("Allocating RAV1...");
       RAV1 := new UInt_7;

       Put_Line ("Allocating RAV2...");
       RAV2 := new UInt_7;

       New_Line;
    end Show_Storage_Check;

On each allocation (:ada:`new UInt_7`), a storage check is performed. Because
there isn't enough reserved storage space before the second allocation, the
checks fails and raises a :ada:`Storage_Error` exception.






.. _Adv_Ada_Tasking_Check:

:ada:`Tasking_Check`
~~~~~~~~~~~~~~~~~~~~

The :ada:`Tasking_Check` ensures that all tasks have been activated
successfully and that no terminated task is called. If the check fails, a
:ada:`Tasking_Error` exception is raised.

.. note::

   This concept was introduced in Ada 2022. It was created to group all checks
   that might raise the :ada:`Tasking_Error` exception.

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Checks_And_Exceptions.Tasking_Check_Error
    :class: ada-run-expect-failure

    package Workers is

        task type Worker  is
            entry Start;
        end Worker;

    end Workers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Workers is

        task body Worker  is
        begin
           Put_Line ("Task has started.");
           delay 1.0;
           Put_Line ("Task has finished.");
        end Worker;

    end Workers;

    with Ada.Text_IO; use Ada.Text_IO;
    with Workers;     use Workers;

    procedure Show_Tasking_Check_Error is
        W : Worker;
    begin
        Put_Line ("W.Start...");
        W.Start;
        Put_Line ("Finished");
    end Show_Tasking_Check_Error;

In this example, the body of :ada:`Worker` doesn't have an :ada:`accept`.
Therefore, no rendezvous can happen for the :ada:`W.Start` call. Since the
task eventually terminates (as you can see in the user messages), the call
to :ada:`Start` constitutes a call to a terminated task. This condition is
checked by the :ada:`Tasking_Check`, which fails in this case, thereby
raising a :ada:`Tasking_Error`.




``Ada.Exceptions`` package
--------------------------

.. note::

    Parts of this section were originally published as
    `Gem #142 : Exception-ally <https://www.adacore.com/gems/gem-142-exceptions>`_

The standard Ada run-time library provides the package :ada:`Ada.Exceptions`.
This package provides a number of services to help analyze exceptions.

Each exception is associated with a (short) message that can be set by the code
that raises the exception, as in the following code:

.. code-block:: ada

    raise Constraint_Error with "some message";

.. admonition:: Historically

    Since Ada 2005, we can use the
    :ada:`raise Constraint_Error with "some message"` syntax.
    In Ada 95, you had to call the :ada:`Raise_Exception` procedure:

    .. code-block:: ada

        Ada.Exceptions.Raise_Exception         --  Ada 95
          (Constraint_Error'Identity, "some message");

    In Ada 83, there was no way to do it at all.

    The new syntax is now very convenient, and developers should be encouraged
    to provide as much information as possible along with the exception.

.. admonition:: In the GNAT toolchain

    The length of the message is limited to 200 characters by default in GNAT,
    and messages longer than that will be truncated.

.. admonition:: In the Ada Reference Manual

    - :arm22:`11.4.1 The Package Exceptions <11-4-1>`


Retrieving exception information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Exceptions also embed information set by the run-time itself that can be
retrieved by calling the :ada:`Exception_Information` function. The function
:ada:`Exception_Information` also displays the :ada:`Exception_Message`.

For example:

.. code-block:: ada

    exception
       when E : others =>
         Put_Line
           (Ada.Exceptions.Exception_Information (E));

.. admonition:: In the GNAT toolchain

    In the case of GNAT, the information provided by an exception might include
    the source location where the exception was raised and a nonsymbolic
    traceback.

You can also retrieve this information individually. Here, you can use:

    - the :ada:`Exception_Name` functions |mdash| and its derivatives
      :ada:`Wide_Exception_Name` and :ada:`Wide_Wide_Exception_Name` |mdash| to
      retrieve the name of an exception.

    - the :ada:`Exception_Message` function to retrieve the message associated
      with an exception.

Let's see a complete example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exceptions_Package.Exception_Info switches=Compiler(-g);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Show_Exception_Info is

       Custom_Exception : exception;

       procedure Nested is
       begin
          raise Custom_Exception
            with "We got a problem";
       end Nested;

    begin
       Nested;

    exception
       when E : others =>
          Put_Line ("Exception info: "
                    & Exception_Information (E));
          Put_Line ("Exception name: "
                    & Exception_Name (E));
          Put_Line ("Exception msg:  "
                    & Exception_Message (E));
    end Show_Exception_Info;


Collecting exceptions
~~~~~~~~~~~~~~~~~~~~~

:ada:`Save_Occurrence`
^^^^^^^^^^^^^^^^^^^^^^

You can save an exception occurrence using the :ada:`Save_Occurrence` procedure.
(Note that a :ada:`Save_Occurrence` function exists as well.)

For example, the following application collects exceptions into a list and
displays them after running the :ada:`Test_Exceptions` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exceptions_Package.Save_Occurrence switches=Compiler(-g);

    with Ada.Exceptions; use Ada.Exceptions;

    package Exception_Tests is

       Custom_Exception : exception;

       type All_Exception_Occur is
         array (Positive range <>) of
           Exception_Occurrence;

       procedure Test_Exceptions
         (All_Occur  : in out All_Exception_Occur;
          Last_Occur :    out Integer);

    end Exception_Tests;

    package body Exception_Tests is

       procedure Save_To_List
         (E          :        Exception_Occurrence;
          All_Occur  : in out All_Exception_Occur;
          Last_Occur : in out Integer)
       is
          L : Integer renames Last_Occur;
          O : All_Exception_Occur renames All_Occur;
       begin
          L := L + 1;
          if L > O'Last then
             raise Constraint_Error
               with "Cannot save occurrence";
          end if;

          Save_Occurrence (Target => O (L),
                           Source => E);
       end Save_To_List;

       procedure Test_Exceptions
         (All_Occur  : in out All_Exception_Occur;
          Last_Occur :    out Integer)
       is

          procedure Nested_1 is
          begin
             raise Custom_Exception
               with "We got a problem";
          exception
             when E : others =>
                Save_To_List (E,
                              All_Occur,
                              Last_Occur);
          end Nested_1;

          procedure Nested_2 is
          begin
             raise Constraint_Error
               with "Constraint is not correct";
          exception
             when E : others =>
                Save_To_List (E,
                              All_Occur,
                              Last_Occur);
          end Nested_2;

       begin
          Last_Occur := 0;

          Nested_1;
          Nested_2;
       end Test_Exceptions;

    end Exception_Tests;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    with Exception_Tests; use Exception_Tests;

    procedure Show_Exception_Info is
       L : Integer;
       O : All_Exception_Occur (1 .. 10);
    begin
       Test_Exceptions (O, L);

       for I in O 'First .. L loop
          Put_Line (Exception_Information (O (I)));
       end loop;
    end Show_Exception_Info;

In the :ada:`Save_To_List` procedure of the :ada:`Exception_Tests` package, we
call the :ada:`Save_Occurrence` procedure to store the exception occurrence to
the :ada:`All_Occur` array. In the :ada:`Show_Exception_Info`, we display all
the exception occurrences that we collected.

:ada:`Read` and :ada:`Write` attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similarly, we can use files to read and write exception occurrences. To do that,
we can simply use the :ada:`Read` and :ada:`Write` attributes.

.. code:: ada no_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exceptions_Package.Exception_Occurrence_Stream
    :class: ada-run

    with Ada.Text_IO;

    with Ada.Streams.Stream_IO;
    use  Ada.Streams.Stream_IO;

    with Ada.Exceptions;
    use  Ada.Exceptions;

    procedure Exception_Occurrence_Stream is

       Custom_Exception : exception;

       S : Stream_Access;

       procedure Nested_1 is
       begin
          raise Custom_Exception
            with "We got a problem";
       exception
          when E : others =>
             Exception_Occurrence'Write (S, E);
       end Nested_1;

       procedure Nested_2 is
       begin
          raise Constraint_Error
            with "Constraint is not correct";
       exception
          when E : others =>
             Exception_Occurrence'Write (S, E);
       end Nested_2;

       F         : File_Type;
       File_Name : constant String :=
                     "exceptions_file.bin";
    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Nested_1;
       Nested_2;

       Close (F);

       Read_Exceptions : declare
          E : Exception_Occurrence;
       begin
          Open (F, In_File, File_Name);
          S := Stream (F);

          while not End_Of_File (F) loop
             Exception_Occurrence'Read (S, E);

             Ada.Text_IO.Put_Line
               (Exception_Information (E));
          end loop;
          Close (F);
       end Read_Exceptions;

    end Exception_Occurrence_Stream;

In this example, we store the exceptions raised in the application in the
`exceptions_file.bin` file. In the exception part of procedures :ada:`Nested_1`
and :ada:`Nested_2`, we call :ada:`Exception_Occurrence'Write` to store an
exception occurence in the file. In the :ada:`Read_Exceptions` block, we read
the exceptions from the the file by calling :ada:`Exception_Occurrence'Read`.


Debugging exceptions in the GNAT toolchain
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is a typical exception handler that catches all unexpected exceptions in
the application:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exceptions_Package.Exception_Information switches=Compiler(-g);

    with Ada.Exceptions;
    with Ada.Text_IO;   use Ada.Text_IO;

    procedure Main is

       procedure Nested is
       begin
          raise Constraint_Error
                  with "some message";
       end Nested;

    begin
       Nested;

    exception
       when E : others =>
          Put_Line
           (Ada.Exceptions.Exception_Information (E));
    end Main;

The output we get when running the application is not very informative. To get
more information, we need to rerun the program in the debugger. To make the
session more interesting though, we should add debug information in the
executable, which means using the ``-g`` switch in the
:program:`gnatmake` command.

The session would look like the following (omitting some of the output from the
debugger):

.. code-block:: none

    > rm *.o      # Cleanup previous compilation
    > gnatmake -g main.adb
    > gdb ./main
    (gdb)  catch exception
    (gdb)  run
    Catchpoint 1, CONSTRAINT_ERROR at 0x0000000000402860 in main.nested () at main.adb:8
    8               raise Constraint_Error with "some message";

    (gdb) bt
    #0  <__gnat_debug_raise_exception> (e=0x62ec40 <constraint_error>) at s-excdeb.adb:43
    #1  0x000000000040426f in ada.exceptions.complete_occurrence (x=x@entry=0x637050)
    at a-except.adb:934
    #2  0x000000000040427b in ada.exceptions.complete_and_propagate_occurrence (
    x=x@entry=0x637050) at a-except.adb:943
    #3  0x00000000004042d0 in <__gnat_raise_exception> (e=0x62ec40 <constraint_error>,
    message=...) at a-except.adb:982
    #4  0x0000000000402860 in main.nested ()
    #5  0x000000000040287c in main ()

And we now know exactly where the exception was raised. But in fact, we could
have this information directly when running the application. For this, we need
to bind the application with the switch ``-E``, which tells the
binder to store exception tracebacks in exception occurrences. Let's recompile
and rerun the application.

.. code-block:: none

    > rm *.o   # Cleanup previous compilation
    > gnatmake -g main.adb -bargs -E
    > ./main

    Exception name: CONSTRAINT_ERROR
    Message: some message
    Call stack traceback locations:
    0x10b7e24d1 0x10b7e24ee 0x10b7e2472

The traceback, as is, is not very useful. We now need to use another tool that
is bundled with GNAT, called :program:`addr2line`. Here is an example of its
use:

.. code-block:: none

    > addr2line -e main --functions --demangle 0x10b7e24d1 0x10b7e24ee 0x10b7e2472
    /path/main.adb:8
    _ada_main
    /path/main.adb:12
    main
    /path/b~main.adb:240

This time we do have a symbolic backtrace, which shows information similar to
what we got in the debugger.

For users on OSX machines, :program:`addr2line` does not exist. On these
machines, however, an equivalent solution exists. You need to link your
application with an additional switch, and then use the tool :program:`atos`,
as in:

.. code-block:: none

    > rm *.o
    > gnatmake -g main.adb -bargs -E -largs -Wl,-no_pie
    > ./main

    Exception name: CONSTRAINT_ERROR
    Message: some message
    Call stack traceback locations:
    0x1000014d1 0x1000014ee 0x100001472
    > atos -o main 0x1000014d1 0x1000014ee 0x100001472
    main__nested.2550 (in main) (main.adb:8)
    _ada_main (in main) (main.adb:12)
    main (in main) + 90

We will now discuss a relatively new switch of the compiler, namely
``-gnateE``. When used, this switch will generate extra
information in exception messages.

Let's amend our test program to:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exceptions_Package.Exception_Information switches=Compiler(-g,-gnateE);

    with Ada.Exceptions;
    with Ada.Text_IO;      use Ada.Text_IO;

    procedure Main is

       procedure Nested (Index : Integer) is
          type T_Array is array (1 .. 2) of Integer;
          T : constant T_Array := (10, 20);
       begin
          Put_Line (T (Index)'Img);
       end Nested;

    begin
       Nested (3);

    exception
       when E : others =>
          Put_Line
           (Ada.Exceptions.Exception_Information (E));
    end Main;

When running the application, we see that the exception information (traceback)
is the same as before, but this time the exception message is set automatically
by the compiler. So we know we got a :ada:`Constraint_Error` because an
incorrect index was used at the named source location
(:file:`main.adb`, line 10). But the significant addition is the second
line of the message, which indicates exactly the cause of the error. Here, we
wanted to get the element at index 3, in an array whose range of valid indexes
is from 1 to 2. (No need for a debugger in this case.)

The column information on the first line of the exception message is also very
useful when dealing with null pointers. For instance, a line such as:

.. code-block:: ada

    A := Rec1.Rec2.Rec3.Rec4.all;

where each of the :ada:`Rec` is itself a pointer, might raise
:ada:`Constraint_Error` with a message "access check failed". This indicates for
sure that one of the pointers is null, and by using the column information it is
generally easy to find out which one it is.


Exception renaming
------------------

We can rename exceptions by using the an exception renaming declaration in this
form :ada:`Renamed_Exception : exception renames Existing_Exception;`. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exception_Renaming.Exception_Renaming
    :class: ada-run-expect-failure

    procedure Show_Exception_Renaming is
       CE : exception renames Constraint_Error;
    begin
       raise CE;
    end Show_Exception_Renaming;

Exception renaming creates a new view of the original exception. If we rename an
exception from package :ada:`A` in package :ada:`B`, that exception will become
visible in package :ada:`B`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Exception_Renaming.Exception_Renaming_View switches=Compiler(-g,-gnateE);

    package Internal_Exceptions is

       Int_E : exception;

    end Internal_Exceptions;

    with Internal_Exceptions;

    package Test_Constraints is

       Ext_E : exception renames
                 Internal_Exceptions.Int_E;

    end Test_Constraints;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    with Test_Constraints; use Test_Constraints;

    procedure Show_Exception_Renaming_View is
    begin
       raise Ext_E;
    exception
       when E : others =>
          Put_Line
           (Ada.Exceptions.Exception_Information (E));
    end Show_Exception_Renaming_View;

Here, we're renaming the :ada:`Int_E` exception in the :ada:`Test_Constraints`
package. The :ada:`Int_E` exception isn't directly visible in the
:ada:`Show_Exception_Renaming` procedure because we're not :ada:`with`\ing the
:ada:`Internal_Exceptions` package. However, it is indirectly visible
in that procedure via the renaming (:ada:`Ext_E`) in the :ada:`Test_Constraints`
package.

.. admonition:: In the Ada Reference Manual

    - :arm22:`8.5.2 Exception Renaming Declarations <8-5-2>`


Out and Uninitialized
---------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #150: Out and Uninitialized <https://www.adacore.com/gems/gem-150out-and-uninitialized>`_

Perhaps surprisingly, the Ada standard indicates cases where objects passed to
:ada:`out` and :ada:`in out` parameters might not be updated when a procedure
terminates due to an exception. Let's take an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Out_Uninitialized.Out_Uninitialized_1

    with Ada.Text_IO;  use Ada.Text_IO;
    procedure Show_Out_Uninitialized is

       procedure Local (A     : in out Integer;
                        Error : Boolean) is
       begin
          A := 1;

          if Error then
             raise Program_Error;
          end if;
       end Local;

       B : Integer := 0;

    begin
       Local (B, Error => True);
    exception
       when Program_Error =>
          Put_Line ("Value for B is"
                    & Integer'Image (B));  --  "0"
    end Show_Out_Uninitialized;

This program outputs a value of 0 for :ada:`B`, whereas the code indicates that
:ada:`A` is assigned before raising the exception, and so the reader might
expect :ada:`B` to also be updated.

The catch, though, is that a compiler must by default pass objects of
elementary types (scalars and access types) by copy and might choose to do so
for other types (records, for example), including when passing :ada:`out` and
:ada:`in out` parameters. So what happens is that while the formal parameter
:ada:`A` is properly initialized, the exception is raised before the new value
of :ada:`A` has been copied back into :ada:`B` (the copy will only happen on a
normal return).

.. admonition:: In the GNAT toolchain

    In general, any code that reads the actual object passed to an :ada:`out` or
    :ada:`in out` parameter after an exception is suspect and should be avoided.
    GNAT has useful warnings here, so that if we simplify the above code to:

    .. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Out_Uninitialized.Out_Uninitialized_2

        with Ada.Text_IO;  use Ada.Text_IO;

        procedure Show_Out_Uninitialized_Warnings is

            procedure Local (A : in out Integer) is
            begin
               A := 1;
               raise Program_Error;
            end Local;

           B : Integer := 0;

        begin
           Local (B);
        exception
           when others =>
              Put_Line ("Value for B is"
                        & Integer'Image (B));
        end Show_Out_Uninitialized_Warnings;

    We now get a compilation warning that the pass-by-copy formal may have no
    effect.

    Of course, GNAT is not able to point out all such errors (see first example
    above), which in general would require full flow analysis.

The behavior is different when using parameter types that the standard mandates
be passed by reference, such as tagged types for instance. So the following
code will work as expected, updating the actual parameter despite the
exception:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Out_Uninitialized.Out_Uninitialized_3

    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Show_Out_Initialized_Rec is

       type Rec is tagged record
          Field : Integer;
       end record;

       procedure Local (A : in out Rec) is
       begin
          A.Field := 1;
          raise Program_Error;
       end Local;

       V : Rec;

    begin
       V.Field := 0;
       Local (V);
    exception
       when others =>
          Put_Line ("Value of Field is"
                    & V.Field'Img); -- "1"
    end Show_Out_Initialized_Rec;

.. admonition:: In the GNAT toolchain

    It's worth mentioning that GNAT provides a pragma called
    :ada:`Export_Procedure` that forces reference semantics on :ada:`out`
    parameters. Use of this pragma would ensure updates of the actual parameter
    prior to abnormal completion of the procedure. However, this pragma only
    applies to library-level procedures, so the examples above have to be
    rewritten to avoid the use of a nested procedure, and really this pragma is
    intended mainly for use in interfacing with foreign code. The code below
    shows an example that ensures that :ada:`B` is set to 1 after the call to
    :ada:`Local`:

    .. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Out_Uninitialized.Out_Uninitialized_4

        package Exported_Procedures is

          procedure Local (A     : in out Integer;
                           Error : Boolean);
          pragma Export_Procedure
            (Local,
            Mechanism => (A => Reference));

        end Exported_Procedures;

        package body Exported_Procedures is

           procedure Local (A     : in out Integer;
                            Error : Boolean) is
           begin A := 1;
              if Error then
                 raise Program_Error;
              end if;
           end Local;

        end Exported_Procedures;

        with Ada.Text_IO; use Ada.Text_IO;

        with Exported_Procedures;
        use  Exported_Procedures;

        procedure Show_Out_Reference is
           B : Integer := 0;
        begin
           Local (B, Error => True);
        exception
           when Program_Error =>
              Put_Line ("Value for B is"
                        & Integer'Image (B)); -- "1"
        end Show_Out_Reference;

In the case of direct assignments to global variables, the behavior in the
presence of exceptions is somewhat different. For predefined exceptions, most
notably :ada:`Constraint_Error`, the optimization permissions allow some
flexibility in whether a global variable is or is not updated when an exception
occurs (see :arm22:`Ada RM 11.6 <11-6>`). For
instance, the following code makes an incorrect assumption:

.. code-block:: none

    X := 0;     -- about to try addition
    Y := Y + 1; -- see if addition raises exception
    X := 1      -- addition succeeded

A program is not justified in assuming that :ada:`X = 0` if the addition raises
an exception (assuming :ada:`X` is a global here). So any such assumptions in a
program are incorrect code which should be fixed.

.. admonition:: In the Ada Reference Manual

    - :arm22:`11.6 Exceptions and Optimization <11-6>`


Suppressing checks
------------------

:ada:`pragma Suppress`
~~~~~~~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Gary Dismukes and published as
    `Gem #63: The Effect of Pragma Suppress <https://www.adacore.com/gems/gem-63>`_.

One of Ada's key strengths has always been its strong typing. The language
imposes stringent checking of type and subtype properties to help prevent
accidental violations of the type system that are a common source of program
bugs in other less-strict languages such as C. This is done using a combination
of compile-time restrictions (legality rules), that prohibit mixing values of
different types, together with run-time checks to catch violations of various
dynamic properties. Examples are checking values against subtype constraints
and preventing dereferences of null access values.

At the same time, Ada does provide certain "loophole" features, such as
:ada:`Unchecked_Conversion`, that allow selective bypassing of the normal
safety features, which is sometimes necessary when interfacing with hardware or
code written in other languages.

Ada also permits explicit suppression of the run-time checks that are there to
ensure that various properties of objects are not violated. This suppression
can be done using :ada:`pragma Suppress`, as well as by using a compile-time
switch on most implementations |mdash| in the case of GNAT, with the ``-gnatp``
switch.

In addition to allowing all checks to be suppressed, :ada:`pragma Suppress`
supports suppression of specific forms of check, such as :ada:`Index_Check` for
array indexing, :ada:`Range_Check` for scalar bounds checking, and
:ada:`Access_Check` for dereferencing of access values. (See section 11.5 of
the Ada Reference Manual for further details.)

Here's a simple example of suppressing index checks within a specific
subprogram:

.. code-block:: ada

   procedure Main is
      procedure Sort_Array (A : in out Some_Array) is
         pragma Suppress (Index_Check);
         --     ^^^^^^^^^^^^^^^^^^^^^
         --   eliminate check overhead
      begin
        ...
      end Sort_Array;
   end Main;

Unlike a feature such as :ada:`Unchecked_Conversion`, however, the purpose of
check suppression is not to enable programs to subvert the type system, though
many programmers seem to have that misconception.

What's important to understand about :ada:`pragma Suppress` is that it only
gives permission to the implementation to remove checks, but doesn't require
such elimination. The intention of :ada:`Suppress` is not to allow bypassing of
Ada semantics, but rather to improve efficiency, and the Ada Reference Manual
has a clear statement to that effect in the note in RM-11.5, paragraph 29:

    There is no guarantee that a suppressed check is actually removed; hence a
    :ada:`pragma Suppress` should be used only for efficiency reasons.

There is associated Implementation Advice that recommends that implementations
should minimize the code executed for checks that have been suppressed, but
it's still the responsibility of the programmer to ensure that the correct
functioning of the program doesn't depend on checks not being performed.

There are various reasons why a compiler might choose not to remove a check. On
some hardware, certain checks may be essentially free, such as null pointer
checks or arithmetic overflow, and it might be impractical or add extra cost to
suppress the check. Another example where it wouldn't make sense to remove
checks is for an operation implemented by a call to a run-time routine, where
the check might be only a small part of a more expensive operation done out of
line.

Furthermore, in many cases GNAT can determine at compile time that a given
run-time check is guaranteed to be violated. In such situations, it gives a
warning that an exception will be raised, and generates code specifically to
raise the exception. Here's an example:

.. code-block:: ada

    X : Integer range 1..10 := ...;

    ..

    if A > B then
       X := X + 1;
      ..
    end if;

For the assignment incrementing :ada:`X`, the compiler will normally generate
machine code equivalent to:

.. code-block:: ada

    Temp := X + 1;
    if Temp > 10 then
       raise Constraint_Error;
    end if;
    X := Temp;

If range checks are suppressed, then the compiler can just generate the
increment and assignment. However, if the compiler is able to somehow prove
that :ada:`X = 10` at this point, it will issue a warning, and replace the
entire assignment with simply:

.. code-block:: ada

    raise Constraint_Error;

even though checks are suppressed. This is appropriate, because

    1. we don't care about the efficiency of buggy code, and

    2. there is no "extra" cost to the check, because if we reach that point,
       the code will unconditionally fail.

One other important thing to note about checks and :ada:`pragma Suppress` is
this statement in the Ada RM (RM-11.5, paragraph 26):

    If a given check has been suppressed, and the corresponding error situation
    occurs, the execution of the program is erroneous.

In Ada, erroneous execution is a bad situation to be in, because it means that
the execution of your program could have arbitrary nasty effects, such as
unintended overwriting of memory. Note also that a program whose "correct"
execution somehow depends on a given check being suppressed might work as the
programmer expects, but could still fail when compiled with a different
compiler, or for a different target, or even with a newer version of the same
compiler. Other changes such as switching on optimization or making a change to
a totally unrelated part of the code could also cause the code to start
failing.

So it's definitely not wise to write code that relies on checks being removed.
In fact, it really only makes sense to suppress checks once there's good reason
to believe that the checks can't fail, as a result of testing or other
analysis. Otherwise, you're removing an important safety feature of Ada that's
intended to help catch bugs.

:ada:`pragma Unsuppress`
~~~~~~~~~~~~~~~~~~~~~~~~

We can use :ada:`pragma Unsuppress` to reverse the effect of a
:ada:`pragma Suppress`. While :ada:`pragma Suppress` gives permission to the
compiler to remove a specific check, :ada:`pragma Unsuppress` revokes that
permission.

Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Control_Flow.Exceptions.Pragma_Unsuppress.Pragma_Unsuppress
    :class: ada-run-expect-failure

    procedure Show_Index_Check is

       type Integer_Array is
         array (Positive range <>) of Integer;

       pragma Suppress (Index_Check);
       --  from now on, the compiler may
       --  eliminate index checks...

       function Unchecked_Value_Of
         (A : Integer_Array;
          I : Integer)
          return Integer
       is
          type Half_Integer_Array is new
            Integer_Array (A'First ..
                           A'First + A'Length / 2);

          A_2 : Half_Integer_Array := (others => 0);
       begin
          return A_2 (I);
       end Unchecked_Value_Of;

       pragma Unsuppress (Index_Check);
       --  from now on, index checks are
       --  typically performed...

       function Value_Of
         (A : Integer_Array;
          I : Integer)
          return Integer
       is
          type Half_Integer_Array is new
            Integer_Array (A'First ..
                           A'First + A'Length / 2);

          A_2 : Half_Integer_Array := (others => 0);
       begin
          return A_2 (I);
       end Value_Of;

       Arr_1 : Integer_Array (1 .. 10) :=
                 (others => 1);

    begin
       Arr_1 (10) := Unchecked_Value_Of (Arr_1, 10);
       Arr_1 (10) := Value_Of (Arr_1, 10);

    end Show_Index_Check;

In this example, we first use a :ada:`pragma Suppress (Index_Check)`, so the
compiler is allowed to remove the index check from the
:ada:`Unchecked_Value_Of` function. (Therefore, depending on the compiler, the
call to the :ada:`Unchecked_Value_Of` function may complete without raising an
exception.) Of course, in this specific example, suppressing the index check
masks a severe issue.

In contrast, an index check is performed in the :ada:`Value_Of` function
because of the :ada:`pragma Unsuppress`. As a result, the index checks fails in
the call to this function, which raises a :ada:`Constraint_Error` exception.


.. admonition:: In the Ada Reference Manual

    - :arm22:`11.5 Suppressing Checks <11-5>`
