Exceptions
==========

.. include:: ../../global.txt

Asserts
-------

When we want to indicate a condition in the code that must always be valid, we
can use the pragma :ada:`Assert`. As the name implies, when we use this pragma,
we're *asserting* some truth about the source-code. (We can also use the
procedural form, as we'll see later.)

.. admonition:: Important

    Another method to assert the truth about the source-code is to use
    :doc:`pre and post-conditions <./contracts>`.

A simple assert has this form:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assert_1

    procedure Show_Pragma_Assert is
       I : constant Integer := 10;

       pragma Assert (I = 10);
    begin
       null;
    end Show_Pragma_Assert;

In this example, we're asserting that the value of :ada:`I` is always 10. We
could also display a message if the assertion is false:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assert_2
    :class: ada-run-expect-failure

    procedure Show_Pragma_Assert is
       I : constant Integer := 11;

       pragma Assert (I = 10, "I is not 10");
    begin
       null;
    end Show_Pragma_Assert;

Similarly, we can use the procedural form of :ada:`Assert`. For example, the
code above can implemented as follows:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Procedure_Assert
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Assertion_Error
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

    - `11.4.2 Pragmas Assert and Assertion_Policy <http://www.ada-auth.org/standards/12rm/html/RM-11-4-2.html>`_


Assertion policies
------------------

We can activate and deactivate assertions based on assertion policies. We can do
that by using the pragma :ada:`Assertion_Policy`. As an argument to this pragma,
we indicate whether a specific policy must be checked or ignored.

For example, we can deactivate assertion checks by specifying
:ada:`Assert => Ignore`. Similarly, we can activate assertion checks by
specifying :ada:`Assert => Check`. Let's see a code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Assertion_Policy_1

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
    `GNAT Reference Manual <https://gcc.gnu.org/onlinedocs/gnat_rm/Pragma-Assertion_005fPolicy.html>`_.

You can specify multiple policies in a single call to :ada:`Assertion_Policy`.
For example, you can activate all policies by writing:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Multiple_Assertion_Policies

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
    `configuration pragmas files <https://gcc.gnu.org/onlinedocs/gnat_ugn/The-Configuration-Pragmas-Files.html#The-Configuration-Pragmas-Files>`_.
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Assert_Procedure_Policy
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

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    :ada:`Default_Initial_Condition` policy

.. admonition:: In the Ada Reference Manual

    - `11.4.2 Pragmas Assert and Assertion_Policy <http://www.ada-auth.org/standards/12rm/html/RM-11-4-2.html>`_

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
| :ada:`Storage_Check`        | :ada:`Storage_Error`    |
+-----------------------------+-------------------------+

In addition, we can use :ada:`All_Checks` to refer to all those checks above at
once.

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    :ada:`Program_Error_Check` and :ada:`Tasking_Check`

Let's discuss each check and see code examples where those checks are
performed. Note that all examples are erroneous, so please avoid reusing them
elsewhere.

Access Check
~~~~~~~~~~~~

As you know, an object of an access type might be null. It would be an error to
dereference this object, as it doesn't indicate a valid position in memory.
Therefore, the access check verifies that an access object is not null when
dereferencing it. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Access_Check
    :class: ada-run-expect-failure

    procedure Show_Access_Check is

       type Access_Integer is access all Integer;

       AI : Access_Integer;
    begin
       AI.all := 10;
    end Show_Access_Check;

Here, the value of :ada:`AI` is null by default, so we cannot dereference it.

The access check also performs this verification when assigning to a subtype
that excludes null (:ada:`not null access`). For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Access_Check_2
    :class: ada-run-expect-failure

    procedure Show_Access_Check is

       type Access_Integer is access all Integer;
       type Non_Null_Access_Integer is not null access all Integer;

       AI   : Access_Integer;
       NNAI : Non_Null_Access_Integer := new Integer;

    begin
       NNAI := Non_Null_Access_Integer (AI);
    end Show_Access_Check;

Here, the value of :ada:`AI` is null (by default), so we cannot assign it to
:ada:`NNAI` because its type excludes null.

Note that, if we remove the :ada:`:= new Integer` assignment from the
declaration of :ada:`NNAI`, the null exclusion fails in the declaration
itself (because the default value of the access type is :ada:`null`).


Discriminant Check
~~~~~~~~~~~~~~~~~~

As we've seen earlier, a variant record is a record with discriminants that
allows for changing its structure. In operations such as an assignment, it's
important to ensure that the discriminants of the objects match |mdash| i.e. to
ensure that the structure of the objects matches. The discriminant check
verifies whether this is the case. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Discriminant_Check
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Discriminant_Check_2
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Division_Check
    :class: ada-run-expect-failure

    package Ops is
       function Div_Op (A, B : Integer) return Integer is
         (A / B);

       function Rem_Op (A, B : Integer) return Integer is
         (A rem B);

       function Mod_Op (A, B : Integer) return Integer is
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Index_Check
    :class: ada-run-expect-failure

    procedure Show_Index_Check is

       type Integer_Array is array (Positive range <>)
         of Integer;

       function Value_Of (A : Integer_Array;
                     I : Integer) return Integer is
          type Half_Integer_Array is new
            Integer_Array (A'First ..
                           A'First + A'Length / 2);

          A_2 : Half_Integer_Array := (others => 0);
       begin
          return A_2 (I);
       end Value_Of;

       Arr_1 : Integer_Array (1 .. 10)  := (others => 1);

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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Length_Check
    :class: ada-run-expect-failure

    procedure Show_Length_Check is

       type Integer_Array is array (Positive range <>)
         of Integer;

       procedure Assign (To   : out Integer_Array;
                         From :     Integer_Array) is
       begin
          To := From;
       end Assign;

       Arr_1 : Integer_Array (1 .. 10);
       Arr_2 : Integer_Array (1 .. 9) := (others => 1);

    begin
       Assign (Arr_1, Arr_2);
    end Show_Length_Check;

Here, the length of :ada:`Arr_1` is 10, while the length of :ada:`Arr_2` is 9,
so we cannot assign :ada:`Arr_2` (:ada:`From` parameter) to :ada:`Arr_1`
(:ada:`To` parameter) in the :ada:`Assign` procedure.

Overflow Check
~~~~~~~~~~~~~~

Operations on scalar objects might lead to overflow, which, if not checked,
lead to wrong information being computed and stored. Therefore, an overflow
check verifies that the value of a scalar object is within the base range of
its type. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Overflow_Check
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Range_Check
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Tag_Check
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


Accessibility Check
~~~~~~~~~~~~~~~~~~~

The accessibility check verifies that the accessibility level of an entity
matches the expected level.

.. todo::

    Add link to "Accessibility levels" section once it's available.

Let's look at an example that mixes access types and anonymous access types.
Here, we use an anonymous access type in the declaration of :ada:`A1` and a
named access type in the declaration of :ada:`A2`:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Accessibility_Check
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Accessibility_Check
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


Allocation Check
~~~~~~~~~~~~~~~~

The allocation check ensures, when a task is about to be created, that its
master has not been completed or the finalization has not been started.

This is an example adapted from
`AI-00280 <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00280.txt?rev=1.12&raw=N>`_:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Allocation_Check
    :class: ada-run-expect-failure

    with Ada.Finalization;
    with Ada.Unchecked_Deallocation;

    package P is
       type T1 is new Ada.Finalization.Controlled with null record;
       procedure Finalize (X : in out T1);

       type T2 is new Ada.Finalization.Controlled with null record;
       procedure Finalize (X : in out T2);

       X1 : T1;

       type T2_Ref is access T2;
       procedure Free is new Ada.Unchecked_Deallocation (T2, T2_Ref);
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
trying to create an object of :ada:`T2` type. This is forbidden and, therefore,
the allocation check raises a :ada:`Program_Error` exception.


Elaboration Check
~~~~~~~~~~~~~~~~~

The elaboration check verifies that subprograms |mdash| or protected entries,
or task activations |mdash| have been elaborated before being called.

This is an example adapted from
`AI-00064 <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00064.txt?rev=1.12&raw=N>`_:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Elaboration_Check
    :class: ada-run-expect-failure

    function P return Integer;

    function P return Integer is
    begin
       return 1;
    end P;

    with P;

    procedure Show_Elaboration_Check is

       function F return Integer;

       type Pointer_To_Func is access function return Integer;

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


Storage Check
~~~~~~~~~~~~~

The storage check ensures that the storage pool has enough space when
allocating memory. Let's revisit an example that we
:ref:`discussed earlier <Adv_Ada_Types_Storage_Size_Error>`:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Storage_Check
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


.. admonition:: In the Ada Reference Manual

    - `11.5 Suppressing Checks <http://www.ada-auth.org/standards/12rm/html/RM-11-5.html>`_


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

    - `11.4.1 The Package Exceptions <http://www.ada-auth.org/standards/12rm/html/RM-11-4-1.html>`_


Retrieving exception information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Exceptions also embed information set by the run-time itself that can be
retrieved by calling the :ada:`Exception_Information` function. The function
:ada:`Exception_Information` also displays the :ada:`Exception_Message`.

For example:

.. code-block:: ada

    exception
        when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));

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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Info switches=Compiler(-g);

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    procedure Show_Exception_Info is

       Custom_Exception : exception;

       procedure Nested is
       begin
          raise Custom_Exception with "We got a problem";
       end Nested;

    begin
       Nested;

    exception
       when E : others =>
          Put_Line ("Exception info: " & Exception_Information (E));
          Put_Line ("Exception name: " & Exception_Name (E));
          Put_Line ("Exception msg:  " & Exception_Message (E));
    end Show_Exception_Info;


Collecting exceptions
~~~~~~~~~~~~~~~~~~~~~

:ada:`Save_Occurrence`
^^^^^^^^^^^^^^^^^^^^^^

You can save an exception occurrence using the :ada:`Save_Occurrence` procedure.
(Note that a :ada:`Save_Occurrence` function exists as well.)

For example, the following application collects exceptions into a list and
displays them after running the :ada:`Test_Exceptions` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Save_Occurrence switches=Compiler(-g);

    with Ada.Exceptions; use Ada.Exceptions;

    package Exception_Tests is

       Custom_Exception : exception;

       type Exception_Occurrences is
         array (Positive range <>) of Exception_Occurrence;

       procedure Test_Exceptions
         (Occurrences    : in out Exception_Occurrences;
          Last_Occurence :    out Integer);

    end Exception_Tests;

    package body Exception_Tests is

       procedure Save_To_List (E              :        Exception_Occurrence;
                               Occurrences    : in out Exception_Occurrences;
                               Last_Occurence : in out Integer) is
          L : Integer renames Last_Occurence;
          O : Exception_Occurrences renames Occurrences;
       begin
          L := L + 1;
          if L > O'Last then
             raise Constraint_Error with  "Cannot save occurrence";
          end if;

          Save_Occurrence (Target => O (L),
                           Source => E);
       end Save_To_List;

       procedure Test_Exceptions
         (Occurrences    : in out Exception_Occurrences;
          Last_Occurence :    out Integer)
       is

          procedure Nested_1 is
          begin
             raise Custom_Exception with "We got a problem";
          exception
             when E : others =>
                Save_To_List (E, Occurrences, Last_Occurence);
          end Nested_1;

          procedure Nested_2 is
          begin
             raise Constraint_Error with "Constraint is not correct";
          exception
             when E : others =>
                Save_To_List (E, Occurrences, Last_Occurence);
          end Nested_2;

       begin
          Last_Occurence := 0;

          Nested_1;
          Nested_2;
       end Test_Exceptions;

    end Exception_Tests;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    with Exception_Tests; use Exception_Tests;

    procedure Show_Exception_Info is
       L : Integer;
       O : Exception_Occurrences (1 .. 10);
    begin
       Test_Exceptions (O, L);

       for I in O 'First .. L loop
          Put_Line (Exception_Information (O (I)));
       end loop;
    end Show_Exception_Info;

In the :ada:`Save_To_List` procedure of the :ada:`Exception_Tests` package, we
call the :ada:`Save_Occurrence` procedure to store the exception occurence to
the :ada:`Occurrences` array. In the :ada:`Show_Exception_Info`, we display all
the exception occurrences that we collected.

:ada:`Read` and :ada:`Write` attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similarly, we can use files to read and write exception occurences. To do that,
we can simply use the :ada:`'Read` and :ada:`'Write` attributes.

.. code:: ada no_button project=Courses.Advanced_Ada.Exceptions.Exception_Occurrence_Stream
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
    with Ada.Exceptions;        use Ada.Exceptions;

    procedure Exception_Occurrence_Stream is

       Custom_Exception : exception;

       S : Stream_Access;

       procedure Nested_1 is
       begin
          raise Custom_Exception with "We got a problem";
       exception
          when E : others =>
             Exception_Occurrence'Write (S, E);
       end Nested_1;

       procedure Nested_2 is
       begin
          raise Constraint_Error with "Constraint is not correct";
       exception
          when E : others =>
             Exception_Occurrence'Write (S, E);
       end Nested_2;

       F         : File_Type;
       File_Name : constant String := "exceptions_file.bin";
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
             Ada.Text_IO.Put_Line (Exception_Information (E));
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Information switches=Compiler(-g);

    with Ada.Exceptions;
    with Ada.Text_IO;   use Ada.Text_IO;

    procedure Main is

        procedure Nested is
        begin
            raise Constraint_Error with "some message";
        end Nested;

    begin
        Nested;

    exception
        when E : others =>
            Put_Line (Ada.Exceptions.Exception_Information (E));
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Information switches=Compiler(-g,-gnateE);

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
            Put_Line (Ada.Exceptions.Exception_Information (E));
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Renaming
    :class: ada-run-expect-failure

    procedure Show_Exception_Renaming is
       CE : exception renames Constraint_Error;
    begin
       raise CE;
    end Show_Exception_Renaming;

Exception renaming creates a new view of the original exception. If we rename an
exception from package :ada:`A` in package :ada:`B`, that exception will become
visible in package :ada:`B`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Exception_Renaming_View switches=Compiler(-g,-gnateE);

    package Internal_Exceptions is

       Int_E : exception;

    end Internal_Exceptions;

    with Internal_Exceptions;

    package Test_Constraints is

       Ext_E : exception renames Internal_Exceptions.Int_E;

    end Test_Constraints;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Exceptions; use Ada.Exceptions;

    with Test_Constraints; use Test_Constraints;

    procedure Show_Exception_Renaming_View is
    begin
       raise Ext_E;
    exception
       when E : others =>
          Put_Line (Ada.Exceptions.Exception_Information (E));
    end Show_Exception_Renaming_View;

Here, we're renaming the :ada:`Int_E` exception in the :ada:`Test_Constraints`
package. The :ada:`Int_E` exception isn't directly visible in the
:ada:`Show_Exception_Renaming` procedure because we're not :ada:`with`\ing the
:ada:`Internal_Exceptions` package. However, it is indirectly visible
in that procedure via the renaming (:ada:`Ext_E`) in the :ada:`Test_Constraints`
package.

.. admonition:: In the Ada Reference Manual

    - `8.5.2 Exception Renaming Declarations <http://www.ada-auth.org/standards/12rm/html/RM-8-5-2.html>`_


Out and Uninitialized
---------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #150: Out and Uninitialized <https://www.adacore.com/gems/gem-150out-and-uninitialized>`_

Perhaps surprisingly, the Ada standard indicates cases where objects passed to
:ada:`out` and :ada:`in out` parameters might not be updated when a procedure
terminates due to an exception. Let's take an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_1

    with Ada.Text_IO;  use Ada.Text_IO;
    procedure Show_Out_Uninitialized is

       procedure Local (A : in out Integer; Error : Boolean) is
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
          Put_Line ("Value for B is" & Integer'Image (B));  --  "0"
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

    .. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_2

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
              Put_Line ("Value for B is" & Integer'Image (B));
        end Show_Out_Uninitialized_Warnings;

    We now get a compilation warning that the pass-by-copy formal may have no
    effect.

    Of course, GNAT is not able to point out all such errors (see first example
    above), which in general would require full flow analysis.

The behavior is different when using parameter types that the standard mandates
be passed by reference, such as tagged types for instance. So the following
code will work as expected, updating the actual parameter despite the
exception:

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_3

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
       when others => Put_Line ("Value of Field is" & V.Field'Img); -- "1"
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

    .. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Out_Uninitialized_4

        package Exported_Procedures is

          procedure Local (A : in out Integer; Error : Boolean);
          pragma Export_Procedure (Local, Mechanism => (A => Reference));

        end Exported_Procedures;

        package body Exported_Procedures is

           procedure Local (A : in out Integer; Error : Boolean) is
           begin A := 1;
              if Error then
                 raise Program_Error;
              end if;
           end Local;

        end Exported_Procedures;

        with Ada.Text_IO;         use Ada.Text_IO;
        with Exported_Procedures; use Exported_Procedures;

        procedure Show_Out_Reference is
           B : Integer := 0;
        begin
           Local (B, Error => True);
        exception
           when Program_Error =>
              Put_Line ("Value for B is" & Integer'Image (B)); -- "1"
        end Show_Out_Reference;

In the case of direct assignments to global variables, the behavior in the
presence of exceptions is somewhat different. For predefined exceptions, most
notably :ada:`Constraint_Error`, the optimization permissions allow some
flexibility in whether a global variable is or is not updated when an exception
occurs (see
`Ada RM 11.6 <http://www.ada-auth.org/standards/12rm/html/RM-11-6.html>`_). For
instance, the following code makes an incorrect assumption:

.. code-block:: none

    X := 0;     -- about to try addition
    Y := Y + 1; -- see if addition raises exception
    X := 1      -- addition succeeded

A program is not justified in assuming that :ada:`X = 0` if the addition raises
an exception (assuming :ada:`X` is a global here). So any such assumptions in a
program are incorrect code which should be fixed.

.. admonition:: In the Ada Reference Manual

    - `11.6 Exceptions and Optimization <http://www.ada-auth.org/standards/12rm/html/RM-11-6.html>`_


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
         pragma Suppress (Index_Check);  -- eliminate check overhead
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

.. code:: ada run_button project=Courses.Advanced_Ada.Exceptions.Pragma_Unsuppress
    :class: ada-run-expect-failure

    procedure Show_Index_Check is

       type Integer_Array is array (Positive range <>)
         of Integer;

       pragma Suppress (Index_Check);
       --  from now on, the compiler may
       --  eliminate index checks...

       function Unchecked_Value_Of (A : Integer_Array;
                     I : Integer) return Integer is
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

       function Value_Of (A : Integer_Array;
                     I : Integer) return Integer is
          type Half_Integer_Array is new
            Integer_Array (A'First ..
                           A'First + A'Length / 2);

          A_2 : Half_Integer_Array := (others => 0);
       begin
          return A_2 (I);
       end Value_Of;

       Arr_1 : Integer_Array (1 .. 10)  := (others => 1);

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

    - `11.5 Suppressing Checks <http://www.ada-auth.org/standards/12rm/html/RM-11-5.html>`_
