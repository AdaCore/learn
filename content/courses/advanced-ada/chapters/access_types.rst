Access Types
============

.. include:: ../../global.txt

Ragged arrays
-------------

Ragged arrays |mdash| also known as jagged arrays |mdash| are non-uniform,
multidimensional arrays. They can be useful to implement tables with varying
number of coefficients, as we discuss as an example in this section.

Uniform multidimensional arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider an algorithm that processes data based on coefficients that depends on
a selected quality level:

+------------------------+--------------+-----+-----+-----+-----+-----+
| Quality level          | Number of    |   #1|   #2|   #3|   #4|   #5|
|                        | coefficients |     |     |     |     |     |
+========================+==============+=====+=====+=====+=====+=====+
| Simplified             |            1 | 0.15|     |     |     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Better                 |            3 | 0.02| 0.16| 0.27|     |     |
+------------------------+--------------+-----+-----+-----+-----+-----+
| Best                   |            5 | 0.01| 0.08| 0.12| 0.20| 0.34|
+------------------------+--------------+-----+-----+-----+-----+-----+

(Note that this is just a bogus table with no real purpose, as we're not
trying to implement any actual algorithm.)

We can implement this table as a two-dimensional array (:ada:`Calc_Table`),
where each quality level has an associated array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

    private

       Calc_Table : constant array (Quality_Level, 1 .. 5) of Float :=
         (Simplified => (0.15, 0.00, 0.00, 0.00, 0.00),
          Better     => (0.02, 0.16, 0.27, 0.00, 0.00),
          Best       => (0.01, 0.08, 0.12, 0.20, 0.34));

       Last : constant array (Quality_Level) of Positive :=
         (Simplified => 1,
          Better     => 3,
          Best       => 5);

    end Data_Processing;

Note that, in this implementation, we have a separate table :ada:`Last` that
indicates the actual number of coefficients of each quality level.

Alternatively, we could use a record (:ada:`Table_Coefficient`) that stores the
number of coefficients and the actual coefficients:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is array (Positive range <>) of Float;

    private

       type Table_Coefficient is record
          Last : Positive;
          Coef : Data (1 .. 5);
       end record;

       Calc_Table : constant array (Quality_Level) of Table_Coefficient :=
         (Simplified => (1, (0.15, 0.00, 0.00, 0.00, 0.00)),
          Better     => (3, (0.02, 0.16, 0.27, 0.00, 0.00)),
          Best       => (5, (0.01, 0.08, 0.12, 0.20, 0.34)));

    end Data_Processing;

In this case, we have a unidimensional array where each component (of
:ada:`Table_Coefficient` type) contains an array (:ada:`Coef`) with the
coefficients.

This is an example of a :ada:`Process` procedure that references the
:ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Uniform_Table

    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in 1 .. Calc_Table (Q).Last loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Note that, to loop over the coefficients, we're using
:ada:`for J in 1 .. Calc_Table (Q).Last loop` instead of
:ada:`for J in Calc_Table (Q)'Range loop`. As we're trying to make a
non-uniform array fit in a uniform array, we cannot simply loop over all
elements using the :ada:`Range` attribute, but must be careful to use the
correct number of elements in the loop instead.

Also, note that :ada:`Calc_Table` has 15 coefficients in total. Out of those
coefficients, 6 coefficients (or 40 percent of the table) aren't being used.
Naturally, this is wasted memory space. We can improve this by using ragged
arrays.


Non-uniform multidimensional array
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ragged arrays are declared by using an access type to an array. By doing that,
each array can be declared with a different size, thereby creating a
non-uniform multidimensional array.

For example, we can declare a constant array :ada:`Table` as a ragged array:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Ragged_Array

    package Data_Processing is

       type Integer_Array is array (Positive range <>) of Integer;

    private

       type Integer_Array_Access is access constant Integer_Array;

       Table : constant array (1 .. 3) of Integer_Array_Access :=
         (1 => new Integer_Array'(1 => 15),
          2 => new Integer_Array'(1 => 12, 2 => 15, 3 => 20),
          3 => new Integer_Array'(1 => 12, 2 => 15, 3 => 20,
                                  4 => 20, 5 => 25, 6 => 30));

    end Data_Processing;

Here, each component of :ada:`Table` is an access to another array. As each
array is allocated via :ada:`new`, those arrays may have different sizes.

We can rewrite the example from the previous subsection using a ragged array
for the :ada:`Calc_Table`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Ragged_Table

    package Data_Processing is

       type Quality_Level is
         (Simplified, Better, Best);

       type Data is array (Positive range <>) of Float;

    private

       type Coefficients is access constant Data;

       Calc_Table : constant array (Quality_Level) of Coefficients :=
         (Simplified => new Data'(1 => 0.15),
          Better     => new Data'(0.02, 0.16, 0.27),
          Best       => new Data'(0.01, 0.08, 0.12, 0.20, 0.34));

    end Data_Processing;

Now, we aren't wasting memory space because each data component has the right
size that is required for each quality level. Also, we don't need to store the
number of coefficients, as this information is automatically available from the
array initialization (via the allocation of the :ada:`Data` array for the
:ada:`Coefficients` type).

This is the adapted :ada:`Process` procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Ragged_Table


    package Data_Processing.Operations is

      procedure Process (D : in out Data;
                         Q :        Quality_Level);

    end Data_Processing.Operations;

    package body Data_Processing.Operations is

       procedure Process (D : in out Data;
                          Q :        Quality_Level) is
       begin
          for I in D'Range loop
             for J in Calc_Table (Q)'Range loop
               --  ... * Calc_Table (Q).Coef (J)
               null;
             end loop;
             --  D (I) := ...
             null;
          end loop;
       end Process;

    end Data_Processing.Operations;

Now, we can simply loop over the coefficients by writing
:ada:`for J in Calc_Table (Q)'Range loop`, as each element of :ada:`Calc_Table`
automatically has the correct range.


Aliasing
--------

.. admonition:: Relevant topics

    - aliased objects
    - aliased parameters

.. todo::

    Complete section!


Dereferencing
-------------

.. todo::

    Complete section!


Implicit Dereferencing
----------------------

.. admonition:: Relevant topics

    - `User-Defined References <http://www.ada-auth.org/standards/2xrm/html/RM-4-1-5.html>`_

.. todo::

    Complete section!


Anonymous Access Types
----------------------

.. admonition:: In the Ada Reference Manual

    - `3.10 Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10.html>`_

.. todo::

    Complete section!


Accessibility Levels: An Introduction
-------------------------------------

.. admonition:: In the Ada Reference Manual

    - `3.10.2 Operations of Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10-2.html>`_

.. todo::

    Complete section!


Unchecked Access
----------------

.. admonition:: Relevant topics

    - `Unchecked Access Value Creation <http://www.ada-auth.org/standards/2xrm/html/RM-13-10.html>`_

.. todo::

    Complete section!


Unchecked Deallocation
----------------------

So far, we've seen multiple examples of using :ada:`new` to allocate objects.
In this section, we discuss how to manually deallocate objects.

Our starting point to manually deallocate an object is the generic
:ada:`Ada.Unchecked_Deallocation` procedure. We first instantiate this
procedure for an access type whose objects we want to be able to deallocate.
For example, let's instantiate it for the :ada:`Integer_Access` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       --
       --  Instantiation of Ada.Unchecked_Deallocation
       --  for the Integer_Access type:
       --
       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access);
    end Integer_Types;

Here, we declare the :ada:`Free` procedure, which we can then use to deallocate
objects that were allocated for the :ada:`Integer_Access` type.

:ada:`Ada.Unchecked_Deallocation` is a generic procedure that we can
instantiate for access types. When declaring an instance of
:ada:`Ada.Unchecked_Deallocation`, we have to specify arguments for:

- the formal :ada:`Object` parameter, which indicates the type of actual
  objects that we want to deallocate; and

- the formal :ada:`Name` parameter, which indicates the access type.

In a type declaration such as :ada:`type Integer_Access is access Integer`,
:ada:`Integer` denotes the :ada:`Object`, while :ada:`Integer_Access` denotes
the :ada:`Name`.

Because each instance of :ada:`Ada.Unchecked_Deallocation` is bound to a
specific access type, we cannot use it for another access type, even if the
type we use for the :ada:`Object` parameter is the same:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Integer_Access);

       type Another_Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Integer,
            Name   => Another_Integer_Access);
    end Integer_Types;

Here, we're declaring two :ada:`Free` procedures: one for the
:ada:`Integer_Access` type, another for the :ada:`Another_Integer_Access`. We
cannot use the :ada:`Free` procedure for the :ada:`Integer_Access` type when
deallocating objects associated with the :ada:`Another_Integer_Access` type,
even though both types are declared as :ada:`access Integer`.

Note that we can use any name when instantiating the
:ada:`Ada.Unchecked_Deallocation` procedure. However, naming it :ada:`Free` is
very common.

Now, let's see a complete example that includes object allocation and
deallocation:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation

    with Ada.Unchecked_Deallocation;

    package Integer_Types is

       type Integer_Access is access Integer;

       procedure Free is
         new Ada.Unchecked_Deallocation (Object => Integer,
                                         Name   => Integer_Access);

       procedure Show_Is_Null (I : Integer_Access);

    end Integer_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Types is

       procedure Show_Is_Null (I : Integer_Access) is
       begin
          if I = null then
             Put_Line ("access value is null.");
          else
             Put_Line ("access value is NOT null.");
          end if;
       end Show_Is_Null;

    end Integer_Types;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I : Integer_Access;

    begin
       Put ("We haven't called new yet... ");
       Show_Is_Null (I);

       Put ("Calling new... ");
       I := new Integer;
       Show_Is_Null (I);

       Put ("Calling Free... ");
       Free (I);
       Show_Is_Null (I);
    end Show_Unchecked_Deallocation;

In the :ada:`Show_Unchecked_Deallocation` procedure, we first allocate an
object for :ada:`I` and then call :ada:`Free (I)` to deallocate it. Also, we
call the :ada:`Show_Is_Null` procedure at three different points: before any
allocation takes place, after allocating an object for :ada:`I`, and after
deallocating that object.

When we deallocate an object via a call to :ada:`Free`, the corresponding
access value |mdash| which was previously pointing to an existing object
|mdash| is set to :ada:`null`. Therefore, :ada:`I = null` after the call to
:ada:`Free`, which is exactly what we see when running this example code.

Note that it is OK to call :ada:`Free` for an access value that has already
been deallocated:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I : Integer_Access;

    begin
       I := new Integer;

       Free (I);
       Free (I);
       Free (I);
    end Show_Unchecked_Deallocation;

The multiple calls to :ada:`Free` in this example don't cause any issues. In
fact, calling :ada:`Free` to an access value that is equal to :ada:`null` is
simply ignored.

.. admonition:: In the Ada Reference Manual

    - `4.8 Allocators <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-4-8.html>`__
    - `13.11.2 Unchecked Storage Deallocation <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-13-11-2.html>`__


Dangling References
~~~~~~~~~~~~~~~~~~~

An access value that points to a non-existent object is called a dangling
reference. Of course, we shouldn't try to dereference a dangling reference
because it causes the :ref:`access check <Adv_Ada_Access_Check>` to fail.

Let's reuse the last example and introduce :ada:`I_2`, which will point to the
same object as :ada:`I`:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation
    :class: ada-run-expect-failure

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I, I_2 : Integer_Access;

    begin
       I := new Integer;

       I_2 := I;

       --  NOTE: I_2 points to the same
       --        object as I.

       Free (I);

       --  NOTE: at this point, I_2 is a
       --        dangling reference!

       --  Calls to Free (I) are OK!

       Free (I);
       Free (I);

       --  A call to Free (I_2) is
       --  NOT OK:

       Free (I_2);
    end Show_Unchecked_Deallocation;

As we've seen before, we can have multiple calls to :ada:`Free (I)`. However,
the call to :ada:`I_2` fails because it points to an object that doesn't exist
anymore.

Because of these potential errors, it is the programmer's responsibility to be
very careful when using unchecked deallocation and avoid creating dangling
references.

For the example we've just seen, a better approach could be to explicitly
assign :ada:`null` to :ada:`I_2` to indicate that it doesn't point to any
specific object:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation

    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I, I_2 : Integer_Access;

    begin
       I := new Integer;

       I_2 := I;

       --  NOTE: I_2 points to the same
       --        object as I.

       Free (I);

       I_2 := null;

       --  NOTE: I_2 doesn't point to any
       --        object, so we can call Free.

       Free (I_2);
    end Show_Unchecked_Deallocation;

Now, calling :ada:`Free (I_2)` doesn't cause any issues because it doesn't
point to any object.


Dereferencing dangling references
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Of course, we shouldn't try to dereference a dangling reference. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation

    with Ada.Text_IO;   use Ada.Text_IO;
    with Integer_Types; use Integer_Types;

    procedure Show_Unchecked_Deallocation is

       I_1, I_2 : Integer_Access;

    begin
       I_1 := new Integer'(42);
       I_2 := I_1;

       Put_Line ("I_1 = " & Integer'Image (I_1.all));
       Put_Line ("I_2 = " & Integer'Image (I_2.all));

       Put_Line ("Freeing I_1");
       Free (I_1);

       if I_1 /= null then
          Put_Line ("I_1 = " & Integer'Image (I_1.all));
       end if;

       if I_2 /= null then
          Put_Line ("I_2 = " & Integer'Image (I_2.all));
       end if;
    end Show_Unchecked_Deallocation;

In this example, we allocate an object for :ada:`I_1` and make :ada:`I_2` point
to the same object. Note, however, that we call :ada:`Free` for :ada:`I_1`, but
not for :ada:`I_2`. Therefore, after that call to :ada:`Free`,
:ada:`I_1 = null`, but :ada:`I_2` still points to the object originally
allocated for :ada:`I_1`.

As mentioned previously, when deallocating an object via a call to :ada:`Free`,
the corresponding access value is set to :ada:`null`. In this case, we can
check for :ada:`null` to detect a dangling reference and avoid dereferencing
it.

The strategy of checking for :ada:`null` before trying to dereferencing the
access value works fine for :ada:`I_1`, but not for :ada:`I_2`, as we can see
when running the example above. In the case of :ada:`I_2`, the access value is
not :ada:`null`: :ada:`I_2` points to an object that doesn't exist anymore. As
expected, dereferencing :ada:`I_2` raises the :ada:`Constraint_Error`
exception.

Because of this potential errors, it is the programmer's responsibility to be
very careful when using unchecked deallocation and avoid creating dangling
reference.


Restrictions for :ada:`Ada.Unchecked_Deallocation`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two unsurprising restrictions for :ada:`Ada.Unchecked_Deallocation`:

1. It cannot be instantiated for access-to-constant types; and

2. It cannot be used when the :ada:`Storage_Size` aspect of a type is zero
   (i.e. when its storage pool is empty).

(Note that this last restriction also applies to the allocation via
:ada:`new`.)

Let's see an example of these restrictions:

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Unchecked_Deallocation_Error
    :class: ada-expect-compile-error

    with Ada.Unchecked_Deallocation;

    procedure Show_Unchecked_Deallocation_Errors is

       type Integer_Access_Zero is access Integer
         with Storage_Size => 0;

       procedure Free is
         new Ada.Unchecked_Deallocation (Object => Integer,
                                         Name   => Integer_Access_Zero);

       type Constant_Integer_Access is access constant Integer;

       --  ERROR: Cannot use access-to-constant type for Name
       procedure Free is
         new Ada.Unchecked_Deallocation (Object => Integer,
                                         Name   => Constant_Integer_Access);

       I : Integer_Access_Zero;

    begin
       --  ERROR: Cannot allocate objects from empty storage pool
       I := new Integer;

       --  ERROR: Cannot deallocate objects from empty storage pool
       Free (I);
    end Show_Unchecked_Deallocation_Errors;

Here, we see that trying to instantiate :ada:`Ada.Unchecked_Deallocation` for
the :ada:`Constant_Integer_Access` type is rejected by the compiler. Similarly,
we cannot allocate or deallocate an object for the :ada:`Integer_Access_Zero`
type because its storage pool is empty.


.. _Adv_Ada_Not_Null_Access:

Null & Not Null Access
----------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #23: Null Considered Harmful <https://www.adacore.com/gems/ada-gem-23>`_
    and `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`_.

Ada, like many languages, defines a special :ada:`null` value for access
types. All values of an access type designate some object of the
designated type, except for :ada:`null`, which does not designate any
object. The null value can be used as a special flag. For example, a
singly-linked list can be null-terminated. A :ada:`Lookup` function can
return :ada:`null` to mean "not found", presuming the result is of an
access type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Return
    :class: ada-syntax-only

    package Show_Null_Return is

       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.
    end Show_Null_Return;

An alternative design for :ada:`Lookup` would be to raise an exception:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Not_Found_Exception
    :class: ada-syntax-only

    package Show_Not_Found_Exception is
       Not_Found : exception;

       function Lookup (T : Table) return Ref_Element;
       --  Raises Not_Found if not found.
       --  Never returns null.
    end Show_Not_Found_Exception;

Neither design is better in all situations; it depends in part on whether
we consider the "not found" situation to be exceptional.

Clearly, the client calling :ada:`Lookup` needs to know whether it can
return :ada:`null`, and if so, what that means. In general, it's a good
idea to document whether things can be null or not, especially for formal
parameters and function results. Prior to Ada 2005, we would do that with
comments. Since Ada 2005, we can use the :ada:`not null` syntax:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Not_Null_Return
    :class: ada-syntax-only

    package Show_Not_Null_Return is
       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return not null Ref_Element;
       --  Possible since Ada 2005.
    end Show_Not_Null_Return;

This is a complete package for the code snippets above:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Complete_Null_Return

    package Example is

       type Element is limited private;
       type Ref_Element is access all Element;

       type Table is limited private;

       Not_Found : constant Ref_Element := null;
       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.

       Not_Found_2 : exception;
       function Lookup_2 (T : Table) return not null Ref_Element;
       --  Raises Not_Found_2 if not found.

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    private
       type Element is limited
          record
             Component : Integer;
          end record;
       type Table is limited null record;
    end Example;

    package body Example is

       An_Element : aliased Element;

       function Lookup (T : Table) return Ref_Element is
          pragma Unreferenced (T);
       begin
          --  ...
          return Not_Found;
       end Lookup;

       function Lookup_2 (T : Table) return not null Ref_Element is
       begin
          --  ...
          raise Not_Found_2;

          return An_Element'Access;
          --  suppress error: 'missing "return" statement in function body'
       end Lookup_2;

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

       procedure R is
       begin
          Q (An_Element'Access);
       end R;

      pragma Unreferenced (R);

    end Example;

In general, it's better to use the language proper for documentation, when
possible, rather than comments, because compile-time and/or run-time
checks can help ensure that the "documentation" is actually true. With
comments, there's a greater danger that the comment will become false
during maintenance, and false documentation is obviously a menace.

In many, perhaps most cases, :ada:`null` is just a tripping hazard. It's
a good idea to put in :ada:`not null` when possible. In fact, a good
argument can be made that :ada:`not null` should be the default, with
extra syntax required when :ada:`null` is wanted. This is the way
`Standard ML <https://en.wikipedia.org/wiki/Standard_ML>`_ works, for
example |mdash| you don't get any special null-like value unless you ask
for it. Of course, because Ada 2005 needs to be compatible with previous
versions of the language, :ada:`not null` cannot be the default for Ada.

One word of caution: access objects are default-initialized to
:ada:`null`, so if you have a :ada:`not null` object (or component) you
had better initialize it explicitly, or you will get
:ada:`Constraint_Error`. :ada:`not null` is more often useful on
parameters and function results, for this reason.

Another advantage of :ada:`not null` over comments is for efficiency.
Consider procedures :ada:`P` and :ada:`Q` in this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Complete_Null_Return

    package Example.Processing is

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    end Example.Processing;

    package body Example.Processing is

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

    end Example.Processing;

Without :ada:`not null`, the generated code for :ada:`P` will do a check
that :ada:`X /= null`, which may be costly on some systems. :ada:`P` is
called in a loop, so this check will likely occur many times. With
:ada:`not null`, the check is pushed to the call site. Pushing checks to
the call site is usually beneficial because

    1. the check might be hoisted out of a loop by the optimizer, or

    2. the check might be eliminated altogether, as in the example
       above, where the compiler knows that :ada:`An_Element'Access` cannot
       be :ada:`null`.

This is analogous to the situation with other run-time checks, such as
array bounds checks:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Process_Array

    package Show_Process_Array is

       type My_Index is range 1 .. 10;
       type My_Array is array (My_Index) of Integer;

       procedure Process_Array (X : in out My_Array; Index : My_Index);

    end Show_Process_Array;

    package body Show_Process_Array is

       procedure Process_Array (X : in out My_Array; Index : My_Index) is
       begin
          X (Index) := X (Index) + 1;
       end Process_Array;

    end Show_Process_Array;

If :ada:`X (Index)` occurs inside :ada:`Process_Array`, there is no need
to check that :ada:`Index` is in range, because the check is pushed to the
caller.


Access to subprograms
---------------------

Static vs. dynamic calls
~~~~~~~~~~~~~~~~~~~~~~~~

In a typical subprogram call, we indicate the subprogram we want to call
statically. For example, let's say we've implemented a procedure :ada:`Proc`
that calls a procedure :ada:`P`:

.. code:: ada compile_button main=proc.adb project=Courses.Advanced_Ada.Access_Types.Subprogram_Call

   procedure P (I : in out Integer);

   procedure P (I : in out Integer) is
   begin
      null;
   end P;

   with P;

   procedure Proc is
      I : Integer := 0;
   begin
      P (I);
   end Proc;

The call to :ada:`P` is statically dispatched: every time :ada:`Proc` runs and
calls :ada:`P`, that call is always to the same procedure. In other words, we
can determine at compilation time which procedure is called.

In contrast, an access to a subprogram allows us to dynamically indicate which
subprogram we want to call. For example, if we change :ada:`Proc` in the code
above to receive the access to a subprogram :ada:`P` as a parameter, the actual
procedure that would be called when running :ada:`Proc` would be determined at
run time, and it might be different for every call to :ada:`Proc`. In this
case, we wouldn't be able to determine at compilation time which
procedure would be called in every case. (In some cases, however, it could
still be possible to determine which procedure is called by analyzing the
argument that is passed to :ada:`Proc`.)


Access to subprogram declaration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We declare an access to a subprogram as a type by writing
:ada:`access procedure` or :ada:`access function` and the corresponding
prototype:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Types

    package Access_To_Subprogram_Types is

       type Access_To_Procedure is
         access procedure (I : in out Integer);

       type Access_To_Function is
         access function (I : Integer) return Integer;

    end Access_To_Subprogram_Types;

Note that, in the type declarations, we list all the parameters that we expect
in the subprogram.

We can use those types to declare access to subprograms |mdash| as subprogram
parameters, for example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Types

    with Access_To_Subprogram_Types; use Access_To_Subprogram_Types;

    package Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure);

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc (P : Access_To_Procedure) is
          I : Integer := 0;
       begin
          P (I);
          --  P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

In the implementation of the :ada:`Proc` procedure of the code example, we call
the :ada:`P` procedure by simply passing :ada:`I` as a parameter. In this case,
:ada:`P` is automatically dereferenced. We may, however, explicitly dereference
:ada:`P` by writing :ada:`P.all (I)`.

In addition, we can declare those subprogram parameters using anonymous types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Params

    package Access_To_Subprogram_Params is

       procedure Proc (P : access procedure (I : in out Integer));

    end Access_To_Subprogram_Params;

    package body Access_To_Subprogram_Params is

       procedure Proc (P : access procedure (I : in out Integer)) is
          I : Integer := 0;
       begin
          --  P (I);
          P.all (I);
       end Proc;

    end Access_To_Subprogram_Params;

Finally, we can get access to a subprogram using the :ada:`Access` attribute:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Access_To_Subprogram_Params

    procedure Add_Ten (I : in out Integer);

    procedure Add_Ten (I : in out Integer) is
    begin
       I := I + 10;
    end Add_Ten;

    with Access_To_Subprogram_Params;
    use  Access_To_Subprogram_Params;

    with Add_Ten;

    procedure Show_Access_To_Subprograms is
    begin
       Proc (Add_Ten'Access);
       --            ^ Getting access to Add_Ten procedure
       --              and passing it to Proc
    end Show_Access_To_Subprograms;

Here, we get access to the :ada:`Add_Ten` procedure and pass it to the
:ada:`Proc` procedure.

.. admonition:: In the Ada Reference Manual

    - `3.10 Access Types <https://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-10.html>`_


Selecting subprograms
~~~~~~~~~~~~~~~~~~~~~

A practical application of access to subprograms is that it enables us to
dynamically select a subprogram and pass it to another subprogram, where it can
then be called.

For example, we may have a :ada:`Process` procedure that receives a logging
procedure as a parameter (:ada:`Log_Proc`). Also, this parameter may be
:ada:`null` by default |mdash| so that no procedure is called if the parameter
isn't specified:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    package Data_Processing is

       type Data_Container is array (Positive range <>) of Float;

       type Log_Procedure is access procedure (D : Data_Container);

       procedure Process (D        : in out Data_Container;
                          Log_Proc :        Log_Procedure := null);

    end Data_Processing;

    package body Data_Processing is

       procedure Process (D        : in out Data_Container;
                          Log_Proc :        Log_Procedure := null) is
       begin
          --  missing processing part...

          if Log_Proc /= null then
             Log_Proc (D);
          end if;
       end Process;

    end Data_Processing;

In the implementation of :ada:`Process`, we check whether :ada:`Log_Proc` is
null or not. (If it's not null, we call the procedure. Otherwise, we just skip
the call.)

Now, let's implement two logging procedures that match the expected form of
the :ada:`Log_Procedure` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Csv (D : Data_Container) is
    begin
       for I in D'First .. D'Last - 1 loop
          Put (D (I)'Image & ", ");
       end loop;
       Put (D (D'Last)'Image);
       New_Line;
    end Log_Csv;

Finally, we implement a test application that selects each of the logging
procedures that we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Log_Procedure

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Log_Element_Per_Line;
    with Log_Csv;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Log_Element_Per_Line ====");
       Process (D, Log_Element_Per_Line'Access);

       Put_Line ("==== Log_Csv ====");
       Process (D, Log_Csv'Access);

       Put_Line ("==== None ====");
       Process (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Log_Element_Per_Line` and :ada:`Log_Csv` procedures. Also, in the third
call, we don't pass any access as an argument, which is then :ada:`null` by
default.


Null exclusion
~~~~~~~~~~~~~~

We can use null exclusion when declaring an access to subprograms. By doing so,
we ensure that a subprogram must be specified |mdash| either as a parameter or
when initializing an access object. Otherwise, an exception is raised. Let's
adapt the previous example and introduce the :ada:`Init_Function` type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    package Data_Processing is

       type Data_Container is array (Positive range <>) of Float;

       type Init_Function is not null access function return Float;

       procedure Process (D         : in out Data_Container;
                          Init_Func :        Init_Function);

    end Data_Processing;

    package body Data_Processing is

       procedure Process (D         : in out Data_Container;
                          Init_Func :        Init_Function) is
       begin
          for I in D'Range loop
             D (I) := Init_Func.all;
          end loop;
       end Process;

    end Data_Processing;


In this case, we specify that :ada:`Init_Function` is :ada:`not null access`
because we want to always be able to call this function in the :ada:`Process`
procedure (i.e. without raising an exception).

When an access to a subprogram doesn't have parameters |mdash| which is the
case for the subprograms of :ada:`Init_Function` type |mdash| we need to
explicitly dereference it by writing :ada:`.all`. (In this case, :ada:`.all`
isn't optional.) Therefore, we have to write :ada:`Init_Func.all` in the
implementation of the :ada:`Process` procedure of the code example.

Now, let's declare two simple functions |mdash| :ada:`Init_Zero` and
:ada:`Init_One` |mdash| that return 0.0 and 1.0, respectively:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    function Init_Zero return Float;
    function Init_One return Float;

    function Init_Zero return Float is
    begin
       return 0.0;
    end Init_Zero;

    function Init_One return Float is
    begin
       return 1.0;
    end Init_One;

Finally, let's see a test application where we select each of the init
functions we've just implemented:

.. code:: ada run_button main=show_access_to_subprograms.adb project=Courses.Advanced_Ada.Access_Types.Access_Init_Function

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    procedure Log_Element_Per_Line (D : Data_Container) is
    begin
       Put_Line ("Elements: ");
       for V of D loop
          Put_Line (V'Image);
       end loop;
       Put_Line ("------");
    end Log_Element_Per_Line;

    with Ada.Text_IO;     use Ada.Text_IO;
    with Data_Processing; use Data_Processing;

    with Init_Zero;
    with Init_One;

    with Log_Element_Per_Line;

    procedure Show_Access_To_Subprograms is
       D : Data_Container (1 .. 5) := (others => 1.0);
    begin
       Put_Line ("==== Init_Zero ====");
       Process (D, Init_Zero'Access);
       Log_Element_Per_Line (D);

       Put_Line ("==== Init_One ====");
       Process (D, Init_One'Access);
       Log_Element_Per_Line (D);

       --  Put_Line ("==== None ====");
       --  Process (D, null);
       --  Log_Element_Per_Line (D);
    end Show_Access_To_Subprograms;

Here, we use the :ada:`Access` attribute to get access to the
:ada:`Init_Zero` and :ada:`Init_One` functions. Also, if we uncomment the call
to :ada:`Process` with :ada:`null` as an argument for the init function, we see
that the :ada:`Constraint_Error` exception is raised at run time |mdash| as the
argument cannot be :ada:`null` due to the null exclusion.

.. admonition:: For further reading...

    .. note::

        This example was originally written by Robert A. Duff and was part of
        the `Gem #24 <https://www.adacore.com/gems/ada-gem-24>`_.

    Here's another example, first with :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           procedure Iterate
             (T      : Table;
              Action : access procedure (X : not null Ref_Element)
              := null);
           --  If Action is null, do nothing.

        end Show_Null_Procedure;

    and without :ada:`null`:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Null_Procedure
        :class: ada-syntax-only

        package Show_Null_Procedure is
           type Element is limited null record;
           --  Not implemented yet

           type Ref_Element is access all Element;

           type Table is limited null record;
           --  Not implemented yet

           procedure Do_Nothing (X : not null Ref_Element) is null;

           procedure Iterate
             (T      : Table;
              Action : not null access procedure (X : not null Ref_Element)
              := Do_Nothing'Access);

        end Show_Null_Procedure;

    The style of the second :ada:`Iterate` is clearly better because it makes
    use of the syntax to indicate that a procedure is expected. This is a
    complete package that includes both versions of the :ada:`Iterate`
    procedure:

    .. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Complete_Not_Null_Procedure

        package Example is

           type Element is limited private;
           type Ref_Element is access all Element;

           type Table is limited private;

           procedure Iterate
             (T : Table;
              Action : access procedure (X : not null Ref_Element)
                                          := null);
           --  If Action is null, do nothing.

           procedure Do_Nothing (X : not null Ref_Element) is null;
           procedure Iterate_2
             (T : Table;
              Action : not null access procedure (X : not null Ref_Element)
                                              := Do_Nothing'Access);

        private
           type Element is limited
              record
                 Component : Integer;
              end record;
           type Table is limited null record;
        end Example;

        package body Example is

           An_Element : aliased Element;

           procedure Iterate
             (T : Table;
              Action : access procedure (X : not null Ref_Element)
                                              := null) is
           begin
              if Action /= null then
                 Action (An_Element'Access);
                 --  In a real program, this would do something more sensible.
              end if;
           end Iterate;

           procedure Iterate_2
             (T : Table;
              Action : not null access procedure (X : not null Ref_Element)
                                              := Do_Nothing'Access) is
           begin
              Action (An_Element'Access);
              --  In a real program, this would do something more sensible.
           end Iterate_2;

        end Example;

        with Example; use Example;

        procedure Show_Example is
           T : Table;
        begin
           Iterate_2 (T);
        end Show_Example;

    The :ada:`not null access procedure` is quite a mouthful, but it's
    worthwhile, and anyway, as mentioned earlier, the compatibility
    requirement requires that the :ada:`not null` be explicit, rather than the
    other way around.


Access to protected subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Up to this point, we've discussed access to *normal* Ada subprograms. In some
situations, however, we might want to have access to protected subprograms.
To do this, we can simply declare a type using :ada:`access protected`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Simple_Protected_Access

    package Simple_Protected_Access is

       type Access_Proc is
         access protected procedure;

       protected Obj is

          procedure Do_Something;

       end Obj;

       Acc : Access_Proc := Obj.Do_Something'Access;

    end Simple_Protected_Access;

    package body Simple_Protected_Access is

       protected body Obj is

          procedure Do_Something is
          begin
             --  Not doing anything
             --  for the moment...
             null;
          end Do_Something;

       end Obj;

    end Simple_Protected_Access;

Here, we declare the :ada:`Access_Proc` type as an access type to protected
procedures. Then, we declare the variable :ada:`Acc` and assign to it the
access to the :ada:`Do_Something` procedure (of the protected object
:ada:`Obj`).

Now, let's discuss a more useful example: a simple system that allows us to
register protected procedures and execute them. This is implemented in
:ada:`Work_Registry` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    package Work_Registry is

       type Work_Id is tagged limited private;

       type Work_Handler is
         access protected procedure (T : Work_Id);

       subtype Valid_Work_Handler is
         not null Work_Handler;

       type Work_Handlers is array (Positive range <>) of Work_Handler;

       protected type Work_Handler_Registry (Last : Positive) is

          procedure Register (T : Valid_Work_Handler);

          procedure Reset;

          procedure Process_All;

       private

          D    : Work_Handlers (1 .. Last);
          Curr : Natural := 0;

       end Work_Handler_Registry;

    private

       type Work_Id is tagged limited null record;

    end Work_Registry;

    package body Work_Registry is

       protected body Work_Handler_Registry is

          procedure Register (T : Valid_Work_Handler) is
          begin
             if Curr < Last then
                Curr := Curr + 1;
                D (Curr) := T;
             end if;
          end Register;

          procedure Reset is
          begin
             Curr := 0;
          end Reset;

          procedure Process_All is
             Dummy_ID : Work_Id;
          begin
             for I in D'First .. Curr loop
                D (I).all (Dummy_ID);
             end loop;
          end Process_All;

       end Work_Handler_Registry;

    end Work_Registry;

Here, we declare the protected :ada:`Work_Handler_Registry` type with the
following subprograms:

- :ada:`Register`, which we can use to register a protected procedure;

- :ada:`Reset`, which we can use to reset the system; and

- :ada:`Process_All`, which we can use to call all procedures that were
  registered in the system.

:ada:`Work_Handler` is our access to protected subprogram type. Also, we
declare the :ada:`Valid_Work_Handler` subtype, which excludes :ada:`null`. By
doing so, we can ensure that only valid procedures are passed to the
:ada:`Register` procedure. In the protected :ada:`Work_Handler_Registry` type,
we store the procedures in an array (of :ada:`Work_Handlers` type).

.. admonition:: Important

    Note that, in the type declaration :ada:`Work_Handler`, we say that the
    protected procedure must have a parameter of :ada:`Work_Id` type. In this
    example, this parameter is just used to *bind* the procedure to the
    :ada:`Work_Handler_Registry` type. The :ada:`Work_Id` type itself is
    actually declared as a null record (in the private part of the package),
    and it isn't really useful on its own.

    If we had declared :ada:`type Work_Handler is access protected procedure;`
    instead, we would be able to register *any* protected procedure into the
    system, even the ones that might not be suitable for the system. By using
    a parameter of :ada:`Work_Id` type, however, we make use of strong
    typing to ensure that only procedures that were designed for the system
    can be registered.

In the next part of the code, we declare the :ada:`Integer_Storage` type,
which is a simple protected type that we use to store an integer value:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    with Work_Registry;

    package Integer_Storage_System is

       protected type Integer_Storage is

          procedure Set (V : Integer);

          procedure Show (T : Work_Registry.Work_Id);

       private

          I : Integer := 0;

       end Integer_Storage;

       type Integer_Storage_Access is access Integer_Storage;

       type Integer_Storage_Array is
         array (Positive range <>) of Integer_Storage_Access;

    end Integer_Storage_System;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Integer_Storage_System is

       protected body Integer_Storage is

          procedure Set (V : Integer) is
          begin
             I := V;
          end Set;

          procedure Show (T : Work_Registry.Work_Id) is
             pragma Unreferenced (T);
          begin
             Put_Line ("Value: " & Integer'Image (I));
          end Show;

       end Integer_Storage;

    end Integer_Storage_System;

For the :ada:`Integer_Storage` type, we declare two procedures:

- :ada:`Set`, which we use to assign a value to the (protected) integer value;
  and

- :ada:`Show`, which we use to show the integer value that is stored in the
  protected object.

The :ada:`Show` procedure has a parameter of :ada:`Work_Id` type, which
indicates that this procedure was designed to be registered in the system of
:ada:`Work_Handler_Registry` type.

Finally, we have a test application in which we declare a registry (:ada:`WHR`)
and an array of "protected integer objects" (:ada:`Int_Stor`):

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function

    with Work_Registry;          use Work_Registry;
    with Integer_Storage_System; use Integer_Storage_System;

    procedure Show_Access_To_Protected_Subprograms is

       WHR      : Work_Handler_Registry (5);
       Int_Stor : Integer_Storage_Array (1 .. 3);

    begin
       --  Allocate and initialize integer storage
       --
       --  (For the initialization, we're just assigning
       --   the index here, but we could really have used
       --   any integer value.)

       for I in Int_Stor'Range loop
          Int_Stor (I) := new Integer_Storage;
          Int_Stor (I).Set (I);
       end loop;

       --  Register handlers

       for I in Int_Stor'Range loop
          WHR.Register (Int_Stor (I).all.Show'Access);
       end loop;

       --  Now, use Process_All to call the handlers
       --  (in this case, the Show procedure for
       --  each protected object from Int_Stor).

       WHR.Process_All;

    end Show_Access_To_Protected_Subprograms;

The work handler registry (:ada:`WHR`) has a maximum capacity of five
procedures, whereas the :ada:`Int_Stor` array has a capacity of three elements.
By calling :ada:`WHR.Register` and passing :ada:`Int_Stor (I).all.Show'Access`,
we register the :ada:`Show` procedure of each protected object from
:ada:`Int_Stor`.

.. admonition:: Important

    Note that the components of the :ada:`Int_Stor` array are of
    :ada:`Integer_Storage_Access` type, which is declared as an access to
    :ada:`Integer_Storage` objects. Therefore, we have to dereference the
    object (by writing :ada:`Int_Stor (I).all`) before getting access to the
    :ada:`Show` procedure (by writing :ada:`.Show'Access`).

    We have to use an access type here because we cannot pass the access (to
    the :ada:`Show` procedure) of a local object in the call to the
    :ada:`Register` procedure. Therefore, the protected objects (of
    :ada:`Integer_Storage` type) cannot be local.

    This issue becomes evident if we replace the declaration of
    :ada:`Int_Stor` with a local array (and then adapt the remaining code). If
    we do this, we get a compilation error in the call to :ada:`Register`:

    .. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Protected_Access_Init_Function
        :class: ada-expect-compile-error

        with Work_Registry;          use Work_Registry;
        with Integer_Storage_System; use Integer_Storage_System;

        procedure Show_Access_To_Protected_Subprograms is

           WHR      : Work_Handler_Registry (5);

           Int_Stor : array (1 .. 3) of Integer_Storage;

        begin
           --  Allocate and initialize integer storage
           --
           --  (For the initialization, we're just assigning
           --   the index here, but we could really have used
           --   any integer value.)

           for I in Int_Stor'Range loop
              --  Int_Stor (I) := new Integer_Storage;
              Int_Stor (I).Set (I);
           end loop;

           --  Register handlers

           for I in Int_Stor'Range loop
              WHR.Register (Int_Stor (I).Show'Access);
              --            ^ ERROR!
           end loop;

           --  Now, call the handlers
           --  (i.e. the Show procedure of each
           --   protected object).

           WHR.Process_All;

        end Show_Access_To_Protected_Subprograms;

    As we've just discussed, this error is due to the fact that
    :ada:`Int_Stor` is now a "local" protected object, and the accessibility
    rules don't allow mixing it with non-local accesses in order to prevent the
    possibility of dangling references.

When we call :ada:`WHR.Process_All`, the registry system calls each procedure
that has been registered with the system. When looking at the values displayed
by the test application, we may notice that each call to :ada:`Show` is
referring to a different protected object. In fact, even though we're passing
just the access to a protected *procedure* in the call to :ada:`Register`, that
access is also associated to a specific protected object. (This is different
from access to non-protected subprograms we've discussed previously: in that
case, there's no object associated.) If we replace the argument to
:ada:`Register` by :ada:`Int_Stor (2).all.Show'Access`, for example, the three
:ada:`Show` procedures registered in the system will now refer to the same
protected object (stored at :ada:`Int_Stor (2)`).

Also, even though we have registered the same procedure (:ada:`Show`) of the
same type (:ada:`Integer_Storage`) in all calls to :ada:`Register`, we could
have used a different protected procedure |mdash| and of a different protected
type. As an exercise, we could, for example, create a new type called
:ada:`Float_Storage` (based on the code that we used for the
:ada:`Integer_Storage` type) and register some objects of :ada:`Float_Storage`
type into the system (with a couple of additional calls to :ada:`Register`). If
we then call :ada:`WHR.Process_All`, we'd see that the system is able to cope
with objects of both :ada:`Integer_Storage` and :ada:`Float_Storage` types. In
fact, the system implemented with the :ada:`Work_Handler_Registry` can be seen
as "type agnostic," as it doesn't care about which type the protected objects
have |mdash| as long as the subprograms we want to register are conformant to
the :ada:`Valid_Work_Handler` type.


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Accessibility checks
    --------------------

    .. note::

        This section was originally written by Ramón Fernández and Robert A. Duff, and
        published as
        `Gem #33: Accessibility Checks <https://www.adacore.com/gems/gem-33>`_,
        `Gem #41 <https://www.adacore.com/gems/gem-41>`_ and
        `Gem #44: <https://www.adacore.com/gems/gem-44>`_.
