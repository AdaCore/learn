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


Access to subprograms
---------------------

.. todo::

    Complete section!


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

..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Accessibility levels
    --------------------


Unchecked access
----------------

.. admonition:: Relevant topics

    - `Unchecked Access Value Creation <http://www.ada-auth.org/standards/2xrm/html/RM-13-10.html>`_

.. todo::

    Complete section!


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

.. code:: ada run_button project=Courses.Advanced_Ada.Access_Types.Complete_Null_Return

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

    end Example;

    with Example; use Example;

    procedure Show_Example is
       T : Table;
       E : Ref_Element;
    begin
       E := Lookup (T);
    end Show_Example;

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

Another advantage of :ada:`not null` over comments is for efficiency.
Consider procedures :ada:`P` and :ada:`Q` in this example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Access_Types.Complete_Not_Null_Procedure

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


..
    REMOVED! TO BE RE-EVALUATED IN 2022:

    Anonymous access types
    ----------------------


Unchecked Deallocation
----------------------

.. admonition:: Relevant topics

    - `Unchecked Storage Deallocation <http://www.ada-auth.org/standards/2xrm/html/RM-13-11-2.html>`_

.. todo::

    Complete section!

