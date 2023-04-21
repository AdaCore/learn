Records
=======

.. include:: ../../global.txt

So far, all the types we have encountered have values that are not
decomposable: each instance represents a single piece of data. Now we are going
to see our first class of composite types: records.

Records allow composing a value out of instances of other types. Each of
those instances will be given a name. The pair consisting of a name and
an instance of a specific type is called a field, or a component.

.. _Intro_Ada_Record_Type_Declaration:

Record type declaration
-----------------------

Here is an example of a simple record declaration:

.. code-block:: ada

    type Date is record
       --  The following declarations are
       --  components of the record
       Day   : Integer range 1 .. 31;
       Month : Months;
       --  You can add custom constraints
       --  on fields
       Year  : Integer range 1 .. 3000;
    end record;

Fields look a lot like variable declarations, except that they are inside of a
record definition.  And as with variable declarations, you can specify
additional constraints when supplying the subtype of the field.

.. code-block:: ada

    type Date is record
       Day   : Integer range 1 .. 31;
       Month : Months := January;
       --  This component has a default value
       Year  : Integer range 1 .. 3000 := 2012;
       --                                 ^ Default value
    end record;

.. _Intro_Ada_Record_Default_Values:

Record components can have default values. When a variable having the record
type is declared, a field with a default initialization will be automatically
set to this value. The value can be any expression of the component type, and
may be run-time computable.

In the remaining sections of this chapter, we see how to use record types. In
addition to that, we discuss more about records in
:doc:`another chapter <./more_about_records>`.


.. _Intro_Ada_Record_Aggregates:

Aggregates
----------

.. code-block:: ada

    --  Positional components
    Ada_Birthday    : Date := (10, December, 1815);

    --  Named components
    Leap_Day_2020   : Date := (Day   => 29,
                               Month => February,
                               Year  => 2020);
    --                         ^ By name

Records have a convenient notation for expressing values, illustrated above.
This notation is called aggregate notation, and the literals are called
aggregates. They can be used in a variety of contexts that we will see
throughout the course, one of which is to initialize records.

An aggregate is a list of values separated by commas and enclosed in
parentheses. It is allowed in any context where a value of the record is
expected.

Values for the components can be specified positionally, as in
:ada:`Ada_Birthday` example, or by name, as in :ada:`Leap_Day_2020`. A mixture
of positional and named values is permitted, but you cannot use a positional
notation after a named one.

Component selection
-------------------

To access components of a record instance, you use an operation that is
called component selection. This is achieved by using the dot notation. For
example, if we declare a variable :ada:`Some_Day` of the :ada:`Date` record
type mentioned above, we can access the :ada:`Year` component by writing
:ada:`Some_Day.Year`.

Let's look at an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Records.Record_Selection

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Record_Selection is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Display_Date (D : Date) is
       begin
          Put_Line ("Day:" & Integer'Image (D.Day)
                    & ", Month: "
                    & Months'Image (D.Month)
                    & ", Year:"
                    & Integer'Image (D.Year));
       end Display_Date;

       Some_Day : Date := (1, January, 2000);

    begin
       Display_Date (Some_Day);

       Put_Line ("Changing year...");
       Some_Day.Year := 2001;

       Display_Date (Some_Day);
    end Record_Selection;

As you can see in this example, we can use the dot notation in the expression
:ada:`D.Year` or :ada:`Some_Day.Year` to access the information stored in that
component, as well as to modify this information in assignments. To be more
specific, when we use :ada:`D.Year` in the call to :ada:`Put_Line`, we're
retrieving the information stored in that component. When we write
:ada:`Some_Day.Year := 2001`, we're overwriting the information that was
previously stored in the :ada:`Year` component of :ada:`Some_Day`.

.. _Intro_Ada_Record_Comp_Renaming:

Renaming
--------

In previous chapters, we've discussed :ref:`subprogram <Intro_Ada_Subprogram_Renaming>`
and :ref:`package <Intro_Ada_Package_Renaming>` renaming. We can rename record
components as well. Instead of writing the full component selection using the
dot notation, we can declare an alias that allows us to access the same
component. This is useful to simplify the implementation of a subprogram, for
example.

We can rename record components by using the :ada:`renames` keyword in a
variable declaration. For example:

.. code-block:: ada

    Some_Day : Date;
    Y        : Integer renames Some_Day.Year;

Here, :ada:`Y` is an alias, so that every time we using :ada:`Y`, we are really
using the :ada:`Year` component of :ada:`Some_Day`.

Let's look at a complete example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Record_Component_Renaming

    package Dates is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Increase_Month
        (Some_Day : in out Date);

       procedure Display_Month
         (Some_Day : Date);

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Dates is

       procedure Increase_Month
         (Some_Day : in out Date)
       is
          --  Renaming components from
          --  the Date record
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;

          --  Renaming function (for Months
          --  enumeration)
          function Next (M : Months)
                         return Months
            renames Months'Succ;
       begin
          if M = December then
             M := January;
             Y := Y + 1;
          else
             M := Next (M);
          end if;
       end Increase_Month;

       procedure Display_Month
         (Some_Day : Date)
       is
          --  Renaming components from
          --  the Date record
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;
       begin
          Put_Line ("Month: "
                    & Months'Image (M)
                    & ", Year:"
                    & Integer'Image (Y));
       end Display_Month;

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;
    with Dates;       use Dates;

    procedure Main is
       D : Date := (1, January, 2000);
    begin
       Display_Month (D);

       Put_Line ("Increasing month...");
       Increase_Month (D);

       Display_Month (D);
    end Main;

We apply renaming to two components of the :ada:`Date` record in the
implementation of the :ada:`Increase_Month` procedure. Then, instead of
directly using :ada:`Some_Day.Month` and :ada:`Some_Day.Year` in the
next operations, we simply use the renamed versions :ada:`M` and :ada:`Y`.

Note that, in the example above, we also rename :ada:`Months'Succ` |mdash|
which is the function that gives us the next month |mdash| to :ada:`Next`.
