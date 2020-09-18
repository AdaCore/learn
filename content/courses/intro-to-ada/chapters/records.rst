Records
=======

:code-config:`reset_accumulator=True`

.. include:: ../../global.txt

So far, all the types we have encountered have values that are not
decomposable: each instance represents a single piece of data. Now we are going
to see our first class of composite types: records.

Records allow composing a value out of instances of other types. Each of
those instances will be given a name. The pair consisting of a name and
an instance of a specific type is called a field, or a component.

Record type declaration
-----------------------

Here is an example of a simple record declaration:

.. code-block:: ada

    type Date is record
       --  The following declarations are components of the record
       Day   : Integer range 1 .. 31;
       Month : Month_Type;
       Year  : Integer range 1 .. 3000; --  You can add custom constraints on fields
    end record;

Fields look a lot like variable declarations, except that they are inside of a
record definition.  And as with variable declarations, you can specify
additional constraints when supplying the subtype of the field.

.. code-block:: ada

    type Date is record
       Day   : Integer range 1 .. 31;
       Month : Month_Type := January;
       --  This component has a default value
       Year  : Integer range 1 .. 3000 := 2012;
       --                                 ^ Default value
    end record;

Record components can have default values. When a variable having the record
type is declared, a field with a default initialization will be automatically
set to this value. The value can be any expression of the component type, and
may be run-time computable.

Aggregates
----------

.. code-block:: ada

    Ada_Birthday    : Date := (10, December, 1815);
    Leap_Day_2020   : Date := (Day => 29, Month => February, Year => 2020);
    --                ^ By name

Records have a convenient notation for expressing values, illustrated above.
This notation is called aggregate notation, and the literals are called
aggregates. They can be used in a variety of contexts that we will see
throughout the course, one of which is to initalize records.

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
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Record_Selection is

       type Month_Type is
         (January, February, March, April, May, June, July,
          August, September, October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Month_Type;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Display_Date (D : Date) is
       begin
          Put_Line ("Day:" & Integer'Image (D.Day)
                    & ", Month: " & Month_Type'Image (D.Month)
                    & ", Year:" & Integer'Image (D.Year));
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
