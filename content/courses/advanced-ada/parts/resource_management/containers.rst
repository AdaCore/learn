Containers
==========

.. include:: ../../../global.txt

.. _Adv_Ada_Aggregate_Aspect:

:ada:`Aggregate` aspect
------------------------

.. note::

    This feature was introduced in Ada 2022.

In a previous chapter, we discussed
:ref:`container aggregates <Adv_Ada_Container_Aggregates>`, which are commonly
used with standard containers. If you look at the type declarations of the
standard containers (in the :ada:`Ada.Containers` packages), you'll notice that
some of them make use of the :ada:`Aggregate` aspect. This aspect is used to
specify which subprograms are called to process a container aggregate for a
data type, let's say a type named :ada:`T`. Suppose we declare an object
:ada:`Obj` like this: :ada:`Obj : T := [1, 2, 3]`. In this case, the
:ada:`Aggregate` aspect specifies which subprograms are going to be called to
process the :ada:`[1, 2, 3]` aggregate.

The :ada:`Aggregate` aspect is used in many declarations of the
:ada:`Ada.Containers` packages. However, this aspect isn't restricted to the
standard containers: we may indeed use the :ada:`Aggregate` aspect to specify
a custom container aggregate for any type other than an array. In this section,
we discuss the elements of the :ada:`Aggregate` aspect and how to use this
aspect to create your own container aggregates.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Container Aggregates <4-3-5>`


Basic syntax
~~~~~~~~~~~~

The :ada:`Aggregate` aspect has the following syntax:

.. code-block:: ada

    type T is private
      with Aggregate =>
        (Empty          => Empty_Func,
         Add_Named      => Add_Named_Proc,
         Add_Unnamed    => Add_Unnamed_Proc,
         New_Indexed    => New_Indexed_Func,
         Assign_Indexed => Assign_Indexed_Proc);

Note that the order of the elements must be exactly as shown above.

Basically, there are three elements you can use in the :ada:`Aggregate` aspect
to specify a procedure that is called when adding an element to the container:
:ada:`Add_Named`, ada:`Add_Unnamed`, and :ada:`Assign_Indexed`.

.. admonition:: Attention

    Remember that an indexed aggregate has an index associated with each
    component. As discussed in the
    :ref:`section on container aggregates <Adv_Ada_Container_Aggregates>`,

    - for indexed positional container aggregates, the index of each component
      is implied by its position;

    - for indexed named container aggregates, the index of each component is
      explicitly indicated.

    We discuss this topic later in more details.

Some restrictions apply to the :ada:`Aggregate` aspect. For example:

- we have to specify at least one of those elements (:ada:`Add_Named`,
  :ada:`Add_Unnamed`, or :ada:`Assign_Indexed`), and

- we cannot specify both :ada:`Add_Named` and :ada:`Add_Unnamed` elements at
  the same time.

We can, however, combine :ada:`Add_Unnamed` and :ada:`Assign_Indexed` in the
same aspect declaration.


Classification
~~~~~~~~~~~~~~

We can classify container aggregates in two categories:

- whether they are indexed or not; and

- whether they are positional or named.

This classification depends on the elements that were used in the declaration
of the :ada:`Aggregate` aspect and whether a key is used in the aggregate. The
following table presents the classification:

+---------+-----------------------+------------+-------+-------------------------------------------------+
| Indexed | Elements in           | Positional | Uses  | Container aggregate: example                    |
|         | :ada:`Aggregate`      | / named    | key   |                                                 |
+=========+=======================+============+=======+=================================================+
| No      | :ada:`Add_Named`      | Named      | Yes   | :ada:`["Key_1" => "Hello", "Key_2" => "World"]` |
|         +-----------------------+------------+-------+-------------------------------------------------+
|         | :ada:`Add_Unnamed`    | Positional | No    | :ada:`["Hello", "World"]`                       |
|         +-----------------------+            |       |                                                 +
|         | :ada:`Assign_Indexed` |            |       |                                                 |
|         | :ada:`Add_Unnamed`    |            |       |                                                 |
+---------+-----------------------+------------+-------+-------------------------------------------------+
| Yes     | :ada:`Assign_Indexed` | Named      | Yes   | :ada:`[1 => "Hello", 2 => "World"]`             |
|         | :ada:`Add_Unnamed`    |            |       |                                                 |
|         +-----------------------+------------+-------+-------------------------------------------------+
|         | :ada:`Assign_Indexed` | Named      | Yes   | :ada:`[1 => "Hello", 2 => "World"]`             |
|         |                       +------------+-------+-------------------------------------------------+
|         |                       | Positional | No    | :ada:`["Hello", "World"]`                       |
+---------+-----------------------+------------+-------+-------------------------------------------------+

The next table presents the typical use-cases:

+-----------------------+-----------------+
| Category              | Typical use     |
|                       |                 |
+=======================+=================+
| :ada:`Add_Named`      | Maps            |
+-----------------------+-----------------+
| :ada:`Add_Unnamed`    | Lists, sets     |
+-----------------------+-----------------+
| :ada:`Add_Unnamed`    | Vectors         |
| :ada:`Assign_Indexed` |                 |
+-----------------------+-----------------+
| :ada:`Assign_Indexed` | (none)          |
+-----------------------+-----------------+

Before we discuss these approaches, let's first look at the :ada:`Empty`
element.


:ada:`Empty`
~~~~~~~~~~~~

The :ada:`Empty` element allows us to specify the behavior for an empty
container, i.e. the simplest version of a container without any components.

Let's assume we a container type :ada:`T` for which we specify an
:ada:`Empty` function in the :ada:`Aggregate` aspect, and we declare an object
:ada:`Obj : T`. In this case, the :ada:`Empty` function is called in one of
two scenarios:

- when we assign a null container to :ada:`Obj` |mdash| by writing
  :ada:`Obj := [];` |mdash| or

- when we assign a container with at least one component to :ada:`Obj` |mdash|
  for example: :ada:`Obj := [1, 2];`.

Let's see a complete code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Empty switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty     => Empty_Func,
            Add_Named => Add_Named_Proc);

       function Empty_Func return T;

       procedure Add_Named_Proc
         (Cont  : in out T;
          Key   :        String;
          Value :        String) is null;

    private

        type T is record
           Cnt : Natural;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func return T is
       begin
          Put_Line ("Calling Empty_Func");

          return (Cnt => 0);
       end Empty_Func;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Container_Aggregate_Empty is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];
    end Show_Container_Aggregate_Empty;

In this example, we specify the :ada:`Empty` function for the :ada:`Aggregate`
aspect of the container type :ada:`T`. (We also use the :ada:`Add_Unnamed`
element. You can ignore it for the moment: we'll discuss it later on.)

The :ada:`A := []` statement in the :ada:`Show_Container_Aggregate_Empty`
procedure calls :ada:`Empty_Func` |mdash| the function specified in the
:ada:`Empty` element of the :ada:`Aggregate` aspect |mdash|, which returns an
object of the container type :ada:`T`, which is then assigned to :ada:`A`. (You
can confirm this by running this example and seeing the
:ada:`Calling Empty_Func` message, which we included in the body of the
:ada:`Empty_Func` function.)

We can also use a constant for the :ada:`Empty` element instead of a function:

.. code:: ada compile_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Empty_Const switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty     => Empty_Const,
            Add_Named => Add_Named_Proc);

       Empty_Const : constant T;

       procedure Add_Named_Proc
         (Cont  : in out T;
          Key   :        String;
          Value :        String) is null;

    private

        type T is record
           Cnt : Natural;
        end record;

        Empty_Const : constant T := (Cnt => 0);

    end Custom_Container_Aggregates;

Here, we simply assign :ada:`Empty_Const` when an actual :ada:`Empty` is
needed.

In addition to this, we can specify a signed integer parameter |mdash| which
indicates the number of components |mdash| for the :ada:`Empty` function:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Empty switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty       => Empty_Func,
            Add_Unnamed => Add_Unnamed_Proc);

       T_Len_Typical : constant := 10;

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T;

       procedure Add_Unnamed_Proc
         (Cont : in out T;
          Item :        String) is null;

    private

        type T is record
           Cnt   : Natural;
           Total : Integer;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T is
       begin
          Put_Line ("Calling Empty_Func ("
                    & "Total => "
                    & Total'Image & ")");

          return (Total => Total,
                  Cnt   => 0);
       end Empty_Func;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Container_Aggregate_Empty is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];

       Put_Line ("A := [""Hello"", ""World""]");
       A := ["Hello", "World"];
    end Show_Container_Aggregate_Empty;

In this example, we specify an :ada:`Empty_Func` function with an
:ada:`Integer` parameter (for the :ada:`Empty` element of the :ada:`Aggregate`
aspect).

The actual argument for the integer parameter of the :ada:`Empty_Func` function
depends on the number of elements we use in the container aggregate. In this
specific example, when we write :ada:`A := []`, then :ada:`Empty_Func (0)` is
called, whereas when we write :ada:`A := ["Hello", "World"]`, this results in
a call to :ada:`Empty_Func (2)`.


:ada:`Add_Named`
~~~~~~~~~~~~~~~~

The :ada:`Add_Named` element of the :ada:`Aggregate` aspect refers to a
procedure that is called when we have a named container aggregate |mdash| i.e.
a container aggregate with components in the :ada:`Key => Value` form |mdash|
that doesn't use indexing.

Note that, when we specify the :ada:`Add_Named` element, we cannot specify any
of these elements: :ada:`Add_Unnamed`, :ada:`New_Indexed` or
:ada:`Assign_Indexed`. In other words, when we specify the :ada:`Add_Named`
element, we can only use the :ada:`Empty` element in the same declaration.

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Named switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty     => Empty_Func,
            Add_Named => Add_Named_Proc);

       T_Len_Typical : constant := 10;

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T;

       procedure Add_Named_Proc
         (Cont  : in out T;
          Key   :        String;
          Value :        String);

    private

        type T is record
           Total : Integer;
           Cnt   : Natural;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T is
       begin
          Put_Line ("Calling Empty_Func ("
                    & "Total => "
                    & Total'Image & ")");

          return (Total => Total,
                  Cnt   => 0);
       end Empty_Func;

       procedure Add_Named_Proc
         (Cont  : in out T;
          Key   :        String;
          Value :        String) is
       begin
          Put_Line ("Calling Add_Named_Proc (Anon, "
                    & "Key => """
                    & Key   & """, "
                    & "Value => """
                    & Value & """)");
       end Add_Named_Proc;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Named_Container_Aggregate is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];

       Put_Line ("A := [""Key_1"" => ""Hello"", "
                 &     """Key_2"" => ""World""]");
       A := ["Key_1" => "Hello",
             "Key_2" => "World"];

    end Show_Named_Container_Aggregate;

When we write :ada:`A := []`, we're just calling :ada:`Empty_Func (0)` |mdash|
as we're using a null container aggregate, there are no components to be added
to the container. However, when we write
:ada:`A := ["Key_1" => "Hello", "Key_2" => "World"]`, we see the following
calls:

- a call to :ada:`Empty_Func (2)` that creates an empty container with two
  components;

- a call to :ada:`Add_Named_Proc (Anon, "Key_1", "Hello")` for the first
  component, and

- a call to :ada:`Add_Named_Proc (Anon, "Key_2", "World")` for the second
  component.

The :ada:`Anon` argument in the calls above indicates that an anonymous object
is first created and then assigned to :ada:`A`.


:ada:`Add_Unnamed`
~~~~~~~~~~~~~~~~~~

The :ada:`Add_Unnamed` element of the :ada:`Aggregate` aspect refers to a
procedure that is called when we have a positional container aggregate.

Let's look at an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Unnamed switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty       => Empty_Func,
            Add_Unnamed => Add_Unnamed_Proc);

       T_Len_Typical : constant := 10;

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T;

       procedure Add_Unnamed_Proc
         (Cont : in out T;
          Item :        String);

    private

        type T is record
           Total : Integer;
           Cnt   : Natural;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T is
       begin
          Put_Line ("Calling Empty_Func ("
                    & "Total => "
                    & Total'Image & ")");

          return (Total => Total,
                  Cnt   => 0);
       end Empty_Func;

       procedure Add_Unnamed_Proc
         (Cont : in out T;
          Item :        String) is
       begin
          Put_Line ("Calling Add_Unnamed_Proc (Anon, "
                    & "Item => """
                    & Item & """)");
       end Add_Unnamed_Proc;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Unnamed_Container_Aggregate is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];

       Put_Line ("A := [""Hello"", ""World""]");
       A := ["Hello", "World"];
    end Show_Unnamed_Container_Aggregate;

The :ada:`A := ["Hello", "World"]` statement from the code above generates the
following calls:

- a call to :ada:`Empty_Func (2)` that creates an empty container with two
  components;

- a call to :ada:`Add_Unnamed_Proc (Anon, "Hello")` for the first component,
  and

- a call to :ada:`Add_Unnamed_Proc (Anon, "World")` for the second component.


:ada:`Assign_Indexed`
~~~~~~~~~~~~~~~~~~~~~

The :ada:`Assign_Indexed` element of the :ada:`Aggregate` aspect refers to a
procedure that is called when we have an indexed container aggregate. Note
that, when we specify the :ada:`Assign_Indexed` element, we must also use the
:ada:`New_Indexed` element in the same aspect declaration.

Let's look at an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Indexed switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty          => Empty_Func,
            New_Indexed    => New_Indexed_Func,
            Assign_Indexed => Assign_Indexed_Proc);

       T_Len_Typical : constant := 10;

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T;

       function New_Indexed_Func
         (First, Last : Positive)
          return T
         with Pre => First = Positive'First;

       procedure Assign_Indexed_Proc
         (Cont  : in out T;
          Index :        Positive;
          Item  :        String);

    private

        type T is record
           Total : Integer;
           Cnt   : Natural;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T is
       begin
          Put_Line
            ("Calling Empty_Func ("
             & "Total => " & Total'Image & ")");

          return (Total => Total,
                  Cnt   => 0);
       end Empty_Func;

       function New_Indexed_Func
         (First, Last : Positive)
          return T is
       begin
          Put_Line
            ("Calling New_Indexed_Func ("
             & "First => " & First'Image & ", "
             & "Last  => " & Last'Image & ")");

          return (Total => Last - First + 1,
                  Cnt   => 0);
       end New_Indexed_Func;

       procedure Assign_Indexed_Proc
         (Cont  : in out T;
          Index :        Positive;
          Item  :        String)
       is
          pragma Unreferenced (Cont);
       begin
          Put_Line
            ("Calling Assign_Indexed_Proc (Anon, "
             & "Index => "   & Index'Image & ", "
             & "Item  => """ & Item & """)");

       end Assign_Indexed_Proc;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Indexed_Container_Aggregate is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];

       Put_Line ("A := [""Hello"", ""World""]");
       A := ["Hello", "World"];

       Put_Line ("A := [1 => ""Hello"", "
                 &     "2 => ""World""]");
       A := [1 => "Hello", 2 => "World"];

       Put_Line ("A := [1 => ""Hello"", "
                 &     "2 => <>, "
                 &     "3 => ""World""]");
       A := [1 => "Hello", 2 => <>, 3 => "World"];

    end Show_Indexed_Container_Aggregate;

The :ada:`A := [1 => "Hello", 2 => "World"]` statement from the code above
generates the following calls:

- a call to :ada:`New_Indexed_Func (First => 1, Last => 2)` that creates an
  empty container with two components (where the first index of the container
  is 1 and the last index is 2);

- a call to :ada:`Assign_Indexed_Proc (Anon, 1, "Hello")` for the first
  component (which is stored at the position with index 1), and

- a call to :ada:`Assign_Indexed_Proc (Anon, 2, "World")` for the second
  component (which is stored at the position with index 2).

Note that, in the case of indexed aggregates, the :ada:`New_Indexed_Func`
function is called instead of the :ada:`Empty` function.

For indexed aggregates, we can use the :ada:`<>` syntax for individual
components. In the code above, we use it in the
:ada:`A := [1 => "Hello", 2 => <>, 3 => "World"]` statement, which generates
the following calls:

- a call to :ada:`New_Indexed_Func (First => 1, Last => 3)` that creates an
  empty container with three components (where the first index of the container
  is 1 and the last index is 3);

- a call to :ada:`Assign_Indexed_Proc (Anon, 1, "Hello")` for the first
  component, and

- a call to :ada:`Assign_Indexed_Proc (Anon, 3, "World")` for the third
  component.

In other words, the :ada:`2 => <>` element from the statement allows us to
allocate a container with more components than we assign to. (There's no
assignment happening at index 2 in the aggregate above: it'll have the default
value or remain uninitialized.)


Combining :ada:`Add_Named` and :ada:`Assign_Indexed`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As mentioned previously, we may specify both :ada:`Add_Named` and
:ada:`Assign_Indexed` elements together in the same aspect declaration. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Aggregates.Container_Aggregate_Unnamed_Indexed switches=Compiler(-gnat2022);

    package Custom_Container_Aggregates is

       type T is private
         with Aggregate =>
           (Empty          => Empty_Func,
            Add_Unnamed    => Add_Unnamed_Proc,
            New_Indexed    => New_Indexed_Func,
            Assign_Indexed => Assign_Indexed_Proc);

       T_Len_Typical : constant := 10;

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T;

       procedure Add_Unnamed_Proc
         (Cont : in out T;
          Item :        String);

       function New_Indexed_Func
         (First, Last : Positive)
          return T
         with Pre => First = Positive'First;

       procedure Assign_Indexed_Proc
         (Cont  : in out T;
          Index :        Positive;
          Item  :        String);

    private

        type T is record
           Total : Integer;
           Cnt   : Natural;
        end record;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Custom_Container_Aggregates is

       function Empty_Func
         (Total : Integer := T_Len_Typical)
          return T is
       begin
          Put_Line ("Calling Empty_Func ("
                    & "Total => "
                    & Total'Image & ")");

          return (Total => Total,
                  Cnt   => 0);
       end Empty_Func;

       procedure Add_Unnamed_Proc
         (Cont : in out T;
          Item :        String) is
       begin
          Put_Line ("Calling Add_Unnamed_Proc (Anon, "
                    & "Item => """ & Item & """)");
       end Add_Unnamed_Proc;

       function New_Indexed_Func
         (First, Last : Positive)
          return T is
       begin
          Put_Line
            ("Calling New_Indexed_Func ("
             & "First => " & First'Image & ", "
             & "Last  => " & Last'Image & ")");

          return (Total => Last - First + 1,
                  Cnt   => 0);
       end New_Indexed_Func;

       procedure Assign_Indexed_Proc
         (Cont  : in out T;
          Index :        Positive;
          Item  :        String)
       is
          pragma Unreferenced (Cont);
       begin
          Put_Line
            ("Calling Assign_Indexed_Proc (Anon, "
             & "Index => "   & Index'Image & ", "
             & "Item  => """ & Item & """)");

       end Assign_Indexed_Proc;

    end Custom_Container_Aggregates;

    with Ada.Text_IO; use Ada.Text_IO;

    with Custom_Container_Aggregates;
    use  Custom_Container_Aggregates;

    procedure Show_Unnamed_Indexed_Container_Aggregate
    is
       A : T;
    begin
       Put_Line ("A := []");
       A := [];

       Put_Line ("A := [""Hello"", ""World""]");
       A := ["Hello", "World"];

       Put_Line
         ("A := [1 => ""Hello"", 2 => ""World""]");
       A := [1 => "Hello", 2 => "World"];
    end Show_Unnamed_Indexed_Container_Aggregate;

Now, the subprogram calls depend on whether the container aggregate is
positional or not:

- for positional aggregates (e.g.: :ada:`["Hello", "World"]`), the
  :ada:`Add_Unnamed` element is used; while

- for named aggregates (:ada:`[1 => "Hello", 2 => "World"]`), the
  :ada:`New_Indexed` / :ada:`Assign_Indexed` elements are used.


User-Defined Iterator Types
---------------------------

.. admonition:: Relevant topics

    - :arm22:`User-Defined Iterator Types <5-5-1>`
    - :arm22:`Generalized Loop Iteration <5-5-2>`
    - :arm22:`Procedural Iterators <5-5-3>`

.. todo::

    Complete section!
