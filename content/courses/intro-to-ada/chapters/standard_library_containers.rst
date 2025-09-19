.. _Intro_Ada_Containers:

Standard library: Containers
============================

.. include:: ../../../global.txt

In previous chapters, we've used arrays as the standard way to group multiple
objects of a specific data type. In many cases, arrays are good enough for
manipulating those objects. However, there are situations that require more
flexibility and more advanced operations. For those cases, Ada provides support
for containers |mdash| such as vectors and sets |mdash| in its standard
library.

We present an introduction to containers here. For a list of all containers
available in Ada, see :ref:`Appendix B <Intro_Ada_Containers_Table>`.

Vectors
-------

In the following sections, we present a general overview of vectors,
including instantiation, initialization, and operations on vector
elements and vectors.

Instantiation
~~~~~~~~~~~~~

Here's an example showing the instantiation and declaration of a
vector :ada:`V`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Inst

    with Ada.Containers.Vectors;

    procedure Show_Vector_Inst is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       V : Integer_Vectors.Vector;
    begin
       null;
    end Show_Vector_Inst;

Containers are based on generic packages, so we can't simply declare
a vector as we would declare an array of a specific type:

.. code-block:: ada

       A : array (1 .. 10) of Integer;

Instead, we first need to instantiate one of those packages.  We
:ada:`with` the container package (:ada:`Ada.Containers.Vectors` in this
case) and instantiate it to create an instance of the generic package for
the desired type.  Only then can we declare the vector using the type from
the instantiated package. This instantiation needs to be done for any
container type from the standard library.

In the instantiation of :ada:`Integer_Vectors`, we indicate that the vector
contains elements of :ada:`Integer` type by specifying it as the
:ada:`Element_Type`.  By setting :ada:`Index_Type` to :ada:`Natural`, we specify
that the allowed range includes all natural numbers. We could have used a
more restrictive range if desired.

Initialization
~~~~~~~~~~~~~~

One way to initialize a vector is from a concatenation of elements.
We use the :ada:`&` operator, as shown in the following example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Init

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Init is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length)
                 & " elements");
    end Show_Vector_Init;

We specify :ada:`use Integer_Vectors`, so we have direct access to the
types and operations from the instantiated package. Also, the example
introduces another operation on the vector: :ada:`Length`, which
retrieves the number of elements in the vector. We can use the dot
notation because :ada:`Vector` is a tagged type, allowing us to write
either :ada:`V.Length` or :ada:`Length (V)`.

Appending and prepending elements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You add elements to a vector using the :ada:`Prepend` and :ada:`Append`
operations. As the names suggest, these operations add elements to the
beginning or end of a vector, respectively. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Append

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Append is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector;
    begin
       Put_Line ("Appending some elements "
                 & "to the vector...");
       V.Append (20);
       V.Append (10);
       V.Append (0);
       V.Append (13);
       Put_Line ("Finished appending.");

       Put_Line ("Prepending some elements"
                 & "to the vector...");
       V.Prepend (30);
       V.Prepend (40);
       V.Prepend (100);
       Put_Line ("Finished prepending.");

       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length)
                 & " elements");
    end Show_Vector_Append;

This example puts elements into the vector in the following sequence: (100,
40, 30, 20, 10, 0, 13).

The Reference Manual specifies that the worst-case complexity must be:

- O(log N) for the :ada:`Append` operation, and

- O(N log N) for the :ada:`Prepend` operation.

Accessing first and last elements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We access the first and last elements of a vector using the
:ada:`First_Element` and :ada:`Last_Element` functions. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_First_Last_Element

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer)    return String
         renames Integer'Image;
       function Img (I : Count_Type) return String
         renames Count_Type'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Img (V.Length)
                 & " elements");

       --  Using V.First_Element to
       --  retrieve first element
       Put_Line ("First element is "
                 & Img (V.First_Element));

       --  Using V.Last_Element to
       --  retrieve last element
       Put_Line ("Last element is "
                 & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

You can swap elements by calling the procedure :ada:`Swap` and retrieving a
reference (a *cursor*) to the first and last elements of the vector by
calling :ada:`First` and :ada:`Last`. A cursor allows us to iterate over a
container and process individual elements from it.

With these operations, we're able to write code to swap the first and last
elements of a vector:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_First_Last_Element

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String
         renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       --  We use V.First and V.Last to retrieve
       --  cursor for first and last elements.
       --  We use V.Swap to swap elements.
       V.Swap (V.First, V.Last);

       Put_Line ("First element is now "
                 & Img (V.First_Element));
       Put_Line ("Last element is now "
                 & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Iterating
~~~~~~~~~

The easiest way to iterate over a container is to use a
:ada:`for E of Our_Container` loop. This gives us a reference (:ada:`E`) to the
element at the current position. We can then use :ada:`E` directly.
For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Iteration

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String
         renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using for ... of loop to iterate:
       --
       for E of V loop
          Put_Line ("- " & Img (E));
       end loop;

    end Show_Vector_Iteration;

This code displays each element from the vector :ada:`V`.

Because we're given a reference, we can display not only the value of an
element but also modify it. For example, we could easily write a loop to
add one to each element of vector :ada:`V`:

.. code-block:: ada

       for E of V loop
          E := E + 1;
       end loop;

We can also use indices to access vector elements. The format is
similar to a loop over array elements: we use a
:ada:`for I in <range>` loop. The range is provided by :ada:`V.First_Index` and
:ada:`V.Last_Index`. We can access the current element by using it as an
array index: :ada:`V (I)`.  For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Index_Iteration

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Index_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using indices in a "for I in ..." loop
       --  to iterate:
       --
       for I in V.First_Index .. V.Last_Index loop
          --  Displaying current index I
          Put ("- ["
               & Extended_Index'Image (I)
               & "] ");

          Put (Integer'Image (V (I)));

          --  We could also use the V.Element (I)
          --  function to retrieve the element at
          --  the current index I

          New_Line;
       end loop;

    end Show_Vector_Index_Iteration;

Here, in addition to displaying the vector elements, we're also
displaying each index, :ada:`I`, just like what we can do for array
indices. Also, we can access the element by using either the short
form :ada:`V (I)` or the longer form :ada:`V.Element (I)` but not :ada:`V.I`.

As mentioned in the previous section, you can use cursors to iterate over
containers. For this, use the function :ada:`Iterate`, which retrieves a
cursor for each position in the vector. The corresponding loop has the
format :ada:`for C in V.Iterate loop`. Like the previous example using
indices, you can again access the current element by using the cursor as an
array index: :ada:`V (C)`. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Cursor_Iteration

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Cursor_Iteration is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Use a cursor to iterate in a loop:
       --
       for C in V.Iterate loop
          --  Using To_Index function to retrieve
          --  the index for the cursor position
          Put ("- ["
               & Extended_Index'Image (To_Index (C))
               & "] ");

          Put (Integer'Image (V (C)));

          --  We could use Element (C) to retrieve
          --  the vector element for the cursor
          --  position

          New_Line;
       end loop;

       --  Alternatively, we could iterate with a
       --  while-loop:
       --
       --  declare
       --     C : Cursor := V.First;
       --  begin
       --     while C /= No_Element loop
       --        some processing here...
       --
       --        C := Next (C);
       --     end loop;
       --  end;

    end Show_Vector_Cursor_Iteration;

Instead of accessing an element in the loop using :ada:`V (C)`, we could
also have used the longer form :ada:`Element (C)`. In this example, we're
using the function :ada:`To_Index` to retrieve the index corresponding to
the current cursor.

As shown in the comments after the loop, we could also use a
:ada:`while ... loop` to iterate over the vector. In this case, we
would start with a cursor for the first element (retrieved by calling
:ada:`V.First`) and then call :ada:`Next (C)` to retrieve a cursor for
subsequent elements. :ada:`Next (C)` returns :ada:`No_Element` when the
cursor reaches the end of the vector.

You can directly modify the elements using a reference.  This is what it
looks like when using both indices and cursors:

.. code-block:: ada

       --  Modify vector elements using index
       for I in V.First_Index .. V.Last_Index loop
          V (I) := V (I) + 1;
       end loop;

       --  Modify vector elements using cursor
       for C in V.Iterate loop
          V (C) := V (C) + 1;
       end loop;

The Reference Manual requires that the worst-case complexity for
accessing an element be O(log N).

Another way of modifying elements of a vector is using a *process
procedure*, which takes an individual element and does some processing on
it.  You can call :ada:`Update_Element` and pass both a cursor and an access
to the process procedure. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Update

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Update is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Add_One (I : in out Integer) is
       begin
          I := I + 1;
       end Add_One;

       V : Vector := 20 & 10 & 12;
    begin
       --
       --  Use V.Update_Element to process elements
       --
       for C in V.Iterate loop
          V.Update_Element (C, Add_One'Access);
       end loop;

    end Show_Vector_Update;

Finding and changing elements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can locate a specific element in a vector by retrieving its index.
:ada:`Find_Index` retrieves the index of the first element matching the value
you're looking for. Alternatively, you can use :ada:`Find` to retrieve a
cursor referencing that element. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Find_Vector_Element

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_Vector_Element is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Using Find_Index to retrieve the index
       --  of element with value 10
       Idx := V.Find_Index (10);
       Put_Line ("Index of element with value 10 is "
                 & Extended_Index'Image (Idx));

       --  Using Find to retrieve the cursor for
       --  the element with value 13
       C   := V.Find (13);
       Idx := To_Index (C);
       Put_Line ("Index of element with value 13 is "
                 & Extended_Index'Image (Idx));
    end Show_Find_Vector_Element;

As we saw in the previous section, we can directly access vector elements
by using either an index or cursor. However, an exception is raised if we
try to access an element with an invalid index or cursor, so we must check
whether the index or cursor is valid before using it to access an element.
In our example, :ada:`Find_Index` or :ada:`Find` might not have found the element
in the vector.  We check for this possibility by comparing the index to
:ada:`No_Index` or the cursor to :ada:`No_Element`. For example:

.. code-block:: ada

       --  Modify vector element using index
       if Idx /= No_Index then
          V (Idx) := 11;
       end if;

       --  Modify vector element using cursor
       if C /= No_Element then
          V (C) := 14;
       end if;

Instead of writing :ada:`V (C) := 14`, we could use the longer form
:ada:`V.Replace_Element (C, 14)`.

Inserting elements
~~~~~~~~~~~~~~~~~~

In the previous sections, we've seen examples of how to add elements to a
vector:

- using the concatenation operator (:ada:`&`) at the vector declaration,
  or

- calling the :ada:`Prepend` and :ada:`Append` procedures.

You may want to insert an element at a specific position, e.g.  before a
certain element in the vector.  You do this by calling :ada:`Insert`. For
example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Insert

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Insert is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 12;
       C : Cursor;
    begin
       Show_Elements (V);

       New_Line;
       Put_Line ("Adding element with value 9");
       Put_Line ("  (before 10)...");

       --
       --  Using V.Insert to insert the element
       --  into the vector
       --
       C := V.Find (10);
       if C /= No_Element then
          V.Insert (C, 9);
       end if;

       Show_Elements (V);

    end Show_Vector_Insert;

In this example, we're looking for an element with the value of 10. If we
find it, we insert an element with the value of 9 before it.

Removing elements
~~~~~~~~~~~~~~~~~

You can remove elements from a vector by passing either a valid index or
cursor to the :ada:`Delete` procedure. If we combine this with the functions
:ada:`Find_Index` and :ada:`Find` from the previous section, we can write a
program that searches for a specific element and deletes it, if found:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Remove_Vector_Element

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Element is
       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Use Find_Index to retrieve index of
       --  the element with value 10
       Idx := V.Find_Index (10);

       --  Checking whether index is valid
       if Idx /= No_Index then
          --  Removing element using V.Delete
          V.Delete (Idx);
       end if;

       --  Use Find to retrieve cursor for
       --  the element with value 13
       C := V.Find (13);

       --  Check whether index is valid
       if C /= No_Element then
          --  Remove element using V.Delete
          V.Delete (C);
       end if;

    end Show_Remove_Vector_Element;

We can extend this approach to delete all elements matching a certain
value. We just need to keep searching for the element in a loop until we
get an invalid index or cursor. For example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Remove_Vector_Elements

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Elements is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 14 & 13;
    begin
       Show_Elements (V);

       --
       --  Remove elements using an index
       --
       declare
          E : constant Integer := 10;
          I : Extended_Index;
       begin
          New_Line;
          Put_Line
            ("Removing all elements with value of "
             & Integer'Image (E) & "...");
          loop
             I := V.Find_Index (E);
             exit when I = No_Index;
             V.Delete (I);
          end loop;
       end;

       --
       --  Remove elements using a cursor
       --
       declare
          E : constant Integer := 13;
          C : Cursor;
       begin
          New_Line;
          Put_Line
            ("Removing all elements with value of "
             & Integer'Image (E) & "...");
          loop
             C := V.Find (E);
             exit when C = No_Element;
             V.Delete (C);
          end loop;
       end;

       Show_Elements (V);
    end Show_Remove_Vector_Elements;

In this example, we remove all elements with the value 10 from the vector
by retrieving their index. Likewise, we remove all elements with the value
13 by retrieving their cursor.

Other Operations
~~~~~~~~~~~~~~~~

We've seen some operations on vector elements. Here, we'll see operations
on the vector as a whole. The most prominent is the concatenation of
multiple vectors, but we'll also see operations on vectors, such as sorting
and sorted merging operations, that view the vector as a sequence of
elements and operate on the vector considering the element's relations to
each other.

We do vector concatenation using the :ada:`&` operator on vectors.  Let's
consider two vectors :ada:`V1` and :ada:`V2`. We can concatenate them by doing
:ada:`V := V1 & V2`. :ada:`V` contains the resulting vector.

The generic package :ada:`Generic_Sorting` is a child package of
:ada:`Ada.Containers.Vectors`. It contains sorting and merging operations.
Because it's a generic package, you can't use it directly, but have to
instantiate it.  In order to use these operations on a vector of integer
values (:ada:`Integer_Vectors`, in our example), you need to instantiate it
directly as a child of :ada:`Integer_Vectors`. The next example makes it clear
how to do this.

After instantiating :ada:`Generic_Sorting`, we make all the operations
available to us with the :ada:`use` statement. We can then call :ada:`Sort` to
sort the vector and :ada:`Merge` to merge one vector into another.

The following example presents code that manipulates three vectors (:ada:`V1`,
:ada:`V2`, :ada:`V3`) using the concatenation, sorting and merging operations:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Vector_Ops

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Ops is

       package Integer_Vectors is new
         Ada.Containers.Vectors
           (Index_Type   => Natural,
            Element_Type => Integer);

       package Integer_Vectors_Sorting is
         new Integer_Vectors.Generic_Sorting;

       use Integer_Vectors;
       use Integer_Vectors_Sorting;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length)
                    & " elements");

          if not V.Is_Empty then
             Put_Line ("Vector elements are: ");
             for E of V loop
                Put_Line ("- " & Integer'Image (E));
             end loop;
          end if;
       end Show_Elements;

       V, V1, V2, V3 : Vector;
    begin
       V1 := 10 & 12 & 18;
       V2 := 11 & 13 & 19;
       V3 := 15 & 19;

       New_Line;
       Put_Line ("---- V1 ----");
       Show_Elements (V1);

       New_Line;
       Put_Line ("---- V2 ----");
       Show_Elements (V2);

       New_Line;
       Put_Line ("---- V3 ----");
       Show_Elements (V3);

       New_Line;
       Put_Line
         ("Concatenating V1, V2 and V3 into V:");

       V := V1 & V2 & V3;

       Show_Elements (V);

       New_Line;
       Put_Line ("Sorting V:");

       Sort (V);

       Show_Elements (V);

       New_Line;
       Put_Line ("Merging V2 into V1:");

       Merge (V1, V2);

       Show_Elements (V1);

    end Show_Vector_Ops;

The Reference Manual requires that the worst-case complexity of a call to
:ada:`Sort` be O(N\ :sup:`2`) and the average complexity be better than
O(N\ :sup:`2`).

Sets
----

Sets are another class of containers. While vectors allow duplicated
elements to be inserted, sets ensure that no duplicated elements exist.

In the following sections, we'll see operations you can perform on
sets. However, since many of the operations on vectors are similar to the
ones used for sets, we'll cover them more quickly here.  Please refer back
to the section on vectors for a more detailed discussion.

Initialization and iteration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To initialize a set, you can call the :ada:`Insert` procedure.  However, if
you do, you need to ensure no duplicate elements are being inserted: if you
try to insert a duplicate, you'll get an exception. If you have less
control over the elements to be inserted so that there may be duplicates,
you can use another option instead:

- a version of :ada:`Insert` that returns a Boolean value
  indicating whether the insertion was successful;

- the :ada:`Include` procedure, which silently ignores any attempt to
  insert a duplicated element.

To iterate over a set, you can use a :ada:`for E of S` loop, as you saw for
vectors. This gives you a reference to each element in the set.

Let's see an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Init

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Init is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       S : Set;
       --  Same as:  S : Integer_Sets.Set;
       C : Cursor;
       Ins : Boolean;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       --  Calling S.Insert(0) now would raise
       --  Constraint_Error because this element
       --  is already in the set. We instead call a
       --  version of Insert that doesn't raise an
       --  exception but instead returns a Boolean
       --  indicating the status

       S.Insert (0, C, Ins);
       if not Ins then
          Put_Line
            ("Error while inserting 0 into set");
       end if;

       --  We can also call S.Include instead
       --  If the element is already present,
       --  the set remains unchanged
       S.Include (0);
       S.Include (13);
       S.Include (14);

       Put_Line ("Set has "
                 & Count_Type'Image (S.Length)
                 & " elements");

       --
       --  Iterate over set using for .. of loop
       --
       Put_Line ("Elements:");
       for E of S loop
           Put_Line ("- " & Integer'Image (E));
       end loop;
    end Show_Set_Init;

Operations on elements
~~~~~~~~~~~~~~~~~~~~~~

In this section, we briefly explore the following operations on sets:

- :ada:`Delete` and :ada:`Exclude` to remove elements;

- :ada:`Contains` and :ada:`Find` to verify the existence of elements.

To delete elements, you call the procedure :ada:`Delete`.  However,
analogously to the :ada:`Insert` procedure above, :ada:`Delete` raises an
exception if the element to be deleted isn't present in the set. If you
want to permit the case where an element might not exist, you can call
:ada:`Exclude`, which silently ignores any attempt to delete a non-existent
element.

:ada:`Contains` returns a Boolean value indicating whether a value is
contained in the set. :ada:`Find` also looks for an element in a set, but
returns a cursor to the element or :ada:`No_Element` if the element doesn't
exist.  You can use either function to search for elements in a set.

Let's look at an example that makes use of these operations:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Element_Ops

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Element_Ops is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          New_Line;
          Put_Line ("Set has "
                    & Count_Type'Image (S.Length)
                    & " elements");
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       S : Set;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       S.Delete (13);

       --  Calling S.Delete (13) again raises
       --  Constraint_Error because the element
       --  is no longer present in the set, so
       --  it can't be deleted. We can call
       --  V.Exclude instead:
       S.Exclude (13);

       if S.Contains (20) then
          Put_Line ("Found element 20 in set");
       end if;

       --  Alternatively, we could use S.Find
       --  instead of S.Contains
       if S.Find (0) /= No_Element then
          Put_Line ("Found element 0 in set");
       end if;

       Show_Elements (S);
    end Show_Set_Element_Ops;

In addition to ordered sets used in the examples above, the standard
library also offers hashed sets. The Reference Manual requires the
following average complexity of each operation:

.. |LOGN_2| replace:: (log N)\ :sup:`2`

+-----------------------+-------------------------+---------------------+
| Operations            | :ada:`Ordered_Sets`     | :ada:`Hashed_Sets`  |
+=======================+=========================+=====================+
| - Insert              | O(|LOGN_2|)             | O(log N)            |
| - Include             | or better               |                     |
| - Replace             |                         |                     |
| - Delete              |                         |                     |
| - Exclude             |                         |                     |
| - Find                |                         |                     |
+-----------------------+-------------------------+---------------------+
| Subprogram using      | O(1)                    | O(1)                |
| cursor                |                         |                     |
+-----------------------+-------------------------+---------------------+

Other Operations
~~~~~~~~~~~~~~~~

The previous sections mostly dealt with operations on individual elements
of a set. But Ada also provides typical set operations: union,
intersection, difference and symmetric difference. In contrast to some
vector operations we've seen before (e.g. :ada:`Merge`), here you can use
built-in operators, such as :ada:`-`. The following table lists the
operations and its associated operator:

+-----------------------+--------------------------------+
| Set Operation         | Operator                       |
+=======================+================================+
| Union                 | :ada:`or`                      |
+-----------------------+--------------------------------+
| Intersection          | :ada:`and`                     |
+-----------------------+--------------------------------+
| Difference            | :ada:`-`                       |
+-----------------------+--------------------------------+
| Symmetric difference  | :ada:`xor`                     |
+-----------------------+--------------------------------+

The following example makes use of these operators:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Set_Ops

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Ops is

       package Integer_Sets is new
         Ada.Containers.Ordered_Sets
           (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          Put_Line ("Elements:");
          for E of S loop
             Put_Line ("- " & Integer'Image (E));
          end loop;
       end Show_Elements;

       procedure Show_Op (S       : Set;
                          Op_Name : String) is
       begin
          New_Line;
          Put_Line (Op_Name
                    & "(set #1, set #2) has "
                    & Count_Type'Image (S.Length)
                    & " elements");
       end Show_Op;

       S1, S2, S3 : Set;
    begin
       S1.Insert (0);
       S1.Insert (10);
       S1.Insert (13);

       S2.Insert (0);
       S2.Insert (10);
       S2.Insert (14);

       S3.Insert (0);
       S3.Insert (10);

       New_Line;
       Put_Line ("---- Set #1 ----");
       Show_Elements (S1);

       New_Line;
       Put_Line ("---- Set #2 ----");
       Show_Elements (S2);

       New_Line;
       Put_Line ("---- Set #3 ----");
       Show_Elements (S3);

       New_Line;
       if S3.Is_Subset (S1) then
          Put_Line ("S3 is a subset of S1");
       else
          Put_Line ("S3 is not a subset of S1");
       end if;

       S3 := S1 and S2;
       Show_Op (S3, "Intersection");
       Show_Elements (S3);

       S3 := S1 or S2;
       Show_Op (S3, "Union");
       Show_Elements (S3);

       S3 := S1 - S2;
       Show_Op (S3, "Difference");
       Show_Elements (S3);

       S3 := S1 xor S2;
       Show_Op (S3, "Symmetric difference");
       Show_Elements (S3);

    end Show_Set_Ops;

Indefinite maps
---------------

The previous sections presented containers for elements of definite
types. Although most examples in those sections presented :ada:`Integer`
types as element type of the containers, containers can also be used with
indefinite types, an example of which is the :ada:`String` type. However,
indefinite types require a different kind of containers designed specially
for them.

We'll also be exploring a different class of containers: maps. They
associate a key with a specific value. An example of a map is the
one-to-one association between a person and their age. If we consider a
person's name to be the key, the value is the person's age.

Hashed maps
~~~~~~~~~~~

Hashed maps are maps that make use of a hash as a key. The hash itself is
calculated by a function you provide.

.. admonition:: In other languages

    Hashed maps are similar to dictionaries in Python and hashes in Perl.
    One of the main differences is that these scripting languages allow
    using different types for the values contained in a single map, while
    in Ada, both the type of key and value are specified in the package
    instantiation and remains constant for that specific map.  You can't
    have a map where two elements are of different types or two keys are of
    different types.  If you want to use multiple types, you must create a
    different map for each and use only one type in each map.

When instantiating a hashed map from
:ada:`Ada.Containers.Indefinite_Hashed_Maps`, we specify following elements:

- :ada:`Key_Type`: type of the key

- :ada:`Element_Type`: type of the element

- :ada:`Hash`: hash function for the :ada:`Key_Type`

- :ada:`Equivalent_Keys`: an equality operator (e.g. :ada:`=`) that indicates
  whether two keys are to be considered equal.

  - If the type specified in :ada:`Key_Type` has a standard operator, you can
    use it, which you do by specifying that operator as the value of
    :ada:`Equivalent_Keys`.

In the next example, we'll use a string as a key type. We'll use the
:ada:`Hash` function provided by the standard library for strings (in the
:ada:`Ada.Strings` package) and the standard equality operator.

You add elements to a hashed map by calling :ada:`Insert`. If an element is
already contained in a map :ada:`M`, you can access it directly by using its
key. For example, you can change the value of an element by calling :ada:`M
("My_Key") := 10`. If the key is not found, an exception is raised.  To
verify if a key is available, use the function :ada:`Contains` (as we've seen
above in the section on sets).

Let's see an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Hashed_Map

    with Ada.Containers.Indefinite_Hashed_Maps;
    with Ada.Strings.Hash;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Hashed_Map is

       package Integer_Hashed_Maps is new
         Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => String,
            Element_Type    => Integer,
            Hash            => Ada.Strings.Hash,
            Equivalent_Keys => "=");

       use Integer_Hashed_Maps;

       M : Map;
       --  Same as:
       --
       --  M : Integer_Hashed_Maps.Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M.
       --  Otherwise an exception is raised.
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": "
                    & Integer'Image (M (C)));
       end loop;

    end Show_Hashed_Map;

Ordered maps
~~~~~~~~~~~~

Ordered maps share many features with hashed maps. The main differences are:

- A hash function isn't needed. Instead, you must provide an ordering
  function (:ada:`<` operator), which the ordered map will use to order
  elements and allow fast access, O(log N), using a binary search.

  - If the type specified in :ada:`Key_Type` has a standard :ada:`<` operator,
    you can use it in a similar way as we did for :ada:`Equivalent_Keys` above
    for hashed maps.

Let's see an example:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Ordered_Map

    with Ada.Containers.Indefinite_Ordered_Maps;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Ordered_Map is

       package Integer_Ordered_Maps is new
         Ada.Containers.Indefinite_Ordered_Maps
           (Key_Type        => String,
            Element_Type    => Integer);

       use Integer_Ordered_Maps;

       M : Map;
    begin
       M.Include ("Alice", 24);
       M.Include ("John",  40);
       M.Include ("Bob",   28);

       if M.Contains ("Alice") then
          Put_Line ("Alice's age is "
                    & Integer'Image (M ("Alice")));
       end if;

       --  Update Alice's age
       --  Key must already exist in M
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": "
                    & Integer'Image (M (C)));
       end loop;

    end Show_Ordered_Map;

You can see a great similarity between the examples above and from the
previous section. In fact, since both kinds of maps share many operations,
we didn't need to make extensive modifications when we changed our example
to use ordered maps instead of hashed maps. The main difference is seen
when we run the examples: the output of a hashed map is usually unordered,
but the output of a ordered map is always ordered, as implied by its name.

Complexity
~~~~~~~~~~

Hashed maps are generally the fastest data structure available to you in
Ada if you need to associate heterogeneous keys to values and search for
them quickly. In most cases, they are slightly faster than ordered maps.
So if you don't need ordering, use hashed maps.

The Reference Manual requires the following average complexity of
operations:

+-----------------------+-------------------------+---------------------+
| Operations            | :ada:`Ordered_Maps`     | :ada:`Hashed_Maps`  |
+=======================+=========================+=====================+
| - Insert              | O(|LOGN_2|) or better   | O(log N)            |
| - Include             |                         |                     |
| - Replace             |                         |                     |
| - Delete              |                         |                     |
| - Exclude             |                         |                     |
| - Find                |                         |                     |
+-----------------------+-------------------------+---------------------+
| Subprogram using      | O(1)                    | O(1)                |
| cursor                |                         |                     |
+-----------------------+-------------------------+---------------------+
