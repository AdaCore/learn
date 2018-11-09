Standard library
================

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Standard package
----------------

.. _Containers:

Containers
----------

Ada's standard library includes support for containers (such as
vectors and sets). We present an introduction to them here. For a list
of all containers available in Ada, see Appendix B.

Vectors
~~~~~~~

In the following sections, we present a general overview of vectors,
including instantiation, initialization, and operations on vector
elements and vectors.

Instantiation
^^^^^^^^^^^^^

Here's an example showing the instantiation and declaration of a
vector ``V``:

.. code:: ada

    with Ada.Containers.Vectors;

    procedure Show_Vector_Inst is

       package Integer_Vectors is new Ada.Containers.Vectors
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

In the instantiation of ``Integer_Vectors``, we indicate that the vector
contains elements of ``Integer`` type by specifying it as the
``Element_Type``.  By setting ``Index_Type`` to ``Natural``, we specify
that the allowed range includes all natural numbers. We could have used a
more restrictive range if desired.

Initialization
^^^^^^^^^^^^^^

One way to initialize a vector is from a concatenation of elements.
We use the :ada:`&` operator, as shown in the following example:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Init is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length) & " elements");
    end Show_Vector_Init;

We specify :ada:`use Integer_Vectors`, so we have direct access to the
types and operations from the instantiated package. Also, the example
introduces another operation on the vector: ``Length``, which
retrieves the number of elements in the vector. We can use the dot
notation because ``Vector`` is a tagged type, allowing us to write
either ``V.Length`` or ``Length (V)``.

Appending and prepending elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You add elements to a vector using the ``Prepend`` and ``Append``
operations. As the names suggest, these operations add elements to the
beginning or end of a vector, respectively. For example:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Append is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector;
    begin
       Put_Line ("Appending some elements to the vector...");
       V.Append (20);
       V.Append (10);
       V.Append (0);
       V.Append (13);
       Put_Line ("Finished appending.");

       Put_Line ("Prepending some elements to the vector...");
       V.Prepend (30);
       V.Prepend (40);
       V.Prepend (100);
       Put_Line ("Finished prepending.");

       Put_Line ("Vector has "
                 & Count_Type'Image (V.Length) & " elements");
    end Show_Vector_Append;

This example puts elements into the vector in the following sequence: (100,
40, 30, 20, 10, 0, 13).

The Reference Manual specifies that the worst-case complexity must be:

- O(:math:`log N`) for the ``Append`` operation, and

- O(:math:`N log N`) for the ``Prepend`` operation.

Accessing first and last elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We access the first and last elements of a vector using the
``First_Element`` and ``Last_Element`` functions. For example:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer)    return String renames Integer'Image;
       function Img (I : Count_Type) return String renames Count_Type'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector has " & Img (V.Length) & " elements");

       --  Using V.First_Element to retrieve first element
       Put_Line ("First element is " & Img (V.First_Element));

       --  Using V.Last_Element to retrieve last element
       Put_Line ("Last element is " & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

You can swap elements by calling the procedure ``Swap`` and retrieving a
reference (a *cursor*) to the first and last elements of the vector by
calling ``First`` and ``Last``. A cursor allows us to iterate over a
container and process individual elements from it.

With these operations, we're able to write code to swap the first and last
elements of a vector:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_First_Last_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String renames Integer'Image;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       --  We use V.First and V.Last to retrieve cursor for first and
       --  last elements.
       --  We use V.Swap to swap elements.
       V.Swap (V.First, V.Last);

       Put_Line ("First element is now " & Img (V.First_Element));
       Put_Line ("Last element is now " & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Iterating
^^^^^^^^^

The easiest way to iterate over a container is to use a :ada:`for E of
Our_Container` loop. This gives us a reference (``E``) to the element
at the current position. We can then use ``E`` directly.  For example:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       function Img (I : Integer) return String renames Integer'Image;

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

This code displays each element from the vector ``V``.

Because we're given a reference, we can display not only the value of an
element but also modify it. For example, we could easily write a loop to
add one to each element of vector ``V``:

.. code-block:: ada

       for E of V loop
          E := E + 1;
       end loop;

We can also use indices to access vector elements. The format is
similar to a loop over array elements: we use a :ada:`for I in
<range>` loop. The range is provided by ``V.First_Index`` and
``V.Last_Index``. We can access the current element by using it as an
array index: ``V (I)``.  For example:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Index_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
    begin
       Put_Line ("Vector elements are: ");

       --
       --  Using indices in a "for I in ..." loop to iterate:
       --
       for I in V.First_Index .. V.Last_Index loop
          --  Displaying current index I
          Put ("- ["
               & Extended_Index'Image (I)
               & "] ");

          Put (Integer'Image (V (I)));

          --  We could also use the V.Element (I) function to retrieve the
          --  element at the current index I

          New_Line;
       end loop;

    end Show_Vector_Index_Iteration;

Here, in addition to displaying the vector elements, we're also
displaying each index, ``I``, just like what we can do for array
indices. Also, we can access the element by using either the short
form ``V (I)`` or the longer form ``V.Element (I)`` but not ``V.I``.

As mentioned in the previous section, you can use cursors to iterate over
containers. For this, use the function ``Iterate``, which retrieves a
cursor for each position in the vector. The corresponding loop has the
format :ada:`for C in V.Iterate loop`. Like the previous example using
indices, you can again access the current element by using the cursor as an
array index: ``V (C)``. For example:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Cursor_Iteration is

       package Integer_Vectors is new Ada.Containers.Vectors
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
          --  Using To_Index function to retrieve index
          --  for the cursor position
          Put ("- ["
               & Extended_Index'Image (To_Index (C))
               & "] ");

          Put (Integer'Image (V (C)));

          --  We could use Element (C) to retrieve the vector
          --  element for the cursor position

          New_Line;
       end loop;

       --  Alternatively, we could iterate with a while-loop:
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

Instead of accessing an element in the loop using ``V (C)``, we could
also have used the longer form ``Element (C)``. In this example, we're
using the function ``To_Index`` to retrieve the index corresponding to
the current cursor.

As shown in the comments after the loop, we could also use a
:ada:`while ... loop` to iterate over the vector. In this case, we
would start with a cursor for the first element (retrieved by calling
``V.First``) and then call ``Next (C)`` to retrieve a cursor for
subsequent elements. ``Next (C)`` returns ``No_Element`` when the
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
accessing an element be O(:math:`log N`).

Another way of modifing elements of a vector is using a *process
procedure*, which takes an individual element and does some processing on
it.  You can call ``Update_Element`` and pass both a cursor and an access
to the process procedure. For example:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Update is

       package Integer_Vectors is new Ada.Containers.Vectors
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can locate a specific element in a vector by retrieving its index.
``Find_Index`` retrieves the index of the first element matching the value
you're looking for. Alternatively, you can use ``Find`` to retrieve a
cursor referencing that element. For example:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Find_Vector_Element is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Using Find_Index to retrieve index of element with value 10
       Idx := V.Find_Index (10);
       Put_Line ("Index of element with value 10 is "
                 & Extended_Index'Image (Idx));

       --  Using Find to retrieve cursor for element with value 13
       C   := V.Find (13);
       Idx := To_Index (C);
       Put_Line ("Index of element with value 13 is "
                 & Extended_Index'Image (Idx));
    end Show_Find_Vector_Element;

As we saw in the previous section, we can directly access vector elements
by using either an index or cursor. However, an exception is raised if we
try to access an element with an invalid index or cursor, so we must check
whether the index or cursor is valid before using it to access an element.
In our example, ``Find_Index`` or ``Find`` might not have found the element
in the vector.  We check for this possibility by comparing the index to
``No_Index`` or the cursor to ``No_Element``. For example:

.. code-block:: ada

       --  Modify vector element using index
       if Idx /= No_Index then
          V (Idx) := 11;
       end if;

       --  Modify vector element using cursor
       if C /= No_Element then
          V (C) := 14;
       end if;

Instead of writing ``V (C) := 14``, we could use the longer form
:ada:`V.Replace_Element (C, 14)`.

Inserting elements
^^^^^^^^^^^^^^^^^^

In the previous sections, we've seen examples of how to add elements to a
vector:

- using the concatenation operator (:ada:`&`) at the vector declaration,
  or

- calling the ``Prepend`` and ``Append`` procedures.

You may want to insert an element at a specific position, e.g.  before a
certain element in the vector.  You do this by calling ``Insert``. For
example:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Insert is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has "
                    & Count_Type'Image (V.Length) & " elements");

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
       Put_Line ("Adding element with value 9 (before 10)...");

       --
       --  Using V.Insert to insert element into vector
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
^^^^^^^^^^^^^^^^^

You can remove elements from a vector by passing either a valid index or
cursor to the ``Delete`` procedure. If we combine this with the functions
``Find_Index`` and ``Find`` from the previous section, we can write a
program that searches for a specific element and deletes it, if found:

.. code:: ada

    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Element is
       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       V : Vector := 20 & 10 & 0 & 13 & 10 & 13;
       Idx : Extended_Index;
       C   : Cursor;
    begin
       --  Use Find_Index to retrieve index of element with value 10
       Idx := V.Find_Index (10);

       --  Checking whether index is valid
       if Idx /= No_Index then
          --  Removing element using V.Delete
          V.Delete (Idx);
       end if;

       --  Use Find to retrieve cursor for element with value 13
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

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Remove_Vector_Elements is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       use Integer_Vectors;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has " & Count_Type'Image (V.Length) & " elements");

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
          Put_Line ("Removing all elements with value of "
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
          Put_Line ("Removing all elements with value of "
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
^^^^^^^^^^^^^^^^

We've seen some operations on vector elements. Here, we'll see operations
on the vector as a whole. The most prominent is the concatenation of
multiple vectors, but we'll also see operations on vectors, such as sorting
and sorted merging operations, that view the vector as a sequence of
elements and operate on the vector considering the element's relations to
each other.

We do vector concatenation using the :ada:`&` operator on vectors.  Let's
consider two vectors ``V1`` and ``V2``. We can concatenate them by doing
:ada:`V := V1 & V2`. ``V`` contains the resulting vector.

The generic package ``Generic_Sorting`` is a child package of
``Ada.Containers.Vectors``. It contains sorting and merging operations.
Because it's a generic package, you can't use it directly, but have to
instantiate it.  In order to use these operations on a vector of integer
values (``Integer_Vectors``, in our example), you need to instantiate it
directly as a child of ``Integer_Vectors``. The next example makes it clear
how to do this.

After instantiating ``Generic_Sorting``, we make all the operations
available to us with the :ada:`use` statement. We can then call ``Sort`` to
sort the vector and ``Merge`` to merge one vector into another.

The following example presents code that manipulates three vectors (``V1``,
``V2``, ``V3``) using the concatenation, sorting and merging operations:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Vectors;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Vector_Ops is

       package Integer_Vectors is new Ada.Containers.Vectors
         (Index_Type   => Natural,
          Element_Type => Integer);

       package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

       use Integer_Vectors;
       use Integer_Vectors_Sorting;

       procedure Show_Elements (V : Vector) is
       begin
          New_Line;
          Put_Line ("Vector has " & Count_Type'Image (V.Length) & " elements");

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
       Put_Line ("Concatenating V1, V2 and V3 into V:");

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
``Sort`` be O(:math:`N^2`) and the average complexity be better than
O(:math:`N^2`).

Sets
~~~~

Sets are another class of containers. While vectors allow duplicated
elements to be inserted, sets ensure that no duplicated elements exist.

In the following sections, we'll see operations you can perform on
sets. However, since many of the operations on vectors are similar to the
ones used for sets, we'll cover them more quickly here.  Please refer back
to the section on vectors for a more detailed discussion.

Initialization and iteration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To initialize a set, you can call the ``Insert`` procedure.  However, if
you do, you need to ensure no duplicate elements are being inserted: if you
try to insert a duplicate, you'll get an exception. If you have less
control over the elements to be inserted so that there may be duplicates,
you can use another option instead:

- a version of ``Insert`` that returns a Boolean value
  indicating whether the insertion was successful;

- the ``Include`` procedure, which silently ignores any attempt to
  insert a duplicated element.

To iterate over a set, you can use a :ada:`for E of S` loop, as you saw for
vectors. This gives you a reference to each element in the set.

Let's see an example:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Init is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
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

       --  Calling S.Insert(0) now would raise Constraint_Error
       --  because this element is already in the set.
       --  We instead call a version of Insert that doesn't raise an
       --  exception but instead returns a Boolean indicating the status

       S.Insert (0, C, Ins);
       if not Ins then
          Put_Line ("Inserting 0 into set was not successful");
       end if;

       --  We can also call S.Include instead
       --  If the element is already present, the set remains unchanged
       S.Include (0);
       S.Include (13);
       S.Include (14);

       Put_Line ("Set has " & Count_Type'Image (S.Length) & " elements");

       --
       --  Iterate over set using for .. of loop
       --
       Put_Line ("Elements:");
       for E of S loop
           Put_Line ("- " & Integer'Image (E));
       end loop;
    end Show_Set_Init;

Operations on elements
^^^^^^^^^^^^^^^^^^^^^^

In this section, we briefly explore the following operations on sets:

- ``Delete`` and ``Exclude`` to remove elements;

- ``Contains`` and ``Find`` to verify the existence of elements.

To delete elements, you call the procedure ``Delete``.  However,
analogously to the ``Insert`` procedure above, ``Delete`` raises an
exception if the element to be deleted isn't present in the set. If you
want to permit the case where an element might not exist, you can call
``Exclude``, which silently ignores any attempt to delete a non-existent
element.

``Contains`` returns a Boolean value indicating whether a value is
contained in the set. ``Find`` also looks for an element in a set, but
returns a cursor to the element or ``No_Element`` if the element doesn't
exist.  You can use either function to search for elements in a set.

Let's look at an example that makes use of these operations:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Element_Ops is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
         (Element_Type => Integer);

       use Integer_Sets;

       procedure Show_Elements (S : Set) is
       begin
          New_Line;
          Put_Line ("Set has " & Count_Type'Image (S.Length) & " elements");
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

       --  Calling S.Delete (13) again raises Constraint_Error
       --  because the element is no longer present
       --  in the set, so it can't be deleted.
       --  We can call V.Exclude instead:
       S.Exclude (13);

       if S.Contains (20) then
          Put_Line ("Found element 20 in set");
       end if;

       --  Alternatively, we could use S.Find instead of S.Contains
       if S.Find (0) /= No_Element then
          Put_Line ("Found element 0 in set");
       end if;

       Show_Elements (S);
    end Show_Set_Element_Ops;

In addition to ordered sets used in the examples above, the standard
library also offers hashed sets. The Reference Manual requires the
following average complexity of each operation:

+-----------------------+----------------------+------------------+
| Operations            | ``Ordered_Sets``     | ``Hashed_Sets``  |
+=======================+======================+==================+
| - Insert              | O(:math:`(log N)^2)` | :math:`O(log N)` |
| - Include             | or better            |                  |
| - Replace             |                      |                  |
| - Delete              |                      |                  |
| - Exclude             |                      |                  |
| - Find                |                      |                  |
+-----------------------+----------------------+------------------+
| Subprogram using      | O(:math:`1`)         | O(:math:`1`)     |
| cursor                |                      |                  |
+-----------------------+----------------------+------------------+

Other Operations
^^^^^^^^^^^^^^^^

The previous sections mostly dealt with operations on individual elements
of a set. But Ada also provides typical set operations: union,
intersection, difference and symmetric difference. In contrast to some
vector operations we've seen before (e.g. ``Merge``), here you can use
built-in operators, such as :ada:`-`. The following table lists the
operations and its associated operator:

+-----------------------+--------------------------------+
| Set Operation         | Operator                       |
+=======================+================================+
| Union                 | :ada:`and`                     |
+-----------------------+--------------------------------+
| Intersection          | :ada:`or`                      |
+-----------------------+--------------------------------+
| Difference            | :ada:`-`                       |
+-----------------------+--------------------------------+
| Symmetric difference  | :ada:`xor`                     |
+-----------------------+--------------------------------+

The following example makes use of these operators:

.. code:: ada

    with Ada.Containers; use Ada.Containers;
    with Ada.Containers.Ordered_Sets;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Set_Ops is

       package Integer_Sets is new Ada.Containers.Ordered_Sets
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
          Put_Line (Op_Name & "(set #1, set #2) has "
                    & Count_Type'Image (S.Length) & " elements");
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
       Show_Op (S3, "Union");
       Show_Elements (S3);

       S3 := S1 or S2;
       Show_Op (S3, "Intersection");
       Show_Elements (S3);

       S3 := S1 - S2;
       Show_Op (S3, "Difference");
       Show_Elements (S3);

       S3 := S1 xor S2;
       Show_Op (S3, "Symmetric difference");
       Show_Elements (S3);

    end Show_Set_Ops;

Indefinite maps
~~~~~~~~~~~~~~~

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
^^^^^^^^^^^

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
``Ada.Containers.Indefinite_Hashed_Maps``, we specify following elements:

- ``Key_Type``: type of the key

- ``Element_Type``: type of the element

- ``Hash``: hash function for the ``Key_Type``

- ``Equivalent_Keys``: an equality operator (e.g. ``=``) that indicates
  whether two keys are to be considered equal.

  - If the type specified in ``Key_Type`` has a standard operator, you can
    use it, which you do by specifing using that operator as the value of
    ``Equivalent_Keys``.

In the next example, we'll use a string as a key type. We'll use the
``Hash`` function provided by the standard library for strings (in the
``Ada.Strings`` package) and the standard equality operator.

You add elements to a hashed map by calling ``Insert``. If an element is
already contained in a map ``M``, you can access it directly by using its
key. For example, you can change the value of an element by calling :ada:`M
("My_Key") := 10`. If the key is not found, an exception is raised.  To
verify if a key is available, use the function ``Contains`` (as we've seen
above in the section on sets).

Let's see an example:

.. code:: ada

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
       --  Same as:  M : Integer_Hashed_Maps.Map;
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
          Put_Line (Key (C) & ": " & Integer'Image (M (C)));
       end loop;

    end Show_Hashed_Map;

Ordered maps
^^^^^^^^^^^^

Ordered maps share many features with hashed maps. The main differences are:

- A hash function isn't needed. Instead, you must provide an ordering
  function (``<`` operator), which the ordered map will use to order
  elements and allow fast access, :math:`O(log n)`, using a binary search.

  - If the type specified in ``Key_Type`` has a standard ``<`` operator, you
    can use it in a similar way as we did for ``Equivalent_Keys`` above for
    hashed maps.

Let's see an example:

.. code:: ada

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
          Put_Line (Key (C) & ": " & Integer'Image (M (C)));
       end loop;

    end Show_Ordered_Map;

You can see a great similarity between the examples above and from the
previous section. In fact, since both kinds of maps share many operations,
we didn't need to make extensive modifications when we changed our example
to use ordered maps instead of hashed maps. The main difference is seen
when we run the examples: the output of a hashed map is usually unordered,
but the output of a ordered map is always ordered, as implied by its name.

Complexity
^^^^^^^^^^

Hashed maps are generally the fastest data structure available to you in
Ada if you need to associate heterogeneous keys to values and search for
them quickly. In most cases, they are slightly faster than ordered maps.
So if you don't need ordering, use hashed maps.

The Reference Manual requires the following average complexity of
operations:

+-----------------------+----------------------+------------------+
| Operations            | ``Ordered_Maps``     | ``Hashed_Maps``  |
+=======================+======================+==================+
| - Insert              | O(:math:`(log N)^2)` | :math:`O(log N)` |
| - Include             | or better            |                  |
| - Replace             |                      |                  |
| - Delete              |                      |                  |
| - Exclude             |                      |                  |
| - Find                |                      |                  |
+-----------------------+----------------------+------------------+
| Subprogram using      | O(:math:`1`)         | O(:math:`1`)     |
| cursor                |                      |                  |
+-----------------------+----------------------+------------------+

Dates & Times
-------------

Date and time handling
~~~~~~~~~~~~~~~~~~~~~~

The standard library supports representing and handling dates and
times. This is part of the :ada:`Ada.Calendar` package. Let's look at a
simple example:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Time is
       Now : Time := Clock;
    begin
       Put_Line ("Current time: " & Image (Now));
    end Display_Current_Time;

This example displays the current date and time, which is retrieved by a
call to the ``Clock`` function. We call the function ``Image`` from the
:ada:`Ada.Calendar.Formatting` package to get a ``String`` for the current
date and time. We could instead retrieve each component using the ``Split``
function. For example:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Year is
       Now         : Time := Clock;

       Now_Year    : Year_Number;
       Now_Month   : Month_Number;
       Now_Day     : Day_Number;
       Now_Seconds : Day_Duration;
    begin
       Split (Now,
              Now_Year,
              Now_Month,
              Now_Day,
              Now_Seconds);

       Put_Line ("Current year  is: " & Year_Number'Image (Now_Year));
       Put_Line ("Current month is: " & Month_Number'Image (Now_Month));
       Put_Line ("Current day   is: " & Day_Number'Image (Now_Day));
    end Display_Current_Year;

Here, we're retrieving each element and displaying it separately.

Delaying using date
^^^^^^^^^^^^^^^^^^^

You can delay an application so that it restarts at a specific date and
time. We saw something similar in the chapter on tasking.  You do this
using a :ada:`delay until` statement. For example:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        := Ada.Calendar.Formatting.Time_Of
         (Year        => 2018,
          Month       => 5,
          Day         => 1,
          Hour        => 15,
          Minute      => 0,
          Second      => 0,
          Sub_Second  => 0.0,
          Leap_Second => False,
          Time_Zone   => TZ);

       --  Next = 2018-05-01 15:00:00.00 (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this example, we specify the date and time by initializing ``Next``
using a call to ``Time_Of``, a function taking the various components
of a date (year, month, etc) and returning an element of the ``Time``
type. Because the date specified is in the past, the :ada:`delay
until` statement won't produce any noticeable effect. However, if we
passed a date in the future, the program would wait until that
specific date and time arrived.

Here we're converting the time to the local timezone. If we don't specify a
timezone, *Coordinated Universal Time* (abbreviated to UTC) is used by
default. By retrieving the time offset to UTC with a call to
``UTC_Time_Offset`` from the :ada:`Ada.Calendar.Time_Zones` package, we can
initialize ``TZ`` and use it in the call to ``Time_Of``.  This is all we
need do to make the information provided to ``Time_Of`` relative to the
local time zone.

We could achieve a similar result by initializing ``Next`` with a
``String``. We can do this with a call to ``Value`` from the
:ada:`Ada.Calendar.Formatting` package. This is the modified code:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next_Specific_Time is
       TZ   : Time_Offset := UTC_Time_Offset;
       Next : Time        := Ada.Calendar.Formatting.Value
         ("2018-05-01 15:00:00.00", TZ);

       --  Next = 2018-05-01 15:00:00.00 (local time-zone)
    begin
       Put_Line ("Let's wait until...");
       Put_Line (Image (Next, True, TZ));

       delay until Next;

       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Specific_Time;

In this example, we're again using ``TZ`` in the call to ``Value`` to
adjust the input time to the current time zone.

In the examples above, we were delaying to a specific date and time.
Just like we saw in the tasking chapter, we could instead specify the
delay relative to the current time. For example, we could delay by 5
seconds, using the current time:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Delay_Next is
       D    : Duration := 5.0;       --  seconds
       Now  : Time     := Clock;
       Next : Time     := Now + D;   --  use duration to
                                     --  specify next point in time
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (D) & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next;

Here, we're specifying a duration of 5 seconds in ``D``, adding it to the
current time from ``Now``, and storing the sum in ``Next``. We then use it
in the :ada:`delay until` statement.

Real-time
~~~~~~~~~

In addition to :ada:`Ada.Calendar`, the standard library also supports time
operations for real-time applications. These are included in the
:ada:`Ada.Real_Time` package. This package also include a ``Time`` type.
However, in the :ada:`Ada.Real_Time` package, the ``Time`` type is used to
represent an absolute clock and handle a time span. This contrasts with the
:ada:`Ada.Calendar`, which uses the ``Time`` type to represent dates and
times.

In the previous section, we used the ``Time`` type from the
:ada:`Ada.Calendar` and the :ada:`delay until` statement to delay an
application by 5 seconds. We could have used the :ada:`Ada.Real_Time`
package instead. Let's modify that example:

.. code:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Delay_Next_Real_Time is
       D     : Time_Span := Seconds (5);
       Next  : Time      := Clock + D;
    begin
       Put_Line ("Let's wait "
                 & Duration'Image (To_Duration (D)) & " seconds...");
       delay until Next;
       Put_Line ("Enough waiting!");
    end Display_Delay_Next_Real_Time;

The main difference is that ``D`` is now a variable of type ``Time_Span``,
defined in the :ada:`Ada.Real_Time` package. We call the function
``Seconds`` to initialize ``D``, but could have gotten a finer granularity
by calling ``Nanoseconds`` instead. Also, we need to first convert ``D`` to
the ``Duration`` type using ``To_Duration`` before we can display it.

Benchmarking
^^^^^^^^^^^^

One interesting application using the :ada:`Ada.Real_Time` package is
benchmarking. We've used that package before in a previous section when
discussing tasking. Let's look at an example of benchmarking:

.. code:: ada

    with Ada.Text_IO;   use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    procedure Display_Benchmarking is

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("Elapsed time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking;

This example defines a dummy ``Computational_Intensive_App`` implemented
using a simple :ada:`delay` statement. We initialize ``Start_Time`` and
``Stop_Time`` from the then-current clock and calculate the elapsed
time. By running this program, we see that the time is roughly 5 seconds,
which is expected due to the :ada:`delay` statement.

A similar application is benchmarking of CPU time.  We can implement this
using the :ada:`Execution_Time` package. Let's modify the previous example
to measure CPU time:

.. code:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    procedure Display_Benchmarking_CPU_Time is

       procedure Computational_Intensive_App is
       begin
          delay 0.5;
       end Computational_Intensive_App;

       Start_Time, Stop_Time : CPU_Time;
       Elapsed_Time          : Time_Span;

    begin
       Start_Time := Clock;

       Computational_Intensive_App;

       Stop_Time    := Clock;
       Elapsed_Time := Stop_Time - Start_Time;

       Put_Line ("CPU time: "
                 & Duration'Image (To_Duration (Elapsed_Time))
                 & " seconds");
    end Display_Benchmarking_CPU_Time;

In this example, ``Start_Time`` and ``Stop_Time`` are of type ``CPU_Time``
instead of ``Time``. However, we still call the ``Clock`` function to
initialize both variables and calculate the elapsed time in the same way as
before. By running this program, we see that the CPU time is significantly
lower than the 5 seconds we've seen before. This is because the
:ada:`delay` statement doesn't require much CPU time.  The results will be
different if we change the implementation of
``Computational_Intensive_App`` to use a mathematical functions in a long
loop. For example:

.. code:: ada

    with Ada.Text_IO;        use Ada.Text_IO;
    with Ada.Real_Time;      use Ada.Real_Time;
    with Ada.Execution_Time; use Ada.Execution_Time;

    with Ada.Numerics.Generic_Elementary_Functions;

    procedure Display_Benchmarking_Math is

       procedure Computational_Intensive_App is
          package Funcs is new Ada.Numerics.Generic_Elementary_Functions
            (Float_Type => Long_Long_Float);
          use Funcs;

          X : Long_Long_Float;
       begin
          for I in 0 .. 1_000_000 loop
             X := Tan (Arctan
                       (Tan (Arctan
                          (Tan (Arctan
                             (Tan (Arctan
                                (Tan (Arctan
                                   (Tan (Arctan
                                      (0.577))))))))))));
          end loop;
       end Computational_Intensive_App;

       procedure Benchm_Elapsed_Time is
          Start_Time, Stop_Time : Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("Elapsed time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_Elapsed_Time;

       procedure Benchm_CPU_Time is
          Start_Time, Stop_Time : CPU_Time;
          Elapsed_Time          : Time_Span;

       begin
          Start_Time := Clock;

          Computational_Intensive_App;

          Stop_Time    := Clock;
          Elapsed_Time := Stop_Time - Start_Time;

          Put_Line ("CPU time: "
                    & Duration'Image (To_Duration (Elapsed_Time))
                    & " seconds");
       end Benchm_CPU_Time;
    begin
       Benchm_Elapsed_Time;
       Benchm_CPU_Time;
    end Display_Benchmarking_Math;

Now that our dummy ``Computational_Intensive_App`` involves mathematical
operations requiring significant CPU time, the measured elapsed and CPU
time are much closer to each other than before.

Strings
--------

We've used strings in many previous examples. In this section, we'll cover
them in more detail.

String operations
~~~~~~~~~~~~~~~~~

Operations on standard strings are available in the
:ada:`Ada.Strings.Fixed` package. As mentioned previously, standard strings
are arrays of elements of :ada:`Character` type with *a
fixed-length*. That's why this child package is called ``Fixed``.

One of the simplest operations provided is counting the number of
substrings available in a string (``Count``) and finding their
corresponding indices (``Index``). Let's look at an example:

.. code:: ada

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Substring is

       S   : String := "Hello" & 3 * " World";
       P   : constant String := "World";
       Idx : Natural;
       Cnt : Natural;
    begin
       Cnt := Ada.Strings.Fixed.Count
         (Source  => S,
          Pattern => P);

       Put_Line ("String: " & S);
       Put_Line ("Count for '" & P & "': " & Natural'Image (Cnt));

       Idx := 0;
       for I in 1 .. Cnt loop
          Idx := Index
            (Source  => S,
             Pattern => P,
             From    => Idx + 1);

          Put_Line ("Found instance of '" & P & "' at position: "
                    & Natural'Image (Idx));
       end loop;

    end Show_Find_Substring;

We initialize the string ``S`` using a multiplication. Writing
:ada:`"Hello" & 3 * " World"` creates the string ``Hello World World
World``. We then call the function ``Count`` to get the number of instances
of the word ``World`` in ``S``.  Next we call the function ``Index`` in a
loop to find the index of each instance of ``World`` in ``S``.

That example looked for instances of a specific substring.  In the next
example, we retrieve all the words in the string. We do this using
``Find_Token`` and specifying whitespaces as separators. For example:

.. code:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Find_Words is

       S   : String := "Hello" & 3 * " World";
       F   : Positive;
       L   : Natural;
       I   : Natural := 1;

       Whitespace : constant Character_Set :=
         To_Set (' ');
    begin
       Put_Line ("String: " & S);
       Put_Line ("String length: " & Integer'Image (S'Length));

       while I in S'Range loop
          Find_Token
            (Source  => S,
             Set     => Whitespace,
             From    => I,
             Test    => Outside,
             First   => F,
             Last    => L);

          exit when L = 0;

          Put_Line ("Found word instance at position "
                    & Natural'Image (F)
                    & ": '" & S (F .. L) & "'");
          --   & "-" & F'Img & "-" & L'Img

          I := L + 1;
       end loop;
    end Show_Find_Words;

We pass a set of characters to be used as delimitators to the procedure
``Find_Token``. This set is a member of the ``Character_Set`` type from the
:ada:`Ada.Strings.Maps` package. We call the ``To_Set`` function (from the
same package) to initialize the set to ``Whitespace`` and then call
``Find_Token`` to loop over each valid index and find the starting index of
each word. We pass ``Outside`` to the ``Test`` parameter of the
``Find_Token`` procedure to indicate that we're looking for indices that
are outside the ``Whitespace`` set, i.e. actual words. The ``First`` and
``Last`` parameters of ``Find_Token`` are output parameters that indicate
the valid range of the substring. We use this information to display the
string (:ada:`S (F .. L)`).

The operations we've looked at so far read strings, but don't modify
them. We next discuss operations that change the content of strings:

+-----------------------+-----------------------------------------+
| Operation             | Description                             |
+=======================+=========================================+
| Insert                | Insert substring in a string            |
+-----------------------+-----------------------------------------+
| Overwrite             | Overwrite a string with a substring     |
+-----------------------+-----------------------------------------+
| Delete                | Delete a substring                      |
+-----------------------+-----------------------------------------+
| Trim                  | Remove whitespaces from a string        |
+-----------------------+-----------------------------------------+

All these operations are available both as functions or procedures.
Functions create a new string but procedures perform the operations in
place. The procedure will raise an exception if the constraints of the
string are not satisfied. For example, if we have a string ``S`` containing
10 characters, inserting a string with two characters (e.g. ``"!!"``) into
it produces a string containing 12 characters. Since it has a fixed length,
we can't increase its size. One possible solution in this case is to
specify that truncation should be applied while inserting the substring.
This keeps the length of ``S`` fixed. Let's see an example that makes use
of both function and procedure versions of ``Insert``, ``Overwrite``, and
``Delete``:

.. code:: ada

    with Ada.Strings;       use Ada.Strings;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Text_IO;       use Ada.Text_IO;

    procedure Show_Adapted_Strings is

       S   : String := "Hello World";
       P   : constant String := "World";
       N   : constant String := "Beautiful";

       procedure Display_Adapted_String
         (Source   : String;
          Before   : Positive;
          New_Item : String;
          Pattern  : String)
       is
          S_Ins_In : String := Source;
          S_Ovr_In : String := Source;
          S_Del_In : String := Source;

          S_Ins : String := Insert (Source, Before, New_Item & " ");
          S_Ovr : String := Overwrite (Source, Before, New_Item);
          S_Del : String := Trim (Delete (Source,
                                          Before,
                                          Before + Pattern'Length - 1),
                                  Ada.Strings.Right);
       begin
          Insert (S_Ins_In,    Before, New_Item, Right);
          Overwrite (S_Ovr_In, Before, New_Item, Right);
          Delete (S_Del_In,    Before, Before + Pattern'Length - 1);

          Put_Line ("Original:  '" & Source & "'");

          Put_Line ("Insert:    '" & S_Ins  & "'");
          Put_Line ("Overwrite: '" & S_Ovr  & "'");
          Put_Line ("Delete:    '" & S_Del  & "'");

          Put_Line ("Insert    (in-place): '" & S_Ins_In & "'");
          Put_Line ("Overwrite (in-place): '" & S_Ovr_In & "'");
          Put_Line ("Delete    (in-place): '" & S_Del_In & "'");
       end Display_Adapted_String;

       Idx : Natural;
    begin
       Idx := Index
         (Source  => S,
          Pattern => P);

       if Idx > 0 then
          Display_Adapted_String (S, Idx, N, P);
       end if;
    end Show_Adapted_Strings;

In this example, we look for the index of the substring ``World`` and
perform operations on this substring within the outer string. The procedure
``Display_Adapted_String`` uses both versions of the operations.  For the
procedural version of ``Insert`` and ``Overwrite``, we apply truncation to
the right side of the string (``Right``). For the ``Delete`` procedure, we
specify the range of the substring, which is replaced by whitespaces. For
the function version of ``Delete``, we also call ``Trim`` which trims the
trailing whitespace.

Bounded and unbounded strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using fixed-length strings is usually good enough for strings that are
initialized when they are declared. However, as seen in the previous
section, procedural operations on strings cause difficulties when done on
fixed-length strings because fixed-length strings are arrays of
characters. The following example shows how cumbersome the initialization
of fixed-length strings can be when it's not performed in the declaration:

.. code:: ada

    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Char_Array is
       S : String (1 .. 15);
       --  Strings are arrays of Character
    begin
       S := "Hello          ";
       --  Alternatively:
       --
       --  #1:
       --      S (1 .. 5)      := "Hello";
       --      S (6 .. S'Last) := (others => ' ');
       --
       --  #2:
       --      S := ('H', 'e', 'l', 'l', 'o', others => ' ');

       Put_Line ("String: " & S);
       Put_Line ("String Length: " & Integer'Image (S'Length));
    end Show_Char_Array;

In this case, we can't simply write :ada:`S := "Hello"` because the
resulting array of characters for the ``Hello`` constant has a different
length than the ``S`` string. Therefore, we need to include trailing
whitespaces to match the length of ``S``. As shown in the example, we could
use an exact range for the initialization ( :ada:`S (1 .. 5)`) or use an
explicit array of individual characters.

When strings are initialized or manipulated at run-time, it's usually
better to use bounded or unbounded strings. An important feature of these
types is that they aren't arrays, so the difficulties presented above don't
apply. Let's start with bounded strings.

Bounded strings
^^^^^^^^^^^^^^^

Bounded strings are defined in the
:ada:`Ada.Strings.Bounded.Generic_Bounded_Length` package. Because
this is a generic package, you need to instantiate it and set the
maximum length of the bounded string. You can then declare bounded
strings of the ``Bounded_String`` type.

Both bounded and fixed-length strings have a maximum length that they
can hold. However, bounded strings are not arrays, so initializing
them at run-time is much easier. For example:

.. code:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 15);
       use B_Str;

       S1, S2 : Bounded_String;

       procedure Display_String_Info (S : Bounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: " & Integer'Image (Length (S)));
          --  String:         S'Length => ok
          --  Bounded_String: S'Length => compilation error
          --                              bounded strings are not arrays!

          Put_Line ("Max.   Length: " & Integer'Image (Max_Length));
       end Display_String_Info;
    begin
       S1 := To_Bounded_String ("Hello");
       Display_String_Info (S1);

       S2 := To_Bounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Bounded_String ("Something longer to say here...",
                                Right);
       Display_String_Info (S1);
    end Show_Bounded_String;

By using bounded strings, we can easily assign to ``S1`` and ``S2``
multiple times during execution. We use the ``To_Bounded_String`` and
``To_String`` functions to convert, in the respective direction, between
fixed-length and bounded strings. A call to ``To_Bounded_String`` raises an
exception if the length of the input string is greater than the maximum
capacity of the bounded string. To avoid this, we can use the truncation
parameter (``Right`` in our example).

Bounded strings are not arrays, so we can't use the :ada:`'Length`
attribute as we did for fixed-length strings. Instead, we call the
``Length`` function, which returns the length of the bounded string. The
``Max_Length`` constant represents the maximum length of the bounded string
that we set when we instantiated the package.

After initializing a bounded string, we can manipulate it. For example, we
can append a string to a bounded string using ``Append`` or concatenate
bounded strings using the :ada:`&` operator.  Like so:

.. code:: ada

    with Ada.Strings;         use Ada.Strings;
    with Ada.Strings.Bounded;
    with Ada.Text_IO;         use Ada.Text_IO;

    procedure Show_Bounded_String_Op is
       package B_Str is new
         Ada.Strings.Bounded.Generic_Bounded_Length (Max => 30);
       use B_Str;

       S1, S2 : Bounded_String;
    begin
       S1 := To_Bounded_String ("Hello");
       --  Alternatively: A := Null_Bounded_String & "Hello";
       Append (S1, " World");
       --  Alternatively: Append (A, " World", Right);
       Put_Line ("String: " & To_String (S1));

       S2 := To_Bounded_String ("Hello!");
       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Bounded_String_Op;

We can initialize a bounded string with an empty string using the
``Null_Bounded_String`` constant. Also, we can use the ``Append`` procedure
and specify the truncation mode like we do with the ``To_Bounded_String``
function.

.. _UnboundedStrings:

Unbounded strings
^^^^^^^^^^^^^^^^^

Unbounded strings are defined in the :ada:`Ada.Strings.Unbounded` package.
This is *not* a generic package, so we don't need to instantiate it before
using the ``Unbounded_String`` type. As you may recall from the previous
section, bounded strings require a package instantiation.

Unbounded strings are similar to bounded strings. The main difference is
that they can hold strings of any size and adjust according to the input
string: if we assign, e.g., a 10-character string to an unbounded string
and later assign a 50-character string, internal operations in the
container ensure that memory is allocated to store the new string. In most
cases, developers don't need to worry about these operations. Also, no
truncation is necessary.

Initialization of unbounded strings is very similar to bounded strings.
Let's look at an example:

.. code:: ada

    with Ada.Strings;           use Ada.Strings;
    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String is
       S1, S2 : Unbounded_String;

       procedure Display_String_Info (S : Unbounded_String) is
       begin
          Put_Line ("String: " & To_String (S));
          Put_Line ("String Length: " & Integer'Image (Length (S)));
       end Display_String_Info;
    begin
       S1 := To_Unbounded_String ("Hello");
       --  Alternatively: A := Null_Unbounded_String & "Hello";
       Display_String_Info (S1);

       S2 := To_Unbounded_String ("Hello World");
       Display_String_Info (S2);

       S1 := To_Unbounded_String ("Something longer to say here...");
       Display_String_Info (S1);
    end Show_Unbounded_String;

Like bounded strings, we can assign to ``S1`` and ``S2`` multiple times
during execution and use the ``To_Unbounded_String`` and ``To_String``
functions to convert back-and-forth between fixed-length strings and
unbounded strings. However, in this case, truncation is not needed.

And, just like for bounded strings, you can use the ``Append`` function and
the :ada:`&` operator for unbounded strings. For example:

.. code:: ada

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO;           use Ada.Text_IO;

    procedure Show_Unbounded_String_Op is
       S1, S2 : Unbounded_String := Null_Unbounded_String;
    begin
       S1 := S1 & "Hello";
       S2 := S2 & "Hello!";

       Append (S1, " World");
       Put_Line ("String: " & To_String (S1));

       S1 := S1 & " " & S2;
       Put_Line ("String: " & To_String (S1));
    end Show_Unbounded_String_Op;

Files and streams
-----------------

This section presents the different options available in Ada for file
input/output (I/O).

Text I/O
~~~~~~~~

In most parts of this course, we used the ``Put_Line`` procedure to display
information on the console. However, this procedure also accepts a
``File_Type`` parameter. For example, you can select between standard
output and standard error by setting this parameter explicitly:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Std_Text_Out is
    begin
       Put_Line (Standard_Output, "Hello World #1");
       Put_Line (Standard_Error,  "Hello World #2");
    end Show_Std_Text_Out;

You can also use this parameter to write information to any text file.  To
create a new file for writing, use the ``Create`` procedure, which
initializes a ``File_Type`` element that you can later pass to ``Put_Line``
(instead of, e.g., ``Standard_Output``). After you finish writing
information, you can close the file by calling the ``Close`` procedure.

You use a similar method to read information from a text file.  However,
when opening the file, you must specify that it's an input file
(``In_File``) instead of an output file. Also, instead of calling the
``Put_Line`` procedure, you call the ``Get_Line`` function to read
information from the file.

Let's see an example that writes information into a new text file and then
reads it back from the same file:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Text_File_IO is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Put_Line (F, "Hello World #2");
       Put_Line (F, "Hello World #3");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Simple_Text_File_IO;

In addition to the ``Create`` and ``Close`` procedures, the standard
library also includes a ``Reset`` procedure, which, as the name implies,
resets (erases) all the information from the file. For example:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Reset is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Reset (F);
       Put_Line (F, "Hello World #2");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Text_File_Reset;

By running this program, we notice that, although we've written the first
string (``Hello World #1``) to the file, it has been erased because of the
call to ``Reset``.

In addition to opening a file for reading or writing, you can also open an
existing file and append to it.  Do this by calling the ``Open`` procedure
with the ``Append_File`` option.

When calling the ``Open`` procedure, an exception is raised if the
specified file isn't found.  Therefore, you should handle exceptions in
that context. The following example deletes a file and then tries to open
the same file for reading:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Input_Except is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       --  Open output file and delete it
       Create (F, Out_File, File_Name);
       Delete (F);

       --  Try to open deleted file
       Open (F, In_File, File_Name);
       Close (F);
    exception
       when Name_Error =>
          Put_Line ("File does not exist");
       when others =>
          Put_Line ("Error while processing input file");
    end Show_Text_File_Input_Except;

In this example, we create the file by calling ``Create`` and then
delete it by calling ``Delete``. After the call to ``Delete``, we can
no longer use the ``File_Type`` element`. After deleting the file, we
try to open the non-existent file, which raises a ``Name_Error``
exception.

Sequential I/O
~~~~~~~~~~~~~~

The previous section presented details about text file I/O. Here, we
discuss doing file I/O in binary format. The first package we'll explore is
the :ada:`Ada.Sequential_IO` package. Because this package is a generic
package, you need to instantiate it for the data type you want to use for
file I/O. Once you've done that, you can use the same procedures we've seen
in the previous section: ``Create``, ``Open``, ``Close``, ``Reset`` and
``Delete``. However, instead of calling the ``Get_Line`` and ``Put_Line``
procedures, you'd call the ``Read`` and ``Write`` procedures.

In the following example, we instantiate the :ada:`Ada.Sequential_IO`
package for floating-point types:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Float_IO is
       package Float_IO is new Ada.Sequential_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Seq_Float_IO;

We use the same approach to read and write complex information. The
following example uses a record that includes a Boolean and a
floating-point value:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Rec_IO is
       type Num_Info is record
          Valid : Boolean := False;
          Value : Float;
       end record;

       procedure Put_Line (N : Num_Info) is
       begin
          if N.Valid then
             Ada.Text_IO.Put_Line ("(ok,     " & Float'Image (N.Value) & ")");
          else
             Ada.Text_IO.Put_Line ("(not ok,  -----------)");
          end if;
       end Put_Line;

       package Num_Info_IO is new Ada.Sequential_IO (Num_Info);
       use Num_Info_IO;

       F         : Num_Info_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  (True,  1.5));
       Write (F,  (False, 2.4));
       Write (F,  (True,  6.7));
       Close (F);

       declare
          Value : Num_Info;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Put_Line (Value);
          end loop;
          Close (F);
       end;
    end Show_Seq_Rec_IO;

As the example shows, we can use the same approach we used for
floating-point types to perform file I/O for this record. Once we
instantiate the :ada:`Ada.Sequential_IO` package for the record type, file
I/O operations are performed the same way.

Direct I/O
~~~~~~~~~~

Direct I/O is available in the :ada:`Ada.Direct_IO` package. This mechanism
is similar to the sequential I/O approach just presented, but allows us to
access any position in the file. The package instantiation and most
operations are very similar to sequential I/O.  To rewrite the
``Show_Seq_Float_IO`` application presented in the previous section to use
the :ada:`Ada.Direct_IO` package, we just need to replace the instances of
the :ada:`Ada.Sequential_IO` package by the :ada:`Ada.Direct_IO`
package. This is the new source code:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_IO is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_IO;

Unlike sequential I/O, direct I/O allows you to access any position in
the file. However, it doesn't offer an option to append information to
a file. Instead, it provides an ``Inout_File`` mode allowing reading
and writing to a file via the same ``File_Type`` element.

To access any position in the file, call the ``Set_Index`` procedure to set
the new position / index.  You can use the ``Index`` function to retrieve
the current index. Let's see an example:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_In_Out_File is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       --  Open file for input / output
       Create (F, Inout_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);

       --  Set index to previous position and overwrite value
       Set_Index (F, Index (F) - 1);
       Write (F,  7.7);

       declare
          Value : Float;
       begin
          --  Set index to start of file
          Set_Index (F, 1);

          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_In_Out_File;

By running this example, we see that the file contains ``7.7``, rather than
the previous ``6.7`` that we wrote.  We overwrote the value by changing the
index to the previous position before doing another write.

In this example we used the ``Inout_File`` mode. Using that mode, we just
changed the index back to the initial position before reading from the file
(:ada:`Set_Index (F, 1)`) instead of closing the file and reopening it for
reading.

Stream I/O
~~~~~~~~~~

All the previous approaches for file I/O in binary format (sequential and
direct I/O) are specific for a single data type (the one we instantiate
them with).  You can use these approaches to write objects of a single data
type that may be an array or record (potentially with many fields), but if
you need to create and process files that include different data types, or
any objects of an unbounded type, these approaches are not
sufficient. Instead, you should use stream I/O.

Stream I/O shares some similarities with the previous approaches.  We still
use the ``Create``, ``Open`` and ``Close`` procedures. However, instead of
accessing the file directly via a ``File_Type`` element, you use a
``Stream_Access`` element. To read and write information, you use the
:ada:`'Read` or :ada:`'Write` attributes of the data types you're reading
or writing.

Let's look at a version of the ``Show_Dir_Float_IO`` procedure from the
previous section that makes use of stream I/O instead of direct I/O:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_Float_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Float'Write (S, 1.5);
       Float'Write (S, 2.4);
       Float'Write (S, 6.7);

       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          S := Stream (F);

          while not End_Of_File (F) loop
             Float'Read (S, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Float_Stream;

After the call to ``Create``, we retrieve the corresponding
``Stream_Access`` element by calling the ``Stream`` function. We then
use this stream to write information to the file via the :ada:`'Write`
attribute of the :ada:`Float` type. After closing the file and
reopening it for reading, we again retrieve the corresponding
``Stream_Access`` element and processed to read information from the
file via the :ada:`'Read` attribute of the :ada:`Float` type.

You can use streams to create and process files containing different data
types within the same file.  You can also read and write unbounded data
types such as strings. However, when using unbounded data types you must
call the :ada:`'Input` and :ada:`'Output` attributes of the unbounded data
type: these attributes write information about bounds or discriminants in
addition to the object's actual data.

The following example shows file I/O that mixes both strings of
different lengths and floating-point values:

.. code-block:: ada

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_String_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";

       procedure Output (S  : Stream_Access;
                         FV : Float;
                         SV : String) is
       begin
          String'Output (S, SV);
          Float'Output (S,  FV);
       end Output;

       procedure Input_Display (S : Stream_Access) is
          SV : String := String'Input (S);
          FV : Float  := Float'Input (S);
       begin
          Ada.Text_IO.Put_Line (Float'Image (FV) & " --- " & SV);
       end Input_Display;

    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Output (S, 1.5, "Hi!!");
       Output (S, 2.4, "Hello world!");
       Output (S, 6.7, "Something longer here...");

       Close (F);

       Open (F, In_File, File_Name);
       S := Stream (F);

       while not End_Of_File (F) loop
          Input_Display (S);
       end loop;
       Close (F);

    end Show_String_Stream;

When you use Stream I/O, no information is written into the file indicating
the type of the data that you wrote.  If a file contains data from
different types, you must reference types in the same order when reading a
file as when you wrote it. If not, the information you get will be
corrupted. Unfortunately, strong data typing doesn't help you in this
case. Writing simple procedures for file I/O (as in the example above) may
help ensuring that the file format is consistent.

Like direct I/O, stream I/O supports also allows you to access any location
in the file. However, when doing so, you need to be extremely careful that
the position of the new index is consistent with the data types you're
expecting.

Numerics
--------

The standard library provides support for common numeric operations on
floating-point types as well as on complex types and matrices. This section
presents a brief introduction to them.

Elementary Functions
~~~~~~~~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Elementary_Functions` package provides common
operations for floating-point types, such as square root, logarithm,
and the trigonometric functions (e.g., sin, cos). For example:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Elementary_Functions;
    use  Ada.Numerics.Elementary_Functions;

    procedure Show_Elem_Math is
       X : Float;
    begin
       X := 2.0;
       Put_Line ("Square root of " & Float'Image (X)
                 & " is " & Float'Image (Sqrt (X)));

       X := e;
       Put_Line ("Natural log of " & Float'Image (X)
                 & " is " & Float'Image (Log (X)));

       X := 10.0 ** 6.0;
       Put_Line ("Log_10      of " & Float'Image (X)
                 & " is " & Float'Image (Log (X, 10.0)));

       X := 2.0 ** 8.0;
       Put_Line ("Log_2       of " & Float'Image (X)
                 & " is " & Float'Image (Log (X, 2.0)));

       X := Pi;
       Put_Line ("Cos         of " & Float'Image (X)
                 & " is " & Float'Image (Cos (X)));

       X := -1.0;
       Put_Line ("Arccos      of " & Float'Image (X)
                 & " is " & Float'Image (Arccos (X)));
    end Show_Elem_Math;

Here we use the standard ``e`` and ``Pi`` constants from the
:ada:`Ada.Numerics` package.

The :ada:`Ada.Numerics.Elementary_Functions` package provides operations
for the :ada:`Float` type. Similar packages are available for
:ada:`Long_Float` and :ada:`Long_Long_Float` types. For example, the
:ada:`Ada.Numerics.Long_Elementary_Functions` package offers the same set
of operations for the :ada:`Long_Float` type. In addition, the
:ada:`Ada.Numerics.Generic_Elementary_Functions` package is a generic
version of the package that you can instantiate for custom floating-point
types. In fact, the :ada:`Elementary_Functions` package can be defined as
follows:

.. code-block:: ada

       package Elementary_Functions is new
         Ada.Numerics.Generic_Elementary_Functions (Float);

Random Number Generation
~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Float_Random` package provides a simple random
number generator for the range between 0.0 and 1.0. To use it, declare a
generator ``G``, which you pass to ``Random``. For example:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

    procedure Show_Float_Random_Num is
       G : Generator;
       X : Uniformly_Distributed;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Float'Image (Uniformly_Distributed'First) & " and "
                 & Float'Image (Uniformly_Distributed'Last)  & ":");
       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Float'Image (X));
       end loop;
    end Show_Float_Random_Num;

The standard library also includes a random number generator for discrete
numbers, which is part of the :ada:`Ada.Numerics.Discrete_Random` package.
Since it's a generic package, you have to instantiate it for the desired
discrete type. This allows you to specify a range for the generator. In the
following example, we create an application that displays random integers
between 1 and 10:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics.Discrete_Random;

    procedure Show_Discrete_Random_Num is

       subtype Random_Range is Integer range 1 .. 10;

       package R is new Ada.Numerics.Discrete_Random (Random_Range);
       use R;

       G : Generator;
       X : Random_Range;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Integer'Image (Random_Range'First) & " and "
                 & Integer'Image (Random_Range'Last)  & ":");
       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Integer'Image (X));
       end loop;
    end Show_Discrete_Random_Num;

Here, package ``R`` is instantiated with the ``Random_Range`` type,
which has a constrained range between 1 and 10. This allows us to
control the range used for the random numbers. We could easily modify
the application to display random integers between 0 and 20 by
changing the specification of the ``Random_Range`` type.  We can also
use floating-point or fixed-point types.

Complex Types
~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Complex_Types` package provides support for complex
number types and the :ada:`Ada.Numerics.Complex_Elementary_Functions`
package provides support for common operations on complex number types,
similar to the :ada:`Ada.Numerics.Elementary_Functions` package. Finally,
you can use the :ada:`Ada.Text_IO.Complex_IO` package to perform I/O
operations on complex numbers. In the following example, we declare
variables of the ``Complex`` type and initialize them using an aggregate:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Complex_Types;
    use  Ada.Numerics.Complex_Types;

    with Ada.Numerics.Complex_Elementary_Functions;
    use  Ada.Numerics.Complex_Elementary_Functions;

    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package C_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;
    begin
       X := (2.0, -1.0);
       Y := (3.0,  4.0);

       Put (X);
       Put (" * ");
       Put (Y);
       Put (" is ");
       Put (X * Y);
       New_Line;
       New_Line;

       R  := 3.0;
       Th := Pi / 2.0;
       X  := Compose_From_Polar (R, Th);
       --  Alternatively:
       --  X := R * Exp ((0.0, Th));
       --  X := R * e ** Complex'(0.0, Th);

       Put ("Polar form:    "
            & Float'Image (R)  & " * e**(i * "
            & Float'Image (Th) & ")");
       New_Line;

       Put ("Modulus     of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (abs (X)));
       New_Line;

       Put ("Argument    of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (Argument (X)));
       New_Line;
       New_Line;

       Put ("Sqrt        of ");
       Put (X);
       Put (" is ");
       Put (Sqrt (X));
       New_Line;
    end Show_Elem_Math;

As we can see from this example, all the common operators, such as :ada:`*`
and :ada:`+`, are available for complex types. You also have typical
operations on complex numbers, such as ``Argument`` and ``Exp``.  In
addition to initializing complex numbers in the cartesian form using
aggregates, you can do so from the polar form by calling the
``Compose_From_Polar`` function.

The :ada:`Ada.Numerics.Complex_Types` and
:ada:`Ada.Numerics.Complex_Elementary_Functions` packages provide
operations for the :ada:`Float` type. Similar packages are available for
:ada:`Long_Float` and :ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Complex_Types` and
:ada:`Ada.Numerics.Generic_Complex_Elementary_Functions` packages are
generic versions that you can instantiate for custom or pre-defined
floating-point types. For example:

.. code-block:: ada

    with Ada.Numerics.Generic_Complex_Types;
    with Ada.Numerics.Generic_Complex_Elementary_Functions;
    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package Complex_Types is new
         Ada.Numerics.Generic_Complex_Types (Float);
       use Complex_Types;

       package Elementary_Functions is new
         Ada.Numerics.Generic_Complex_Elementary_Functions (Complex_Types);
       use Elementary_Functions;

       package C_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;

Vector and Matrix Manipulation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Real_Arrays` package provides support for
vectors and matrices. It includes common matrix operations such as
inverse, determinant, eigenvalues in addition to simpler operators
such as matrix addition and multiplication. You can declare vectors
and matrices using the ``Real_Vector`` and ``Real_Matrix`` types,
respectively.

The following example uses some of the operations from the
:ada:`Ada.Numerics.Real_Arrays` package:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    with Ada.Numerics.Real_Arrays;
    use  Ada.Numerics.Real_Arrays;

    procedure Show_Matrix is

       procedure Put_Vector (V : Real_Vector) is
       begin
          Put ("    (");
          for I in V'Range loop
             Put (Float'Image (V (I)) & " ");
          end loop;
          Put_Line (")");
       end Put_Vector;

       procedure Put_Matrix (M : Real_Matrix) is
       begin
          for I in M'Range (1) loop
             Put ("    (");
             for J in M'Range (2) loop
                Put (Float'Image (M (I, J)) & " ");
             end loop;
             Put_Line (")");
          end loop;
       end Put_Matrix;

       V1       : Real_Vector := (1.0, 3.0);
       V2       : Real_Vector := (75.0, 11.0);

       M1       : Real_Matrix :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));
       M2       : Real_Matrix :=
                    ((31.0, 11.0, 10.0),
                     (34.0, 16.0, 11.0),
                     (32.0, 12.0, 10.0),
                     (31.0, 13.0, 10.0));
       M3       : Real_Matrix := ((1.0, 2.0),
                                  (2.0, 3.0));
    begin
       Put_Line ("V1");
       Put_Vector (V1);
       Put_Line ("V2");
       Put_Vector (V2);
       Put_Line ("V1 * V2 =");
       Put_Line ("    "
                 & Float'Image (V1 * V2));
       Put_Line ("V1 * V2 =");
       Put_Matrix (V1 * V2);
       New_Line;

       Put_Line ("M1");
       Put_Matrix (M1);
       Put_Line ("M2");
       Put_Matrix (M2);
       Put_Line ("M2 * Transpose(M1) =");
       Put_Matrix (M2 * Transpose (M1));
       New_Line;

       Put_Line ("M3");
       Put_Matrix (M3);
       Put_Line ("Inverse (M3) =");
       Put_Matrix (Inverse (M3));
       Put_Line ("abs Inverse (M3) =");
       Put_Matrix (abs Inverse (M3));
       Put_Line ("Determinant (M3) =");
       Put_Line ("    "
                 & Float'Image (Determinant (M3)));
       Put_Line ("Solve (M3, V1) =");
       Put_Vector (Solve (M3, V1));
       Put_Line ("Eigenvalues (M3) =");
       Put_Vector (Eigenvalues (M3));
       New_Line;
    end Show_Matrix;

Matrix dimensions are automatically determined from the aggregate used for
initialization when you don't specify them. You can, however, also use
explicit ranges. For example:

.. code-block:: ada

       M1       : Real_Matrix (1 .. 2, 1 .. 3) :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));

The :ada:`Ada.Numerics.Real_Arrays` package implements operations for the
:ada:`Float` type. Similar packages are available for :ada:`Long_Float` and
:ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Real_Arrays` package is a generic version that
you can instantiate with custom floating-point types. For example, the
:ada:`Real_Arrays` package can be defined as follows:

.. code-block:: ada

       package Real_Arrays is new
         Ada.Numerics.Generic_Real_Arrays (Float);

