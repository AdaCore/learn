Standard library
================
:code-config:`reset_accumulator=True`

Standard package
----------------

.. _Containers:

Containers
----------

Ada includes support for containers (such as vectors and sets) in its
standard library. This section presents an introduction to the topic. For
a list of all containers available in Ada, please refer to Appendix B.

Vectors
~~~~~~~

The following subsections present a general overview of vectors,
including instantiation, initialization, operations on vector elements
and operation on vectors.

Instantiation
^^^^^^^^^^^^^

The following example shows the instantiation and declaration of a vector
``V``:

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

After including the container package (:ada:`Ada.Containers.Vectors` in
this case), we need to instantiate it. This is due to the fact that
containers are based on generic packages. Therefore, we cannot simply
declare a vector as we would declare an array of a specific type, e.g.:

.. code-block:: ada

       A : array (1 .. 10) of Integer;

Instead, we need to first create an instance of the generic package for
the specific type and declare it using the corresponding type from the
instantiated package. As indicated above, this instantiation needs to be
done for any container type from the standard library.

In the instantiation of ``Integer_Vectors``, we define that the vector
contains elements of ``Integer`` type by specifying the ``Element_Type``.
Also, by setting ``Index_Type`` to ``Natural``, we specify that the
allowed range includes all natural numbers. We could, instead, use a more
restrict range if we wanted to.

Initialization
^^^^^^^^^^^^^^

In order to initialize a vector, we can use a concatenation of elements.
This is achieved by using the :ada:`&` operator, as shown in the following
example:

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

Note that the example specifies :ada:`use Integer_Vectors`, so that we
have direct access to the types and operations from the instantiated
package. Also, the example introduces another operation on the vector:
``Length``, which retrieves the number of elements in the vector. Note
that the dot notation is possible because ``Vector`` is a tagged type,
so that we can simply write ``V.Length`` instead of ``Length(V)``.

Appending and prepending elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can also add elements to a vector by using the ``Prepend`` and
``Append`` operations. As the name suggests, these operations add
elements to the beginning or to the end of the vector, respectively. For
example:

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

This example fills the vector with elements in the following sequence:
(100, 40, 30, 20, 10, 0, 13).

According to the Reference Manual, the worst-case complexity should be:

- O(:math:`log N`) for the ``Append`` operation, and

- O(:math:`N log N`) for the ``Prepend`` operation.

Accessing first and last elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can access the first and last elements of a vector by using the
functions ``First_Element`` and ``Last_Element``. For example:

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

Also, we can swap elements by calling the procedure ``Swap``. In addition,
we can retrieve a reference (cursor) for the first and last elements of
the vector by calling the functions ``First`` and ``Last``. A cursor
allows for iterating over a container and processing elements from the
container.

With these operations, we're able to write some code that swaps the first
and last elements of a vector:

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
       --  Using V.First and V.Last to retrieve cursor for first and
       --  last elements.
       --  Using V.Swap to swap elements.
       V.Swap (V.First, V.Last);

       Put_Line ("First element is now " & Img (V.First_Element));
       Put_Line ("Last element is now " & Img (V.Last_Element));
    end Show_Vector_First_Last_Element;

Iterating
^^^^^^^^^

The easiest way for iterating over a container is by using a
:ada:`for E of Our_Container` loop. This will give us a reference to the
element of the current position (``E``). We can then use ``E`` directly.
For example:

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

This code will display each element from the vector ``V``.

Note that, in addition to displaying the value of an element, we can also
modify its value. For example, we could easily write a loop to add one to
each element of vector ``V``:

.. code-block:: ada

       for E of V loop
          E := E + 1;
       end loop;

In case of vectors, we can also use indices to access elements. The format
is similar to a loop over array elements: we use a :ada:`for I in <range>`
loop in this case. The range is provided by ``V.First_Index`` and
``V.Last_Index``. We can access the current element by using it as an
array index: ``V (I)``. For example:

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

          --  We could also use V.Element (I) function to retrieve the
          --  element for the current index I

          New_Line;
       end loop;

    end Show_Vector_Index_Iteration;

Note that, in addition to displaying the vector elements, we're also
displaying the index ``I`` itself, similar to what we can do with array
indices. Also, we can access the element by using the short form ``V (I)``
or the longer form ``V.Element (I)``.

As mentioned in the previous section, we can use cursors to iterate over
containers. For this, we use the function ``Iterate``, which retrieves
a cursor for each position of the vector. The corresponding loop has the
format :ada:`for C in V.Iterate loop`. Similar to the previous example
using indices, we can again access the current element by using the cursor
as an array index: ``V (C)``. For example:

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
       --  Using cursor in a loop to iterate:
       --
       for C in V.Iterate loop
          --  Using To_Index function in order to retrieve index
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

Note that, instead of accessing an element in the loop using ``V (C)``,
we could also have used the longer form ``Element (C)``. Also, we're using
the function ``To_Index`` to retrieve the corresponding index for the
current cursor.

Moreover, as indicated above in the comments after the iteration loop, we
could also use a :ada:`while ... loop` to iterate over the vector. In this
case, we would start with a cursor for the first element (retrieved by
calling ``V.First``) and then call ``Next (C)`` to retrieve a cursor for
the next positions. ``Next (C)`` returns ``No_Element`` when the cursor
reaches the end of the vector.

As mentioned above, we can directly modify the elements using a reference.
This is how it looks like when dealing with indices and cursors:

.. code-block:: ada

       --  Modify vector elements using index
       for I in V.First_Index .. V.Last_Index loop
          V (I) := V (I) + 1;
       end loop;

       --  Modify vector elements using cursor
       for C in V.Iterate loop
          V (C) := V (C) + 1;
       end loop;

According to the Reference Manual, the worst-case complexity of accessing
an element should be O(:math:`log N`).

Another approach to modify elements of a vector is to use a process
procedure, which takes the individual elements and does some processing on
them. In this case, we can call ``Update_Element`` and pass a cursor
and an access to the process procedure. For example:

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
       --  Using V.Update_Element to process elements
       --
       for C in V.Iterate loop
          V.Update_Element (C, Add_One'Access);
       end loop;

    end Show_Vector_Update;

Finding and changing elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can find a certain element in a vector by retrieving its index. This
can be achieved by using ``Find_Index``, which retrieves the index of the
first element that matches the element we're looking for. Alternatively,
we could use ``Find`` to retrieve the cursor for that element. For
example:

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

As shown in the previous section about iteration, we can directly access
vector elements by using either an index or cursor. However, before
doing that, we should check whether the index or cursor is valid, since
``Find_Index`` or ``Find`` might not have found the element in the vector.
An exception will be raised if we try to access an element with an invalid
index or cursor. We do this check by comparing the index to ``No_Index``
and the cursor to ``No_Element``. For example:

.. code-block:: ada

       --  Modify vector element using index
       if Idx /= No_Index then
          V (Idx) := 11;
       end if;

       --  Modify vector element using cursor
       if C /= No_Element then
          V (C) := 14;
       end if;

Alternatively, instead of writing ``V (C) := 14``, we could use the longer
form :ada:`V.Replace_Element (C, 14)`.

Inserting elements
^^^^^^^^^^^^^^^^^^

In the previous sections, we've seen examples of how to insert elements
into a vector:

- using the concatenation operator (:ada:`&`) at the vector declaration,
  or

- calling the ``Prepend`` and ``Append`` procedures.

In some cases, we want to insert an element at a specific position, e.g.
before a certain element in the vector. We can do this by calling
``Insert``. For example:

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

In this example, we're looking for an element with value 10. If we find
it, we then insert an element with value 9 before it.

Removing elements
^^^^^^^^^^^^^^^^^

We can remove elements from a vector by using the ``Delete`` procedure
with either a valid index or cursor. If we combine this with the functions
``Find_Index`` and ``Find`` from the previous section, we'll be able to
write a program that searches for a specific element and deletes it:

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
       --  Using Find_Index to retrieve index of element with value 10
       Idx := V.Find_Index (10);

       --  Checking whether index is valid
       if Idx /= No_Index then
          --  Removing element using V.Delete
          V.Delete (Idx);
       end if;

       --  Using Find to retrieve cursor for element with value 13
       C := V.Find (13);

       --  Checking whether index is valid
       if C /= No_Element then
          --  Removing element using V.Delete
          V.Delete (C);
       end if;

    end Show_Remove_Vector_Element;

This approach can be extended to delete all elements that match a certain
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
       --  Removing elements using an index
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
       --  Removing elements using a cursor
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

In this example, we remove all elements from the vector that have the
value 10 by retrieving their index. Likewise,  we remove all elements with
the value 13 by retrieving their cursor.

Other Operations
^^^^^^^^^^^^^^^^

We've already seen some operations on vector elements in the last
sections. In this section, we'll see operations on the vector as a
whole. The most prominent example is the concatenation of multiple
vectors. Also, we'll see operations on a vector as a sequence of elements,
i.e. considering their relation to each other. Examples are sorting and
sorted merging operations.

Vector concatenation is done by using the :ada:`&` operator on vectors.
Let's consider two vectors ``V1`` and ``V2``. We can concatenate them with
the following statement: :ada:`V := V1 & V2`. ``V`` will contain the
resulting vector.

The generic package ``Generic_Sorting`` is a child package of
``Ada.Containers.Vectors``. It contains sorting and merging operations.
Because it's a generic package, we cannot use it directly, but have to
instantiate it, too. Also, in order to use these operations in our vector
of integer values (``Integer_Vectors``), we need to instantiate it
directly as a child from ``Integer_Vectors``. The next example will make
this clear.

After the instantiation of ``Generic_Sorting`` is done, we can make all
operations available with the :ada:`use` statement. We can then proceed to
call ``Sort`` in order to sort the vector and ``Merge`` in order to merge
one vector into another one.

The following code example presents an application that operates on three
vectors (``V1``, ``V2``, ``V3``) and uses the concatenation, sorting and
sorted merging operations:

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

According to the Reference Manual, the worst-case complexity of a call to
``Sort`` should be O(:math:`N^2`), and the average complexity should be better
than O(:math:`N^2`).

Sets
~~~~

Sets are another class of containers. While vectors allow for duplicated
elements to be inserted, sets ensure that no duplicated elements exist at
any moment.

In the following subsections, we'll look into operations that can be
performed on sets. However, since many of the operations on vectors are
similar to the ones used for sets, we'll cover them more quickly here.
Please refer to the section on vectors for a more detailed discussion.

Initialization and iteration
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In order to initialize a set, we can call the ``Insert`` procedure.
However, we need to make sure that no duplicated element is being
inserted. Otherwise, we'll get an exception. If we have less control
about the elements to be inserted (e.g.: when elements are being passed
as arguments and inserted into a set), we might want to use one of the
following options:

- use a version of ``Insert`` that provides an output Boolean value
  indicating whether the insertion was successful;

- use the ``Include`` procedure, which silently ignores any attempt to
  insert a duplicated element.

In order to iterate over a set, we can use a :ada:`for E of S` loop, as
we did for vectors. This will give us a reference to each element from
the set.

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
       C : Cursor;
       Ins : Boolean;
    begin
       S.Insert (20);
       S.Insert (10);
       S.Insert (0);
       S.Insert (13);

       --  Calling S.Insert(0) now would raise Constraint_Error exception
       --  because this element is already in the set
       --  We can call a version of Insert that does not raise an exception,
       --  but returns a Boolean indicating the status

       S.Insert (0, C, Ins);
       if not Ins then
          Put_Line ("Inserting 0 into set was not successful");
       end if;

       --  We can also call S.Include instead
       --  If the element is already available, the set remains the same
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

In this section, we will briefly look into the following operations on
sets:

- ``Delete`` and ``Exclude`` to remove elements;

- ``Contains`` and ``Find`` to verify the existence of elements.

In order to delete elements, we can call the procedure ``Delete``.
However, similarly to the situation we've seen with ``Insert``
in the previous section, ``Delete`` raises an exception when
the element to be deleted doesn't exist in the set. Therefore, in case
an element might not exist in a set, we can call ``Exclude``, which
silently ignores any attempt to delete a non-existent element.

``Contains`` returns a Boolean value indicating whether a value is
contained in the set. ``Find`` also looks for an element in a set, but
returns a cursor to the element or ``No_Element`` in case the element
doesn't exist in the set. Both functions can be used to search for
elements in a set.

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

       --  Calling S.Delete (13) again raises a Constraint_Error
       --  exception because the element does not exist anymore
       --  in the set, so it cannot be deleted.
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
library also offers hashed sets. According to the Reference Manual, this
is the average complexity of some operations:

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

The previous sections dealt with operations on individual elements of a
set. We'll now look into typical set operations: union, intersection,
difference and symmetric difference. In contrast to some vector
operations we've seen before (e.g. ``Merge``), we can use built-in
operators, such as :ada:`-`. The following table lists the operations and
the associated operator:

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

In the previous sections, we dealt with containers for elements of
definite types. In most source-code examples of those sections, we've
seen :ada:`Integer` types as element type of the containers. An
example of indefinite type is a :ada:`String` type. Indefinite types
require a different kind of containers specially designed for them.

Also, we will be looking into a different class of containers: maps. They
allow for associating a key to a specific value. An example of a map is
the one-to-one association between a person and its age. If we consider
a person's name to be the key, the value would be the person's age.

Hashed maps
^^^^^^^^^^^

Hashed maps are maps that make use of a hash as a key. The hash itself is
calculated by a function indicated by the programmer.

.. admonition:: In other languages

    Hashed maps are similar to dictionaries in Python and hashes in Perl.
    One of the main differences is that these scripting languages allow
    for using different types for the values contained in a map, while in
    Ada, the type of both key and value is specified in the package
    instantiation and cannot be changed for that specific map. In order
    to use multiple types, different maps need to be specified.

When instantiating a hashed map from
``Ada.Containers.Indefinite_Hashed_Maps``, we need to specify following
elements:

- ``Key_Type``: type of the key

- ``Element_Type``: type of the element

- ``Hash``: hash function for the ``Key_Type``

- ``Equivalent_Keys``: an equality operator (e.g. ``=``) that is used
  to determine if two keys are equal.

  - If the type specified in ``Key_Type`` has a standard operator, we
    might use it.

In the next example, we'll use a string as a key type. Also, we'll use the
``Hash`` function provided by the standard library for strings (in the
``Ada.Strings`` package).

We can include elements into a hashed map by calling ``Insert``. If an
element is already contained in a map ``M``, we can access it directly by
using its key. For example, we may change the value of an element by
calling :ada:`M ("My_Key") := 10`. If the key is not found, however, an
exception is raised. In order to verify if a key is available, we can use
the function ``Contains`` (as we've seen in the section about sets).

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
       --  Otherwise, an exception will be raised.
       M ("Alice") := 25;

       New_Line; Put_Line ("Name & Age:");
       for C in M.Iterate loop
          Put_Line (Key (C) & ": " & Integer'Image (M (C)));
       end loop;

    end Show_Hashed_Map;

Ordered maps
^^^^^^^^^^^^

Ordered maps share many features with hashed maps. The main differences
are:

- A hash function is not needed. Instead, you need to provide an ordering
  function (``<`` operator). The ordered map will then use it to order
  elements, and allow pretty fast access --- O (log n) using a binary
  search.

  - If the type specified in ``Key_Type`` has a standard ``<`` operator,
    we might use it.

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

There is a great similarity between the example above and the one from the
previous section. In fact, since maps share many operations, we
don't need to make extensive adaptations in order to change our example
to use ordered maps instead of hashed maps. The main difference we notice
here is when we run the applications: while the output of a hashed map
is usually unordered, the output of a ordered map is always ordered, as
its name implies.

Complexity
^^^^^^^^^^

Hashed maps are generally the fastest data structure available to you in
Ada if you need to associate heterogeneous keys to values and search for
them quickly. In most cases, they are slightly faster than ordered maps.
Therefore, if you don't need ordering, prefer hashed maps.

According to the Reference Manual, this is the average complexity of some
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

The standard library provides support for representing and handling date
and time. This is part of the :ada:`Ada.Calendar` package. Let's look at
a simple example:

.. code:: ada

    with Ada.Calendar;            use Ada.Calendar;
    with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
    with Ada.Text_IO;             use Ada.Text_IO;

    procedure Display_Current_Time is
       Now : Time := Clock;
    begin
       Put_Line ("Current time: " & Image (Now));
    end Display_Current_Time;

This application displays the current date and time, which is retrieved
by a call to the ``Clock`` function. We call the function ``Image`` from
the :ada:`Ada.Calendar.Formatting` package in order to get a ``String``
for the current date and time. We could, instead, retrieve each component
using the ``Split`` function. For example:

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

We can delay an application, so that it restarts at a specific date and
time. This is achieved by using a :ada:`delay until` statement. For
example:

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

In this application, the date and time is specified by initializing
``Next`` with a call to ``Time_Of``. This function takes the various
components of a date (year, month, etc) and returns an element of ``Time``
type. Because the date specified in the application is in the past, the
:ada:`delay until` statement won't produce any noticeable effect. However,
if we'd use a date in the future, the application would be waiting until
that specific date and time would have arrived.

Note that, in the application above, we're taking care of adapting the
time to the local time-zone. By default, *Coordinated Universal Time*
(abbreviated to UTC) is used. By retrieving the time offset to UTC with a
call to ``UTC_Time_Offset`` from the :ada:`Ada.Calendar.Time_Zones`
package, we can initialize ``TZ`` and use it in the call to ``Time_Of``.
This is all we need in order to make the information provided to
``Time_Of`` relative to the local time zone.

We could achieve a similar result by initializing ``Next`` with a
``String``. This can be done by a call to ``Value`` from the
:ada:`Ada.Calendar.Formatting` package. This is the adapted application:

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

In this example, we're again using ``TZ`` in the call to ``Value`` in
order to adjust the input time to the current time zone.

In the examples above, we were delaying the application to a specific
date and time. We could, however, delay the application relatively to the
current time. For example, we could delay the application by 5 seconds
using the current time:

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

Here, we're specifying a duration of 5 seconds in ``D``, adding it to
the current time from ``Now`` and storing the sum in ``Next``. This will
then be used for the :ada:`delay until` statement.

Real-time
~~~~~~~~~

In addition to :ada:`Ada.Calendar`, the standard library also supports
time operations for real-time applications. This is implemented by the
:ada:`Ada.Real_Time` package. This package also include a ``Time`` type.
However, in the :ada:`Ada.Real_Time` package, the ``Time`` type is used
to represent an absolute clock and handle time span. This contrasts with
the :ada:`Ada.Calendar`, which uses the ``Time`` type to represent dates
and times.

In the previous section, we used the ``Time`` type from the
:ada:`Ada.Calendar` and the :ada:`delay until` statement to delay an
application by 5 seconds. We could have used the :ada:`Ada.Real_Time`
package instead. Let's adapt that example:

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
which is defined in the :ada:`Ada.Real_Time` package. We call the function
``Seconds`` to initialize ``D``. We could have a finer granularity by
calling ``Nanoseconds`` instead. Also, we need to first convert ``D`` to
the ``Duration`` type using ``To_Duration`` before we can display it.

Benchmarking
^^^^^^^^^^^^

One interesting application using the :ada:`Ada.Real_Time` package is
benchmarking. We've used it before in a previous section when discussed
tasking. Let's look at an example of benchmarking:

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

This example defines a dummy ``Computational_Intensive_App`` that is
implemented using a simple :ada:`delay` statement. We initialize
``Start_Time`` and ``Stop_Time`` with the current clock and calculate the
elapsed time. By running this application, we see that the elapsed time is
roughly 5 seconds, as expected due to the :ada:`delay` statement.

A similar application is benchmarking of CPU time. This can be implemented
by using the :ada:`Execution_Time`. Let's adapt the previous example to
measure CPU time:

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
initialize both variables and calculate the elapsed time in the same way
as before. By running this application, we see that the CPU time is
significantly lower than the 5 seconds we've seen before. This is caused
by the fact that the :ada:`delay` statement doesn't require much CPU time.
Results are different if we adapt the implementation of
``Computational_Intensive_App`` to make use of mathematical functions in
a long loop. For example:

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
          for I in 0 .. 5_000_000 loop
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

Now that our dummy ``Computational_Intensive_App`` makes use of
mathematical operations that require significant CPU time, the measured
elapsed and CPU time should be much closer to each other than before.

Strings
--------

In previous sections, we've used strings in many source-code examples. In
this section, we will discuss them in more details.

String operations
~~~~~~~~~~~~~~~~~

Operations on standard strings are available in the
:ada:`Ada.Strings.Fixed` package. As mentioned in previous sections,
standard strings are arrays of elements of :ada:`Character` type with *a
fixed-length*. That's why this child package is called ``Fixed``.

One of the most simple operations available is counting the number of
substrings available in a string (using ``Count``) and finding their
corresponding indices (using ``Index``). Let's look at an example:

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

Notice that we initialize the string ``S`` using a multiplication. Writing
:ada:`"Hello" & 3 * " World"` creates the string
``Hello World World World``. We then use the function ``Count`` to get the
number of instances of the word ``World`` in ``S``.  Next, we use the
function ``Index`` in a loop to find the index of each instance of
``World`` in ``S``.

In the previous example, we looked for instances of a specific substring.
In the next example, we will retrieve all the words in the string. We can
do this by using ``Find_Token`` and using whitespaces as separators. For
example:

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

The procedure ``Find_Token`` requires a set of characters that will be
used as delimitators. This is represented by the ``Character_Set`` from
the :ada:`Ada.Strings.Maps` package. We use the ``To_Set`` function (from
the same package) to initialize the set in ``Whitespace``. We then call
``Find_Token`` in a loop over valid indices to find the starting index of
each word. For the ``Test`` parameter of the ``Find_Token`` procedure, we
pass ``Outside`` as an argument, which indicates that we're looking for
indices that are outside the ``Whitespace`` set, i.e. actual words. The
``First`` and ``Last`` parameters of ``Find_Token`` are output parameters
that indicate the valid range of the substring. We can use this
information to display the string (:ada:`S (F .. L)`).

The operations that we've looked so far only iterate over strings, but
don't modify them. We will now look into following operations that change
the content of strings:

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

Note that all these operations are available as functions or procedures.
In case functions are used, a new string is created. If we use a
procedure, however, the operations are performed in place. In this case,
we need to make sure that the string constraints are respected. Otherwise,
the procedure might raise an exception. For example, let's say we have a
string ``S`` containing 10 characters. Inserting a string with two
characters  (e.g. ``"!!"``) into ``S`` would require a string containing
12 characters. Since ``S`` has a fixed length, we cannot increase its
size. The easiest solution in this case is to specify that truncation
should be applied while inserting the substring, so that the length of
``S`` remains fixed. Let's see an example that makes use of both function
and procedure version of ``Insert``, ``Overwrite`` and ``Delete``:

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
perform operations on this substring. The procedure
``Display_Adapted_String`` makes use of both versions of the operations.
For the procedural version of ``Insert`` and ``Overwrite``, truncation is
applied on the right side of the string (``Right``). For the ``Delete``
procedure, we specify the range of the substring, which is substituted by
whitespaces. For the function version of ``Delete``, we call ``Trim`` in
addition, which trims the trailing whitespaces.

Bounded and unbounded strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using fixed-length strings is usually good enough for strings that are
initialized at declaration time. However, as seen in the previous section
with procedural operations on strings, we might encounter difficulties
when performing operations on fixed-length strings. This is mainly due
to the fact that fixed-length strings are essentially arrays of
characters. The following example shows how cumbersome the initialization
of fixed-length strings can be when it's not performed at declaration
time:

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

In this case, we cannot simply write :ada:`S := "Hello"`, because the
resulting array of characters for the ``Hello`` string would have a
different length than the ``S`` string. Therefore, we need to include the
trailing whitespaces in order to match the length of ``S``. As indicated
in the example, we could use an exact range for the initialization (
:ada:`S (1 .. 5)`) or use an explicit array of individual characters.

When strings are initialized or manipulated at run-time, it's usually
better to use bounded or unbounded strings. An important feature of these
types is that they are not arrays, so that they are not affected by the
difficulties presented in the previous example. Let's start by looking
into bounded strings.

Bounded strings
^^^^^^^^^^^^^^^

Bounded strings are defined in the
:ada:`Ada.Strings.Bounded.Generic_Bounded_Length` package. Because this is
a generic package, we need to instantiate it first and set the maximum
length of the bounded strings. We can then declare bounded strings using
the ``Bounded_String`` type.

Both bounded strings and fixed-length strings have a maximum length that
they can store. However, because bounded strings are not arrays,
initializing them at run-time is much easier. For example:

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

By using bounded strings, we can easily initialize ``S1`` and ``S2``
multiple times at run-time. We use the ``To_Bounded_String`` and
``To_String`` functions to convert back-and-forth between fixed-length
strings and bounded strings. In case of ``To_Bounded_String``, if the
length of the input string is greater than the maximum capacity of the
bounded string, an exception will be raised. In order to avoid this, we
can use the truncation parameter (``Right`` in our example).

Also note that, because bounded strings are not arrays, we cannot use the
:ada:`'Length` attribute as we did for fixed-length strings. In this case,
we call the ``Length`` function, which returns the length of the string
stored in the bounded string. In order to get the maximum length of the
bounded string, we can use the ``Max_Length`` constant.

After initializing bounded strings, we may manipulate them. For example,
we may append a string to a bounded string using ``Append`` or concatenate
bounded strings using the :ada:`&`  operator. For example:

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

Note that we may initialize a bounded string with an empty string using
the ``Null_Bounded_String`` constant. Also note that the ``Append``
procedure allows for specifying the truncation mode --- similar to the
``To_Bounded_String`` function.

.. _UnboundedStrings:

Unbounded strings
^^^^^^^^^^^^^^^^^

Unbounded strings are defined in the :ada:`Ada.Strings.Unbounded` package.
Since this is not a generic package, we don't need to instantiate a
package before using the ``Unbounded_String`` type. As you may recall from
the previous section, bounded strings require a package instantiation.

In general, unbounded strings are similar to bounded strings. The main
difference is that they can hold strings of any size and adapt according
to the input string: if we initialize an unbounded string with e.g. a
10-character string and then with a 50-character string, internal
operations in the container will make sure that memory is allocated to
store the new string. In most cases, developers don't need to worry about
these operations. Also, no truncation is necessary in this case.

Initialization of unbounded strings is very similar to bounded strings.
Let's look at the example:

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

Similar to bounded strings, we can easily initialize ``S1`` and ``S2``
multiple times at run-time and use the ``To_Unbounded_String`` and
``To_String`` functions to convert back-and-forth between fixed-length
strings and unbounded strings. However, in this case, truncation is not
needed.

Similar to bounded strings, we can use the ``Append`` function and the
:ada:`&` operator for unbounded strings. For example:

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

In many sections of this course, the ``Put_Line`` procedure was used to
display information on the console. However, this procedure also accepts
a ``File_Type`` parameter. For example, we may select between the standard
output and the standard error by setting this parameter explicitly:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Std_Text_Out is
    begin
       Put_Line (Standard_Output, "Hello World #1");
       Put_Line (Standard_Error,  "Hello World #2");
    end Show_Std_Text_Out;

In addition, this parameter can be used to write information to any
text file. In order to create a new file for writing, we use the
``Create`` procedure. This procedure initializes a ``File_Type`` element
that can be used as argument for ``Put_Line`` (instead of e.g.
``Standard_Output``). After we finish writing information, we have to
close the file by calling the ``Close`` procedure.

A similar method is used for retrieving information from a text file.
However, when opening the file, we need to specify that it's an input
file (``In_File``) instead of an output file. Also, instead of calling the
``Put_Line`` procedure, we call the ``Get_Line`` function to retrieve
information from the file.

Let's see an example that writes information into a new text file and then
reads information from the same file:

.. code:: ada

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
resets all the information from the file. For example:

.. code:: ada

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

By running this application, we notice that, although we've written the
first string (``Hello World #1``) to the file, it has been erased because
of the call to ``Reset``.

In addition to opening a file for reading or writing, we may also open an
existing file and append information to it. This can be done by calling
the ``Open`` procedure with the ``Append_File`` option.

When calling the ``Open`` procedure, we need to be aware that, if the
specified file is not found, an exception will be raised. Therefore, we
should handle exceptions in this context. The following application
deletes a file and then tries to open the same file for reading:

.. code:: ada

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

Note that, in this example, we create the file by calling ``Create`` and
then delete it by calling the ``Delete`` procedure. After a call to
``Delete``, we cannot use the ``File_Type`` element --- for this reason,
we don't have a call to ``Close`` after the call to ``Create``. After
deleting the file, we try to open the non-existent file, which raises a
``Name_Error`` exception.

Sequential I/O
~~~~~~~~~~~~~~

The previous section presented details about text file I/O. In this
section, we will discuss file I/O in binary format. The first package
we'll see is the :ada:`Ada.Sequential_IO` package. Because this package is
a generic package, we need to instantiate it for the data type we want to
use for file I/O. Apart from that, we can make use of the same
procedures we've seen in the previous section: ``Create``, ``Open``,
``Close``, ``Reset`` and ``Delete``. However, instead of calling the
``Get_Line`` and ``Put_Line`` procedures, we'll call the ``Read`` and
``Write`` procedures.

In the following example, we instantiate the :ada:`Ada.Sequential_IO`
package for floating-point types:

.. code:: ada

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

We can use the same approach to read and write complex information. The
following example makes use of a record that includes a Boolean and a
floating-point element:

.. code:: ada

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

As the example clearly shows, the same approach used for floating-point
types can be used to generate file I/O for this record. As soon as we
instantiate the :ada:`Ada.Sequential_IO` package for the record type, all
file I/O operations are the same.

Direct I/O
~~~~~~~~~~

Direct I/O is available in the :ada:`Ada.Direct_IO` package. This file I/O
approach is similar to the sequential I/O approach presented in the
previous section. For example, the package instantiation and most
operations are very similar. If we want to rewrite the
``Show_Seq_Float_IO`` application presented in the previous section to
make use of the :ada:`Ada.Direct_IO` package, we basically just need to
replace the instances of the :ada:`Ada.Sequential_IO` package by the
:ada:`Ada.Direct_IO` package. This is the adapted source-code:

.. code:: ada

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

The main difference between sequential I/O and direct I/O is that direct
I/O allows for accessing any position in the file. Also, it doesn't offer
an option to append information to a file. Instead, it has an
``Inout_File`` mode that allows for reading and writing to a file using
the same ``File_Type`` element.

In order to access any position in the file, we call the ``Set_Index``
procedure to set the new position / index. Also, we may use the ``Index``
function to retrieve the current index. Let's see an example:

.. code:: ada

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

       --  Set index to previous position again and overwrite value
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

By running this example, we see that, although we've written ``6.7`` to
the file, this information was overwritten with the ``7.7`` value after
we changed the index to the previous position.

Note that, in this example, we make use of the ``Inout_File`` mode.
Instead of closing the file and reopening it for reading, we simply change
the index to the initial position before reading information from the
file (:ada:`Set_Index (F, 1)`)

Stream I/O
~~~~~~~~~~

The previous approaches for file I/O in binary format (sequential and
direct I/O) are specifically tailored for a single data type. For example,
we may select these approaches for writing complex records of a single
data type. However, as soon as we need to create and process files that
include different data types or unbounded types, these approaches are not
suitable anymore. Instead, we should use stream I/O in this case.

Stream I/O share some similarities with the previous approaches. For
example, we still make use of the ``Create``, ``Open`` and ``Close``
procedures. However, instead of accessing the file directly via a
``File_Type`` element, we use a ``Stream_Access`` element. Also, in order
to read and write information, we use the :ada:`'Read` and :ada:`'Write`
attributes of the data types.

Let's look at an adaptation of the ``Show_Dir_Float_IO`` procedure from
the previous section that makes use of stream I/O instead of direct I/O:

.. code:: ada

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

Note that, after the call to the ``Create`` procedure, we retrieve the
corresponding ``Stream_Access`` element by calling the ``Stream``
function. We then use this stream to write information to the file by
using the :ada:`'Write` attribute of the :ada:`Float` type. After closing
the file and reopening it for reading, we again retrieve the corresponding
``Stream_Access`` element. We then processed to read information from the
file by using the :ada:`'Read` attribute of the :ada:`Float` type.

As mentioned above, we can use streams to create and process files that
make use of different data types in the same file. Also, we may use
unbounded data types such as strings. When using unbounded data types,
however, we need to call the :ada:`'Input` and :ada:`'Output` attributes
of the unbounded data type because, in addition to writing the information
for the data type, these attributes include information about bounds or
discriminants in the file.

The following example shows file I/O that intercalates strings of
different lengths and floating-point values:

.. code:: ada

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

We need to be aware that, when parsing a file that contains information
from different data types, we should make sure that the same order is used
as when the file was written. Otherwise, the information we'll get will
be corrupted. Unfortunately, strong data typing cannot directly help
software developers in this case. Writing simple procedures for file I/O
(as in the example above) may help ensuring that the file format is
consistent. Also note that, like direct I/O, stream I/O supports access to
any index in the file. Therefore, when changing the index, we need to make
sure that the new index is correctly aligned with the data types that
we're expecting.

Numerics
--------

The standard library provides support for common numeric operations on
floating-point types, as well as complex types and matrices. This section
presents a brief introduction into the topic.

Elementary Functions
~~~~~~~~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Elementary_Functions` package provides
common operations for floating-point types, such as square-root,
logarithm, cosine, etc. For example:

.. code:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Elementary_Functions;
    use  Ada.Numerics.Elementary_Functions;

    procedure Show_Elem_Math is
       X : Float;
    begin
       X := 2.0;
       Put_Line ("Square-root of " & Float'Image (X)
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

Note that, in the example above, we make use of the standard ``e``
and ``Pi`` constants from the :ada:`Ada.Numerics` package.

The :ada:`Ada.Numerics.Elementary_Functions` package targets the
:ada:`Float` type. Similar packages are available for :ada:`Long_Float`
and  :ada:`Long_Long_Float` types. For example, the
:ada:`Ada.Numerics.Long_Elementary_Functions` package offers the same set
of operations for the :ada:`Long_Float` type. In addition, the
:ada:`Ada.Numerics.Generic_Elementary_Functions` package is a generic
version of the package that can be instantiated for custom
floating-point types. In fact, the
:ada:`Elementary_Functions` package can be defined as
follows:

.. code-block:: ada

       package Elementary_Functions is new
         Ada.Numerics.Generic_Elementary_Functions (Float);

As expected, we can instantiate the
:ada:`Generic_Elementary_Functions` package for any custom
floating-point type definition.

Random Number Generation
~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Float_Random` package provides a simple random
number generator for the range between 0 and 1. In order to use it, we
need to declare a generator ``G``, which can then be used in calls to
``Random``. For example:

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
numbers. This is part of the :ada:`Ada.Numerics.Discrete_Random` package.
Since it's a generic package, we have to instantiate it for the desired
discrete type. This allows us to specify a range for the generator. In
the following example, we create an application that displays random
numbers between 1 and 10:

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

In this example, package ``R`` is instantiated with the ``Random_Range``
type, which has a constrained range between 1 and 10. This allows us for
controlling the range used for the random numbers. We could easily modify
the application to display random numbers between 0 and 20 by changing the
specification of the ``Random_Range`` type.

Complex Types
~~~~~~~~~~~~~

The :ada:`Ada.Numerics.Complex_Types` package provides support for
complex number types. In addition, the
:ada:`Ada.Numerics.Complex_Elementary_Functions` package provides
support for common operations on complex number types --- similar to the
:ada:`Ada.Numerics.Elementary_Functions` package. Finally, the
:ada:`Ada.Text_IO.Complex_IO` package can be used to display complex
numbers. In the following example, we declare variables of ``Complex``
type and initialize them using an aggregate:

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

As we can see in this example, all common operators, such as :ada:`*` and
:ada:`+`, are available for complex number types. Moreover, we have
typical operations on complex numbers, such as ``Argument`` and ``Exp``.
Also, in addition to initializing complex numbers in the cartesian form
using aggregates, we can use the polar form by calling the
``Compose_From_Polar`` function.

The :ada:`Ada.Numerics.Complex_Types` and
:ada:`Ada.Numerics.Complex_Elementary_Functions` packages target the
:ada:`Float` type. Similar packages are available for :ada:`Long_Float`
and  :ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Complex_Types` and
:ada:`Ada.Numerics.Generic_Complex_Elementary_Functions` packages are
generic versions that can be instantiated for custom or pre-defined
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
vectors and matrices. It includes common matrix operations --- such as
inverse, determinant, eigenvalues --- in addition to typical operators
such as matrix addition and multiplication. We can declare vectors and
matrices using the ``Real_Vector`` and ``Real_Matrix`` types,
respectively.

The following example makes use of some of the operations from the
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

When not specified, matrix dimensions will automatically be determined
from the aggregate used for initialization. We could, however, make use of
explicit ranges. For example:

.. code-block:: ada

       M1       : Real_Matrix (1 .. 2, 1 .. 3) :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));

The :ada:`Ada.Numerics.Real_Arrays` package targets the
:ada:`Float` type. Similar packages are available for :ada:`Long_Float`
and  :ada:`Long_Long_Float` types. In addition, the
:ada:`Ada.Numerics.Generic_Real_Arrays` package is a generic
version that can be instantiated for custom floating-point types. For
example, the :ada:`Real_Arrays` package can be defined as follows:

.. code-block:: ada

       package Real_Arrays is new
         Ada.Numerics.Generic_Real_Arrays (Float);

Dynamic allocation and reclamation
----------------------------------
