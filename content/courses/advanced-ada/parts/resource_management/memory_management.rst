Memory Management
=================

.. include:: ../../../global.txt

Maximum allocation size and alignment
-------------------------------------

We've seen details about the :ada:`Size` and :ada:`Object_Size` attributes in
the section about
:ref:`data representation <Adv_Ada_Data_Representation_Sizes>`.
:ref:`Later on <Adv_Ada_Storage_Size_Attribute>`, we also mentioned the
:ada:`Storage_Size` attribute.

In this section, we expand our discussion on sizes and talk about the
:ada:`Max_Size_In_Storage_Elements` and the :ada:`Max_Alignment_For_Allocation`
attributes. These attributes return values that are important in the allocation
of :ref:`memory subpools <Adv_Ada_Memory_Pools>` via the :ada:`Allocate`
procedure from the :ada:`System.Storage_Pools` and the
:ada:`System.Storage_Pools.Subpools` packages:

.. code-block:: ada

    procedure Allocate(
      Pool                     : in out Root_Storage_Pool;
      Storage_Address          :    out Address;
      Size_In_Storage_Elements :        Storage_Elements.Storage_Count;
      Alignment                :        Storage_Elements.Storage_Count);

    procedure Allocate (
      Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          :    out Address;
      Size_In_Storage_Elements :        Storage_Elements.Storage_Count;
      Alignment                :        Storage_Elements.Storage_Count);

In fact, the :ada:`Max_Size_In_Storage_Elements` attribute indicates the
maximum value that can be used for the actual :ada:`Size_In_Storage_Elements`
parameter of the :ada:`Allocate` procedure . Likewise, the
:ada:`Max_Alignment_For_Allocation` attribute indicates the maximum value for
the actual :ada:`Alignment` parameter of the :ada:`Allocate` procedure. (We
discuss more details about this procedure later on.)

The :ada:`Allocate` procedure is called when we allocate memory for access
types. Therefore, the value returned by the :ada:`Max_Size_In_Storage_Elements`
attribute for a subtype :ada:`S` indicates the maximum value of storage
elements when allocating memory for an access type whose designated subtype is
:ada:`S`, while the :ada:`Max_Alignment_For_Allocation` attribute indicates the
maximum alignment that we can use when we allocate memory via the :ada:`new`
allocator.

.. admonition:: Relevant topics

    - :arm22:`13.11 Storage Management <13-11>`
    - :arm22:`13.11.1 Storage Allocation Attributes <13-11-1>`
    - :arm22:`13.11.4 Storage Subpools <13-11-4>`


Code example with scalar type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's see a simple type :ada:`T` and two types based on it |mdash| an array and
an access type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    package Custom_Types is

       type T is new Integer;

       type T_Array is
         array (Positive range <>) of T;

       type T_Access is access T;

    end Custom_Types;

The test procedure :ada:`Show_Sizes` shows the values returned by the
:ada:`Size`, :ada:`Max_Size_In_Storage_Elements`, and
:ada:`Max_Alignment_For_Allocation` attributes for the :ada:`T` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
    begin
       Put_Line
       ("T'Size:                                "
        & Integer'Image
            (T'Size
             / System.Storage_Unit)
        & " storage elements ("
        & T'Size'Image
        & " bits)");

       Put_Line
       ("T'Max_Size_In_Storage_Elements:        "
        & T'Max_Size_In_Storage_Elements'Image
        & " storage elements ("
        & Integer'Image
            (T'Max_Size_In_Storage_Elements
             * System.Storage_Unit)
        & " bits)");

       Put_Line
       ("T'Max_Alignment_For_Allocation:        "
        & T'Max_Alignment_For_Allocation'Image
        & " storage elements ("
        & Integer'Image
            (T'Max_Alignment_For_Allocation
             * System.Storage_Unit)
        & " bits)");

    end Show_Sizes;

On a typical desktop PC, you might get 4 storage elements (corresponding to 32
bits) as the value returned by these attributes.

In the original implementation of the :ada:`Custom_Types` package, we allowed
the compiler to select the size of type :ada:`T`. We can be more specific in
the type declarations and use the :ada:`Size` aspect for that type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    package Custom_Types is

       type T is new Integer
         with Size => 48;

       type T_Array is
         array (Positive range <>) of T;

       type T_Access is access T;

    end Custom_Types;

Let's see how this change affects the :ada:`Size`,
:ada:`Max_Size_In_Storage_Elements`, and :ada:`Max_Alignment_For_Allocation`
attributes:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
    begin
       Put_Line
       ("T'Size:                                "
        & Integer'Image
            (T'Size
             / System.Storage_Unit)
        & " storage elements ("
        & T'Size'Image
        & " bits)");

       Put_Line
       ("T'Max_Size_In_Storage_Elements:        "
        & T'Max_Size_In_Storage_Elements'Image
        & " storage elements ("
        & Integer'Image
            (T'Max_Size_In_Storage_Elements
             * System.Storage_Unit)
        & " bits)");

       Put_Line
       ("T'Max_Alignment_For_Allocation:        "
        & T'Max_Alignment_For_Allocation'Image
        & " storage elements ("
        & Integer'Image
            (T'Max_Alignment_For_Allocation
             * System.Storage_Unit)
        & " bits)");

    end Show_Sizes;


If the code compiles, you should see that :ada:`T'Size` now corresponds to 6
storage elements (i.e. 48 bits). On a typical desktop PC, the value of
:ada:`T'Max_Size_In_Storage_Elements` and :ada:`T'Max_Alignment_For_Allocation`
should have increased to 8 storage elements (64 bits).


Code example with array type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that using the :ada:`Size` and :ada:`Max_Size_In_Storage_Elements`
attributes on array types can give you a potentially higher number:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
    begin
       Put_Line
       ("T_Array'Max_Size_In_Storage_Elements:  "
        & T_Array'Max_Size_In_Storage_Elements'Image
        & " storage elements ("
        & Long_Integer'Image
            (T_Array'Max_Size_In_Storage_Elements
             * System.Storage_Unit)
        & " bits)");

       Put_Line
       ("T_Array'Max_Alignment_For_Allocation:  "
        & T_Array'Max_Alignment_For_Allocation'Image
        & " storage elements ("
        & Integer'Image
            (T_Array'Max_Alignment_For_Allocation
             * System.Storage_Unit)
        & " bits)");

       Put_Line
       ("T_Array'Size:                          "
        & Long_Integer'Image
            (T_Array'Size
             / System.Storage_Unit)
        & " storage elements ("
        & T_Array'Size'Image
        & " bits)");

    end Show_Sizes;

In this case, these values indicate the maximum amount of memory that is
theoretically available for the array in the memory pool. This information
allows us to calculate the (theoretical) maximum number of components for an
array of this type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Memory_Management.Max_Allocation_Size_Alignment

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
    begin

       Put_Line
       ("T_Array: Max. number of components:  "
        & Long_Integer'Image
            (T_Array'Max_Size_In_Storage_Elements /
              (T'Size
               / System.Storage_Unit))
        & " components");

    end Show_Sizes;

By dividing the value returned by the :ada:`Max_Size_In_Storage_Elements`
attribute with the size of each individual component, we can get the maximum
number of components.


Storage elements
----------------

We saw parts of the :ada:`System.Storage_Elements` package while discussing
:ref:`addresses <Adv_Ada_Addresses>`. However, we haven't discussed yet
the main types from that package: :ada:`Storage_Element` and
:ada:`Storage_Array`.

We defined :ref:`storage elements <Adv_Ada_Storage_Elements>` previously.
In the :ada:`System.Storage_Elements` package, a storage element is represented
by the :ada:`Storage_Element` type. Its size (:ada:`Storage_Element'Size`) is
equal to :ada:`Storage_Unit` |mdash| which we also mentioned previously.

The :ada:`Storage_Array` type is an array type of storage elements. This is its
definition:

.. code-block:: ada

    type Storage_Array is
      array (Storage_Offset range <>) of
        aliased Storage_Element;

A storage array is used to represent a contiguous sequence of storage elements
in memory. In other words, you can think of an object of :ada:`Storage_Array`
type as a (memory) buffer.

.. admonition:: Important

    Note that arrays of :ada:`Storage_Array` type are guaranteed by the
    language to be contiguous. In contrast, storage pools are not required to
    be contiguous blocks of memory. However, each memory allocation in a
    storage pool returns a pointer to a contiguous block of memory.

.. admonition:: For further reading

    Note that the :ada:`Storage_Offset` is an integer type with a range defined
    by the compiler implementation. It's used not only
    in the definition of the :ada:`Storage_Array` but also in
    :ref:`address arithmetic <Adv_Ada_Address_Arithmetic>`, which we discussed
    in an earlier chapter.

In fact, the :ada:`Storage_Array` is used in the generic :ada:`Storage_IO`
package to define a memory buffer:

.. code-block:: ada

    with System.Storage_Elements;
    use  System.Storage_Elements;

    subtype Buffer_Type is
      Storage_Array (1 .. Buffer_Size);

Let's see a simple example that makes use of the :ada:`Storage_IO` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Storage_Elements

    pragma Ada_2022;

    with Ada.Text_IO;    use Ada.Text_IO;
    with Ada.Storage_IO;

    procedure Show_Storage_IO is
       type Rec is record
          A, B : Integer;
          C    : Float;
       end record;

       package Rec_Storage_IO is new
         Ada.Storage_IO (Element_Type => Rec);
       use Rec_Storage_IO;

       Buf    : Buffer_Type;
       R1, R2 : Rec;
    begin
       R1 := (1, 2, 3.0);
       Put_Line ("R1 : " & R1'Image);

       --  Writing from R1 to the buffer Buf:
       Write (Buf, R1);

       --  Reading from the buffer Buf to R2:
       Read (Buf, R2);

       Put_Line ("R2 : " & R2'Image);
    end Show_Storage_IO;

In this example, we instantiate the :ada:`Storage_IO` package for the
:ada:`Rec` type and declare a buffer :ada:`Buf` of :ada:`Buffer_Type` type.
(Note that :ada:`Buf` is essentially an array of :ada:`Storage_Array` type.)
We then use this buffer and write an element to it (via :ada:`Write`) and read
from it (via :ada:`Read`).

.. admonition:: Relevant topics

    - :arm22:`13.7.1 The Package System.Storage_Elements <13-7-1>`
    - :arm22:`A.9 The Generic Package Storage_IO <A-9>`


.. _Adv_Ada_Finalization:

Finalization
------------

.. admonition:: Relevant topics

    - :arm22:`Assignment and Finalization <7-6>`
    - :arm22:`Completion and Finalization <7-6-1>`

.. todo::

    Complete section!


.. _Adv_Ada_Memory_Pools:

Memory pools
------------

.. admonition:: Relevant topics

    - :arm22:`Memory pools <13-11>`
    - :arm22:`Default Storage Pools <13-11-3>`
    - :arm22:`Storage subpools <13-11-4>`
    - :arm22:`Subpool Reclamation <13-11-5>`

.. todo::

    Complete section!


Secondary stack
---------------

.. admonition:: Relevant topics

    - GNAT-specific secondary stack

.. todo::

    Complete section!
