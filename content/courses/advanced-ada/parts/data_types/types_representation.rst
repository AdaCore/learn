Types and Representation
========================

.. include:: ../../../global.txt

.. _Adv_Ada_Enumeration_Representation_Clauses:

Enumeration Representation Clauses
----------------------------------

We have talked about the internal code of an enumeration
:ref:`in another section <Adv_Ada_Enumeration_Position_Internal_Code>`.
We may change this internal code by using a representation clause, which has
the following format:

.. code-block:: ada

    for Primary_Color is (Red   =>    1,
                          Green =>    5,
                          Blue  => 1000);

The value of each code in a representation clause must be distinct. However, as
you can see above, we don't need to use sequential values |mdash| the values
must, however, increase for each enumeration.

We can rewrite the previous example using a representation clause:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Enumeration_Representation_Clauses.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed,
                    Thu, Fri,
                    Sat, Sun);

       for Day use (Mon => 2#00000001#,
                    Tue => 2#00000010#,
                    Wed => 2#00000100#,
                    Thu => 2#00001000#,
                    Fri => 2#00010000#,
                    Sat => 2#00100000#,
                    Sun => 2#01000000#);

    end Days;

    with Ada.Text_IO; use Ada.Text_IO;
    with Days;        use Days;

    procedure Show_Days is
    begin
       for D in Day loop
          Put_Line (Day'Image (D)
                    & " position      = "
                    & Integer'Image (Day'Pos (D)));
          Put_Line (Day'Image (D)
                    & " internal code = "
                    & Integer'Image
                        (Day'Enum_Rep (D)));
       end loop;
    end Show_Days;

Now, the value of the internal code is the one that we've specified in the
representation clause instead of being equivalent to the value of the
enumeration position.

In the example above, we're using binary values for each enumeration |mdash|
basically viewing the integer value as a bit-field and assigning one bit for
each enumeration. As long as we maintain an increasing order, we can use
totally arbitrary values as well. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Enumeration_Representation_Clauses.Enumeration_Values

    package Days is

       type Day is (Mon, Tue, Wed,
                    Thu, Fri,
                    Sat, Sun);

       for Day use (Mon =>  5,
                    Tue =>  9,
                    Wed => 42,
                    Thu => 49,
                    Fri => 50,
                    Sat => 66,
                    Sun => 99);

    end Days;


Data Representation
-------------------

This section provides a glimpse on attributes and aspects used for data
representation. They are usually used for embedded applications because of
strict requirements that are often found there. Therefore, unless you have
very specific requirements for your application, in most cases, you won't need
them. However, you should at least have a rudimentary understanding of them.
To read a thorough overview on this topic, please refer to the
:ref:`Introduction to Embedded Systems Programming <Intro_Embedded_Sys_Prog_Low_Level_Programming>`
course.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.2 Packed Types <13-2>`
    - :arm22:`13.3 Operational and Representation Attributes <13-3>`
    - :arm22:`13.5.3 Bit Ordering <13-5-3>`

Sizes
~~~~~

Ada offers multiple attributes to retrieve the size of a type or an object:

+-----------------------+-----------------------------------------------------+
| Attribute             | Description                                         |
+=======================+=====================================================+
| :ada:`Size`           | Size of the representation of a subtype or an       |
|                       | object.                                             |
+-----------------------+-----------------------------------------------------+
| :ada:`Object_Size`    | Size of a component or an aliased object.           |
|                       |                                                     |
+-----------------------+-----------------------------------------------------+
| :ada:`Component_Size` | Size of a component of an array.                    |
+-----------------------+-----------------------------------------------------+
| :ada:`Storage_Size`   | Number of storage elements reserved for an access   |
|                       | type or a task object.                              |
+-----------------------+-----------------------------------------------------+

For the first three attributes, the size is measured in bits. In the case of
:ada:`Storage_Size`, the size is measured in storage elements. Note that the
size information depends your target architecture. We'll discuss some examples
to better understand the differences among those attributes.

Size attribute and aspect
^^^^^^^^^^^^^^^^^^^^^^^^^

Let's start with a code example using the :ada:`Size` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_S32 is range 0 .. 127
         with Size => 32;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       V1 : UInt_7;
       V2 : UInt_7_S32;
    begin
       Put_Line ("UInt_7'Size:            "
                 & UInt_7'Size'Image);
       Put_Line ("UInt_7'Object_Size:     "
                 & UInt_7'Object_Size'Image);
       Put_Line ("V1'Size:                "
                 & V1'Size'Image);
       New_Line;

       Put_Line ("UInt_7_S32'Size:        "
                 & UInt_7_S32'Size'Image);
       Put_Line ("UInt_7_S32'Object_Size: "
                 & UInt_7_S32'Object_Size'Image);
       Put_Line ("V2'Size:                "
                 & V2'Size'Image);
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7'Size:             7
    UInt_7'Object_Size:      8
    V1'Size:                 8

    UInt_7_S32'Size:         32
    UInt_7_S32'Object_Size:  32
    V2'Size:                 32

When we use the :ada:`Size` attribute for a type :ada:`T`, we're retrieving the
minimum number of bits necessary to represent objects of that type. Note that
this is not the same as the actual size of an object of type :ada:`T` because
the compiler will select an object size that is appropriate for the target
architecture.

In the example above, the size of the :ada:`UInt_7` is 7 bits, while the most
appropriate size to store objects of this type in the memory of our target
architecture is 8 bits. To be more specific, the range of :ada:`UInt_7`
(0 .. 127) can be perfectly represented in 7 bits. However, most target
architectures don't offer 7-bit registers or 7-bit memory storage, so 8 bits is
the most appropriate size in this case.

We can retrieve the size of an object of type :ada:`T` by using the
:ada:`Object_Size`. Alternatively, we can use the :ada:`Size` attribute
directly on objects of type :ada:`T` to retrieve their actual size |mdash| in
our example, we write :ada:`V1'Size` to retrieve the size of :ada:`V1`.

In the example above, we've used both the :ada:`Size` attribute (for example,
:ada:`UInt_7'Size`) and the :ada:`Size` aspect (:ada:`with Size => 32`).
While the size attribute is a function that returns the size, the size aspect
is a request to the compiler to verify that the expected size can be used on
the target platform. You can think of this attribute as a dialog between the
developer and the compiler:

    (Developer) "I think that :ada:`UInt_7_S32` should be stored using at
    least 32 bits. Do you agree?"

    (Ada compiler) "For the target platform that you selected, I can confirm
    that this is indeed the case."

Depending on the target platform, however, the conversation might play out like
this:

    (Developer) "I think that :ada:`UInt_7_S32` should be stored using at
    least 32 bits. Do you agree?"

    (Ada compiler) "For the target platform that you selected, I cannot
    possibly do it! COMPILATION ERROR!"

Component size
^^^^^^^^^^^^^^

Let's continue our discussion on sizes with an example that makes use of the
:ada:`Component_Size` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Array is
         array (Positive range <>) of UInt_7;

       type UInt_7_Array_Comp_32 is
         array (Positive range <>) of UInt_7
           with Component_Size => 32;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       Arr_1 : UInt_7_Array (1 .. 20);
       Arr_2 : UInt_7_Array_Comp_32 (1 .. 20);
    begin
       Put_Line
         ("UInt_7_Array'Size:                   "
          & UInt_7_Array'Size'Image);
       Put_Line
         ("UInt_7_Array'Object_Size:            "
          & UInt_7_Array'Object_Size'Image);
       Put_Line
         ("UInt_7_Array'Component_Size:         "
          & UInt_7_Array'Component_Size'Image);
       Put_Line
         ("Arr_1'Component_Size:                "
          & Arr_1'Component_Size'Image);
       Put_Line
         ("Arr_1'Size:                          "
          & Arr_1'Size'Image);
       New_Line;

       Put_Line
         ("UInt_7_Array_Comp_32'Object_Size:    "
          & UInt_7_Array_Comp_32'Size'Image);
       Put_Line
         ("UInt_7_Array_Comp_32'Object_Size:    "
          & UInt_7_Array_Comp_32'Object_Size'Image);
       Put_Line
         ("UInt_7_Array_Comp_32'Component_Size: "
          &
          UInt_7_Array_Comp_32'Component_Size'Image);
       Put_Line
         ("Arr_2'Component_Size:                "
          & Arr_2'Component_Size'Image);
       Put_Line
         ("Arr_2'Size:                          "
          & Arr_2'Size'Image);
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Array'Size:                    17179869176
    UInt_7_Array'Object_Size:             17179869176
    UInt_7_Array'Component_Size:          8
    Arr_1'Component_Size:                 8
    Arr_1'Size:                           160

    UInt_7_Array_Comp_32'Size:            68719476704
    UInt_7_Array_Comp_32'Object_Size:     68719476704
    UInt_7_Array_Comp_32'Component_Size:  32
    Arr_2'Component_Size:                 32
    Arr_2'Size:                           640

Here, the value we get for :ada:`Component_Size` of the :ada:`UInt_7_Array`
type is 8 bits, which matches the :ada:`UInt_7'Object_Size` |mdash| as we've
seen in the previous subsection. In general, we expect the component size to
match the object size of the underlying type.

However, we might have component sizes that aren't equal to the object size of
the component's type. For example, in the declaration of the
:ada:`UInt_7_Array_Comp_32` type, we're using the :ada:`Component_Size` aspect
to query whether the size of each component can be 32 bits:

.. code-block:: ada

    type UInt_7_Array_Comp_32 is
      array (Positive range <>) of UInt_7
        with Component_Size => 32;

If the code compiles, we see this value when we use the :ada:`Component_Size`
attribute. In this case, even though :ada:`UInt_7'Object_Size` is 8 bits, the
component size of the array type (:ada:`UInt_7_Array_Comp_32'Component_Size`)
is 32 bits.

Note that we can use the :ada:`Component_Size` attribute with data types, as
well as with actual objects of that data type. Therefore, we can write
:ada:`UInt_7_Array'Component_Size` and :ada:`Arr_1'Component_Size`, for
example.

This big number (17179869176 bits) for :ada:`UInt_7_Array'Size` and
:ada:`UInt_7_Array'Object_Size` might be surprising for you. This is due to the
fact that Ada is reporting the size of the :ada:`UInt_7_Array` type for the
case when the complete range is used. Considering that we specified a positive
range in the declaration of the :ada:`UInt_7_Array` type, the maximum length
on this machine is 2\ :sup:`31` - 1. The object size of an array type is
calculated by multiplying the maximum length by the component size. Therefore,
the object size of the :ada:`UInt_7_Array` type corresponds to the
multiplication of 2\ :sup:`31` - 1 components (maximum length) by 8 bits
(component size).


.. _Adv_Ada_Storage_Size_Attribute:

Storage size
^^^^^^^^^^^^

To complete our discussion on sizes, let's look at this example of storage
sizes:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Sizes

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Access is access UInt_7;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       AV1, AV2 : UInt_7_Access;
    begin
       Put_Line
         ("UInt_7_Access'Storage_Size:          "
          & UInt_7_Access'Storage_Size'Image);
       Put_Line
         ("UInt_7_Access'Storage_Size (bits):   "
          & Integer'Image (UInt_7_Access'Storage_Size
                           * System.Storage_Unit));

       Put_Line
         ("UInt_7'Size:               "
          & UInt_7'Size'Image);
       Put_Line
         ("UInt_7_Access'Size:        "
          & UInt_7_Access'Size'Image);
       Put_Line
         ("UInt_7_Access'Object_Size: "
          & UInt_7_Access'Object_Size'Image);
       Put_Line
         ("AV1'Size:                  "
          & AV1'Size'Image);
       New_Line;

       Put_Line ("Allocating AV1...");
       AV1 := new UInt_7;
       Put_Line ("Allocating AV2...");
       AV2 := new UInt_7;
       New_Line;

       Put_Line
         ("AV1.all'Size:              "
          & AV1.all'Size'Image);
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Access'Storage_Size:           0
    UInt_7_Access'Storage_Size (bits):    0

    UInt_7'Size:                7
    UInt_7_Access'Size:         64
    UInt_7_Access'Object_Size:  64
    AV1'Size:                   64

    Allocating AV1...
    Allocating AV2...

    AV1.all'Size:               8

As we've mentioned earlier on, :ada:`Storage_Size` corresponds to the number of
storage elements reserved for an access type or a task object. In this case,
we see that the storage size of the :ada:`UInt_7_Access` type is zero. This is
because we haven't indicated that memory should be reserved for this data type.
Thus, the compiler doesn't reserve memory and simply sets the size to zero.

Because :ada:`Storage_Size` gives us the number of storage elements, we have
to multiply this value by :ada:`System.Storage_Unit` |mdash| which gives
us the size (in bits) of a single storage element |mdash| to get the total
storage size in bits. (In this particular example, however, the multiplication
doesn't make any difference, as the number of storage elements is zero.)

Note that the size of our original data type :ada:`UInt_7` is 7 bits, while the
size of its corresponding access type :ada:`UInt_7_Access` (and the access
object :ada:`AV1`) is 64 bits. This is due to the fact that the access type
doesn't contain an object, but rather memory information about an object. You
can retrieve the size of an object allocated via :ada:`new` by first
dereferencing it |mdash| in our example, we do this by writing
:ada:`AV1.all'Size`.

.. _Adv_Ada_Types_Storage_Size_Error:

Now, let's use the :ada:`Storage_Size` aspect to actually reserve memory for
this data type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Sizes
    :class: ada-run-expect-failure

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type UInt_7_Reserved_Access is access UInt_7
         with Storage_Size => 8;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;
    with System;

    with Custom_Types; use Custom_Types;

    procedure Show_Sizes is
       RAV1, RAV2 : UInt_7_Reserved_Access;
    begin
       Put_Line
       ("UInt_7_Reserved_Access'Storage_Size:        "
        & UInt_7_Reserved_Access'Storage_Size'Image);

       Put_Line
       ("UInt_7_Reserved_Access'Storage_Size (bits): "
        & Integer'Image
            (UInt_7_Reserved_Access'Storage_Size
             * System.Storage_Unit));

       Put_Line
         ("UInt_7_Reserved_Access'Size:        "
          & UInt_7_Reserved_Access'Size'Image);
       Put_Line
         ("UInt_7_Reserved_Access'Object_Size: "
          & UInt_7_Reserved_Access'Object_Size'Image);
       Put_Line
         ("RAV1'Size:                          "
          & RAV1'Size'Image);
       New_Line;

       Put_Line ("Allocating RAV1...");
       RAV1 := new UInt_7;
       Put_Line ("Allocating RAV2...");
       RAV2 := new UInt_7;
       New_Line;
    end Show_Sizes;

Depending on your target architecture, you may see this output:

::

    UInt_7_Reserved_Access'Storage_Size:         8
    UInt_7_Reserved_Access'Storage_Size (bits):  64

    UInt_7_Reserved_Access'Size:         64
    UInt_7_Reserved_Access'Object_Size:  64
    RAV1'Size:                           64

    Allocating RAV1...
    Allocating RAV2...

    raised STORAGE_ERROR : s-poosiz.adb:108 explicit raise

In this case, we're reserving 8 storage elements in the declaration of
:ada:`UInt_7_Reserved_Access`.

.. code-block:: ada

    type UInt_7_Reserved_Access is access UInt_7
      with Storage_Size => 8;

Since each storage unit corresponds to one byte (8 bits) in this architecture,
we're reserving a maximum of 64 bits for the :ada:`UInt_7_Reserved_Access`
type.

This example raises an exception at runtime |mdash| a storage error, to be more
specific. This is because the maximum reserved size is 64 bits, and the size of
a single access object is 64 bits as well. Therefore, after the first
allocation, the reserved storage space is already consumed, so we cannot
allocate a second access object.

This behavior might be quite limiting in many cases. However, for certain
applications where memory is very constrained, this might be exactly what we
want to see. For example, having an exception being raised when the allocated
memory for this data type has reached its limit might allow the application to
have enough memory to at least handle the exception gracefully.

Alignment
~~~~~~~~~

For many algorithms, it's important to ensure that we're using the appropriate
alignment. This can be done by using the :ada:`Alignment` attribute and the
:ada:`Alignment` aspect. Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Alignment

    package Custom_Types is

       type UInt_7 is range 0 .. 127;

       type Aligned_UInt_7 is new UInt_7
         with Alignment => 4;

    end Custom_Types;

    with Ada.Text_IO;  use Ada.Text_IO;

    with Custom_Types; use Custom_Types;

    procedure Show_Alignment is
       V         : constant UInt_7         := 0;
       Aligned_V : constant Aligned_UInt_7 := 0;
    begin
       Put_Line
         ("UInt_7'Alignment:           "
          & UInt_7'Alignment'Image);
       Put_Line
         ("UInt_7'Size:                "
          & UInt_7'Size'Image);
       Put_Line
         ("UInt_7'Object_Size:         "
          & UInt_7'Object_Size'Image);
       Put_Line
         ("V'Alignment:                "
          & V'Alignment'Image);
       Put_Line
         ("V'Size:                     "
          & V'Size'Image);
       New_Line;

       Put_Line
         ("Aligned_UInt_7'Alignment:   "
          & Aligned_UInt_7'Alignment'Image);
       Put_Line
         ("Aligned_UInt_7'Size:        "
          & Aligned_UInt_7'Size'Image);
       Put_Line
         ("Aligned_UInt_7'Object_Size: "
          & Aligned_UInt_7'Object_Size'Image);
       Put_Line
         ("Aligned_V'Alignment:        "
          & Aligned_V'Alignment'Image);
       Put_Line
         ("Aligned_V'Size:             "
          & Aligned_V'Size'Image);
       New_Line;
    end Show_Alignment;

Depending on your target architecture, you may see this output:

::

    UInt_7'Alignment:            1
    UInt_7'Size:                 7
    UInt_7'Object_Size:          8
    V'Alignment:                 1
    V'Size:                      8

    Aligned_UInt_7'Alignment:    4
    Aligned_UInt_7'Size:         7
    Aligned_UInt_7'Object_Size:  32
    Aligned_V'Alignment:         4
    Aligned_V'Size:              32

In this example, we're reusing the :ada:`UInt_7` type that we've already been
using in previous examples. Because we haven't specified any alignment for the
:ada:`UInt_7` type, it has an alignment of 1 storage unit (or 8 bits). However,
in the declaration of the :ada:`Aligned_UInt_7` type, we're using the
:ada:`Alignment` aspect to request an alignment of 4 storage units (or 32
bits):

.. code-block:: ada

    type Aligned_UInt_7 is new UInt_7
      with Alignment => 4;

When using the :ada:`Alignment` attribute for the :ada:`Aligned_UInt_7` type,
we can confirm that its alignment is indeed 4 storage units (bytes).

Note that we can use the :ada:`Alignment` attribute for both data types and
objects |mdash| in the code above, we're using :ada:`UInt_7'Alignment` and
:ada:`V'Alignment`, for example.

Because of the alignment we're specifying for the :ada:`Aligned_UInt_7` type,
its size |mdash| indicated by the :ada:`Object_Size` attribute |mdash| is 32
bits instead of 8 bits as for the :ada:`UInt_7` type.

Note that you can also retrieve the alignment associated with a class using
:ada:`S'Class'Alignment`. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Class_Alignment

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Class_Alignment is

       type Point_1D is tagged record
          X : Integer;
       end record;

       type Point_2D is new Point_1D with record
          Y : Integer;
       end record
         with Alignment => 16;

       type Point_3D is new Point_2D with record
          Z : Integer;
       end record;

    begin
       Put_Line ("1D_Point'Alignment:       "
                 & Point_1D'Alignment'Image);
       Put_Line ("1D_Point'Class'Alignment: "
                 & Point_1D'Class'Alignment'Image);
       Put_Line ("2D_Point'Alignment:       "
                 & Point_2D'Alignment'Image);
       Put_Line ("2D_Point'Class'Alignment: "
                 & Point_2D'Class'Alignment'Image);
       Put_Line ("3D_Point'Alignment:       "
                 & Point_3D'Alignment'Image);
       Put_Line ("3D_Point'Class'Alignment: "
                 & Point_3D'Class'Alignment'Image);
    end Show_Class_Alignment;

Overlapping Storage
~~~~~~~~~~~~~~~~~~~

Algorithms can be designed to perform in-place or out-of-place processing. In
other words, they can take advantage of the fact that input and output arrays
share the same storage space or not.

We can use the :ada:`Has_Same_Storage` and the :ada:`Overlaps_Storage`
attributes to retrieve more information about how the storage space of two
objects related to each other:

- the :ada:`Has_Same_Storage` attribute indicates whether two objects have the
  exact same storage.

  - A typical example is when both objects are exactly the same, so they
    obviously share the same storage. For example, for array :ada:`A`,
    :ada:`A'Has_Same_Storage (A)` is always :ada:`True`.

- the :ada:`Overlaps_Storage` attribute indicates whether two objects have at
  least one bit in common.

  - Note that, if two objects have the same storage, this implies that their
    storage also overlaps. In other words, :ada:`A'Has_Same_Storage (B) = True`
    implies that :ada:`A'Overlaps_Storage (B) = True`.


Let's look at this example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Overlapping_Storage

    package Int_Array_Processing is

       type Int_Array is
         array (Positive range <>) of Integer;

       procedure Show_Storage (X : Int_Array;
                               Y : Int_Array);

       procedure Process (X :     Int_Array;
                          Y : out Int_Array);

    end Int_Array_Processing;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Int_Array_Processing is

       procedure Show_Storage (X : Int_Array;
                               Y : Int_Array) is
       begin
          if X'Has_Same_Storage (Y) then
             Put_Line
             ("Info: X and Y have the same storage.");
          else
             Put_Line
               ("Info: X and Y don't have"
                & "the same storage.");
          end if;
          if X'Overlaps_Storage (Y) then
             Put_Line
               ("Info: X and Y overlap.");
          else
             Put_Line
               ("Info: X and Y don't overlap.");
          end if;
       end Show_Storage;

       procedure Process (X :     Int_Array;
                          Y : out Int_Array) is
       begin
          Put_Line ("==== PROCESS ====");
          Show_Storage (X, Y);

          if X'Has_Same_Storage (Y) then
             Put_Line ("In-place processing...");
          else
             if not X'Overlaps_Storage (Y) then
                Put_Line
                  ("Out-of-place processing...");
             else
                Put_Line
                  ("Cannot process "
                   & "overlapping arrays...");
             end if;
          end if;
          New_Line;
       end Process;

    end Int_Array_Processing;

    with Int_Array_Processing;
    use  Int_Array_Processing;

    procedure Main is
       A : Int_Array (1 .. 20) := (others => 3);
       B : Int_Array (1 .. 20) := (others => 4);
    begin
       Process (A, A);
       --  In-place processing:
       --  sharing the exact same storage

       Process (A (1 .. 10), A (10 .. 20));
       --  Overlapping one component: A (10)

       Process (A (1 .. 10), A (11 .. 20));
       --  Out-of-place processing:
       --  same array, but not sharing any storage

       Process (A, B);
       --  Out-of-place processing:
       --  two different arrays
    end Main;

In this code example, we implement two procedures:

- :ada:`Show_Storage`, which shows storage information about two arrays by
  using the :ada:`Has_Same_Storage` and :ada:`Overlaps_Storage` attributes.

- :ada:`Process`, which are supposed to process an input array :ada:`X` and
  store the processed data in the output array :ada:`Y`.

    - Note that the implementation of this procedure is actually just a
      mock-up, so that no processing is actually taking place.

We have four different instances of how we can call the :ada:`Process`
procedure:

- in the :ada:`Process (A, A)` call, we're using the same array for the input
  and output arrays. This is a perfect example of in-place processing. Because
  the input and the output arrays arguments are actually the same object, they
  obviously share the exact same storage.

- in the :ada:`Process (A (1 .. 10), A (10 .. 20))` call, we're using two
  slices of the :ada:`A` array as input and output arguments. In this case, a
  single component of the :ada:`A` array is shared: :ada:`A (10)`. Because the
  storage space is overlapping, but not exactly the same, neither in-place nor
  out-of-place processing can usually be used in this case.

- in the :ada:`Process (A (1 .. 10), A (11 .. 20))` call, even though we're
  using the same array :ada:`A` for the input and output arguments, we're using
  slices that are completely independent from each other, so that the input and
  output arrays are not sharing any storage in this case. Therefore, we can use
  out-of-place processing.

- in the :ada:`Process (A, B)` call, we have two different arrays |mdash| which
  obviously don't share any storage space |mdash|, so we can use out-of-place
  processing.

Packed Representation
~~~~~~~~~~~~~~~~~~~~~

As we've seen previously, the minimum number of bits required to represent a
data type might be less than the actual number of bits used to store an object
of that same type. We've seen an example where :ada:`UInt_7'Size` was 7 bits,
while :ada:`UInt_7'Object_Size` was 8 bits. The most extreme case is the one
for the :ada:`Boolean` type: in this case, :ada:`Boolean'Size` is 1 bit, while
:ada:`Boolean'Object_Size` might be 8 bits (or even more on certain
architectures). In such cases, we have 7 (or more) unused bits in memory for
each object of :ada:`Boolean` type. In other words, we're wasting memory. On
the other hand, we're gaining speed of access because we can directly access
each element without having to first change its internal representation back
and forth. We'll come back to this point later.

The situation is even worse when implementing bit-fields, which can be
declared as an array of :ada:`Boolean` components. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Non_Packed_Flags

    package Flag_Definitions is

       type Flags is
         array (Positive range <>) of Boolean;

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Flags is
       Flags_1 : Flags (1 .. 8);
    begin
       Put_Line ("Boolean'Size:           "
                 & Boolean'Size'Image);
       Put_Line ("Boolean'Object_Size:    "
                 & Boolean'Object_Size'Image);
       Put_Line ("Flags_1'Size:           "
                 & Flags_1'Size'Image);
       Put_Line ("Flags_1'Component_Size: "
                 & Flags_1'Component_Size'Image);
    end Show_Flags;

Depending on your target architecture, you may see this output:

::

    Boolean'Size:            1
    Boolean'Object_Size:     8
    Flags_1'Size:            64
    Flags_1'Component_Size:  8

In this example, we're declaring the :ada:`Flags` type as an array of
:ada:`Boolean` components. As we can see in this case, although the size of the
:ada:`Boolean` type is just 1 bit, an object of this type has a size of 8 bits.
Consequently, each component of the :ada:`Flags` type has a size of 8 bits.
Moreover, an array with 8 components of :ada:`Boolean` type |mdash| such as
the :ada:`Flags_1` array |mdash| has a size of 64 bits.

Therefore, having a way to compact the representation |mdash| so that we can
store multiple objects without wasting storage space |mdash| may help us
improving memory usage. This is actually possible by using the :ada:`Pack`
aspect. For example, we could extend the previous example and declare a
:ada:`Packed_Flags` type that makes use of this aspect:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Packed_Flags

    package Flag_Definitions is

       type Flags is
         array (Positive range <>) of Boolean;

       type Packed_Flags is
         array (Positive range <>) of Boolean
           with Pack;

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Packed_Flags is
       Flags_1 : Flags (1 .. 8);
       Flags_2 : Packed_Flags (1 .. 8);
    begin
       Put_Line ("Boolean'Size:           "
                 & Boolean'Size'Image);
       Put_Line ("Boolean'Object_Size:    "
                 & Boolean'Object_Size'Image);
       Put_Line ("Flags_1'Size:           "
                 & Flags_1'Size'Image);
       Put_Line ("Flags_1'Component_Size: "
                 & Flags_1'Component_Size'Image);
       Put_Line ("Flags_2'Size:           "
                 & Flags_2'Size'Image);
       Put_Line ("Flags_2'Component_Size: "
                 & Flags_2'Component_Size'Image);
    end Show_Packed_Flags;

Depending on your target architecture, you may see this output:

::

    Boolean'Size:            1
    Boolean'Object_Size:     8
    Flags_1'Size:            64
    Flags_1'Component_Size:  8
    Flags_2'Size:            8
    Flags_2'Component_Size:  1

In this example, we're declaring the :ada:`Flags_2` array of
:ada:`Packed_Flags` type. Its size is 8 bits |mdash| instead of the 64 bits
required for the :ada:`Flags_1` array. Because the array type
:ada:`Packed_Flags` is packed, we can now effectively use this type to store an
object of :ada:`Boolean` type using just 1 bit of the memory, as indicated by
the :ada:`Flags_2'Component_Size` attribute.

In many cases, we need to convert between a *normal* representation (such as
the one used for the :ada:`Flags_1` array above) to a packed representation
(such as the one for the :ada:`Flags_2` array). In many programming languages,
this conversion may require writing custom code with manual bit-shifting and
bit-masking to get the proper target representation. In Ada, however, we just
need to indicate the actual type conversion, and the compiler takes care of
generating code containing bit-shifting and bit-masking to performs the type
conversion.

Let's modify the previous example and introduce this type conversion:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Data_Representation.Flag_Conversion

    package Flag_Definitions is

       type Flags is
         array (Positive range <>) of Boolean;

       type Packed_Flags is
         array (Positive range <>) of Boolean
           with Pack;

       Default_Flags : constant Flags :=
         (True, True, False, True,
          False, False, True, True);

    end Flag_Definitions;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Flag_Definitions; use Flag_Definitions;

    procedure Show_Flag_Conversion is
       Flags_1 : Flags (1 .. 8);
       Flags_2 : Packed_Flags (1 .. 8);
    begin
       Flags_1 := Default_Flags;
       Flags_2 := Packed_Flags (Flags_1);

       for I in Flags_2'Range loop
          Put_Line (I'Image & ": "
                    & Flags_1 (I)'Image & ", "
                    & Flags_2 (I)'Image);
       end loop;
    end Show_Flag_Conversion;

In this extended example, we're now declaring :ada:`Default_Flags` as an array
of constant flags, which we use to initialize :ada:`Flags_1`.

The actual conversion happens with :ada:`Flags_2 := Packed_Flags (Flags_1)`.
Here, the type conversion :ada:`Packed_Flags()` indicates that we're converting
from the normal representation (used for the :ada:`Flags` type) to the packed
representation (used for :ada:`Packed_Flags` type). We don't need to write more
code than that to perform the correct type conversion.

Also, by using the same strategy, we could read information from a packed
representation. For example:

.. code-block:: ada

    Flags_1 := Flags (Flags_2);

In this case, we use :ada:`Flags()` to convert from a packed representation to
the normal representation.

We elaborate on the topic of converting between data representations in the
section on :ref:`changing data representation <Adv_Ada_Changing_Data_Representation>`.

Trade-offs
^^^^^^^^^^

As indicated previously, when we're using a packed representation (vs. using a
standard *unpacked* representation), we're trading off speed of access for less
memory consumption. The following table summarizes this:

+----------------+----------------------+-------------------------+
| Representation | More speed of access | Less memory consumption |
+================+======================+=========================+
| Unpacked       | X                    |                         |
+----------------+----------------------+-------------------------+
| Packed         |                      | X                       |
+----------------+----------------------+-------------------------+

On one hand, we have better memory usage when we apply packed representations
because we may save many bits for each object. On the other hand, there's a
cost associated with accessing those packed objects because they need to be
unpacked before we can actually access them. In fact, the compiler generates
code |mdash| using bit-shifting and bit-masking |mdash| that converts a packed
representation into an unpacked representation, which we can then access. Also,
when storing a packed object, the compiler generates code that converts the
unpacked representation of the object into the packed representation.

This packing and unpacking mechanism has a performance cost associated with it,
which results in less speed of access for packed objects. As usual in those
circumstances, before using packed representation, we should assess whether
memory constraints are more important than speed in our target architecture.

.. _Adv_Ada_Record_Representation_Storage_Clauses:

Record Representation and storage clauses
-----------------------------------------

In this section, we discuss how to use record representation clauses to specify
how a record is represented in memory. Our goal is to provide a brief
introduction into the topic. If you're interested in more details, you can find
a thorough discussion about record representation clauses in the
:ref:`Introduction to Embedded Systems Programming <Intro_Embedded_Sys_Prog_Low_Level_Programming>`
course.

Let's start with the simple approach of declaring a record type without
providing further information. In this case, we're basically asking the
compiler to select a reasonable representation for that record in the memory of
our target architecture.

Let's see a simple example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_1

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

Considering a typical 64-bit PC architecture with 8-bit storage units, and
:ada:`Integer` defined as a 32-bit type, we get this memory representation:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | component } | { { 0 | 1 | 2 | 3 } | A } | { { 4 | 5 | 6 | 7 } | B }"
            shape = "record"
        ];
   }

Each storage unit is a position in memory. In the graph above, the numbers on
the top (0, 1, 2, ...) represent those positions for record :ada:`R`.

In addition, we can show the bits that are used for components :ada:`A` and
:ada:`B`:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | bits | component } |  { { { 0 | #0 .. 7 } | { 1 | #8 .. #15 } | { 2 | #16 .. #23 } | { 3 | #24 .. #31 } } | A } | { { { 4 | #0 .. 7 } | { 5 | #8 .. #15 } | { 6 | #16 .. #23 } | { 7 | #24 .. #31 } } | B }"
            shape = "record"
        ];
   }

The memory representation we see in the graph above can be described in Ada
using representation clauses, as you can see in the code starting at the
:ada:`for R use record` line in the code example below |mdash| we'll discuss
the syntax and further details right after this example.

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_2

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

       --  Representation clause for record R:
       for R use record
          A at 0 range 0 .. 31;
          --   ^ starting memory position
          B at 4 range 0 .. 31;
          --           ^ first bit .. last bit
       end record;

    end P;

Here, we're specifying that the :ada:`A` component is stored in the bits #0 up
to #31 starting at position #0. Note that the position itself doesn't represent
an absolute address in the device's memory; instead, it's relative to the
memory space reserved for that record. The :ada:`B` component has the same
32-bit range, but starts at position #4.

This is a generalized view of the syntax:

.. code-block:: ada

    for Record_Type use record
       Component_Name at Start_Position
                      range First_Bit .. Last_Bit;
    end record;

These are the elements we see above:

- :ada:`Component_Name`: name of the component (from the record type
  declaration);

- :ada:`Start_Position`: start position |mdash| in storage units |mdash| of the
  memory space reserved for that component;

- :ada:`First_Bit`: first bit (in the start position) of the component;

- :ada:`Last_Bit`: last bit of the component.

Note that the last bit of a component might be in a different storage unit.
Since the :ada:`Integer` type has a larger width (32 bits) than the storage
unit (8 bits), components of that type span over multiple storage units.
Therefore, in our example, the first bit of component :ada:`A` is at position
#0, while the last bit is at position #3.

Also note that the last eight bits of component :ada:`A` are bits #24 .. #31.
If we think in terms of storage units, this corresponds to bits #0 .. #7 of
position #3. However, when specifying the last bit in Ada, we always use the
:ada:`First_Bit` value as a reference, not the position where those bits might
end up. Therefore, we write :ada:`range 0 .. 31`, well knowing that those 32
bits span over four storage units (positions #0 .. #3).

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.5.1 Record Representation Clauses <13-5-1>`

Storage Place Attributes
~~~~~~~~~~~~~~~~~~~~~~~~

We can retrieve information about the start position, and the first and last
bits of a component by using the storage place attributes:

- :ada:`Position`, which retrieves the start position of a component;

- :ada:`First_Bit`, which retrieves the first bit of a component;

- :ada:`Last_Bit`, which retrieves the last bit of a component.

Note, however, that these attributes can only be used with actual records, and
not with record types.

We can revisit the previous example and verify how the compiler represents the
:ada:`R` type in memory:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Storage_Place_Attributes

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    with P;           use P;

    procedure Show_Storage is
       R1 : R;
    begin
       Put_Line ("R'Size:              "
                 & R'Size'Image);
       Put_Line ("R'Object_Size:       "
                 & R'Object_Size'Image);
       New_Line;

       Put_Line ("System.Storage_Unit: "
                 & System.Storage_Unit'Image);
       New_Line;

       Put_Line ("R1.A'Position  : "
                 & R1.A'Position'Image);
       Put_Line ("R1.A'First_Bit : "
                 & R1.A'First_Bit'Image);
       Put_Line ("R1.A'Last_Bit  : "
                 & R1.A'Last_Bit'Image);
       New_Line;

       Put_Line ("R1.B'Position  : "
                 & R1.B'Position'Image);
       Put_Line ("R1.B'First_Bit : "
                 & R1.B'First_Bit'Image);
       Put_Line ("R1.B'Last_Bit  : "
                 & R1.B'Last_Bit'Image);
    end Show_Storage;

.. only:: builder_html

    On a typical 64-bit PC architecture, you probably see this output:

    ::

        R'Size:               64
        R'Object_Size:        64
        System.Storage_Unit:  8

        R1.A'Position  :  0
        R1.A'First_Bit :  0
        R1.A'Last_Bit  :  31

        R1.B'Position  :  4
        R1.B'First_Bit :  0
        R1.B'Last_Bit  :  31

First of all, we see that the size of the :ada:`R` type is 64 bits, which can
be explained by those two 32-bit integer components. Then, we see that
components :ada:`A` and :ada:`B` start at positions #0 and #4, and each one
makes use of bits in the range from #0 to #31. This matches the graph we've
seen above.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.5.2 Storage Place Attributes <13-5-2>`

Using Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use representation clauses to change the way the compiler handles
memory for a record type. For example, let's say we want to have an empty
storage unit between components :ada:`A` and :ada:`B`. We can use a
representation clause where we specify that component :ada:`B` starts at
position #5 instead of #4, leaving an empty byte after component :ada:`A` and
before component :ada:`B`:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ position | bits | component } |  { { { 0 | #0 .. 7 } | { 1 | #8 .. #15 } | { 2 | #16 .. #23 } | { 3 | #24 .. #31 } } | A } | { 4 |  |  } | { { { 5 | #0 .. 7 } | { 6 | #8 .. #15 } | { 7 | #16 .. #23 } | { 8 | #24 .. #31 } } | B }"
            shape = "record"
        ];
   }

This is the code that implements that:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

       for R use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        "
                 & R'Size'Image);
       Put_Line ("R'Object_Size: "
                 & R'Object_Size'Image);
    end Show_Empty_Byte;

When running the application above, we see that, due to the extra byte in the
record representation, the sizes increase. On a typical 64-bit PC,
:ada:`R'Size` is now 76 bits, which reflects the additional eight bits that we
introduced between components :ada:`A` and :ada:`B`. Depending on the target
architecture, you may also see that :ada:`R'Object_Size` is now 96 bits, which
is the size the compiler selects as the most appropriate for this record type.
As we've mentioned in the previous section, we can use aspects to request a
specific size to the compiler. In this case, we could use the
:ada:`Object_Size` aspect:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record
         with Object_Size => 72;

       for R use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        "
                 & R'Size'Image);
       Put_Line ("R'Object_Size: "
                 & R'Object_Size'Image);
    end Show_Empty_Byte;

If the code compiles, :ada:`R'Size` and :ada:`R'Object_Size` should now have
the same value.

Derived Types And Representation Clauses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In some cases, you might want to modify the memory representation of a record
without impacting existing code. For example, you might want to use a record
type that was declared in a package that you're not allowed to change. Also,
you would like to modify its memory representation in your application. A nice
strategy is to derive a type and use a representation clause for the derived
type.

We can apply this strategy on our previous example. Let's say we would like to
use record type :ada:`R` from package :ada:`P` in our application, but we're
not allowed to modify package :ada:`P` |mdash| or the record type, for that
matter. In this case, we could simply derive :ada:`R` as :ada:`R_New` and use a
representation clause for :ada:`R_New`. This is exactly what we do in the
specification of the child package :ada:`P.Rep`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Derived_Rep_Clauses_Empty_Byte

    package P is

       type R is record
          A : Integer;
          B : Integer;
       end record;

    end P;

    package P.Rep is

       type R_New is new R
         with Object_Size => 72;

       for R_New use record
          A at 0 range 0 .. 31;
          B at 5 range 0 .. 31;
       end record;

    end P.Rep;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;
    with P.Rep;       use P.Rep;

    procedure Show_Empty_Byte is
    begin
       Put_Line ("R'Size:        "
                 & R'Size'Image);
       Put_Line ("R'Object_Size: "
                 & R'Object_Size'Image);

       Put_Line ("R_New'Size:        "
                 & R_New'Size'Image);
       Put_Line ("R_New'Object_Size: "
                 & R_New'Object_Size'Image);
    end Show_Empty_Byte;

When running this example, we see that the :ada:`R` type retains the memory
representation selected by the compiler for the target architecture, while the
:ada:`R_New` has the memory representation that we specified.

Representation on Bit Level
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A very common application of representation clauses is to specify individual
bits of a record. This is particularly useful, for example, when mapping
registers or implementing protocols.

Let's consider the following fictitious register as an example:

.. graphviz::

    digraph foo {
        "Record_R" [
            label = "{ bit | component } | { { 0 | 1 }  | S } | { { 2 | 3 } | (reserved) } | { 4 | Error } | { { 5 | 6 | 7 } | V1 }"
            shape = "record"
        ];
   }

Here, :ada:`S` is the current status, :ada:`Error` is a flag, and :ada:`V1`
contains a value. Due to the fact that we can use representation clauses to
describe individual bits of a register as records, the implementation becomes
as simple as this:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_Simple_Reg

    package P is

      type Status is (Ready, Waiting,
                      Processing, Done);
      type UInt_3 is range 0 .. 2 ** 3 - 1;

       type Simple_Reg is record
          S     : Status;
          Error : Boolean;
          V1    : UInt_3;
       end record;

       for Simple_Reg use record
          S     at 0 range 0 .. 1;
          --  Bit #2 and 3: reserved!
          Error at 0 range 4 .. 4;
          V1    at 0 range 5 .. 7;
       end record;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Simple_Reg is
    begin
       Put_Line ("Simple_Reg'Size:        "
                 & Simple_Reg'Size'Image);
       Put_Line ("Simple_Reg'Object_Size: "
                 & Simple_Reg'Object_Size'Image);
    end Show_Simple_Reg;

As we can see in the declaration of the :ada:`Simple_Reg` type, each component
represents a field from our register, and it has a fixed location (which
matches the register representation we see in the graph above). Any operation
on the register is as simple as accessing the record component. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Record_Representation_Storage_Clauses.Rep_Clauses_Simple_Reg

    with Ada.Text_IO; use Ada.Text_IO;

    with P;           use P;

    procedure Show_Simple_Reg is
       Default : constant Simple_Reg :=
                   (S     => Ready,
                    Error => False,
                    V1    => 0);

       R : Simple_Reg := Default;
    begin
       Put_Line ("R.S:  " & R.S'Image);

       R.V1 := 4;

       Put_Line ("R.V1: " & R.V1'Image);
    end Show_Simple_Reg;

As we can see in the example, to retrieve the current status of the register,
we just have to write :ada:`R.S`. To update the *V1* field of the register with
the value 4, we just have to write :ada:`R.V1 := 4`. No extra code |mdash|
such as bit-masking or bit-shifting |mdash| is needed here.

.. admonition:: In other languages

    Some programming languages require that developers use complicated,
    error-prone approaches |mdash| which may include manually bit-shifting and
    bit-masking variables |mdash| to retrieve information from or store
    information to individual bits or registers. In Ada, however, this is
    efficiently handled by the compiler, so that developers only need to
    correctly describe the register mapping using representation clauses.


.. _Adv_Ada_Changing_Data_Representation:

Changing Data Representation
----------------------------

.. note::

    This section was originally written by Robert Dewar and published as
    `Gem #27: Changing Data Representation <https://www.adacore.com/gems/gem-27>`_
    and `Gem #28 <https://www.adacore.com/gems/gem-28>`_.

A powerful feature of Ada is the ability to specify the exact data layout. This
is particularly important when you have an external device or program that
requires a very specific format. Some examples are:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Com_Packet

    package Communication is

       type Com_Packet is record
          Key : Boolean;
          Id  : Character;
          Val : Integer range 100 .. 227;
       end record;

       for Com_Packet use record
          Key at 0 range 0 .. 0;
          Id  at 0 range 1 .. 8;
          Val at 0 range 9 .. 15;
       end record;

    end Communication;

which lays out the fields of a record, and in the case of :ada:`Val`, forces a
biased representation in which all zero bits represents 100. Another example
is:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Array_Rep

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);

       type Arr is array (1 .. 16) of Val
         with Component_Size => 3;

    end Array_Representation;

which forces the components to take only 3 bits, crossing byte boundaries as
needed. A final example is:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Enum_Rep

    package Enumeration_Representation is

       type Status is (Off, On, Unknown);
       for Status use (Off     => 2#001#,
                       On      => 2#010#,
                       Unknown => 2#100#);

    end Enumeration_Representation;

which allows specified values for an enumeration type, instead of the efficient
default values of 0, 1, 2.

In all these cases, we might use these representation clauses to match external
specifications, which can be very useful. The disadvantage of such layouts is
that they are inefficient, and accessing individual components, or, in the case
of the enumeration type, looping through the values can increase space and
time requirements for the program code.

One approach that is often effective is to read or write the data in question
in this specified form, but internally in the program represent the data in the
normal default layout, allowing efficient access, and do all internal
computations with this more efficient form.

To follow this approach, you will need to convert between the efficient format
and the specified format. Ada provides a very convenient method for doing this,
as described in :arm22:`RM 13.6 "Change of Representation" <13-6>`.

The idea is to use type derivation, where one type has the specified format and
the other has the normal default format. For instance for the array case above,
we would write:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Array_Rep

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);
       type Arr is array (1 .. 16) of Val;

       type External_Arr is new Arr
         with Component_Size => 3;

    end Array_Representation;

Now we read and write the data using the :ada:`External_Arr` type. When we want
to convert to the efficient form, :ada:`Arr`, we simply use a type conversion.

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Array_Rep

    with Array_Representation;
    use  Array_Representation;

    procedure Using_Array_For_IO is
       Input_Data  : External_Arr;
       Work_Data   : Arr;
       Output_Data : External_Arr;
    begin
       --  (read data into Input_Data)

       --  Now convert to internal form
       Work_Data := Arr (Input_Data);

       --  (computations using efficient
       --   Work_Data form)

       --  Convert back to external form
       Output_Data := External_Arr (Work_Data);

    end Using_Array_For_IO;

Using this approach, the quite complex task of copying all the data of the
array from one form to another, with all the necessary masking and shift
operations, is completely automatic.

Similar code can be used in the record and enumeration type cases. It is even
possible to specify two different representations for the two types, and
convert from one form to the other, as in:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Enum_Rep

    package Enumeration_Representation is

       type Status_In is (Off, On, Unknown);
       type Status_Out is new Status_In;

       for Status_In use (Off     => 2#001#,
                          On      => 2#010#,
                          Unknown => 2#100#);
       for Status_Out use (Off     => 103,
                           On      => 1045,
                           Unknown => 7700);

    end Enumeration_Representation;

There are two restrictions that must be kept in mind when using this feature.
First, you have to use a derived type. You can't put representation clauses on
subtypes, which means that the conversion must always be explicit. Second,
there is a rule :arm22:`RM 13.1 <13-1>` (10) that restricts the placement of
interesting representation clauses:

    10 For an untagged derived type, no type-related representation items are
    allowed if the parent type is a by-reference type, or has any user-defined
    primitive subprograms.

All the representation clauses that are interesting from the point of view of
change of representation are "type related", so for example, the following
sequence would be illegal:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.Array_Rep_2
    :class: ada-expect-compile-error

    package Array_Representation is

       type Val is (A, B, C, D, E, F, G, H);
       type Arr is array (1 .. 16) of Val;

       procedure Rearrange (Arg : in out Arr);

       type External_Arr is new Arr
         with Component_Size => 3;

    end Array_Representation;

Why these restrictions? Well, the answer is a little complex, and has to do
with efficiency considerations, which we will address below.

Restrictions
~~~~~~~~~~~~

In the previous subsection, we discussed the use of derived types and
representation clauses to achieve automatic change of representation. More
accurately, this feature is not completely automatic, since it requires you to
write an explicit conversion. In fact there is a principle behind the design
here which says that a change of representation should never occur implicitly
behind the back of the programmer without such an explicit request by means of
a type conversion.

The reason for that is that the change of representation operation can be very
expensive, since in general it can require component by component copying,
changing the representation on each component.

Let's have a look at the ``-gnatG`` expanded code to see what is hidden under
the covers here. For example, the conversion :ada:`Arr (Input_Data)` from the
previous example generates the following expanded code:

.. code-block::

       B26b : declare
          [subtype p__TarrD1 is integer range 1 .. 16]
          R25b : p__TarrD1 := 1;
       begin
          for L24b in 1 .. 16 loop
             [subtype p__arr___XP3 is
               system__unsigned_types__long_long_unsigned range 0 ..
               16#FFFF_FFFF_FFFF#]
             work_data := p__arr___XP3!((work_data and not shift_left!(
               16#7#, 3 * (integer(L24b - 1)))) or shift_left!(p__arr___XP3!
               (input_data (R25b)), 3 * (integer(L24b - 1))));
             R25b := p__TarrD1'succ(R25b);
          end loop;
       end B26b;

That's pretty horrible! In fact, we could have simplified it for this section,
but we have left it in its original form, so that you can see why it is nice to
let the compiler generate all this stuff so you don't have to worry about it
yourself.

Given that the conversion can be pretty inefficient, you don't want to convert
backwards and forwards more than you have to, and the whole approach is only
worthwhile if we'll be doing extensive computations involving the value.

The expense of the conversion explains two aspects of this feature that are not
obvious. First, why do we require derived types instead of just allowing
subtypes to have different representations, avoiding the need for an explicit
conversion?

The answer is precisely that the conversions are expensive, and you don't want
them happening behind your back. So if you write the explicit conversion, you
get all the gobbledygook listed above, but you can be sure that this never
happens unless you explicitly ask for it.

This also explains the restriction we mentioned in previous subsection from
:arm22:`RM 13.1 <13-1>` (10):

    10 For an untagged derived type, no type-related representation items are
    allowed if the parent type is a by-reference type, or has any user-defined
    primitive subprograms.

It turns out this restriction is all about avoiding implicit changes of
representation. Let's have a look at how type derivation works when there are
primitive subprograms defined at the point of derivation. Consider this
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.My_Int

    package My_Ints is

       type My_Int_1 is range 1 .. 10;

       function Odd (Arg : My_Int_1)
                     return Boolean;

       type My_Int_2 is new My_Int_1;

    end My_Ints;

    package body My_Ints is

       function Odd (Arg : My_Int_1)
                    return Boolean is
         (True);
       --  Dummy implementation!

    end My_Ints;

Now when we do the type derivation, we inherit the function :ada:`Odd` for
:ada:`My_Int_2`. But where does this function come from? We haven't
written it explicitly, so the compiler somehow materializes this new implicit
function. How does it do that?

We might think that a complete new function is created including a body in
which :ada:`My_Int_2` replaces :ada:`My_Int_1`, but that would be impractical
and expensive. The actual mechanism avoids the need to do this by use of
implicit type conversions. Suppose after the above declarations, we write:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.My_Int

    with My_Ints; use My_Ints;

    procedure Using_My_Int is
       Var : My_Int_2;
    begin

       if Odd (Var) then
          --   ^ Calling Odd function
          --     for My_Int_2 type.
          null;
       end if;

    end Using_My_Int;

The compiler translates this as:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Changing_Data_Representation.My_Int

    with My_Ints; use My_Ints;

    procedure Using_My_Int is
       Var : My_Int_2;
    begin

       if Odd (My_Int_1 (Var)) then
          --   ^ Converting My_Int_2 to
          --     My_Int_1 type before
          --     calling Odd function.
          null;
       end if;

    end Using_My_Int;

This implicit conversion is a nice trick, it means that we can get the effect
of inheriting a new operation without actually having to create it.
Furthermore, in a case like this, the type conversion generates no code,
since :ada:`My_Int_1` and :ada:`My_Int_2` have the same representation.

But the whole point is that they might not have the same representation if one
of them had a representation clause that made the representations different,
and in this case the implicit conversion inserted by the compiler could be
expensive, perhaps generating the junk we quoted above for the :ada:`Arr` case.
Since we never want that to happen implicitly, there is a rule to prevent it.

The business of forbidding by-reference types (which includes all tagged
types) is also driven by this consideration. If the representations are the
same, it is fine to pass by reference, even in the presence of the conversion,
but if there was a change of representation, it would force a copy, which would
violate the by-reference requirement.

So to summarize this section, on the one hand Ada gives you a very convenient
way to trigger these complex conversions between different representations. On
the other hand, Ada guarantees that you never get these potentially expensive
conversions happening unless you explicitly ask for them.

.. _Adv_Ada_Valid_Attribute:

Valid Attribute
---------------

When receiving data from external sources, we're subjected to problems such as
transmission errors. If not handled properly, erroneous data can lead to major
issues in an application.

One of those issues originates from the fact that transmission errors might
lead to invalid information stored in memory. When proper checks are active,
using invalid information is detected at runtime and an exception is raised at
this point, which might then be handled by the application.

Instead of relying on exception handling, however, we could instead ensure that
the information we're about to use is valid. We can do this by using the
:ada:`Valid` attribute. For example, if we have a variable :ada:`Var`, we can
verify that the value stored in :ada:`Var` is valid by writing
:ada:`Var'Valid`, which returns a :ada:`Boolean` value. Therefore, if the value
of :ada:`Var` isn't valid, :ada:`Var'Valid` returns :ada:`False`, so we can
have code that handles this situation before we actually make use of
:ada:`Var`. In other words, instead of handling a potential exception in other
parts of the application, we can proactively verify that input information is
correct and avoid that an exception is raised.

In the next example, we show an application that

- generates a file containing mock-up data, and then

- reads information from this file as state values.

The mock-up data includes valid and invalid states.

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Valid_Attribute.Valid_States
    :class: ada-run

    procedure Create_Test_File (File_Name : String);

    with Ada.Sequential_IO;

    procedure Create_Test_File (File_Name : String)
    is
       package Integer_Sequential_IO is new
         Ada.Sequential_IO (Integer);
       use Integer_Sequential_IO;

       F : File_Type;
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1);
       Write (F,  2);
       Write (F,  4);
       Write (F,  3);
       Write (F,  2);
       Write (F,  10);
       Close (F);
    end Create_Test_File;

    with Ada.Sequential_IO;

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off     => 1,
                      On      => 2,
                      Waiting => 4);

       package State_Sequential_IO is new
         Ada.Sequential_IO (State);

       procedure Read_Display_States
         (File_Name : String);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Read_Display_States
         (File_Name : String)
       is
          use State_Sequential_IO;

          F : State_Sequential_IO.File_Type;
          S : State;

          procedure Display_State (S : State) is
          begin
             --  Before displaying the value,
             --  check whether it's valid or not.
             if S'Valid then
                Put_Line (S'Image);
             else
                Put_Line ("Invalid value detected!");
             end if;
          end Display_State;

       begin
          Open (F, In_File, File_Name);

          while not End_Of_File (F) loop
             Read (F, S);
             Display_State (S);
          end loop;

          Close (F);
       end Read_Display_States;

    end States;

    with States;           use States;
    with Create_Test_File;

    procedure Show_States_From_File is
       File_Name : constant String := "data.bin";
    begin
       Create_Test_File (File_Name);
       Read_Display_States (File_Name);
    end Show_States_From_File;

.. only:: builder_html

    When running the application, you'd see this output:

    ::

        OFF
        ON
        WAITING
        Invalid value detected!
        ON
        Invalid value detected!

Let's start our discussion on this example with the :ada:`States` package,
which contains the declaration of the :ada:`State` type. This type is a simple
enumeration containing three states: :ada:`Off`, :ada:`On` and :ada:`Waiting`.
We're assigning specific integer values for this type by declaring an
enumeration representation clause. Note that we're using the :ada:`Size` aspect
to request that objects of this type have the same size as the :ada:`Integer`
type. This becomes important later on when parsing data from the file.

In the :ada:`Create_Test_File` procedure, we create a file containing integer
values, which is parsed later by the :ada:`Read_Display_States` procedure. The
:ada:`Create_Test_File` procedure doesn't contain any reference to the
:ada:`State` type, so we're not constrained to just writing information that is
valid for this type. On the contrary, this procedure makes use of the
:ada:`Integer` type, so we can write any integer value to the file. We use this
strategy to write both valid and invalid values of :ada:`State` to the file.
This allows us to simulate an environment where transmission errors occur.

We call the :ada:`Read_Display_States` procedure to read information from the
file and display each state stored in the file. In the main loop of this
procedure, we call :ada:`Read` to read a state from the file and store it in
the :ada:`S` variable. We then call the nested :ada:`Display_State` procedure
to display the actual state stored in :ada:`S`. The most important line of code
in the :ada:`Display_State` procedure is the one that uses the :ada:`Valid`
attribute:

.. code-block:: ada

    if S'Valid then

In this line, we're verifying that the :ada:`S` variable contains a valid state
before displaying the actual information from :ada:`S`. If the value stored in
:ada:`S` isn't valid, we can handle the issue accordingly. In this case, we're
simply displaying a message indicating that an invalid value was detected. If
we didn't have this check, the :ada:`Constraint_Error` exception would be
raised when trying to use invalid data stored in :ada:`S` |mdash| this would
happen, for example, after reading the integer value 3 from the input file.

In summary, using the :ada:`Valid` attribute is a good strategy we can employ
when we know that information stored in memory might be corrupted.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.9.2 The Valid Attribute <13-9-2>`

Unchecked Union
---------------

We've introduced variant records back in the
:ref:`Introduction to Ada course <Intro_Ada_Variant_Records>`.
In simple terms, a variant record is a record with discriminants that allows
for changing its structure. Basically, it's a record containing a :ada:`case`.

The :ada:`State_Or_Integer` declaration in the :ada:`States` package below is
an example of a variant record:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.State_Or_Integer

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off     => 1,
                      On      => 2,
                      Waiting => 4);

       type State_Or_Integer (Use_Enum : Boolean) is
       record
          case Use_Enum is
             when False => I : Integer;
             when True  => S : State;
          end case;
       end record;

       procedure Display_State_Value
         (V : State_Or_Integer);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value
         (V : State_Or_Integer)
       is
       begin
          Put_Line ("State: " & V.S'Image);
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

As mentioned in the previous course, if you try to access a component that is
not valid for your record, a :ada:`Constraint_Error` exception is raised. For
example, in the implementation of the :ada:`Display_State_Value` procedure,
we're trying to retrieve the value of the integer component (:ada:`I`) of the
:ada:`V` record. When calling this procedure, the :ada:`Constraint_Error`
exception is raised as expected because :ada:`Use_Enum` is set to :ada:`True`,
so that the :ada:`I` component is invalid |mdash| only the :ada:`S` component
is valid in this case.

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Variant_Rec_Error is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value (V);
    end Show_Variant_Rec_Error;

In addition to not being able to read the value of a component that isn't
valid, assigning a value to a component that isn't valid also raises an
exception at runtime. In this example, we cannot assign to :ada:`V.I`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Variant_Rec_Error is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.I := 4;
       --  Error: V.I cannot be accessed because
       --         Use_Enum is set to True.
    end Show_Variant_Rec_Error;

We may circumvent this limitation by using the :ada:`Unchecked_Union` aspect.
For example, we can derive a new type from :ada:`State_Or_Integer` and use
this aspect in its declaration. We do this in the declaration of the
:ada:`Unchecked_State_Or_Integer` type below.

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer

    package States is

       type State is (Off, On, Waiting)
         with Size => Integer'Size;

       for State use (Off     => 1,
                      On      => 2,
                      Waiting => 4);

       type State_Or_Integer (Use_Enum : Boolean) is
       record
          case Use_Enum is
             when False => I : Integer;
             when True  => S : State;
          end case;
       end record;

       type Unchecked_State_Or_Integer
         (Use_Enum : Boolean) is new
           State_Or_Integer (Use_Enum)
             with Unchecked_Union;

       procedure Display_State_Value
         (V : Unchecked_State_Or_Integer);

    end States;

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value
         (V : Unchecked_State_Or_Integer)
       is
       begin
          Put_Line ("State: " & V.S'Image);
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

Because we now use the :ada:`Unchecked_State_Or_Integer` type for the input
parameter of the :ada:`Display_State_Value` procedure, no exception is raised
at runtime, as both components are now accessible. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer

    with States; use States;

    procedure Show_Unchecked_Union is
       V : State_Or_Integer (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value
         (Unchecked_State_Or_Integer (V));
    end Show_Unchecked_Union;

Note that, in the call to the :ada:`Display_State_Value` procedure, we first
need to convert the :ada:`V` argument from the :ada:`State_Or_Integer` to the
:ada:`Unchecked_State_Or_Integer` type.

Also, we can assign to any of the components of a record that has the
:ada:`Unchecked_Union` aspect. In our example, we can now assign to both the
:ada:`S` and the :ada:`I` components of the :ada:`V` record:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer
             (Use_Enum => True);
    begin
       V := (Use_Enum => True, S => On);
       Display_State_Value (V);

       V := (Use_Enum => False, I => 4);
       Display_State_Value (V);
    end Show_Unchecked_Union;

In the example above, we're use an aggregate in the assignments to :ada:`V`. By
doing so, we avoid that :ada:`Use_Enum` is set to the *wrong* component. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer
             (Use_Enum => True);
    begin
       V.S := On;
       Display_State_Value (V);

       V.I := 4;
       --  Error: cannot directly assign to V.I,
       --         as Use_Enum is set to True.

       Display_State_Value (V);
    end Show_Unchecked_Union;

Here, even though the record has the :ada:`Unchecked_Union` attribute, we
cannot directly assign to the :ada:`I` component because :ada:`Use_Enum` is set
to :ada:`True`, so only the :ada:`S` is accessible. We can, however, read its
value, as we do in the :ada:`Display_State_Value` procedure.

Be aware that, due to the fact the union is not checked, we might write invalid
data to the record. In the example below, we initialize the :ada:`I` component
with 3, which is a valid integer value, but results in an invalid value for
the :ada:`S` component, as the value 3 cannot be mapped to the representation
of the :ada:`State` type.

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer
    :class: ada-run-expect-failure

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer
             (Use_Enum => True);
    begin
       V := (Use_Enum => False, I => 3);
       Display_State_Value (V);
    end Show_Unchecked_Union;

To mitigate this problem, we could use the :ada:`Valid` attribute |mdash|
discussed in the previous section |mdash| for the :ada:`S` component before
trying to use its value in the implementation of the :ada:`Display_State_Value`
procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Unchecked_Union.Unchecked_State_Or_Integer

    with Ada.Text_IO; use Ada.Text_IO;

    package body States is

       procedure Display_State_Value
         (V : Unchecked_State_Or_Integer)
       is
       begin
          if V.S'Valid then
             Put_Line ("State: " & V.S'Image);
          else
             Put_Line ("State: <invalid>");
          end if;
          Put_Line ("Value: " & V.I'Image);
       end Display_State_Value;

    end States;

    with States; use States;

    procedure Show_Unchecked_Union is
       V : Unchecked_State_Or_Integer
             (Use_Enum => True);
    begin
       V := (Use_Enum => False, I => 3);
       Display_State_Value (V);
    end Show_Unchecked_Union;


However, in general, you should avoid using the :ada:`Unchecked_Union` aspect
due to the potential issues you might introduce into your application. In the
majority of the cases, you don't need it at all |mdash| except for special
cases such as when interfacing with C code that makes use of union types or
solving very specific problems when doing low-level programming.

.. admonition:: In the Ada Reference Manual

    - :arm22:`B.3.3 Unchecked Union Types <B-3-3>`

Shared variable control
-----------------------

Ada has built-in support for handling both volatile and atomic data. Let's
start by discussing volatile objects.

.. admonition:: In the Ada Reference Manual

    - :arm22:`C.6 Shared Variable Control <C-6>`

Volatile
~~~~~~~~

A :wikipedia:`volatile <Volatile_(computer_programming)>`
object can be described as an object in memory whose value may change between
two consecutive memory accesses of a process A |mdash| even if process A itself
hasn't changed the value. This situation may arise when an object in memory is
being shared by multiple threads. For example, a thread *B* may modify the
value of that object between two read accesses of a thread *A*. Another typical
example is the one of
:wikipedia:`memory-mapped I/O <Memory-mapped_I/O>`, where
the hardware might be constantly changing the value of an object in memory.

Because the value of a volatile object may be constantly changing, a compiler
cannot generate code to store the value of that object in a register and then
use the value from the register in subsequent operations. Storing into a
register is avoided because, if the value is stored there, it would be outdated
if another process had changed the volatile object in the meantime. Instead,
the compiler generates code in such a way that the process must read the value
of the volatile object from memory for each access.

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Volatile_Object_Ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Object is
       Val : Long_Float with Volatile;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Long_Float (I);
       end loop;

       Put_Line ("Val: " & Long_Float'Image (Val));
    end Show_Volatile_Object;

In this example, :ada:`Val` has the :ada:`Volatile` aspect, which makes the
object volatile. We can also use the :ada:`Volatile` aspect in type
declarations. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Volatile_Type

    package Shared_Var_Types is

       type Volatile_Long_Float is new
         Long_Float with Volatile;

    end Shared_Var_Types;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Volatile_Type is
       Val : Volatile_Long_Float;
    begin
       Val := 0.0;
       for I in 0 .. 999 loop
          Val := Val + 2.0 * Volatile_Long_Float (I);
       end loop;

       Put_Line ("Val: "
                 & Volatile_Long_Float'Image (Val));
    end Show_Volatile_Type;

Here, we're declaring a new type :ada:`Volatile_Long_Float` in the
:ada:`Shared_Var_Types` package. This type is based on the :ada:`Long_Float`
type and uses the :ada:`Volatile` aspect. Any object of this type is
automatically volatile.

In addition to that, we can declare components of an array to be volatile. In
this case, we can use the :ada:`Volatile_Components` aspect in the array
declaration. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Volatile_Array_Components

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Volatile_Array_Components is
       Arr : array (1 .. 2) of Long_Float
               with Volatile_Components;
    begin
       Arr := (others => 0.0);

       for I in 0 .. 999 loop
          Arr (1) := Arr (1) +  2.0 * Long_Float (I);
          Arr (2) := Arr (2) + 10.0 * Long_Float (I);
       end loop;

       Put_Line ("Arr (1): "
                 & Long_Float'Image (Arr (1)));
       Put_Line ("Arr (2): "
                 & Long_Float'Image (Arr (2)));
    end Show_Volatile_Array_Components;

Note that it's possible to use the :ada:`Volatile` aspect for the array
declaration as well:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Volatile_Array

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Long_Float
               with Volatile;

    end Shared_Var_Types;

Note that, if the :ada:`Volatile` aspect is specified for an object, then the
:ada:`Volatile_Components` aspect is also specified automatically |mdash| if it
makes sense in the context, of course. In the example above, even though
:ada:`Volatile_Components` isn't specified in the declaration of the :ada:`Arr`
array , it's automatically set as well.

Independent
~~~~~~~~~~~

When you write code to access a single object in memory, you might actually be
accessing multiple objects at once. For example, when you declare types that
make use of representation clauses |mdash| as we've seen in previous sections
|mdash|, you might be accessing multiple objects that are grouped together in
a single storage unit. For example, if you have components :ada:`A` and
:ada:`B` stored in the same storage unit, you cannot update :ada:`A` without
actually writing (the same value) to :ada:`B`. Those objects aren't
independently addressable because, in order to access one of them, we have to
actually address multiple objects at once.

When an object is independently addressable, we call it an independent object.
In this case, we make sure that, when accessing that object, we won't be
simultaneously accessing another object. As a consequence, this feature limits
the way objects can be represented in memory, as we'll see next.

To indicate that an object is independent, we use the :ada:`Independent`
aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Object

    package Shared_Var_Types is

       I : Integer with Independent;

    end Shared_Var_Types;

Similarly, we can use this aspect when declaring types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Type

    package Shared_Var_Types is

       type Independent_Boolean is new Boolean
         with Independent;

       type Flags is record
          F1 : Independent_Boolean;
          F2 : Independent_Boolean;
       end record;

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Independent_Boolean` type and using
it in the declaration of the :ada:`Flag` record type. Let's now derive the
:ada:`Flags` type and use a representation clause for the derived type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Type
    :class: ada-expect-compile-error

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. 0;
          F2 at 0 range 1 .. 1;
          --            ^  ERROR: start position of
          --                      F2 is wrong!
          --    ^          ERROR: F1 and F2 share the
          --                      same storage unit!
       end record;

    end Shared_Var_Types.Representation;

As you can see when trying to compile this example, the representation clause
that we used for :ada:`Rep_Flags` isn't following these limitations:

1. The size of each independent component must be a multiple of a storage unit.

2. The start position of each independent component must be a multiple of a
   storage unit.

For example, for architectures that have a storage unit of one byte |mdash|
such as standard desktop computers |mdash|, this means that the size and the
position of independent components must be a multiple of a byte. Let's correct
the issues in the code above by:

- setting the size of each independent component to correspond to
  :ada:`Storage_Unit` |mdash| using a range between 0 and
  :ada:`Storage_Unit - 1` |mdash|, and

- setting the start position to zero.

This is the corrected version:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 1 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

Note that the representation that we're now using for :ada:`Rep_Flags` is most
likely the representation that the compiler would have chosen for this data
type. We could, however, have added an empty storage unit between :ada:`F1` and
:ada:`F2` |mdash| by simply writing :ada:`F2 at 2 ...`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Type

    with System;

    package Shared_Var_Types.Representation is

       type Rep_Flags is new Flags;

       for Rep_Flags use record
          F1 at 0 range 0 .. System.Storage_Unit - 1;
          F2 at 2 range 0 .. System.Storage_Unit - 1;
       end record;

    end Shared_Var_Types.Representation;

As long as we follow the rules for independent objects, we're still allowed to
use representation clauses that don't correspond to the one that the compiler
might select.

For arrays, we can use the :ada:`Independent_Components` aspect:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Independent_Components

    package Shared_Var_Types is

       Flags : array (1 .. 8) of Boolean
                 with Independent_Components;

    end Shared_Var_Types;

We've just seen in a previous example that some representation clauses might
not work with objects and types that have the :ada:`Independent` aspect. The
same restrictions apply when we use the :ada:`Independent_Components` aspect.
For example, this aspect prevents that array components are packed when the
:ada:`Pack` aspect is used. Let's discuss the following erroneous code example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Packed_Independent_Components
    :class: ada-expect-compile-error

    package Shared_Var_Types is

       type Flags is
         array (Positive range <>) of Boolean
           with Independent_Components, Pack;

       F : Flags (1 .. 8) with Size => 8;

    end Shared_Var_Types;

As expected, this code doesn't compile. Here, we can have either independent
components, or packed components. We cannot have both at the same time because
packed components aren't independently addressable. The compiler warns us that
the :ada:`Pack` aspect won't have any effect on independent components. When we
use the :ada:`Size` aspect in the declaration of :ada:`F`, we confirm this
limitation. If we remove the :ada:`Size` aspect, however, the code is compiled
successfully because the compiler ignores the :ada:`Pack` aspect and allocates
a larger size for :ada:`F`:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Packed_Independent_Components

    package Shared_Var_Types is

       type Flags is
         array (Positive range <>) of Boolean
           with Independent_Components, Pack;

    end Shared_Var_Types;

    with Ada.Text_IO; use Ada.Text_IO;
    with System;

    with Shared_Var_Types; use Shared_Var_Types;

    procedure Show_Flags_Size is
       F : Flags (1 .. 8);
    begin
       Put_Line ("Flags'Size:      "
                 & F'Size'Image & " bits");
       Put_Line ("Flags (1)'Size:  "
                 & F (1)'Size'Image & " bits");
       Put_Line ("# storage units: "
                 & Integer'Image
                     (F'Size /
                      System.Storage_Unit));
    end Show_Flags_Size;

As you can see in the output of the application, even though we specify the
:ada:`Pack` aspect for the :ada:`Flags` type, the compiler allocates eight
storage units, one per each component of the :ada:`F` array.


.. _Adv_Ada_Shared_Variable_Control_Atomic:

Atomic
~~~~~~

An atomic object is an object that only accepts atomic reads and updates. The
Ada standard specifies that "for an atomic object (including an atomic
component), all reads and updates of the object as a whole are indivisible."
In this case, the compiler must generate Assembly code in such a way that reads
and updates of an atomic object must be done in a single instruction, so that
no other instruction could execute on that same object before the read or
update completes.

.. admonition:: In other contexts

    Generally, we can say that operations are said to be atomic when they can
    be completed without interruptions. This is an important requirement when
    we're performing operations on objects in memory that are shared between
    multiple processes.

    This definition of atomicity above is used, for example, when implementing
    databases. However, for this section, we're using the term "atomic"
    differently. Here, it really means that reads and updates must be performed
    with a single Assembly instruction.

    For example, if we have a 32-bit object composed of four 8-bit bytes, the
    compiler cannot generate code to read or update the object using four 8-bit
    store / load instructions, or even two 16-bit store / load instructions.
    In this case, in order to maintain atomicity, the compiler must generate
    code using one 32-bit store / load instruction.

    Because of this strict definition, we might have objects for which the
    :ada:`Atomic` aspect cannot be specified. Lots of machines support integer
    types that are larger than the native word-sized integer. For example, a
    16-bit machine probably supports both 16-bit and 32-bit integers, but only
    16-bit integer objects can be marked as atomic |mdash| or, more generally,
    only objects that fit into at most 16 bits.

Atomicity may be important, for example, when dealing with shared hardware
registers. In fact, for certain architectures, the hardware may require that
memory-mapped registers are handled atomically. In Ada, we can use the
:ada:`Atomic` aspect to indicate that an object is atomic. This is how we can
use the aspect to declare a shared hardware register:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Object

    with System;

    package Shared_Var_Types is

    private
       R : Integer
             with Atomic,
                  Address =>
                    System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

Note that the :ada:`Address` aspect allows for assigning a variable to a
specific location in the memory. In this example, we're using this aspect to
specify the address of the memory-mapped register.

Later on, we talk again about the
:ref:`Address aspect <Adv_Ada_Address_Aspect>` and the GNAT-specific
:ref:`System'To_Address attribute <Adv_Ada_System_To_Address>`.

In addition to atomic objects, we can declare atomic types |mdash| similar to
what we've seen before for volatile objects. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Types

    with System;

    package Shared_Var_Types is

       type Atomic_Integer is new Integer
         with Atomic;

    private
       R : Atomic_Integer
             with Address =>
                    System'To_Address (16#FFFF00A0#);

    end Shared_Var_Types;

In this example, we're declaring the :ada:`Atomic_Integer` type, which is an
atomic type. Objects of this type |mdash| such as :ada:`R` in this example
|mdash| are automatically atomic.

We can also declare atomic array components:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Array_Components

    package Shared_Var_Types is

    private
       Arr : array (1 .. 2) of Integer
               with Atomic_Components;

    end Shared_Var_Types;

This example shows the declaration of the :ada:`Arr` array, which has atomic
components |mdash| the atomicity of its components is indicated by the
:ada:`Atomic_Components` aspect.

Note that if an object is atomic, it is also volatile and independent. In other
words, these type declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Volatile_Independent

    package Shared_Var_Types is

       type Atomic_Integer_1 is new Integer
         with Atomic;

       type Atomic_Integer_2 is new Integer
         with Atomic,
              Volatile,
              Independent;

    end Shared_Var_Types;

A simular rule applies to components of an array. When we use the
:ada:`Atomic_Components`, the following aspects are implied: :ada:`Volatile`,
:ada:`Volatile_Components` and :ada:`Independent_Components`. For example,
these array declarations are equivalent:

.. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Shared_Variable_Control.Atomic_Volatile_Independent

    package Shared_Var_Types is

       Arr_1 : array (1 .. 2) of Integer
                 with Atomic_Components;

       Arr_2 : array (1 .. 2) of Integer
                 with Atomic_Components,
                      Volatile,
                      Volatile_Components,
                      Independent_Components;

    end Shared_Var_Types;


..
    TO BE DONE:

   :ada:`Full_Access_Only`
   ~~~~~~~~~~~~~~~~~~~~~~~

   .. admonition:: Relevant topics

      - **Briefly** discuss :ada:`Full_Access_Only`
      - :arm22:`The Package System.Atomic_Operations <C-6-1>`

   .. todo::

      Add to previous section!


.. _Adv_Ada_Addresses:

Addresses
---------

In other languages, such as C, the concept of pointers and addresses plays
a prominent role. (In fact, in C, many optimizations rely on the usage of
pointer arithmetic.) The concept of addresses does exist in Ada, but it's
mainly reserved for very specific applications, mostly related to low-level
programming. In general, other approaches |mdash| such as using access types
|mdash| are more than sufficient. (We discuss
:doc:`access types <../resource_management/access_types>` in another chapter.
Also, later on in that chapter, we discuss the
:ref:`relation between access types and addresses <Adv_Ada_Access_Address>`.)
In this section, we discuss some details about using addresses in Ada.

We make use of the :ada:`Address` type, which is defined in the :ada:`System`
package, to handle addresses. In contrast to other programming languages (such
as C or C++), an address in Ada isn't an integer value |mdash| its definition
actually depends on the compiler implementation, but for now, let's consider
it to be a private type.

The :ada:`Address` type has support for
:ref:`address comparison <Adv_Ada_Address_Comparison>` and
:ref:`address arithmetic <Adv_Ada_Address_Arithmetic>` (also
known as *pointer arithmetic* in C). We discuss these topics later in this
section. First, let's talk about the :ada:`Address` attribute and the
:ada:`Address` aspect.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.7 The Package System <13-7>`


.. _Adv_Ada_Address_Attribute:

Address attribute
~~~~~~~~~~~~~~~~~

The :ada:`Address` attribute allows us to get the address of an object.
For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Address_Attribute

    with System; use System;

    procedure Use_Address is
       I : aliased Integer := 5;
       A : Address;
    begin
       A := I'Address;
    end Use_Address;

Here, we're assigning the address of the :ada:`I` object to the :ada:`A` address.

.. admonition:: In the GNAT toolchain

    GNAT offers a very useful extension to the :ada:`System` package to
    retrieve a string for an address: :ada:`System.Address_Image`. This is the
    function profile:

    .. code-block:: ada

        function System.Address_Image
          (A : System.Address) return String;

    We can use this function to display the address in an user message, for
    example:

    .. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Show_Address_Attribute

        with Ada.Text_IO; use Ada.Text_IO;
        with System.Address_Image;

        procedure Show_Address_Attribute is
           I  : aliased Integer := 5;
        begin
           Put_Line ("Address : "
                     & System.Address_Image (I'Address));
        end Show_Address_Attribute;

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.3 Operational and Representation Attributes <13-3>`
    - :arm22:`13.7 The Package System <13-7>`


.. _Adv_Ada_Address_Aspect:

Address aspect
~~~~~~~~~~~~~~

Usually, we let the compiler select the address of an object in memory, or let
it use a register to store that object. However, we can specify the address of
an object with the :ada:`Address` aspect. In this case, the compiler won't
select an address automatically, but use the address that we're specifying. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Address_Aspect

    with System; use System;
    with System.Address_Image;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Address is

       I_Main   : Integer;
       I_Mapped : Integer
                    with Address => I_Main'Address;
    begin
       Put_Line ("I_Main'Address   : "
                  & System.Address_Image
                      (I_Main'Address));
       Put_Line ("I_Mapped'Address : "
                  & System.Address_Image
                      (I_Mapped'Address));
    end Show_Address;

This approach allows us to create an overlay. For example:

.. code:: ada no_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Simple_Overlay
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Overlay is
       type State is (Off, State_1, State_2)
         with Size => Integer'Size;

       for State use (Off     => 0,
                      State_1 => 32,
                      State_2 => 64);

       S : State;
       I : Integer
         with Address => S'Address, Import, Volatile;
    begin
       S := State_2;
       Put_Line ("I = " & Integer'Image (I));
    end Simple_Overlay;

Here, :ada:`I` is an overlay of :ada:`S`, as it uses :ada:`S'Address`. With
this approach, we can either use the enumeration directly (by using the
:ada:`S` object of :ada:`State` type) or its integer representation (by using
the :ada:`I` variable).

.. _Adv_Ada_System_To_Address:

.. admonition:: In the GNAT toolchain

    We could call the GNAT-specific :ada:`System'To_Address` attribute when using
    the :ada:`Address` aspect, as we did while talking about the
    :ref:`Atomic <Adv_Ada_Shared_Variable_Control_Atomic>` aspect:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Show_Access_Address

        with System;

        package Shared_Var_Types is

        private
           R : Integer
                 with Atomic,
                      Address =>
                        System'To_Address (16#FFFF00A0#);

        end Shared_Var_Types;

    In this case, :ada:`R` will refer to the address in memory that we're
    specifying (:ada:`16#FFFF00A0#` in this case).

    As explained in the
    `GNAT Reference Manual <https://gcc.gnu.org/onlinedocs/gnat_rm/Attribute-To_005fAddress.html>`_,
    the :ada:`System'To_Address` attribute denotes a function identical to
    :ada:`To_Address` (from the :ada:`System.Storage_Elements` package) except
    that it is a static attribute. (We talk about the
    :ref:`To_Address function <Adv_Ada_Address_Integer>` function later on.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.3 Operational and Representation Attributes <13-3>`
    - :arm22:`13.7 The Package System <13-7>`
    - :arm22:`13.7.1 The Package System.Storage_Elements <13-7-1>`


.. _Adv_Ada_Address_Comparison:

Address comparison
~~~~~~~~~~~~~~~~~~

We can compare addresses using the common comparison operators. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Address_Aspect

    with System; use System;
    with System.Address_Image;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Address is

       I, J : Integer;
    begin
       Put_Line ("I'Address   : "
                  & System.Address_Image
                      (I'Address));
       Put_Line ("J'Address   : "
                  & System.Address_Image
                      (J'Address));

       if I'Address = J'Address then
          Put_Line ("I'Address = J'Address");
       elsif I'Address < J'Address then
          Put_Line ("I'Address < J'Address");
       else
          Put_Line ("I'Address > J'Address");
       end if;
    end Show_Address;

In this example, we compare the address of the :ada:`I` object with the address
of the :ada:`J` object using the :ada:`=`, :ada:`<` and :ada:`>` operators.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.7 The Package System <13-7>`


.. _Adv_Ada_Address_Integer:

Address to integer conversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :ada:`System.Storage_Elements` package offers an integer representation of
an address via the :ada:`Integer_Address` type, which is an integer type
unrelated to common integer types such as :ada:`Integer` and
:ada:`Long_Integer`. (The actual definition of :ada:`Integer_Address` is
compiler-dependent, and it can be a signed or modular integer subtype.)

We can convert between the :ada:`Address` and :ada:`Integer_Address` types by
using the :ada:`To_Address` and :ada:`To_Integer` functions. Let's see an
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Pointer_Arith_Ada

    with System;      use System;

    with System.Storage_Elements;
    use  System.Storage_Elements;

    with System.Address_Image;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Address is
       I      : Integer;
       A1, A2 : Address;
       IA     : Integer_Address;
    begin
       A1 := I'Address;
       IA := To_Integer (A1);
       A2 := To_Address (IA);

       Put_Line ("A1 : "
                  & System.Address_Image (A1));
       Put_Line ("IA : "
                  & Integer_Address'Image (IA));
       Put_Line ("A2 : "
                  & System.Address_Image (A2));
    end Show_Address;

Here, we retrieve the address of the :ada:`I` object and store it in the
:ada:`A1` address. Then, we convert :ada:`A1` to an integer address by calling
:ada:`To_Integer` (and store it in :ada:`IA`). Finally, we convert this
integer address back to an actual address by calling :ada:`To_Address`.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.7.1 The Package System.Storage_Elements <13-7-1>`


.. _Adv_Ada_Address_Arithmetic:

Address arithmetic
~~~~~~~~~~~~~~~~~~~~

Although Ada supports address arithmetic, which we discuss in this section, it
should be reserved for very specific applications such as low-level
programming. However, even in situations that require close access to the
underlying hardware, using address arithmetic might not be the approach you
should consider |mdash| make sure to evaluate other options first!

Ada supports address arithmetic via the :ada:`System.Storage_Elements` package,
which includes operators such as :ada:`+` and :ada:`-` for addresses. Let's see
a code example where we iterate over an array by incrementing an address that
*points* to each component in memory:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Pointer_Arith_Ada

    with System;      use System;

    with System.Storage_Elements;
    use  System.Storage_Elements;

    with System.Address_Image;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Address is

       Arr : array (1 .. 10) of Integer;
       A   : Address := Arr'Address;
       --               ^^^^^^^^^^^
       --   Initializing address object with
       --   address of the first component of Arr.
       --
       --   We could write this as well:
       --   ___ := Arr (1)'Address

    begin
       for I in Arr'Range loop
          declare
             Curr : Integer
                      with Address => A;
          begin
             Curr := I;
             Put_Line ("Curr'Address : "
                       & System.Address_Image
                           (Curr'Address));
          end;

          --
          --  Address arithmetic
          --
          A := A + Storage_Offset (Integer'Size)
                     / Storage_Unit;
          --     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          --       Moving to next component
       end loop;

       for I in Arr'Range loop
         Put_Line ("Arr ("
                   & Integer'Image (I)
                   & ") :"
                   & Integer'Image (Arr (I)));
       end loop;
    end Show_Address;

In this example, we initialize the address :ada:`A` by retrieving the address
of the first component of the array :ada:`Arr`. (Note that we could have
written :ada:`Arr(1)'Address` instead of :ada:`Arr'Address`. In any
case, the language guarantees that :ada:`Arr'Address` gives us the address of
the first component, i.e. :ada:`Arr'Address = Arr(1)'Address`.)

Then, in the loop, we declare
an overlay :ada:`Curr` using the current value of the :ada:`A` address. We can
then operate on this overlay |mdash| here, we assign :ada:`I` to :ada:`Curr`.
Finally, in the loop, we increment address :ada:`A` and make it *point* to the
next component in the :ada:`Arr` array |mdash| to do so, we calculate the size
of an :ada:`Integer` component in storage units. (For details on storage units,
see the section on
:ref:`storage size attribute <Adv_Ada_Storage_Size_Attribute>`.)

.. admonition:: In other languages

    The code example above corresponds (more or less) to the following C code:

    .. code:: c manual_chop run_button main=main.c project=Courses.Advanced_Ada.Data_Types.Type_Representation.Addresses.Pointer_Arith_C

        !main.c
        #include <stdio.h>

        int main(int argc, const char * argv[])
        {
            int i;
            int arr[10];

            int *a = arr;
            /* int *a = &arr[0]; */

            for (i = 0; i < 10; i++)
            {
                *a++ = i;
                printf("curr address: %p\n", a);
            }

            for (i = 0; i < 10; i++)
            {
                printf("arr[%d]: %d\n", i, arr[i]);
            }

            return 0;
        }

    While pointer arithmetic is very common in C, using address arithmetic in
    Ada is far from common, and it should be only used when it's really
    necessary to do so.

.. admonition:: In the Ada Reference Manual

    - :arm22:`13.3 Operational and Representation Attributes <13-3>`
    - :arm22:`13.7.1 The Package System.Storage_Elements <13-7-1>`


Discarding names
----------------

As we know, we can use the :ada:`Image` attribute of a type to get a string
associated with this type. This is useful for example when we want to display a
user message for an enumeration type:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Discarding_Names.Enumeration_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Enumeration_Image is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       M : constant Months := January;
    begin
       Put_Line ("Month: "
                 & Months'Image (M));
    end Show_Enumeration_Image;

This is similar to having this code:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Discarding_Names.Enumeration_Image

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Enumeration_Image is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       M : constant Months := January;

       function Months_Image (M : Months)
                              return String is
       begin
          case M is
             when January   => return "JANUARY";
             when February  => return "FEBRUARY";
             when March     => return "MARCH";
             when April     => return "APRIL";
             when May       => return "MAY";
             when June      => return "JUNE";
             when July      => return "JULY";
             when August    => return "AUGUST";
             when September => return "SEPTEMBER";
             when October   => return "OCTOBER";
             when November  => return "NOVEMBER";
             when December  => return "DECEMBER";
          end case;
       end Months_Image;

    begin
       Put_Line ("Month: "
                 & Months_Image (M));
    end Show_Enumeration_Image;

Here, the :ada:`Months_Image` function associates a string with each month of
the :ada:`Months` enumeration. As expected, the compiler needs to store the
strings used in the :ada:`Months_Image` function when compiling this code.
Similarly, the compiler needs to store strings for the :ada:`Months`
enumeration for the :ada:`Image` attribute.

Sometimes, we don't need to call the :ada:`Image` attribute for a type. In
this case, we could save some storage by eliminating the strings associated
with the type. Here, we can use the :ada:`Discard_Names` aspect to request the
compiler to reduce |mdash| as much as possible |mdash| the amount of storage
used for storing names for this type. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Data_Types.Type_Representation.Discarding_Names.Discard_Names

    procedure Show_Discard_Names is
       pragma Warnings (Off, "is not referenced");

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December)
         with Discard_Names;

       M : constant Months := January;
    begin
       null;
    end Show_Discard_Names;

In this example, the compiler attempts to not store strings associated with
the :ada:`Months` type duration compilation.

Note that the :ada:`Discard_Names` aspect is available for enumerations,
exceptions, and tagged types.

.. admonition:: In the GNAT toolchain

    If we add this statement to the :ada:`Show_Discard_Names` procedure above:

    .. code-block:: ada

        Put_Line ("Month: "
                  & Months'Image (M));

    we see that the application displays "0" instead of "JANUARY". This is
    because GNAT doesn't store the strings associated with the :ada:`Months`
    type when we use the :ada:`Discard_Names` aspect for the :ada:`Months`
    type. (Therefore, the :ada:`Months'Image` attribute doesn't have that
    information.) Instead, the compiler uses the integer value of the
    enumeration, so that :ada:`Months'Image` returns the corresponding string
    for this integer value.

.. admonition:: In the Ada Reference Manual

    - :arm22:`Aspect Discard_Names <C-5>`
