Access types (pointers)
=======================

.. include:: ../../global.txt

Overview
--------

Pointers are a potentially dangerous construct, which conflicts with Ada's
underlying philosophy.

There are two ways in which Ada helps shield programmers from the dangers of
pointers:

1. One approach, which we have already seen, is to provide alternative features
   so that the programmer does not need to use pointers. Parameter modes,
   arrays, and varying size types are all constructs that can replace typical
   pointer usages in C.

2. Second, Ada has made pointers as safe and restricted as possible, but allows
   "escape hatches" when the programmer explicitly requests them and presumably
   will be exercising such features with appropriate care.

.. TODO: Add paragraph and link below when advanced course is ready

..
   This course covers the basics of Ada pointers, which are known as "access
   values". There are generally better ways than to resort to the advanced
   features directly but if you need to use features that are potentially unsafe,
   you can learn more about those unsafe features
   ACCESS_TYPES_ADVANCED_LINK.

Here is how you declare a simple pointer type, or access type, in Ada:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    package Dates is
       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer;
       end record;
    end Dates;

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc is access Date;
        --                      ^ "Designated type"
        --                      ^ Date_Acc values point
        --                        to Date objects

        D : Date_Acc := null;
        --              ^ Literal for
        --                "access to nothing"
        --  ^ Access to date
    end Access_Types;

This illustrates how to:

- Declare an access type whose values point to ("designate") objects from a
  specific type
- Declare a variable (access value) from this access type
- Give it a value of :ada:`null`

In line with Ada's strong typing philosophy, if you declare a second access
type whose designated type is Date, the two access types will be incompatible
with each other:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types
    :class: ada-expect-compile-error

    with Dates; use Dates;

    package Access_Types is
        --  Declare an access type
        type Date_Acc   is access Date;
        type Date_Acc_2 is access Date;

        D  : Date_Acc   := null;
        D2 : Date_Acc_2 := D;
        --                 ^ Invalid! Different types
    end Access_Types;

.. admonition:: In other languages

    In most other languages, pointer types are structurally, not nominally
    typed, like they are in Ada, which means that two pointer types will be the
    same as long as they share the same target type and accessibility rules.

    Not so in Ada, which takes some time getting used to. A seemingly simple
    problem is, if you want to have a canonical access to a type, where should
    it be declared? A commonly used pattern is that if you need an access type
    to a specific type you "own", you will declare it along with the type:

    .. code-block:: ada

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Allocation (by type)
--------------------

Once we have declared an access type, we need a way to give variables of the
types a meaningful value! You can allocate a value of an access type
with the :ada:`new` keyword in Ada.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Allocate a new Date record
    end Access_Types;

If the type you want to allocate needs constraints, you can put them in the
subtype indication, just as you would do in a variable declaration:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type String_Acc is access String;
       --                        ^
       --  Access to unconstrained array type
       Msg : String_Acc;
       --    ^ Default value is null

       Buffer : String_Acc :=
         new String (1 .. 10);
       --            ^ Constraint required
    end Access_Types;

In some cases, though, allocating just by specifying the type is not ideal, so
Ada also allows you to initialize along with the allocation. This is done via
the qualified expression syntax:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc   :=
               new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;


.. _Intro_Ada_Access_Dereferencing:

Dereferencing
-------------

The last important piece of Ada's access type facility is how to get from an
access value to the object that is pointed to, that is, how to dereference the
pointer. Dereferencing a pointer uses the :ada:`.all` syntax in Ada, but is
often not needed |mdash| in many cases, the access value will be implicitly
dereferenced for you:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;

       D     : Date_Acc :=
                 new Date'(30, November, 2011);

       Today : Date := D.all;
       --              ^ Access value dereference
       J     : Integer := D.Day;
       --                 ^ Implicit dereference
       --                   for record and array
       --                   components
       --                   Equivalent to D.all.day
    end Access_Types;

Other features
--------------

As you might know if you have used pointers in C or C++, we are still missing
features that are considered fundamental to the use of pointers, such as:

- Pointer arithmetic (being able to increment or decrement a pointer in order
  to point to the next or previous object)

- Manual deallocation - what is called :c:`free` or :c:`delete` in C. This is
  a potentially unsafe operation. To keep within the realm of safe
  Ada, you need to never deallocate manually.

Those features exist in Ada, but are only available through specific standard
library APIs.

.. TODO: Add paragraph and link below when advanced course is ready

..
   You can read more about those in the
   advanced course on memory management ACCESS_TYPES_ADVANCED_LINK.

.. attention::

    The guideline in Ada is that most of the time you can avoid manual
    allocation, and you should.

    There are many ways to avoid manual allocation, some of which have been
    covered (such as parameter modes). The language also provides library
    abstractions to avoid pointers:

    1. One is the use of :ref:`containers <Intro_Ada_Containers>`. Containers help users
       avoid pointers, because container memory is automatically managed.

    2. A container to note in this context is the
       :rat12:`Indefinite holder <8-5>`.
       This container allows you to store a value of an indefinite type such as
       String.

    3. GNATCOLL has a library for smart pointers, called
       `Refcount <https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-refcount.ads>`_
       Those pointers' memory is automatically managed, so that when an
       allocated object has no more references to it, the memory is
       automatically deallocated.

Mutually recursive types
------------------------

The linked list is a common idiom in data structures; in Ada this would be most
naturally defined through two types, a record type and an access type, that are
mutually dependent.  To declare mutually dependent types, you can use an
incomplete type declaration:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Simple_List

    package Simple_List is
       type Node;
       --  This is an incomplete type declaration,
       --  which is completed in the same
       --  declarative region.

       type Node_Acc is access Node;

       type Node is record
          Content    : Natural;
          Prev, Next : Node_Acc;
       end record;
    end Simple_List;
