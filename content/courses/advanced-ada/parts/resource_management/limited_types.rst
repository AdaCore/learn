Limited Types
=============

.. include:: ../../../global.txt

So far, we discussed nonlimited types in most cases. In this chapter, we
discuss limited types.

We can think of limited types as an easy way to avoid inappropriate semantics.
For example, a lock should not be copied |mdash| neither directly, via
assignment, nor with pass-by-copy. Similarly, a *file*, which is really a file
descriptor, should not be copied. In this chapter, we'll see example of
unwanted side-effects that arise if we don't use limited types for these cases.


Assignment and equality
-----------------------

Limited types have the following restrictions, which we discussed in the
:ref:`Introduction to Ada <Intro_Ada_Limited_Types>` course:

- copying objects of limited types via direct assignments is forbidden; and

- there's no predefined equality operator for limited types.

(Of course, in the case of nonlimited types, assignments are possible and the
equality operator is available.)

By having these restrictions for limited types, we avoid inappropriate
side-effects for assignment and equality operations. As an example of
inappropriate side-effects, consider the case when we apply those operations on
record types that have components of access types:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Assignment_Equality.Wrong_Assignment_Equality

    package Nonlimited_Types is

       type Simple_Rec is private;

       type Integer_Access is access Integer;

       function Init (I : Integer) return Simple_Rec;

       procedure Set (E : Simple_Rec;
                      I : Integer);

       procedure Show (E      : Simple_Rec;
                       E_Name : String);

    private

       type Simple_Rec is record
          V : Integer_Access;
       end record;

    end Nonlimited_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Nonlimited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);
          end return;
       end Init;

       procedure Set (E : Simple_Rec;
                      I : Integer) is
       begin
          E.V.all := I;
       end Set;

       procedure Show (E      : Simple_Rec;
                       E_Name : String) is
       begin
          Put_Line (E_Name
                    & ".V.all = "
                    & Integer'Image (E.V.all));
       end Show;

    end Nonlimited_Types;

    with Ada.Text_IO;      use Ada.Text_IO;
    with Nonlimited_Types; use Nonlimited_Types;

    procedure Show_Wrong_Assignment_Equality is
       A, B : Simple_Rec := Init (0);

       procedure Show_Compare is
       begin
          if A = B then
             Put_Line ("A = B");
          else
             Put_Line ("A /= B");
          end if;
       end Show_Compare;
    begin

       Put_Line ("A := Init (0); A := Init (0);");
       Show (A, "A");
       Show (B, "B");
       Show_Compare;
       Put_Line ("--------");

       Put_Line ("Set (A, 2); Set (B, 3);");
       Set (A, 2);
       Set (B, 3);

       Show (A, "A");
       Show (B, "B");
       Put_Line ("--------");

       Put_Line ("B := A");
       B := A;

       Show (A, "A");
       Show (B, "B");
       Show_Compare;
       Put_Line ("--------");

       Put_Line ("Set (B, 7);");
       Set (B, 7);

       Show (A, "A");
       Show (B, "B");
       Show_Compare;
       Put_Line ("--------");

    end Show_Wrong_Assignment_Equality;

In this code, we declare the :ada:`Simple_Rec` type in the
:ada:`Nonlimited_Types` package and use it in the
:ada:`Show_Wrong_Assignment_Equality` procedure. In principle, we're already
doing many things right here. For example, we're declaring the
:ada:`Simple_Rec` type private, so that the component :ada:`V` of access
type is encapsulated. Programmers that declare objects of this type cannot
simply mess up with the :ada:`V` component. Instead, they have to call the
:ada:`Init` function and the :ada:`Set` procedure to initialize and change,
respectively, objects of the :ada:`Simple_Rec` type. That being said, there are
two problems with this code, which we discuss next.

The first problem we can identify is that the first call to :ada:`Show_Compare`
shows that :ada:`A` and :ada:`B` are different, although both have the same
value in the :ada:`V` component (:ada:`A.V.all = 0` and :ada:`B.V.all = 0`)
|mdash| this was set by the call to the :ada:`Init` function. What's happening
here is that the :ada:`A = B` expression is comparing the access values
(:ada:`A.V = B.V`), while we might have been expecting it to compare the actual
integer values after dereferencing (:ada:`A.V.all = B.V.all`). Therefore, the
predefined equality function of the :ada:`Simple_Rec` type is useless and
dangerous for us, as it misleads us to expect something that it doesn't do.

After the assignment of :ada:`A` to :ada:`B` (:ada:`B := A`), the information
that the application displays seems to be correct |mdash| both :ada:`A.V.all`
and :ada:`B.V.all` have the same value of two. However, when assigning the
value seven to :ada:`B` by calling :ada:`Set (B, 7)`, we see that the value of
:ada:`A.V.all` has also changed. What's happening here is that the previous
assignment (:ada:`B := A`) has actually assigned access values
(:ada:`B.V := A.V`), while we might have been expecting it to assign the
dereferenced values (:ada:`B.V.all := A.V.all`). Therefore, we cannot simply
directly assign objects of :ada:`Simple_Rec` type, as this operation changes
the internal structure of the type due to the presence of components of access
type.

For these reasons, forbidding these operations for the :ada:`Simple_Rec` type
is the most appropriate software design decision. If we still need assignment
and equality operators, we can implement custom subprograms for the limited
type. We'll discuss this topic in the next sections.

In addition to the case when we have components of access types, limited types
are useful for example when we want to avoid the situation in which the same
information is copied to multiple objects of the same type.

.. admonition:: In the Ada Reference Manual

    - :arm22:`7.5 Limited Types <7-5>`


.. _Adv_Ada_Limited_Types_Assignments:

Assignments
~~~~~~~~~~~

Assignments are forbidden when using objects of limited types. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Assignment_Equality.Assignment
    :class: ada-expect-compile-error

    package Limited_Types is

       type Simple_Rec is limited private;

       type Integer_Access is access Integer;

       function Init (I : Integer) return Simple_Rec;

    private

       type Simple_Rec is limited record
          V : Integer_Access;
       end record;

    end Limited_Types;

    package body Limited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);
          end return;
       end Init;

    end Limited_Types;


    with Limited_Types; use Limited_Types;

    procedure Show_Limited_Assignment is
       A, B : Simple_Rec := Init (0);
    begin
       B := A;
    end Show_Limited_Assignment;

In this example, we declare the limited private type :ada:`Simple_Rec` and two
objects of this type (:ada:`A` and :ada:`B`) in the
:ada:`Show_Limited_Assignment` procedure. (We discuss more about limited
private types :ref:`later <Adv_Ada_Limited_Private_Types>`).

As expected, we get a compilation error for the :ada:`B := A` statement (in the
:ada:`Show_Limited_Assignment` procedure). If we
need to copy two objects of limited type, we have to provide a custom procedure
to do that. For example, we can implement a :ada:`Copy` procedure for the
:ada:`Simple_Rec` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Assignment_Equality.Assignment

    package Limited_Types is

       type Integer_Access is access Integer;

       type Simple_Rec is limited private;

       function Init (I : Integer) return Simple_Rec;

       procedure Copy (From :        Simple_Rec;
                       To   : in out Simple_Rec);

    private

       type Simple_Rec is limited record
          V : Integer_Access;
       end record;

    end Limited_Types;

    package body Limited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);
          end return;
       end Init;

       procedure Copy (From :        Simple_Rec;
                       To   : in out Simple_Rec)
       is
       begin
          --  Copying record components
          To.V.all := From.V.all;
       end Copy;

    end Limited_Types;


    with Limited_Types; use Limited_Types;

    procedure Show_Limited_Assignment is
       A, B : Simple_Rec := Init (0);
    begin
       Copy (From => A, To => B);
    end Show_Limited_Assignment;

The :ada:`Copy` procedure from this example copies the dereferenced values of
:ada:`From` to :ada:`To`, which matches our expectation for the
:ada:`Simple_Rec`. Note that we could have also implemented a
:ada:`Shallow_Copy` procedure to copy the actual access values (i.e.
:ada:`To.V := From.V`). However, having this kind of procedure can be dangerous
in many case, so this design decision must be made carefully. In any case,
using limited types ensures that only the assignment subprograms that are
explicitly declared in the package specification are available.


Equality
~~~~~~~~

Limited types don't have a predefined equality operator. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Assignment_Equality.Equality
    :class: ada-expect-compile-error

    package Limited_Types is

       type Integer_Access is access Integer;

       type Simple_Rec is limited private;

       function Init (I : Integer) return Simple_Rec;

    private

       type Simple_Rec is limited record
          V : Integer_Access;
       end record;

    end Limited_Types;

    package body Limited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);
          end return;
       end Init;

    end Limited_Types;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Limited_Types; use Limited_Types;

    procedure Show_Limited_Equality is
       A : Simple_Rec := Init (5);
       B : Simple_Rec := Init (6);
    begin
       if A = B then
          Put_Line ("A = B");
       else
          Put_Line ("A /= B");
       end if;
    end Show_Limited_Equality;

As expected, the comparison :ada:`A = B` triggers a compilation error because
no predefined :ada:`=` operator is available for the :ada:`Simple_Rec` type.
If we want to be able to compare objects of this type, we have to implement
the :ada:`=` operator ourselves. For example, we can do that for the
:ada:`Simple_Rec` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Assignment_Equality.Equality

    package Limited_Types is

       type Integer_Access is access Integer;

       type Simple_Rec is limited private;

       function Init (I : Integer) return Simple_Rec;

       function "=" (Left, Right : Simple_Rec)
                     return Boolean;

    private

       type Simple_Rec is limited record
          V : Integer_Access;
       end record;

    end Limited_Types;

    package body Limited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);
          end return;
       end Init;

       function "=" (Left, Right : Simple_Rec)
                     return Boolean is
       begin
          --  Comparing record components
          return Left.V.all = Right.V.all;
       end "=";

    end Limited_Types;

    with Ada.Text_IO;   use Ada.Text_IO;
    with Limited_Types; use Limited_Types;

    procedure Show_Limited_Equality is
       A : Simple_Rec := Init (5);
       B : Simple_Rec := Init (6);
    begin
       if A = B then
          Put_Line ("A = B");
       else
          Put_Line ("A /= B");
       end if;
    end Show_Limited_Equality;

Here, the :ada:`=` operator compares the dereferenced values of :ada:`Left.V`
and :ada:`Right.V`, which matches our expectation for the :ada:`Simple_Rec`
type. Declaring types as limited ensures that we don't have unreasonable
equality comparisons, and allows us to create reasonable replacements when
required.

.. admonition:: In other languages

    In C++, you can overload the assignment operator. For example:

    .. code-block:: cpp

        class Simple_Rec
        {
        public:
            // Overloaded assignment
            Simple_Rec& operator= (const Simple_Rec& obj);
        private:
        int *V;
        };

    In Ada, however, we can only define the equality operator (:ada:`=`).
    Defining the assignment operator (:ada:`:=`) is not possible. The following
    code triggers a compilation error as expected:

    .. code-block:: ada

        package Limited_Types is

           type Integer_Access is access Integer;

           type Simple_Rec is limited private;

           procedure ":=" (To   : in out Simple_Rec
                           From :        Simple_Rec);

           -- ...

        end Limited_Types;


.. _Adv_Ada_Limited_Private_Types:

Limited private types
---------------------

As we've seen in code examples from the previous section, we can apply
:ref:`information hiding <Adv_Ada_Type_View>` to limited types. In other words,
we can declare a type as :ada:`limited private` instead of just :ada:`limited`.
For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Private_Types.Limited_Private

    package Simple_Recs is

       type Rec is limited private;

    private

       type Rec is limited record
          I : Integer;
       end record;

    end Simple_Recs;

In this case, in addition to the fact that assignments are forbidden for
objects of this type (because :ada:`Rec` is limited), we cannot access the
record components.

Note that in this example, both partial and full views of the :ada:`Rec`
record are of limited type. In the next sections, we discuss how the partial
and full views can have non-matching declarations.

.. admonition:: In the Ada Reference Manual

    - :arm22:`7.5 Limited Types <7-5>`


.. _Adv_Ada_Partial_Full_View_Limited:

Partial and full view of limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example, both partial and full views of the :ada:`Rec` type
were limited. We may actually declare a type as :ada:`limited private` (in the
public part of a package), while its full view is nonlimited. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Private_Types.Limited_Partial_Full_View

    package Simple_Recs is

       type Rec is limited private;
       --  Partial view of Rec is limited

    private

       type Rec is record
       --  Full view of Rec is nonlimited
          I : Integer;
       end record;

    end Simple_Recs;

In this case, only the partial view of :ada:`Rec` is limited, while its full
view is nonlimited.

Note that the opposite |mdash| declaring a type as :ada:`private` and its full
full view as :ada:`limited private` |mdash| is not possible. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Private_Types.Limited_Partial_Full_View
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Rec is private;

    private

       type Rec is limited record
          I : Integer;
       end record;

    end Simple_Recs;

As expected, we get a compilation error in this case. The issue is that the
partial view cannot be allowed to mislead the client about what's possible.
In this case, if the partial view allows assignment, then the full view must
actually provide assignment. But the partial view can restrict what is actually
possible, so a limited partial view need not be completed in the full view as a
limited type.


Limited and nonlimited in full view
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Declaring the full view of a type as limited or nonlimited has implications in
the way we can use objects of this type in the package body. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Private_Types.Limited_Partial_Full_View
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Rec_Limited_Full is limited private;
       type Rec_Nonlimited_Full is limited private;

       procedure Copy
         (From :        Rec_Limited_Full;
          To   : in out Rec_Limited_Full);
       procedure Copy
         (From :        Rec_Nonlimited_Full;
          To   : in out Rec_Nonlimited_Full);

    private

       type Rec_Limited_Full is limited record
          I : Integer;
       end record;

       type Rec_Nonlimited_Full is record
          I : Integer;
       end record;

    end Simple_Recs;

    package body Simple_Recs is

       procedure Copy
         (From :        Rec_Limited_Full;
          To   : in out Rec_Limited_Full)
       is
       begin
          To := From;
          --  ERROR: assignment is forbidden because
          --         Rec_Limited_Full is limited in
          --         its full view.
       end Copy;

       procedure Copy
         (From :        Rec_Nonlimited_Full;
          To   : in out Rec_Nonlimited_Full)
       is
       begin
          To := From;
          --  OK: assignment is allowed because
          --      Rec_Nonlimited_Full is
          --      nonlimited in its full view.
       end Copy;

    end Simple_Recs;

Here, both :ada:`Rec_Limited_Full` and :ada:`Rec_Nonlimited_Full` are declared
as :ada:`private limited`. However, :ada:`Rec_Limited_Full` type is limited in
its full view, while :ada:`Rec_Nonlimited_Full` is nonlimited. As expected,
the compiler complains about the :ada:`To := From` assignment in the
:ada:`Copy` procedure for the :ada:`Rec_Limited_Full` type because its full
view is limited (so no assignment is possible). Of course, in the case of the
objects of :ada:`Rec_Nonlimited_Full` type, this assignment is perfectly fine.

.. _Adv_Ada_Tagged_Limited_Private_Types:

Tagged limited private types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For tagged private types, the partial and full views must match: if a tagged
type is limited in the partial view, it must be limited in the full view. For
example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Private_Types.Tagged_Limited_Private_Types

    package Simple_Recs is

       type Rec is tagged limited private;

    private

       type Rec is tagged limited record
          I : Integer;
       end record;

    end Simple_Recs;

Here, the tagged :ada:`Rec` type is limited both in its partial and full views.
Any mismatch in one of the views triggers a compilation error. (As an
exercise, you may remove any of the :ada:`limited` keywords from the code
example and try to compile it.)

.. admonition:: For further reading...

   This rule is for the sake of dynamic dispatching and classwide types. The
   compiler must not allow any of the types in a derivation class |mdash| the
   set of types related by inheritance |mdash| to be different regarding
   assignment and equality (and thus inequality). That's necessary because we
   are meant to be able to manipulate objects of any type in the entire set of
   types via the partial view presented by the root type, without knowing which
   specific tagged type is involved.

.. todo::

    Add link to section that explains this topic in more details (once it's
    available).


Subtypes of Limited Types
-------------------------

.. todo::

    Complete section!


Deriving from limited types
---------------------------

In this section, we discuss the implications of deriving from limited types.
As usual, let's start with a simple example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Type

    package Simple_Recs is

       type Rec is limited null record;

       type Rec_Derived is new Rec;

    end Simple_Recs;

In this example, the :ada:`Rec_Derived` type is derived from the :ada:`Rec`
type. Note that the :ada:`Rec_Derived` type is limited because its ancestor is
limited, even though the :ada:`limited` keyword doesn't show up in the
declaration of the :ada:`Rec_Derived` type. Note that we could have actually
used the :ada:`limited` keyword here:

.. code-block:: ada

       type Rec_Derived is limited new Rec;

Therefore, we cannot use the assignment operator for objects of
:ada:`Rec_Derived` type:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Type
    :class: ada-expect-compile-error

    with Simple_Recs; use Simple_Recs;

    procedure Test_Limitedness is
       Dummy_1, Dummy_2 : Rec_Derived;
    begin
       Dummy_2 := Dummy_1;
    end Test_Limitedness;

Note that we cannot derive a limited type from a nonlimited ancestor:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Type_Nonlimited_Ancestor
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Rec is null record;

       type Rec_Derived is limited new Rec;

    end Simple_Recs;

As expected, the compiler indicates that :ada:`Rec` should be of limited type.

In fact, all the types in a derivation class are the same |mdash| either
limited or not. (That is especially important with dynamic dispatching via
tagged types. We discuss this topic in another chapter.)

.. admonition:: In the Ada Reference Manual

    - :arm22:`7.3 Private Types and Private Extensions <7-3>`
    - :arm22:`7.5 Limited Types <7-5>`


Deriving from limited private types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Of course, we can also derive from limited private types. However, there are
more rules in this case than the ones we've seen so far. Let's start with an
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Private_Type
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Rec is limited private;

    private

       type Rec is limited null record;

    end Simple_Recs;

    package Simple_Recs.Ext is

       type Rec_Derived is new Rec;

       --  OR:
       --
       --  type Rec_Derived is
       --    limited new Rec;

    end Simple_Recs.Ext;

    with Simple_Recs.Ext; use Simple_Recs.Ext;

    procedure Test_Limitedness is
       Dummy_1, Dummy_2 : Rec_Derived;
    begin
       Dummy_2 := Dummy_1;
    end Test_Limitedness;

Here, :ada:`Rec_Derived` is a limited type derived from the (limited private)
:ada:`Rec` type. We can verify that :ada:`Rec_Derived` type is limited
because the compilation of the :ada:`Test_Limitedness` procedure fails.

Any type derived from a limited type is always limited, even if the full view
of its ancestor is nonlimited. For example, let's modify the full view of
:ada:`Rec` and make it nonlimited:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Private_Type
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Rec is limited private;

    private

       type Rec is null record;

    end Simple_Recs;

Here, :ada:`Rec_Derived` is a limited type because the partial view of
:ada:`Rec` is limited. The fact that the full view of :ada:`Rec` is nonlimited
doesn't affect the :ada:`Rec_Derived` type |mdash| as we can verify with the
compilation error in the :ada:`Test_Limitedness` procedure.

Note, however, that a derived type becomes nonlimited in the private part or
the body of a child package if it isn't explicitly limited. For example,
because we're declaring :ada:`Rec_Derived` as :ada:`is new Rec` in the child
package (:ada:`Simple_Recs.Ext`), we're saying that :ada:`Rec_Derived` is
limited *outside* this package, but nonlimited in the :ada:`Simple_Recs.Ext`
package. We can verify this by copying the code from the :ada:`Test_Limitedness`
procedure to a new procedure in the body of the :ada:`Simple_Recs.Ext` package:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Limited_Private_Type

    package Simple_Recs.Ext
      with Elaborate_Body is

      --  Rec_Derived is derived from Rec, which is a
      --  limited private type that is nonlimited in
      --  its full view.
      --
      --  Rec_Derived isn't explicitly limited.
      --  Therefore, it's nonlimited in the private
      --  part of Simple_Recs.Ext and its package
      --  body.
      --
      type Rec_Derived is new Rec;

    end Simple_Recs.Ext;

    package body Simple_Recs.Ext is

       procedure Test_Child_Limitedness is
          Dummy_1, Dummy_2 : Rec_Derived;
       begin
          --  Here, Rec_Derived is a nonlimited
          --  type because Rec is nonlimited in
          --  its full view.

          Dummy_2 := Dummy_1;
       end Test_Child_Limitedness;

    end Simple_Recs.Ext;

    --  We copied the code to the
    --  Test_Child_Limitedness procedure (in the
    --  body of the Simple_Recs.Ext package) and
    --  commented it out here.
    --
    --  You may uncomment the code to verify
    --  that Rec_Derived is limited in this
    --  procedure.
    --

    --  with Simple_Recs.Ext; use Simple_Recs.Ext;

    procedure Test_Limitedness is
       --  Dummy_1, Dummy_2 : Rec_Derived;
    begin
       --  Dummy_2 := Dummy_1;
       null;
    end Test_Limitedness;

In the :ada:`Test_Child_Limitedness` procedure of the :ada:`Simple_Recs.Ext`
package, we can use the :ada:`Rec_Derived` as a nonlimited type because its
ancestor :ada:`Rec` is nonlimited in its full view. (Of course, if we uncomment
the code in the :ada:`Test_Limitedness` procedure, compilation fails there
because :ada:`Rec_Derived` is viewed as descending from a limited type.)


Deriving from tagged limited private types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The rules for deriving from tagged limited private types are slightly different
than ones we've seen in the previous section. Let's look at an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Tagged_Limited_Private_Type
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Tagged_Rec is tagged limited private;

    private

       type Tagged_Rec is tagged limited null record;

    end Simple_Recs;

    package Simple_Recs.Ext is

       type Rec_Derived is new
         Tagged_Rec with private;

    private

       type Rec_Derived is new
         Tagged_Rec with null record;

    end Simple_Recs.Ext;

    with Simple_Recs.Ext; use Simple_Recs.Ext;

    procedure Test_Limitedness is
       Dummy_1, Dummy_2 : Rec_Derived;
    begin
       Dummy_2 := Dummy_1;
    end Test_Limitedness;

In this example, :ada:`Rec_Derived` is a tagged limited type derived from the
:ada:`Tagged_Rec` type. (Again, we can verify the limitedness of the
:ada:`Rec_Derived` type with the :ada:`Test_Limitedness` procedure.)

As explained previously, the derived type (:ada:`Rec_Derived`) is a limited
type, even though the :ada:`limited` keyword doesn't appear in its
declaration. We could, of course, include the :ada:`limited` keyword in the
declaration of :ada:`Rec_Derived`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Tagged_Limited_Private_Type
    :class: ada-expect-compile-error

    package Simple_Recs.Ext is

       type Rec_Derived is limited new
         Tagged_Rec with private;

    private

       type Rec_Derived is limited new
         Tagged_Rec with null record;

    end Simple_Recs.Ext;

(Obviously, if we include the :ada:`limited` keyword in the partial view of
the derived type, we must include it in its full view as well.)


Deriving from limited interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The rules for limited interfaces are different from the ones for limited tagged
types. In contrast to the rule we've seen in the previous section, a type that
is derived from a limited type isn't automatically limited. In other words, it
doesn't inherit the *limitedness* from the interface. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Interface_Limited_Private

    package Simple_Recs is

       type Limited_IF is limited interface;

    end Simple_Recs;

    package Simple_Recs.Ext is

       type Rec_Derived is new
         Limited_IF with private;

    private

       type Rec_Derived is new
         Limited_IF with null record;

    end Simple_Recs.Ext;

    with Simple_Recs.Ext; use Simple_Recs.Ext;

    procedure Test_Limitedness is
       Dummy_1, Dummy_2 : Rec_Derived;
    begin
       Dummy_2 := Dummy_1;
    end Test_Limitedness;

Here, :ada:`Rec_Derived` is derived from the limited :ada:`Limited_IF`
interface. As we can see, the :ada:`Test_Limitedness` compiles fine because
:ada:`Rec_Derived` is nonlimited.

Of course, if we want :ada:`Rec_Derived` to be limited, we can make this
explicit in the type declaration:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Deriving_From_Limited_Types.Derived_Interface_Limited_Private
    :class: ada-expect-compile-error

    package Simple_Recs.Ext is

       type Rec_Derived is limited new
         Limited_IF with private;

    private

       type Rec_Derived is limited new
         Limited_IF with null record;

    end Simple_Recs.Ext;

    with Simple_Recs.Ext; use Simple_Recs.Ext;

    procedure Test_Limitedness is
       Dummy_1, Dummy_2 : Rec_Derived;
    begin
       Dummy_2 := Dummy_1;
    end Test_Limitedness;

Now, compilation of :ada:`Test_Limitedness` fails because :ada:`Rec_Derived` is
explicitly limited.


Immutably Limited Types
-----------------------

.. todo::

    Complete section!


Limited Types and Discriminants
-------------------------------

.. todo::

    Complete section!


Record components of limited type
---------------------------------

In this section, we discuss the implications of using components of limited
type. Let's start by declaring a record component of limited type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Record_Components_Limited_Type.Record_Components_Limited_Type

    package Simple_Recs is

       type Int_Rec is limited record
          V : Integer;
       end record;

       type Rec is limited record
          IR : Int_Rec;
       end record;

    end Simple_Recs;

As soon as we declare a record component of some limited type, the whole record
is limited. In this example, the :ada:`Rec` record is limited due to the
presence of the :ada:`IR` component of limited type.

Also, if we change the declaration of the :ada:`Rec` record from the previous
example and remove the :ada:`limited` keyword, the type itself remains
implicitly limited. We can see that when trying to assign to objects of
:ada:`Rec` type in the :ada:`Show_Implicitly_Limited` procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Record_Components_Limited_Type.Record_Components_Limited_Type
    :class: ada-expect-compile-error

    package Simple_Recs is

       type Int_Rec is limited record
          V : Integer;
       end record;

       type Rec is record
          IR : Int_Rec;
       end record;

    end Simple_Recs;

    with Simple_Recs; use Simple_Recs;

    procedure Show_Implicitly_Limited is
       A, B : Rec;
    begin
       B := A;
    end Show_Implicitly_Limited;

Here, the compiler indicates that the assignment is forbidden because the
:ada:`Rec` type has a component of limited type. The rationale for this rule is
that an object of a limited type doesn't allow assignment or equality,
including the case in which that object is a component of some enclosing
composite object. If we allowed the enclosing object to be copied or tested for
equality, we'd be doing it for all the components, too.

.. admonition:: In the Ada Reference Manual

    - :arm22:`3.8 Record Types <3-8>`


Limited types and aggregates
----------------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #1: Limited Types in Ada 2005 <https://www.adacore.com/gems/gem-1>`_
    and `Gem #2 <https://www.adacore.com/gems/gem-2>`_.

In this section, we focus on using aggregates to initialize limited types.

.. admonition:: Historically

    Prior to Ada 2005, aggregates were illegal for limited types. Therefore,
    we would be faced with a difficult choice: Make the type limited, and
    initialize it like this:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.Full_Coverage_Rules_Limited_Ada95

        with Ada.Strings.Unbounded;
        use  Ada.Strings.Unbounded;

        package Persons is

           type Limited_Person;
           type Limited_Person_Access is
             access all Limited_Person;

           type Limited_Person is limited record
              Name      : Unbounded_String;
              Age       : Natural;
           end record;

        end Persons;

        with Ada.Strings.Unbounded;
        use  Ada.Strings.Unbounded;

        with Persons; use Persons;

        procedure Show_Non_Aggregate_Init is
           X : Limited_Person;
        begin
           X.Name := To_Unbounded_String ("John Doe");
           X.Age := 25;
        end Show_Non_Aggregate_Init;

    which has the maintenance problem the full coverage rules are supposed to
    prevent. Or, make the type non-limited, and gain the benefits of
    aggregates, but lose the ability to prevent copies.


.. _Adv_Ada_Full_Coverage_Rules_Limited_Types:

Full coverage rules for limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, we discussed
:ref:`full coverage rules for aggregates <Adv_Ada_Full_Coverage_Rules_Aggregates>`.
We can also use them for limited types.

.. admonition:: Historically

    The full coverage rules have been aiding maintenance since Ada 83. However,
    prior to Ada 2005, we couldn't use them for limited types.

Suppose we have the following limited type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package Persons is

       type Limited_Person;
       type Limited_Person_Access is
         access all Limited_Person;

       type Limited_Person is limited record
          Self : Limited_Person_Access :=
                   Limited_Person'Unchecked_Access;
          Name : Unbounded_String;
          Age  : Natural;
          Shoe_Size : Positive;
       end record;

    end Persons;

This type has a self-reference; it doesn't make sense to copy objects,
because :ada:`Self` would end up pointing to the wrong place. Therefore,
we would like to make the type limited, to prevent developers from
accidentally making copies. After all, the type is probably private, so
developers using this package might not be aware of the problem. We could
also solve that problem with controlled types, but controlled types are
expensive, and add unnecessary complexity if not needed.

.. todo::

   Add link to chapter on controlled types (once it's available).

We can initialize objects of limited type with an aggregate. Here, we can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.Full_Coverage_Rules_Limited

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    with Persons; use Persons;

    procedure Show_Aggregate_Box_Init is
       X : aliased Limited_Person :=
             (Self      => <>,
              Name      =>
                To_Unbounded_String ("John Doe"),
              Age       => 25,
              Shoe_Size => 10);
    begin
       null;
    end Show_Aggregate_Box_Init;

The :ada:`Self => <>` means use the default value of
:ada:`Limited_Person'Unchecked_Access`. Since :ada:`Limited_Person`
appears inside the type declaration, it refers to the "current instance"
of the type, which in this case is :ada:`X`. Thus, we are setting
:ada:`X.Self` to be :ada:`X'Unchecked_Access`.

One very important requirement should be noted: the implementation is
required to build the value of :ada:`X` *in place*; it cannot construct
the aggregate in a temporary variable and then copy it into :ada:`X`,
because that would violate the whole point of limited objects |mdash|
you can't copy them.

.. admonition:: Historically

    Since Ada 2005, an aggregate is allowed to be limited; we can say:

    .. code:: ada run_button main=show_aggregate_init.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.Full_Coverage_Rules_Limited

        with Ada.Strings.Unbounded;
        use  Ada.Strings.Unbounded;
        with Persons; use Persons;

        procedure Show_Aggregate_Init is

           X : aliased Limited_Person :=
                 (Self      => null, -- Wrong!
                  Name      =>
                    To_Unbounded_String ("John Doe"),
                  Age       => 25,
                  Shoe_Size => 10);
        begin
           X.Self := X'Unchecked_Access;
        end Show_Aggregate_Init;

    It seems uncomfortable to set the value of :ada:`Self` to the wrong value
    (:ada:`null`) and then correct it. It also seems annoying that we have a
    (correct) default value for :ada:`Self`, but prior to Ada 2005, we
    couldn't use defaults with aggregates. Since Ada 2005, a new syntax in
    aggregates is available: :ada:`<>` means "use the default value, if any".
    Therefore, we can replace :ada:`Self => null` by :ada:`Self => <>`.

.. admonition:: Important

    Note that using :ada:`<>` in an aggregate can be dangerous, because it can
    leave some components uninitialized. :ada:`<>` means "use the default
    value". If the type of a component is scalar, and there is no
    record-component default, then there is no default value.

    For example, if we have an aggregate of type :ada:`String`, like this:

    .. code:: ada run_button main=show_string_box_init.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.String_Box_Init

        procedure Show_String_Box_Init is
            Uninitialized_Const_Str : constant String :=
                                        (1 .. 10 => <>);
        begin
           null;
        end Show_String_Box_Init;

    we end up with a 10-character string all of whose characters are invalid
    values. Note that this is no more nor less dangerous than this:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Aggregates.Dangerous_String

        procedure Show_Dangerous_String is
            Uninitialized_String_Var : String (1 .. 10);
            --  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            --  no initialization

            Uninitialized_Const_Str : constant String :=
                Uninitialized_String_Var;
        begin
           null;
        end Show_Dangerous_String;

    As always, one must be careful about uninitialized scalar objects.


Constructor functions for limited types
---------------------------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #3 <https://www.adacore.com/gems/gem-3>`_.

Given that we can use build-in-place aggregates for limited types,
the obvious next step is to allow such aggregates to be wrapped in an
abstraction |mdash| namely, to return them from functions. After all,
interesting types are usually private, and we need some way for clients
to create and initialize objects.

.. admonition:: Historically

    Prior to Ada 2005, constructor functions (that is, functions that create
    new objects and return them) were not allowed for limited types. Since
    Ada 2005, fully-general constructor functions are allowed.

Let's see an example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package P is
       task type Some_Task_Type;

       protected type Some_Protected_Type is
          --  dummy type
       end Some_Protected_Type;

       type T (<>) is limited private;
       function Make_T (Name : String) return T;
       --       ^^^^^^
       --  constructor function
    private
       type T is limited
          record
             Name    : Unbounded_String;
             My_Task : Some_Task_Type;
             My_Prot : Some_Protected_Type;
          end record;
    end P;

    package body P is

       task body Some_Task_Type is
       begin
          null;
       end Some_Task_Type;

       protected body Some_Protected_Type is
       end Some_Protected_Type;

       function Make_T (Name : String) return T is
       begin
          return (Name   =>
                    To_Unbounded_String (Name),
                  others => <>);
       end Make_T;

    end P;

Given the above, clients can say:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions

    with P; use P;

    procedure Show_Constructor_Function is
       My_T : T := Make_T
                     (Name => "Bartholomew Cubbins");
    begin
       null;
    end Show_Constructor_Function;

As for aggregates, the result of :ada:`Make_T` is built in place (that is,
in :ada:`My_T`), rather than being created and then copied into
:ada:`My_T`. Adding another level of function call, we can do:

.. code:: ada run_button main=show_rumplestiltskin_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions

    with P; use P;

    procedure Show_Rumplestiltskin_Constructor is

       function Make_Rumplestiltskin return T is
       begin
           return Make_T (Name => "Rumplestiltskin");
       end Make_Rumplestiltskin;

       Rumplestiltskin_Is_My_Name : constant T :=
         Make_Rumplestiltskin;
    begin
       null;
    end Show_Rumplestiltskin_Constructor;

It might help to understand the implementation model: In this case,
:ada:`Rumplestiltskin_Is_My_Name` is allocated in the usual way (on the
stack, presuming it is declared local to some subprogram). Its address is
passed as an extra implicit parameter to :ada:`Make_Rumplestiltskin`,
which then passes that same address on to :ada:`Make_T`, which then builds
the aggregate in place at that address. Limited objects must never be
copied! In this case, :ada:`Make_T` will initialize the :ada:`Name`
component, and create the :ada:`My_Task` and :ada:`My_Prot` components,
all directly in :ada:`Rumplestiltskin_Is_My_Name`.

.. admonition:: Historically

    Note that :ada:`Rumplestiltskin_Is_My_Name` is constant. Prior to
    Ada 2005, it was impossible to create a constant limited object, because
    there was no way to initialize it.

The :ada:`(<>)` on type :ada:`T` means that it has *unknown
discriminants* from the point of view of the client. This is a trick that
prevents clients from creating default-initialized objects (that is,
:ada:`X : T;` is illegal). Thus clients must call :ada:`Make_T` whenever
an object of type :ada:`T` is created, giving package :ada:`P` full
control over initialization of objects.

Ideally, limited and non-limited types should be just the same, except for
the essential difference: you can't copy limited objects (and there's no
language-defined equality operator). By allowing
functions and aggregates for limited types, we're very close to this goal.
Some languages have a specific feature called *constructor*. In Ada, a
*constructor* is just a function that creates a new object.

.. admonition:: Historically

    Prior to Ada 2005, *constructors* only worked for non-limited types. For
    limited types, the only way to *construct* on declaration was via default
    values, which limits you to one constructor. And the only way to pass
    parameters to that construction was via discriminants.

    Consider the following package:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions_2

        with Ada.Containers.Ordered_Sets;

        package Aux is
           generic
              with package OS is new
                Ada.Containers.Ordered_Sets (<>);
           function Gen_Singleton_Set
             (Element : OS.Element_Type)
              return OS.Set;
        end Aux;

        package body Aux is
           function Gen_Singleton_Set
             (Element : OS.Element_Type)
              return OS.Set
           is
           begin
              return S : OS.Set := OS.Empty_Set do
                 S.Insert (Element);
              end return;
           end Gen_Singleton_Set;
        end Aux;

    Since Ada 2005, we can say:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions_2

        with Ada.Containers.Ordered_Sets;
        with Aux;

        procedure Show_Set_Decl is

           package Integer_Sets is new
             Ada.Containers.Ordered_Sets
               (Element_Type => Integer);
           use Integer_Sets;

           function Singleton_Set is new
             Aux.Gen_Singleton_Set
               (OS => Integer_Sets);

           This_Set : Set := Empty_Set;
           That_Set : Set := Singleton_Set
                               (Element => 42);
        begin
           null;
        end Show_Set_Decl;

    whether or not :ada:`Set` is limited. :ada:`This_Set : Set := Empty_Set;`
    seems clearer than:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Constructor_Functions_Limited_Types.Constructor_Functions_2

        with Ada.Containers.Ordered_Sets;

        procedure Show_Set_Decl is

           package Integer_Sets is new
             Ada.Containers.Ordered_Sets
               (Element_Type => Integer);
           use Integer_Sets;

           This_Set : Set;
        begin
           null;
        end Show_Set_Decl;

    which might mean "default-initialize to the empty set" or might mean
    "leave it uninitialized, and we'll initialize it in later".

Return objects
--------------

.. _Adv_Ada_Extended_Return_Statements_Limited:

Extended return statements for limited types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #10: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-10>`_.

Previously, we discussed
:ref:`extended return statements <Adv_Ada_Extended_Return_Statements>`.
For most types, extended return statements are no big deal |mdash| it's just
syntactic sugar. But for limited types, this syntax is almost essential:

.. code:: ada no_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Extended_Return_Statements_Limited_Types.Extended_Return_Limited_Error

    package Task_Construct_Error is

       task type Task_Type (Discriminant : Integer);

       function Make_Task (Val : Integer)
                           return Task_Type;

    end Task_Construct_Error;

    package body Task_Construct_Error is

       task body Task_Type is
       begin
          null;
       end Task_Type;

       function Make_Task (Val : Integer)
                           return Task_Type
       is
          Result : Task_Type
                     (Discriminant => Val * 3);
       begin
          --  some statements...
          return Result; -- Illegal!
       end Make_Task;

    end Task_Construct_Error;

The return statement here is illegal, because :ada:`Result` is local to
:ada:`Make_Task`, and returning it would involve a copy, which makes no
sense (which is why task types are limited). Since Ada 2005, we can write
constructor functions for task types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Extended_Return_Statements_Limited_Types.Extended_Return_Limited

    package Task_Construct is

       task type Task_Type (Discriminant : Integer);

       function Make_Task (Val : Integer)
                           return Task_Type;

    end Task_Construct;

    package body Task_Construct is

       task body Task_Type is
       begin
          null;
       end Task_Type;

       function Make_Task (Val : Integer)
                           return Task_Type is
       begin
          return Result : Task_Type
                            (Discriminant => Val * 3)
          do
             --  some statements...
             null;
          end return;
       end Make_Task;

    end Task_Construct;

If we call it like this:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Extended_Return_Statements_Limited_Types.Extended_Return_Limited

    with Task_Construct; use Task_Construct;

    procedure Show_Task_Construct is
       My_Task : Task_Type := Make_Task (Val => 42);
    begin
       null;
    end Show_Task_Construct;

:ada:`Result` is created *in place* in :ada:`My_Task`. :ada:`Result` is
temporarily considered local to :ada:`Make_Task` during the
:ada:`-- some statements` part, but as soon as :ada:`Make_Task` returns,
the task becomes more global. :ada:`Result` and :ada:`My_Task` really are
one and the same object.

When returning a task from a function, it is activated after the function
returns. The :ada:`-- some statements` part had better not try to call one
of the task's entries, because that would deadlock. That is, the entry
call would wait until the task reaches an accept statement, which will
never happen, because the task will never be activated.

Initialization and function return
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As mentioned in the previous section, the object of limited type returned by
the initialization function is built *in place*. In other words, the return
object is built in the object that is the target of the assignment statement.

For example, we can see this when looking at the address of the object
*returned* by the :ada:`Init` function, which we call to initialize the limited
type :ada:`Simple_Rec`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Extended_Return_Statements_Limited_Types.Initialization_Return_Do

    package Limited_Types is

       type Integer_Access is access Integer;

       type Simple_Rec is limited private;

       function Init (I : Integer) return Simple_Rec;

    private

       type Simple_Rec is limited record
          V : Integer_Access;
       end record;

    end Limited_Types;

    with Ada.Text_IO;           use Ada.Text_IO;
    with System;
    with System.Address_Image;

    package body Limited_Types is

       function Init (I : Integer) return Simple_Rec
       is
       begin
          return E : Simple_Rec do
             E.V := new Integer'(I);

             Put_Line ("E'Address (Init):  "
                       & System.Address_Image
                           (E'Address));
          end return;
       end Init;

    end Limited_Types;

    with Ada.Text_IO;           use Ada.Text_IO;
    with System;
    with System.Address_Image;

    with Limited_Types;         use Limited_Types;

    procedure Show_Limited_Init is
    begin
       declare
          A : Simple_Rec := Init (0);
       begin
          Put_Line ("A'Address (local): "
                    & System.Address_Image
                        (A'Address));
       end;
       Put_Line ("----");

       declare
          B : Simple_Rec := Init (0);
       begin
          Put_Line ("B'Address (local): "
                    & System.Address_Image
                        (B'Address));
       end;
    end Show_Limited_Init;

When running this code example and comparing the address of the object :ada:`E`
in the :ada:`Init` function and the object that is being initialized in the
:ada:`Show_Limited_Init` procedure, we see that the return object :ada:`E` (of
the :ada:`Init` function) and the local object in the :ada:`Show_Limited_Init`
procedure are the same object.

.. admonition:: Important

   When we use non-limited types, we're actually copying the returned object
   |mdash| which was locally created in the function |mdash| to the object that
   we're assigning the function to.

   For example, let's modify the previous code and make :ada:`Simple_Rec`
   non-limited:

       .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Extended_Return_Statements_Limited_Types.Initialization_Return_Copy

        package Non_Limited_Types is

           type Integer_Access is access Integer;

           type Simple_Rec is private;

           function Init (I : Integer)
                          return Simple_Rec;

        private

           type Simple_Rec is record
              V : Integer_Access;
           end record;

        end Non_Limited_Types;

        with Ada.Text_IO;           use Ada.Text_IO;
        with System;
        with System.Address_Image;

        package body Non_Limited_Types is

           function Init (I : Integer)
                          return Simple_Rec is
           begin
              return E : Simple_Rec do
                 E.V := new Integer'(I);

                 Put_Line ("E'Address (Init):  "
                           & System.Address_Image
                               (E'Address));
              end return;
           end Init;

        end Non_Limited_Types;

        with Ada.Text_IO;           use Ada.Text_IO;
        with System;
        with System.Address_Image;

        with Non_Limited_Types;
        use  Non_Limited_Types;

        procedure Show_Non_Limited_Init_By_Copy is
           A, B : Simple_Rec;
        begin
           declare
              A : Simple_Rec := Init (0);
           begin
              Put_Line ("A'Address (local): "
                        & System.Address_Image
                            (A'Address));
           end;
           Put_Line ("----");

           declare
              B : Simple_Rec := Init (0);
           begin
              Put_Line ("B'Address (local): "
                        & System.Address_Image
                            (B'Address));
           end;
        end Show_Non_Limited_Init_By_Copy;

    In this case, we see that the local object :ada:`E` in the :ada:`Init`
    function is not the same as the object it's being assigned to in the
    :ada:`Show_Non_Limited_Init_By_Copy` procedure. In fact, :ada:`E` is being
    copied to :ada:`A` and :ada:`B`.


Building objects from constructors
----------------------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #11: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-11>`_.

We've earlier seen examples of constructor functions for limited types
similar to this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package P is
       task type Some_Task_Type;

       protected type Some_Protected_Type is
          --  dummy type
       end Some_Protected_Type;

       type T is limited private;
       function Make_T (Name : String) return T;
       --       ^^^^^^
       --  constructor function
    private
       type T is limited
          record
             Name    : Unbounded_String;
             My_Task : Some_Task_Type;
             My_Prot : Some_Protected_Type;
          end record;
    end P;

    package body P is

       task body Some_Task_Type is
       begin
          null;
       end Some_Task_Type;

       protected body Some_Protected_Type is
       end Some_Protected_Type;

       function Make_T (Name : String) return T is
       begin
          return (Name   =>
                    To_Unbounded_String (Name),
                  others => <>);
       end Make_T;

    end P;

    package P.Aux is
       function Make_Rumplestiltskin return T;
    end P.Aux;

    package body P.Aux is

       function Make_Rumplestiltskin return T is
       begin
          return Make_T (Name => "Rumplestiltskin");
       end Make_Rumplestiltskin;

    end P.Aux;

It is useful to consider the various contexts in which these functions may
be called. We've already seen things like:

.. code:: ada run_button main=show_rumplestiltskin_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Rumplestiltskin_Constructor is
       Rumplestiltskin_Is_My_Name : constant T :=
         Make_Rumplestiltskin;
    begin
       null;
    end Show_Rumplestiltskin_Constructor;

in which case the limited object is built directly in a standalone object.
This object will be finalized whenever the surrounding scope is left.

We can also do:

.. code:: ada run_button main=show_parameter_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Parameter_Constructor is
       procedure Do_Something (X : T) is null;
    begin
       Do_Something (X => Make_Rumplestiltskin);
    end Show_Parameter_Constructor;

Here, the result of the function is built directly in the formal parameter
:ada:`X` of :ada:`Do_Something`. :ada:`X` will be finalized as soon as we
return from :ada:`Do_Something`.

We can allocate initialized objects on the heap:

.. code:: ada run_button main=show_heap_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Heap_Constructor is

       type T_Ref is access all T;

       Global : T_Ref;

       procedure Heap_Alloc is
          Local : T_Ref;
          To_Global : Boolean := True;
       begin
          Local := new T'(Make_Rumplestiltskin);
          if To_Global then
             Global := Local;
          end if;
       end Heap_Alloc;

    begin
       null;
    end Show_Heap_Constructor;

The result of the function is built directly in the heap-allocated object,
which will be finalized when the scope of :ada:`T_Ref` is left (long after
:ada:`Heap_Alloc` returns).

We can create another limited type with a component of type :ada:`T`, and
use an aggregate:

.. code:: ada run_button main=show_outer_type.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Outer_Type is

       type Outer_Type is limited record
          This : T;
          That : T;
       end record;

       Outer_Obj : Outer_Type :=
                    (This => Make_Rumplestiltskin,
                     That => Make_T (Name => ""));

    begin
       null;
    end Show_Outer_Type;

As usual, the function results are built in place, directly in
:ada:`Outer_Obj.This` and :ada:`Outer_Obj.That`, with no copying involved.

The one case where we *cannot* call such constructor functions is in an
assignment statement:

.. code:: ada run_button main=show_illegal_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors
    :class: ada-expect-compile-error

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Illegal_Constructor is
       Rumplestiltskin_Is_My_Name : T;
    begin
       Rumplestiltskin_Is_My_Name :=
         Make_T (Name => "");  --  Illegal!
    end Show_Illegal_Constructor;

which is illegal because assignment statements involve copying. Likewise,
we can't copy a limited object into some other object:

.. code:: ada run_button main=show_illegal_constructor.adb project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Building_Objects_From_Constructors.Building_Objs_From_Constructors
    :class: ada-expect-compile-error

    with P;     use P;
    with P.Aux; use P.Aux;

    procedure Show_Illegal_Constructor is
       Rumplestiltskin_Is_My_Name : constant T :=
         Make_T (Name => "");
       Other : T :=
         Rumplestiltskin_Is_My_Name; -- Illegal!
    begin
       null;
    end Show_Illegal_Constructor;

Default initialization
----------------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #12: Limited Types in Ada 2005 <https://www.adacore.com/gems/ada-gem-12>`_.

Consider the following type declaration:

.. code:: ada no_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Default_Initialization.Default_Init
    :class: ada-syntax-only

    package Type_Defaults is
       type Color_Enum is (Red, Blue, Green);

       type T is private;
    private
       type T is
          record
             Color     : Color_Enum := Red;
             Is_Gnarly : Boolean := False;
             Count     : Natural;
          end record;

       procedure Do_Something;
    end Type_Defaults;

If we want to say, "make :ada:`Count` equal :ada:`100`, but initialize
:ada:`Color` and :ada:`Is_Gnarly` to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Default_Initialization.Default_Init

    package body Type_Defaults is

       Object_100 : constant T :=
                      (Color     => <>,
                       Is_Gnarly => <>,
                       Count     => 100);

       procedure Do_Something is null;

    end Type_Defaults;

.. admonition:: Historically

    Prior to Ada 2005, the following style was common:

    .. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Default_Initialization.Default_Init

        package body Type_Defaults is

           Object_100 : constant T :=
                          (Color     => Red,
                           Is_Gnarly => False,
                           Count     => 100);

           procedure Do_Something is null;

        end Type_Defaults;

    Here, we only wanted :ada:`Object_100` to be a default-initialized
    :ada:`T`, with :ada:`Count` equal to :ada:`100`. It's a little bit annoying
    that we had to write the default values :ada:`Red` and :ada:`False` twice.
    What if we change our mind about :ada:`Red`, and forget to change it in all
    the relevant places? Since Ada 2005, the :ada:`<>` notation comes to the
    rescue, as we've just seen.

On the other hand, if we want to say, "make :ada:`Count` equal :ada:`100`,
but initialize all other components, including the ones we might add next
week, to their defaults", we can do this:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Default_Initialization.Default_Init

    package body Type_Defaults is

       Object_100 : constant T := (Count  => 100,
                                   others => <>);

       procedure Do_Something is null;

    end Type_Defaults;

Note that if we add a component :ada:`Glorp : Integer;` to type :ada:`T`,
then the :ada:`others` case leaves :ada:`Glorp` undefined just as this
code would do:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Default_Initialization.Default_Init

    package body Type_Defaults is

       procedure Do_Something is
          Object_100 : T;
       begin
          Object_100.Count := 100;
       end Do_Something;

    end Type_Defaults;

Therefore, you should be careful and think twice before using
:ada:`others`.


.. _Adv_Ada_Limited_Types_As_Parameters:

Limited types as parameter
--------------------------

Previously, we saw that
:ref:`parameters can be passed by copy or by reference <Adv_Ada_Parameter_Modes_Associations>`.
Also, we discussed the concept of by-copy and by-reference types. *Explicitly*
limited types are by-reference types. Consequently, parameters of these types
are always passed by reference.

Here, it's important to understand when a type is explicitly limited and when
it's not |mdash| using the :ada:`limited` keyword in a part of the declaration
doesn't necessary ensure this, as we'll see later.

.. admonition:: For further reading...

    As an example, consider the case of a lock (as an abstract data type). If
    such a lock object were passed by copy, the :ada:`Acquire` and
    :ada:`Release` operations would be working on copies of this object, not on
    the original one. This would lead to timing-dependent bugs.

    .. todo::

        Add link to chapter the in the Ada Idioms course that explains this
        topic in more details (once it's available).

Let's start with an example of an explicitly limited type:

.. code:: ada no_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Parameters.Explicitly_Limited_Types

    with System;

    package Simple_Recs is

       type Rec is limited record
          I : Integer;
       end record;

       procedure Proc (R : in out Rec;
                       A :    out System.Address);

    end Simple_Recs;

In this example, :ada:`Rec` is a by-reference type because the type declaration
is an explicit limited record. Therefore, the parameter :ada:`R` of the
:ada:`Proc` procedure is passed by reference.

We can run the :ada:`Test` application below and compare the address of the
:ada:`R` object from :ada:`Test` to the address of the :ada:`R` parameter of
:ada:`Proc` to determine whether both :ada:`R` \s refer to the same object or
not:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Parameters.Explicitly_Limited_Types

    package body Simple_Recs is

       procedure Proc (R : in out Rec;
                       A :    out System.Address) is
       begin
          R.I := 0;
          A   := R'Address;
       end Proc;

    end Simple_Recs;

    with Ada.Text_IO;           use Ada.Text_IO;
    with System;                use System;
    with System.Address_Image;
    with Simple_Recs;           use Simple_Recs;

    procedure Test is
       R : Rec;

       AR_Proc, AR_Test : System.Address;
    begin
       AR_Proc := R'Address;

       Proc (R, AR_Test);

       Put_Line ("R'Address (Proc): "
                 & System.Address_Image (AR_Proc));
       Put_Line ("R'Address (Test): "
                 & System.Address_Image (AR_Test));

       if AR_Proc = AR_Test then
          Put_Line ("R was passed by reference.");
       else
          Put_Line ("R was passed by copy.");
       end if;

    end Test;

When running the :ada:`Test` application, we confirm that :ada:`R` was passed
by reference. Note, however, that the fact that :ada:`R` was passed by
reference doesn't automatically imply that :ada:`Rec` is a by-reference type:
the type could have been ambiguous, and the compiler could have just decided to
pass the parameter by reference in this case. (We'll discuss this ambiguity
later.)

The :ada:`Rec` type is also explicitly limited when it's declared limited in
the private type's completion (in the package's private part):

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Parameters.Explicitly_Limited_Types
    :class: ada-syntax-only

    with System;

    package Simple_Recs is

       type Rec is limited private;

       procedure Proc (R : in out Rec;
                       A :    out System.Address);

    private

       type Rec is limited record
          I : Integer;
       end record;

    end Simple_Recs;

In this case, :ada:`Rec` is limited both in the partial and in the full view,
so it's considered explicitly limited.

If we make the full view of the :ada:`Rec` non-limited (by removing the
:ada:`limited` keyword in the private part), then the :ada:`Rec` type
isn't explicitly limited anymore:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Limited_Types.Limited_Types_Parameters.Explicitly_Limited_Types
    :class: ada-syntax-only

    with System;

    package Simple_Recs is

       type Rec is limited private;

       procedure Proc (R : in out Rec;
                       A :    out System.Address);

    private

       type Rec is record
          I : Integer;
       end record;

    end Simple_Recs;

Now, even though the :ada:`Rec` type was declared as limited private, the full
view indicates that it's actually a non-limited type, so it isn't explicitly
limited. In this case, :ada:`Rec` is neither a by-copy nor a by-reference
type. Therefore, the decision whether the parameter is passed by reference or
by copy is made by the compiler.

.. admonition:: In the Ada Reference Manual

    - :arm22:`6.2 Formal Parameter Modes <6-2>`
    - :arm22:`6.4.1 Parameter Associations <6-4-1>`
    - :arm22:`7.5 Limited Types <7-5>`

