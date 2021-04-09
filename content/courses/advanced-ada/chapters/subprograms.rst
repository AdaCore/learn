Subprograms
===========

.. include:: ../../global.txt

Expression functions
--------------------

.. todo::

    Complete section!


Quantified Expressions
----------------------

.. admonition:: Relevant topics

    - `Quantified Expressions <http://www.ada-auth.org/standards/2xrm/html/RM-4-5-8.html>`_

.. todo::

    Complete section!


Declare Expressions
-------------------

.. admonition:: Relevant topics

    - `Declare Expressions <http://www.ada-auth.org/standards/2xrm/html/RM-4-5-9.html>`_

.. todo::

    Complete section!


Reduction Expressions
---------------------

.. admonition:: Relevant topics

    - `Reduction Expressions <http://www.ada-auth.org/standards/2xrm/html/RM-4-5-10.html>`_

.. todo::

    Complete section!


Overloading
-----------

.. note::

    This section was originally written by Bob Duff and published as
    `Gem #50: Overload Resolution <https://www.adacore.com/gems/gem-50>`_.

Ada allows overloading of subprograms, which means that two or more
subprogram declarations with the same name can be visible at the same
place. Here, "name" can refer to operator symbols, like :ada:`"+"`. Ada
also allows overloading of various other notations, such as literals and
aggregates.

In most languages that support overloading, overload resolution is done
"bottom up" |mdash| that is, information flows from inner constructs to outer
constructs. As usual, computer folks draw their trees upside-down, with
the root at the top. For example, if we have two procedures :ada:`Print`:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading

    procedure Show_Overloading is

       package Types is
          type Sequence is null record;
          type Set is null record;

          procedure Print (S : Sequence) is null;
          procedure Print (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin

       --  Compiler selects Print (S : Sequence)
       Print (X);
    end Show_Overloading;

the type of :ada:`X` determines which :ada:`Print` is meant in the call.

Ada is unusual in that it supports top-down overload resolution as well:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading

    procedure Show_Top_Down_Overloading is

       package Types is
          type Sequence is null record;
          type Set is null record;

          function Empty return Sequence is ((others => <>));
          function Empty return Set  is ((others => <>));

          procedure Print_Sequence (S : Sequence) is null;
          procedure Print_Set (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin
       --  Compiler selects function Empty return Sequence
       Print_Sequence (Empty);
    end Show_Top_Down_Overloading;

The type of the formal parameter :ada:`S` of :ada:`Print_Sequence`
determines which :ada:`Empty` is meant in the call. In C++, for example,
the equivalent of the :ada:`Print (X)` example would resolve, but the
:ada:`Print_Sequence (Empty)` would be illegal, because C++ does not use
top-down information.

If we overload things too heavily, we can cause ambiguities:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Overloading
    :class: ada-expect-compile-error

    procedure Show_Overloading_Error is

       package Types is
          type Sequence is null record;
          type Set is null record;

          function Empty return Sequence is ((others => <>));
          function Empty return Set  is ((others => <>));

          procedure Print (S : Sequence) is null;
          procedure Print (S : Set) is null;
       end Types;

       use Types;

       X : Sequence;
    begin
       Print (Empty);  -- Illegal!
    end Show_Overloading_Error;

The call is ambiguous, and therefore illegal, because there are two
possible meanings. One way to resolve the ambiguity is to use a qualified
expression to say which type we mean:

.. code-block:: ada

    Print (Sequence'(Empty));

Note that we're now using both bottom-up and top-down overload resolution:
:ada:`Sequence'` determines which :ada:`Empty` is meant (top down) and
which :ada:`Print` is meant (bottom up). You can qualify an expression,
even if it is not ambiguous according to Ada rules |mdash| you might want
to clarify the type because it might be ambiguous for human readers.

Of course, you could instead resolve the :ada:`Print (Empty)` example by
modifying the source code so the names are unique, as in the earlier
examples. That might well be the best solution, assuming you can modify
the relevant sources. Too much overloading can be confusing. How much is
"too much" is in part a matter of taste.

Ada really needs to have top-down overload resolution, in order to resolve
literals. In some languages, you can tell the type of a literal by looking
at it, for example appending ``L`` (letter el) means "the type of this
literal is long int". That sort of kludge won't work in Ada, because we
have an open-ended set of integer types:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Literal_Resolution

    procedure Show_Literal_Resolution is

       type Apple_Count is range 0 .. 100;

       procedure Peel (Count : Apple_Count) is null;
    begin
       Peel (20);
    end Show_Literal_Resolution;

You can't tell by looking at the literal :ada:`20` what its type is. The
type of formal parameter :ada:`Count` tells us that :ada:`20` is an
:ada:`Apple_Count`, as opposed to some other type, such as
:ada:`Standard.Long_Integer`.

Technically, the type of :ada:`20` is :ada:`universal_integer`, which is
implicitly converted to :ada:`Apple_Count` |mdash| it's really the result
type of that implicit conversion that is at issue. But that's an obscure
point |mdash| you won't go *too* far wrong if you think of the integer
literal notation as being overloaded on all integer types.

Developers sometimes wonder why the compiler can't resolve something that
seems obvious. For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Literal_Resolution_Error
    :class: ada-expect-compile-error

    procedure Show_Literal_Resolution_Error is

       type Apple_Count is range 0 .. 100;
       procedure Slice (Count : Apple_Count) is null;

       type Orange_Count is range 0 .. 10_000;
       procedure Slice (Count : Orange_Count) is null;
    begin
       Slice (Count => (10_000));  --  Illegal!
    end Show_Literal_Resolution_Error;

This call is ambiguous, and therefore illegal. But why? Clearly the
developer must have meant the :ada:`Orange_Count` one, because
:ada:`10_000` is out of range for :ada:`Apple_Count`. And all the relevant
expressions happen to be static.

Well, a good rule of thumb in language design (for languages with
overloading) is that the overload resolution rules should not be
"too smart". We want this example to be illegal to avoid confusion on the
part of developers reading the code. As usual, a qualified expression
fixes it:

.. code-block:: ada

    Slice (Count => Orange_Count'(10_000));

Another example, similar to the literal, is the aggregate. Ada uses a
simple rule: the type of an aggregate is determined top down (i.e., from
the context in which the aggregate appears). Bottom-up information is not
used; that is, the compiler does not look inside the aggregate in order to
determine its type.

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Record_Resolution_Error
    :class: ada-expect-compile-error

    procedure Show_Record_Resolution_Error is

       type Complex is record
          Re, Im : Float;
       end record;

       procedure Grind (X : Complex) is null;
       procedure Grind (X : String) is null;
    begin
       Grind (X => (Re => 1.0, Im => 1.0));  --  Illegal!
    end Show_Record_Resolution_Error;

There are two :ada:`Grind` procedures visible, so the type of the
aggregate could be :ada:`Complex` or :ada:`String`, so it is ambiguous and
therefore illegal. The compiler is not required to notice that there is
only one type with components :ada:`Re` and :ada:`Im`, of some real type
|mdash| in fact, the compiler is not *allowed* to notice that, for
overloading purposes.

We can qualify as usual:

.. code-block:: ada

    Grind (X => Complex'(Re => 1.0, Im => 1.0));

Only after resolving that the type of the aggregate is :ada:`Complex` can
the compiler look inside and make sure :ada:`Re` and :ada:`Im` make sense.

This not-too-smart rule for aggregates helps prevent confusion on the part
of developers reading the code. It also simplifies the compiler, and
makes the overload resolution algorithm reasonably efficient.


Operator Overloading
--------------------

.. todo::

    Complete section!


Nonreturning procedures
-----------------------

.. admonition:: Relevant topics

    - `Nonreturning Procedures <http://www.ada-auth.org/standards/2xrm/html/RM-6-5-1.html>`_

.. todo::

    Complete section!


Inline subprograms
------------------

.. admonition:: Relevant topics

    - **Briefly** mention :ada:`Inline` aspect mentioned in
      `Inline Expansion of Subprograms <http://www.ada-auth.org/standards/2xrm/html/RM-6-3-2.html>`_

.. todo::

    Complete section!


Null Procedures
---------------

.. admonition:: Relevant topics

    - `Null Procedures <http://www.ada-auth.org/standards/2xrm/html/RM-6-7.html>`_
    - Mention application for (non-tagged) type extensions

.. todo::

    Complete section!
