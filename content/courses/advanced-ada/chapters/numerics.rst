Numerics
========

.. include:: ../../global.txt

Modular Types
-------------

.. note::

    This section was originally written by Robert A. Duff and published as
    `Gem #26: The Mod Attribute <https://www.adacore.com/gems/gem-26>`_.

Ada has two kinds of integer type: signed and modular:

.. code:: ada compile_button project=Courses.Advanced_Ada.Numerics.Modular_1

    package Num_Types is

       type Signed_Integer is range 1..1_000_000;
       type Modular is mod 2**32;

    end Num_Types;

Operations on signed integers can overflow: if the result is outside the base
range, :ada:`Constraint_Error` will be raised. The base range of
:ada:`Signed_Integer` is the range of :ada:`Signed_Integer'Base`, which is
chosen by the compiler, but is likely to be something like
:ada:`-2**31 .. 2**31-1`.

Operations on modular integers use modular (wraparound) arithmetic.

For example:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       X : Modular;
    begin
       X := 1;
       Put_Line (X'Image);

       X := -X;
       Put_Line (X'Image);
    end Show_Modular;

Negating X gives -1, which wraps around to :ada:`2**32-1`, i.e. all-one-bits.

But what about a type conversion from signed to modular? Is that a signed
operation (so it should overflow) or is it a modular operation (so it should
wrap around)? The answer in Ada is the former |mdash| that is, if you try to
convert, say, :ada:`Integer'(-1)` to :ada:`Modular`, you will get
:ada:`Constraint_Error`:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is
       I : Integer := -1;
       X : Modular := 1;
    begin
       X := Modular (I);  --  raises Constraint_Error
       Put_Line (X'Image);
    end Show_Modular;

In Ada 95, the only way to do that conversion is to use
:ada:`Unchecked_Conversion`, which is somewhat uncomfortable. Furthermore, if
you're trying to convert to a generic formal modular type, how do you know what
size of signed integer type to use? Note that :ada:`Unchecked_Conversion` might
malfunction if the source and target types are of different sizes.

A small feature added to Ada 2005 solves the problem: the :ada:`Mod` attribute:

.. code:: ada run_button project=Courses.Advanced_Ada.Numerics.Modular_1

    with Ada.Text_IO; use Ada.Text_IO;

    with Num_Types;   use Num_Types;

    procedure Show_Modular is

       I : constant Integer := -1;
       X : Modular := 1;
    begin
       X := Modular'Mod (I);
       Put_Line (X'Image);
    end Show_Modular;

The :ada:`Mod` attribute will correctly convert from any integer type to a
given modular type, using wraparound semantics.

.. todo::

    Complete section!


Numeric Literals
----------------

.. note::

    This section was originally written by Franco Gasperoni and published as
    `Gem #7: The Beauty of Numeric Literals in Ada <https://www.adacore.com/gems/ada-gem-7>`_.


Operations
----------

.. admonition:: Relevant topics

    - operations on floating-point types: :ada:`S'Digits`

    - operations on fixed-point types: :ada:`S'Small`, :ada:`S'Delta`,
      :ada:`S'Fore`, :ada:`S'Aft`, :ada:`S'Digits`, ...

.. todo::

    Complete section!


Big Numbers
-----------

.. admonition:: Relevant topics

    - `Big Numbers <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-5.html>`_
    - `Big Integers <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-6.html>`_
    - `Big Reals <http://www.ada-auth.org/standards/2xrm/html/RM-A-5-7.html>`_

.. todo::

    Complete section!


Floating-Point Types
--------------------

.. admonition:: Relevant topics

    - Brief mentioning relevant parts of
      `Model of Floating Point Arithmetic <http://www.ada-auth.org/standards/2xrm/html/RM-G-2-1.html>`_

.. todo::

    Complete section!


Fixed-Point Types
-----------------

.. admonition:: Relevant topics

    - Brief mentioning relevant parts of
      `Model of Fixed Point Arithmetic <http://www.ada-auth.org/standards/2xrm/html/RM-G-2-3.html>`_

.. todo::

    Complete section!
