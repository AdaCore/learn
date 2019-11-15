Performance considerations
==========================

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

Overall expectations
--------------------

All in all, there should not be significant performances differences between
code written in Ada and code written in C, provided that there are semantically
equivalent. Taking the current GNAT implementation and its GCC C counterpart
for example, most of the code generation and optimization phases are shared
between C and Ada |mdash| so there's not one compiler more efficient than the
other. Furthermore, the two languages are fairly similar in the way they
implement imperative semantics, in particular with regards to memory management
or control flow. They should be equivalent on average.

However, it's not uncommon to have projects developing similar pieces of code
in Ada and C to confirm relative performances, and to observe differences from
20 |ndash| 30% to sometimes 5 or 20 times slower. This usually comes from the
fact that, while the two piece appear semantically equivalent, they happen to
be actually quite different. This section will list some of the most common
suspects and their resolution.

Switches and optimizations
--------------------------

.. todo::

    Complete section!


Checks and assertions
---------------------

.. todo::

    Complete section!


Dynamic v.s static structures
-----------------------------

Ada generally speaking provides more ways than C or C++ to write simple dynamic
structures, that is to say structures that have constraints computed after
variables. For example, it's quite typical to have initial values in record
types:

.. code-block:: ada

    type R is record
       F : Some_Field := Call_To_Some_Function;
    end record;

However, the consequences of the above is that any declaration of a instance of
this type without an explicit value for :ada:`V` will issue a call to
:ada:`Call_To_Some_Function`. More subtle issue may arise with elaboration. For
example, it's possible to write:

.. code-block:: ada

    with Some_Function_Call;
    with Some_Other_Function_Call;

    package Values is
       A_Start : Integer := Some_Function_Call;
       A_End   : Integer := Some_Other_Function_Call;
    end Values;

.. code-block:: ada

    with Values; use Values;

    package P is
       type Arr is array (Integer range A_Start .. A_End) of Integer;
    end P;

It may indeed be appealing to be able to change the values of :ada:`A_Start`
and :ada:`A_End` at startup as to align a series of arrays dynamically. The
consequence, however, is that these values will not be know statically, so any
code that needs to access to boundaries of the array will need to read data
from memory. While it's perfectly fine most of the time, there may be
situations where performances are so critical that static values for array
boundaries must be enforced.

Here's a last case which may also be surprising:

.. code-block:: ada

    package P is
       type Arr is array (Integer range <>) of Integer;

       type R (D1, D2 : Integer) is record
          F1 : Arr (1 .. D1);
          F2 : Arr (1 .. D2);
       end record;
    end P;

In the code above, :ada:`R` contains two arrays, :ada:`F1` and :ada:`F2`,
respectively constrained by the discriminant :ada:`D1` and :ada:`D2`. The
consequence is, however, that to access :ada:`F2`, the run-time needs to know
how large :ada:`F1` is, which is dynamically constrained when creating an
instance. Therefore, accessing to :ada:`F2` requires a computation involving
:ada:`D1` which is slower than |mdash| say |mdash| two pointers in an C array
that would point to two different arrays.

Generally speaking, when values are used in data structures, it's useful to
always consider where they're coming from, and if their value is static
(computed by the compiler) or dynamic (only known at run-time). There's nothing
fundamentally wrong with dynamically constrained types, unless they appear is
performance-critical pieces of the application.

Pointers v.s. data copies
-------------------------

.. todo::

    Complete section!
