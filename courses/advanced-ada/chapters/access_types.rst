:orphan:

:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Access Types
============

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Null & Not Null Access
----------------------

:code-config:`reset_accumulator=True`

Ada, like many languages, defines a special :ada:`null` value for access
types. All values of an access type designate some object of the
designated type, except for :ada:`null`, which does not designate any
object. The null value can be used as a special flag. For example, a
singly-linked list can be null-terminated. A :ada:`Lookup` function can
return :ada:`null` to mean "not found", presuming the result is of an
access type:

.. code:: ada
    :class: ada-syntax-only

    package Show_Null_Return is

       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.
    end Show_Null_Return;

An alternative design for :ada:`Lookup` would be to raise an exception:

.. code:: ada
    :class: ada-syntax-only

    package Show_Not_Found_Exception is
       Not_Found : exception;

       function Lookup (T : Table) return Ref_Element;
       --  Raises Not_Found if not found.
       --  Never returns null.
    end Show_Not_Found_Exception;

Neither design is better in all situations; it depends in part on whether
we consider the "not found" situation to be exceptional.

Clearly, the client calling :ada:`Lookup` needs to know whether it can
return :ada:`null`, and if so, what that means. In general, it's a good
idea to document whether things can be null or not, especially for formal
parameters and function results. Prior to Ada 2005, we would do that with
comments. Since Ada 2005, we can use the :ada:`not null` syntax:

.. code:: ada
    :class: ada-syntax-only

    package Show_Not_Null_Return is
       type Ref_Element is access all Element;

       Not_Found : constant Ref_Element := null;

       function Lookup (T : Table) return not null Ref_Element;
       --  Possible since Ada 2005.
    end Show_Not_Null_Return;

This is a complete package for the code snippets above:

.. code:: ada

    package Example is

       type Element is limited private;
       type Ref_Element is access all Element;

       type Table is limited private;

       Not_Found : constant Ref_Element := null;
       function Lookup (T : Table) return Ref_Element;
       --  Returns Not_Found if not found.

       Not_Found_2 : exception;
       function Lookup_2 (T : Table) return not null Ref_Element;
       --  Raises Not_Found_2 if not found.

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    private
       type Element is limited
          record
             Component : Integer;
          end record;
       type Table is limited null record;
    end Example;

    package body Example is

       An_Element : aliased Element;

       function Lookup (T : Table) return Ref_Element is
       begin
          --  ...
          return Not_Found;
       end Lookup;

       function Lookup_2 (T : Table) return not null Ref_Element is
       begin
          --  ...
          raise Not_Found_2;

          return An_Element'Access;
          --  suppress error: 'missing "return" statement in function body'
       end Lookup_2;

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

       procedure R is
       begin
          Q (An_Element'Access);
       end R;

    end Example;

In general, it's better to use the language proper for documentation, when
possible, rather than comments, because compile-time and/or run-time
checks can help ensure that the "documentation" is actually true. With
comments, there's a greater danger that the comment will become false
during maintenance, and false documentation is obviously a menace.

In many, perhaps most cases, :ada:`null` is just a tripping hazard. It's
a good idea to put in :ada:`not null` when possible. In fact, a good
argument can be made that :ada:`not null` should be the default, with
extra syntax required when :ada:`null` is wanted. This is the way
`Standard ML <https://en.wikipedia.org/wiki/Standard_ML>`_ works, for
example --- you don't get any special null-like value unless you ask for
it. Of course, because Ada 2005 needs to be compatible with previous
versions of the language, :ada:`not null` cannot be the default for Ada.

One word of caution: access objects are default-initialized to
:ada:`null`, so if you have a :ada:`not null` object (or component) you
had better initialize it explicitly, or you will get
:ada:`Constraint_Error`. :ada:`not null` is more often useful on
parameters and function results, for this reason.

:code-config:`reset_accumulator=True`

Here's another example, first with :ada:`null`:

.. code:: ada
    :class: ada-syntax-only

    package Show_Null_Procedure is
       type Element is limited null record;
       --  Not implemented yet

       type Ref_Element is access all Element;

       type Table is limited null record;
       --  Not implemented yet

       procedure Iterate
         (T      : Table;
          Action : access procedure (X : not null Ref_Element)
          := null);
       --  If Action is null, do nothing.

    end Show_Null_Procedure;

and without :ada:`null`:

.. code:: ada
    :class: ada-syntax-only

    package Show_Null_Procedure is
       type Element is limited null record;
       --  Not implemented yet

       type Ref_Element is access all Element;

       type Table is limited null record;
       --  Not implemented yet

       procedure Do_Nothing (X : not null Ref_Element) is null;

       procedure Iterate
         (T      : Table;
          Action : not null access procedure (X : not null Ref_Element)
          := Do_Nothing'Access);

    end Show_Null_Procedure;

:code-config:`reset_accumulator=True`

The style of the second :ada:`Iterate` is clearly better because it makes
use of the syntax to indicate that a procedure is expected. This is a
complete package that includes both versions of the :ada:`Iterate`
procedure:

.. code:: ada

    package Example is

       type Element is limited private;
       type Ref_Element is access all Element;

       type Table is limited private;

       procedure Iterate
         (T : Table;
          Action : access procedure (X : not null Ref_Element)
                                          := null);
       --  If Action is null, do nothing.

       procedure Do_Nothing (X : not null Ref_Element) is null;
       procedure Iterate_2
         (T : Table;
          Action : not null access procedure (X : not null Ref_Element)
                                          := Do_Nothing'Access);

    private
       type Element is limited
          record
             Component : Integer;
          end record;
       type Table is limited null record;
    end Example;

    package body Example is

       An_Element : aliased Element;

       procedure Iterate
         (T : Table;
          Action : access procedure (X : not null Ref_Element)
                                          := null) is
       begin
          if Action /= null then
             Action (An_Element'Access);
             --  In a real program, this would do something more sensible.
          end if;
       end Iterate;

       procedure Iterate_2
         (T : Table;
          Action : not null access procedure (X : not null Ref_Element)
                                          := Do_Nothing'Access) is
       begin
          Action (An_Element'Access);
          --  In a real program, this would do something more sensible.
       end Iterate_2;

    end Example;

The :ada:`not null access procedure` is quite a mouthful, but it's
worthwhile, and anyway, as mentioned earlier, the compatibility
requirement requires that the :ada:`not null` be explicit, rather than the
other way around.

Another advantage of :ada:`not null` over comments is for efficiency.
Consider procedures :ada:`P` and :ada:`Q` in this example:

.. code:: ada

    package Example.Processing is

       procedure P (X : not null Ref_Element);

       procedure Q (X : not null Ref_Element);

    end Example.Processing;

    package body Example.Processing is

       procedure P (X : not null Ref_Element) is
       begin
          X.all.Component := X.all.Component + 1;
       end P;

       procedure Q (X : not null Ref_Element) is
       begin
          for I in 1 .. 1000 loop
             P (X);
          end loop;
       end Q;

    end Example.Processing;

Without :ada:`not null`, the generated code for :ada:`P` will do a check
that :ada:`X /= null`, which may be costly on some systems. :ada:`P` is
called in a loop, so this check will likely occur many times. With
:ada:`not null`, the check is pushed to the call site. Pushing checks to
the call site is usually beneficial because

    1. the check might be hoisted out of a loop by the optimizer, or

    2. the check might be eliminated altogether, as in the example
       above, where the compiler knows that :ada:`An_Element'Access` cannot
       be :ada:`null`.

This is analogous to the situation with other run-time checks, such as
array bounds checks:

.. code:: ada

    package Show_Process_Array is

       type My_Index is range 1 .. 10;
       type My_Array is array (My_Index) of Integer;

       procedure Process_Array (X : in out My_Array; Index : My_Index);

    end Show_Process_Array;

    package body Show_Process_Array is

       procedure Process_Array (X : in out My_Array; Index : My_Index) is
       begin
          X (Index) := X (Index) + 1;
       end Process_Array;

    end Show_Process_Array;

If :ada:`X (Index)` occurs inside :ada:`Process_Array`, there is no need
to check that :ada:`Index` is in range, because the check is pushed to the
caller.

:code-config:`reset_accumulator=True`
