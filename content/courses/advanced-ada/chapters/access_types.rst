:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Access Types
============

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Null & Not Null Access
----------------------

.. sectionauthor:: Bob Duff

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

.. code:: ada run_button

    with Example; use Example;

    procedure Show_Example is
       T : Table;
       E : Ref_Element;
    begin
       E := Lookup (T);
    end;

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
example |mdash| you don't get any special null-like value unless you ask
for it. Of course, because Ada 2005 needs to be compatible with previous
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

.. code:: ada run_button

    with Example; use Example;

    procedure Show_Example is
       T : Table;
    begin
       Iterate_2 (T);
    end;

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

:code-config:`reset_accumulator=True`

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

Accessibility checks
--------------------

.. sectionauthor:: Ramón Fernández

.. sectionauthor:: Bob Duff

Introduction
~~~~~~~~~~~~

Ada is a block-structured language, which means that developers can nest
blocks of code inside other blocks. At the end of a block, all objects
declared inside of it go out of scope, so they no longer exist. Therefore,
the language disallows pointers to objects in blocks with a deeper nesting
level.

In order to prevent dangling references, every entity is associated with a
number, called its *accessibility level*, according to Ada's
accessibility rules. When certain references are made to an entity of an
access type, the accessibility level of the entity is checked against the
level allowed by the context so that no dangling pointers can occur.

Consider the following example:

.. code:: ada run_button
    :class: ada-expect-compile-error

    procedure Static_Check is
       type Global is access all Integer;
       X : Global;

       procedure Init is
          Y : aliased Integer := 0;
       begin
          X := Y'Access; -- Illegal!
       end Init;

    begin
       Init;
       --  ...
    end Static_Check;

The assignment is illegal because when the procedure :ada:`Init` finishes,
the object :ada:`Y` no longer exists, thus making :ada:`X` a danging
pointer. The compiler will detect this situation and flag the error.

The beauty of the accessibility rules is that most of them can be checked
and enforced at compile time, just by using statically known accessibility
levels.

However, there are cases when it is not possible to statically determine
the accessibility level that an entity will have during program execution.
In these cases, the compiler will insert a run-time check to raise an
exception if a dangling pointer can be created:

.. code:: ada run_button
    :class: ada-run-expect-failure

    procedure Access_Params is

       type Integer_Access is access all Integer;
       Data : Integer_Access;

       procedure Init_Data (Value : access Integer) is
       begin
          Data := Integer_Access (Value);
          --  this conversion performs a dynamic accessibility check
       end Init_Data;

       procedure Process (D : Integer_Access) is null;

       X : aliased Integer := 1;

    begin
       Init_Data (X'Access); -- This is OK

       declare
          Y : aliased Integer := 2;
       begin
          Init_Data (Y'Access); --  Trouble!
       end;
       --  Y no longer exists!

       Process (Data);
    end Access_Params;

In the example above, we cannot know at compile time the accessibility
level of the object that will be passed to :ada:`Init_Data`, so the
compiler inserts a run-time check to make sure that the assignment
:ada:`Data := ...` does not cause a dangling reference |mdash| and to raise
an exception if it would.

In summary, when it comes to dangling references, Ada makes it very hard
for you to shoot yourself in the foot!

:code-config:`reset_accumulator=True`

Anonymous access types
~~~~~~~~~~~~~~~~~~~~~~

Since Ada 2005, we may use anonymous access types in a more general
manner, adding considerable power to the object-oriented programming
features of the language. The accessibility rules have been
correspondingly augmented to ensure safety by preventing the possibility
of dangling references. The new rules have been designed with programming
flexibility in mind, as well as to allow the compiler to enforce checks
statically.

The accessibility levels in the new contexts for anonymous access types
are generally determined by the scope where they are declared. This makes
it possible to perform compile-time accessibility checks.

Another rule that allows for static accessibility checks relates to
derived types: a type derivation does not create new accessibility level
for the derived type, but just takes that of the parent type:

.. code:: ada run_button
    :class: ada-expect-compile-error

     procedure Example_1 is

        type Node is record
           N : access Integer;
        end record;

        List : Node;

        procedure P is
           type Other_Node is new Node;
        begin
           declare
              L : aliased Integer := 1;
              Data : Other_Node := Other_Node'(N => L'Access);
              --  L'Access is illegal!
           begin
              List := Node (Data);
           end;
        end P;

     begin
        P;
     end Example_1;

In the above example, we don't need to worry about expensive run-time
checks on assignment or return of an object of type :ada:`Other_Node`; we
know it has the same accessibility level as type :ada:`Node`, making the
:ada:`Access` attribute illegal. If this were not prevented, after
returning from :ada:`P`, :ada:`List.N` would be a dangling reference.

Since Ada 2005, we may also use functions to return objects of anonymous
access types. In this case, the accessibility level of the object is
statically determined by the scope of the function declaration. Consider
the following example:

.. code:: ada run_button
    :class: ada-expect-compile-error

    procedure Example_2 is

       type Rec is record
          V : access Integer;
          --  ... other elements
       end record;

       Global : aliased Integer := 1;

       function F1 (X : Boolean) return Rec is
          Local : aliased Integer := 2;

          --  Nested function returns anonymous access values
          --  with different nesting depths

          function F2 (Y : Boolean) return access Integer is
          begin
             if Y then
                return Global'Access;
             else
                return Local'Access;
             end if;
          end F2;

       begin
          return (V => F2 (X)); -- Illegal
       end F1;

       Data : Rec;
    begin
       Data := F1 (True);
    end Example_2;

In this example, applying the aforementioned rule, the compiler statically
determines that this accessibility level is the scope where :ada:`F2` is
declared, which is deeper than the accessibility level of :ada:`Rec`. So
even though the call :ada:`F1 (True)` would provide a valid value for
:ada:`V`, the code is illegal. The accessibility restriction is
conservative, to keep the rules simple, and so that the compiler is not
required to perform data flow analysis to determine legality (not to
mention that, in general, the legality would be undecidable).

The new rules also take into account discriminants of an anonymous access
type (which are technically referred to as access discriminants). Since
Ada 2005, access discriminants are permitted for non-limited types.
Consequently, it's necessary to disallow defaults for access discriminants
of non-limited types. Thus, the following declaration is illegal:

.. code:: ada run_button
    :class: ada-expect-compile-error

    procedure Example_Default_Access is

       Default : aliased Integer;

       type Rec (D : access Integer := Default'Access) is record
          V : Integer := D.all;
       end record;

       --  This can be fixed with: "type Rec (...) is limited record"
    begin
       null;
    end Example_Default_Access;

This restriction is needed to prevent the discriminant from creating a
dangling reference due to an assignment of the record object; it ensures
that the object and the discriminant are bound together for their
lifetime.

Special care must be taken when types with access discriminants are used
with allocators and return statements. The accessibility rules require the
compiler to perform static checks when new objects containing access
discriminants are created or returned. Consider the following example:

.. code:: ada run_button
    :class: ada-expect-compile-error

    procedure Example_3 is

       type Node (D : access Integer) is record
          V : Integer;
       end record;

       type Ptr is access all Node;

       Global_Value : aliased Integer := 1;
       Other_Data   : Integer         := 2;

       procedure P is
          Local_Value : aliased Integer := 3;
          R1          : Ptr;
          R2          : Ptr;
       begin
          R1 := new Node'(D => Global_Value'Access, V => Other_Data);
          --  This is legal

          R2 := new Node'(D => Local_Value'Access, V => Other_Data);
          --  This is illegal
       end P;
    begin
       null;
    end Example_3;

The allocator for :ada:`R1` is legal, since the accessibility level of
:ada:`Global'Access` is the same as the accessibility level of :ada:`D`.
However, the allocator for :ada:`R2` is illegal, because the accessibility
level of :ada:`Local'Access` is deeper than the accessibility level of
:ada:`D`, and assigning :ada:`R2` to an object outside :ada:`P` could lead
to a dangling reference.

In summary, these rules forbid the creation of an object in a storage pool
that contains an access discriminant pointing to some area of memory, be
it a part of the stack or some other storage pool, with a shorter
lifetime, thus preventing the discriminant from pointing to a nonexistent
object.

:code-config:`reset_accumulator=True`

Unchecked Access
~~~~~~~~~~~~~~~~

In the previous sections, we showed how the accessibility rules help
prevent dangling pointers, by ensuring that pointers cannot point from
longer-lived scopes to shorter-lived ones. But what if we actually want to
do that?

In some cases, it is necessary to store a reference to a local object in
a global data structure. You can do that by using :ada:`'Unchecked_Access`
instead of :ada:`'Access`. The :ada:`Unchecked` in the name reminds you
that you are bypassing the normal accessibility rules. To prevent dangling
pointers, you need to remove the pointer from the global data structure
before leaving the scope of the object.

As for any unsafe feature, it is a good idea to encapsulate
:ada:`'Unchecked_Access`, rather than scattering it all around the
program. You can do this using limited controlled types. The idea is that
:ada:`Initialize` plants a pointer to the object in some global data
structure, and :ada:`Finalize` removes the pointer just before it becomes
a dangling pointer.

Here is an example. Let's assume there are no tasks, and no heap-allocated
objects |mdash| otherwise, we would need a more complicated data structure,
such as a doubly-linked list, with locking. We keep a stack of objects,
implemented as a linked list via :ada:`Stack_Top` and chained through the
:ada:`Prev` component.

.. code:: ada

    private with Ada.Finalization;

    package Objects is

       type Object (Name : access constant String) is limited private;
       --  The Name is just to illustrate what's going on by printing it out.

       procedure For_All_Objects
         (Action : not null access procedure (X : Object));
       --  Iterate through all existing Objects in reverse order of creation,
       --  calling Action for each one.

       procedure Print_All_Objects;
       --  Print out the Names of all Objects in reverse order of creation.

       --  ... other operations

    private

       use Ada;

       type Object (Name : access constant String) is new
         Finalization.Limited_Controlled with record
          --  ... other components
          Prev : access Object := null; -- previous Object on the stack
       end record;

       procedure Initialize (X : in out Object);
       procedure Finalize (X : in out Object);

    end Objects;

    with Ada.Text_IO;

    package body Objects is

       Stack_Top : access Object := null;

       procedure Initialize (X : in out Object) is
       begin
          --  Push X onto the stack:
          X.Prev := Stack_Top;
          Stack_Top := X'Unchecked_Access;
       end Initialize;

       procedure Finalize (X : in out Object) is
       begin
          pragma Assert (Stack_Top = X'Unchecked_Access);
          --  Pop X from the stack:
          Stack_Top := X.Prev;
          X.Prev := null;  --  not really necessary, but safe
       end Finalize;

       procedure For_All_Objects
         (Action : not null access procedure (X : Object)) is
          --  Loop through the stack from top to bottom.
          Item : access Object := Stack_Top;
       begin
          while Item /= null loop
             Action (Item.all);
             Item := Item.Prev;
          end loop;
       end For_All_Objects;

       procedure Print_All_Objects is
          --  Iterate through the stack using For_All_Objects, passing
          --  Print_One_Object to print each one.
          procedure Print_One_Object (X : Object) is
          begin
             Text_IO.Put_Line ("  " & X.Name.all);
          end Print_One_Object;
       begin
          For_All_Objects (Print_One_Object'Access);
       end Print_All_Objects;

    end Objects;

.. code:: ada run_button

    with Ada.Text_IO; use Ada;
    with Objects; use Objects;

    procedure Main is

       This_Object : Object (Name => new String'("This_Object"));

       procedure Nested is
          That_Object : Object (Name => new String'("That_Object"));
       begin
          Text_IO.Put_Line ("Inside Nested:");
          Print_All_Objects;
       end Nested;

    begin
       Nested;

       Text_IO.Put_Line ("After Nested returns:");
       Print_All_Objects;
    end Main;

All occurrences of :ada:`'Unchecked_Access` are encapsulated in the
:ada:`Objects` package, and clients of :ada:`Objects` (such as
:ada:`Main`, below at the end) can freely declare :ada:`Objects` without
worrying about dangling pointers. :ada:`Stack_Top` can never dangle,
because :ada:`Finalize` cleans up, even in the case of exceptions and
aborts.

Note that :ada:`'Unchecked_Access` is applied to a formal parameter of
type :ada:`Object`, which is legal because formals of tagged types are
defined to be aliased. Note also that :ada:`Print_All_Objects` has no
visibility on the objects it is printing.

Observe that :ada:`That_Object` is not printed by the second call to
:ada:`Print_All_Objects`, because it no longer exists at that time.

:code-config:`reset_accumulator=True`
