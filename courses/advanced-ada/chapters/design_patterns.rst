:orphan:

:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Design Patterns
===============

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

Factory Functions
-----------------

Suppose we have a generic package that declares a stack class. The root of
the hierarchy would be as follows:

.. code:: ada

    generic
       type Element_Type is private;
    package Stacks is
       type Stack is abstract tagged null record;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is abstract;
    end Stacks;

Assume there are various concrete types in the class, say an unbounded
stack (that automatically grows as necessary) and a bounded stack
(implemented as a fixed-size array).

Now suppose we want to assign one stack to another, irrespective of the
specific stack type, something like this:

.. code:: ada

    generic
    package Stacks.Ops is

       procedure Op (T : in out Stack'Class; S : Stack'Class);

    end Stacks.Ops;

    package body Stacks.Ops is

       procedure Op (T : in out Stack'Class; S : Stack'Class) is
       begin
          T := S;  -- raises exception if tags don't match
          --  ...
       end Op;

    end Stacks.Ops;

This compiles, but isn't very robust, since if the tag of the target stack
doesn't match the tag of the source stack, then an exception will occur.
Our goal here is to figure out how to assign stack objects (whose type is
class-wide) in a manner such that the assignment is guaranteed to work
without raising a tag-mismatch exception.

One way to do this is to make an assignment-style operation that is
primitive for the type, so that it will dispatch according to the type of
the target stack. If the type of the source stack is class-wide, then
there can't be a tag mismatch (and hence no exception) since there's only
one controlling parameter.

(Note that you could do it the other way too, by dispatching on the tag of
the source stack. You could even make the operation class-wide, so that it
doesn't need to dispatch at all. The idea is to avoid passing more than a
single controlled operand.)

The assign operation would be declared like this:

.. code:: ada

    generic
       type Element_Type is private;
    package Stacks is
       type Stack is abstract tagged null record;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is abstract;

       procedure Assign
         (Target : in out Stack;
          Source : Stack'Class) is abstract;
       --  NEW: Assign procedure for Stack'Class

       function Length
         (Container : Stack) return Natural is abstract;

    end Stacks;

which would allow us to rewrite the above assignment statement as:

.. code:: ada

    generic
    package Stacks.Ops_2 is

       procedure Op (T : in out Stack'Class; S : Stack'Class);

    end Stacks.Ops_2;

    package body Stacks.Ops_2 is

       procedure Op (T : in out Stack'Class; S : Stack'Class) is
       begin
          T.Assign (S);  -- dispatches according T's tag
          --  ...
       end Op;

    end Stacks.Ops_2;

Each type in the class will have to override :ada:`Assign`. As an example,
let's follow the steps the necessary to implement the operation for the
bounded stack type. Its spec would look like this:

.. code:: ada

    generic
    package Stacks.Bounded_G is

       type Stack (Capacity : Natural) is
         new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class);

       function Length
         (Container : Stack) return Natural;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       type Stack (Capacity : Natural) is
         new Stacks.Stack with
          record
             Elements  : Element_Array (1 .. Capacity);
             Top_Index : Natural := 0;
          end record;

       function Length
         (Container : Stack) return Natural
       is (Container.Top_Index);

    end Stacks.Bounded_G;

This is just a canonical implementation of a bounded container form, that
uses a discriminant to control how much storage for the object is
allocated. The interesting part is implementing the :ada:`Assign`
operation, since we need some way to iterate over items in the source
stack. Here's a skeleton of the implementation:

.. code:: ada

    package body Stacks.Bounded_G is

       procedure Assign
         (Target : in out Stack;  -- bounded form
          Source : Stacks.Stack'Class)
       is
          --  ...
       begin
          --  ...
          for I in reverse 1 .. Source.Length loop
             --  Target.Elements (I) := < get curr elem of source >
             --  < move to next elem of source >
             null;
          end loop;
          --   ...
       end Assign;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is null;

    end Stacks.Bounded_G;

Note carefully that, assuming we visit items of the source stack in
top-to-bottom order, it's not a simple matter of pushing items onto the
target stack, since if we did that the items would end up in reverse
order. That's the reason why we populate the target stack array in
reverse, starting from largest index (the top of the stack) and working
backwards (towards the bottom of the stack).

The question is, how do you iterate over the source stack? Assume that
each specific type in the stack class has its own iterator type, matched
to that stacks's particular representation (similar to how the containers
in the standard library are implemented). The issue is that the type of
the source stack formal parameter is class-wide. How do we get an iterator
for the source stack actual parameter, if its specific type is not known
(not known statically, that is)?

The answer is, just ask the stack for one! A tagged type has dispatching
operations, some of which can be functions, so here we just need a
dispatching function to return an iterator object. The idiom of
dispatching on an object whose type is in one class, to return an object
whose type is in another class, is called a *factory function* or
*dispatching constructor*.

An operation can only be primitive for one tagged type, so if the
operation dispatches on the stack parameter then the function return type
must be class-wide. We now introduce type :ada:`Cursor`, the root of the
stack iterator hierarchy, and amend the stack class with a factory
function for cursors:

:code-config:`reset_accumulator=True`

.. code:: ada

    generic
       type Element_Type is private;
    package Stacks is
       type Stack is abstract tagged null record;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is abstract;

       procedure Assign
         (Target : in out Stack;
          Source : Stack'Class) is abstract;
       --  NEW: Assign procedure for Stack'Class

       function Length
         (Container : Stack) return Natural is abstract;

       procedure Clear
         (Container : in out Stack);

       type Cursor is abstract tagged null record;  -- the iterator

       function Top_Cursor  -- the factory function
         (Container : not null access constant Stack)
           return Cursor'Class is abstract;

       --  primitive ops for the Cursor class
       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       --  procedure Previous (Position : in out Cursor);

    end Stacks;

Each type in the stack class will override :ada:`Top_Cursor`, to return a
cursor that can be used to visit the items in that stack object. We can
now complete our implementation of the :ada:`Assign` operation for bounded
stacks as follows:

.. code:: ada

    generic
    package Stacks.Bounded_G is

       type Stack (Capacity : Natural) is
         new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class);

       function Length
         (Container : Stack) return Natural;

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       type Stack (Capacity : Natural) is
         new Stacks.Stack with
          record
             Elements  : Element_Array (1 .. Capacity);
             Top_Index : Natural := 0;
          end record;

       function Length
         (Container : Stack) return Natural
       is (Container.Top_Index);

    end Stacks.Bounded_G;

.. code:: ada

    package body Stacks.Bounded_G is

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class)
       is
          C : Stacks.Cursor'Class := Source.Top_Cursor;  -- dispatches

       begin
          Target.Clear;

          for I in reverse 1 .. Source.Length loop
             Target.Elements (I) := C.Element;  -- dispatches
             C.Next;  -- dispatches
          end loop;

          Target.Top_Index := Source.Length;
       end Assign;

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class
       is
       begin
          if Container.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, Container.Top_Index);
          end if;
       end Top_Cursor;

    end Stacks.Bounded_G;

The :ada:`Source` parameter has a class-wide type, which means the call to
:ada:`Top_Cursor` dispatches (since :ada:`Top_Cursor` is primitive for the
type). This is exactly what we want, since different stack types will have
different representations, and will therefore require different kinds of
cursors. The cursor object (here, :ada:`C`) returned by the factory
function is itself class-wide, which means that cursor operations also
dispatch. The function call :ada:`C.Element` returns the element of
:ada:`Source` at the current position of the cursor, and :ada:`C.Next`
advances the cursor to the next position (towards the bottom of the
stack).

:code-config:`reset_accumulator=True`

This is the complete source-code:

.. code:: ada

    generic
       type Element_Type is private;
       with function "=" (L, R : Element_Type) return Boolean is <>;
    package Stacks is
       pragma Pure;

       type Stack is abstract tagged null record;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is abstract;

       function Top
         (Container : Stack) return Element_Type is abstract;

       procedure Pop (Container : in out Stack) is abstract;

       function Length
         (Container : Stack) return Natural is abstract;

       procedure Clear (Container : in out Stack) is abstract;

       procedure Assign
         (Target : in out Stack;
          Source : Stack'Class) is abstract;

       type Cursor is abstract tagged null record;

       function Top_Cursor
         (Container : not null access constant Stack)
         return Cursor'Class is abstract;

       function Bottom_Cursor
         (Container : not null access constant Stack)
         return Cursor'Class is abstract;

       function Has_Element (Position : Cursor) return Boolean is abstract;

       function Element
         (Position : Cursor) return Element_Type is abstract;

       procedure Next (Position : in out Cursor) is abstract;
       procedure Previous (Position : in out Cursor) is abstract;

    end Stacks;

.. code:: ada

    generic
    package Stacks.Bounded_G is
       pragma Pure;

       type Stack (Capacity : Natural) is new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       function Top
         (Container : Stack) return Element_Type;

       procedure Pop
         (Container : in out Stack);

       function Length
         (Container : Stack) return Natural;

       procedure Clear (Container : in out Stack);

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class);

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class;

       function Bottom_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       function "=" (L, R : Element_Array) return Boolean is abstract;

       type Stack (Capacity : Natural) is new Stacks.Stack with record
          Elements  : Element_Array (1 .. Capacity);
          Top_Index : Natural := 0;
       end record;

       type Cursor is new Stacks.Cursor with record
          Container : access constant Stack;
          Index     : Natural := 0;
       end record;

       function Has_Element (Position : Cursor) return Boolean;

       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       procedure Previous (Position : in out Cursor);

    end Stacks.Bounded_G;

.. code:: ada

    private with Ada.Finalization;

    generic
    package Stacks.Unbounded_G is
       pragma Preelaborate;

       type Stack is new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       function Top
         (Container : Stack) return Element_Type;

       procedure Pop
         (Container : in out Stack);

       function Length
         (Container : Stack) return Natural;

       procedure Clear (Container : in out Stack);

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class);

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class;

       function Bottom_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       function "=" (L, R : Element_Array) return Boolean is abstract;

       type Element_Array_Access is access Element_Array;

       type Rep_Type is new Ada.Finalization.Controlled with record
          Elements  : Element_Array_Access;
          Top_Index : Natural := 0;
       end record;

       overriding
       procedure Adjust (Rep : in out Rep_Type);

       overriding
       procedure Finalize (Rep : in out Rep_Type);

       type Stack is new Stacks.Stack with record
          Rep : Rep_Type;
       end record;

       type Cursor is new Stacks.Cursor with record
          Container : access constant Stack;
          Index     : Natural := 0;
       end record;

       function Has_Element (Position : Cursor) return Boolean;

       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       procedure Previous (Position : in out Cursor);

    end Stacks.Unbounded_G;

.. code:: ada

    with System;  use type System.Address;

    package body Stacks.Bounded_G is

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type)
       is
          E : Element_Array renames Container.Elements;
          I : Natural renames Container.Top_Index;

       begin
          E (I + 1) := Item;
          I := I + 1;
       end Push;

       function Top
         (Container : Stack) return Element_Type
       is (Container.Elements (Container.Top_Index));

       procedure Pop
         (Container : in out Stack)
       is
          I : Natural renames Container.Top_Index;

       begin
          I := I - 1;
       end Pop;

       function Length
         (Container : Stack) return Natural
       is (Container.Top_Index);

       procedure Clear (Container : in out Stack) is
       begin
          Container.Top_Index := 0;
       end Clear;

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class)
       is
          --  C : Stacks.Cursor'Class := Source.Top_Cursor;
          C : Stacks.Cursor'Class := Top_Cursor (Source'Unchecked_Access);

       begin
          if Target'Address = Source'Address then
             return;
          end if;

          if Source.Length > Target.Capacity then
             raise Constraint_Error;
          end if;

          Target.Clear;

          for I in reverse 1 .. Source.Length loop
             Target.Elements (I) := C.Element;
             C.Next;
          end loop;

          Target.Top_Index := Source.Length;
       end Assign;

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class
       is
       begin
          if Container.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, Container.Top_Index);
          end if;
       end Top_Cursor;

       function Bottom_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class
       is
       begin
          if Container.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, 1);
          end if;
       end Bottom_Cursor;

       function Has_Element (Position : Cursor) return Boolean is
         (Position.Index > 0);

       function Element
         (Position : Cursor) return Element_Type
       is
          S : Stack renames Position.Container.all;
          I : constant Positive range 1 .. S.Top_Index := Position.Index;

       begin
          return S.Elements (I);
       end Element;

       procedure Next (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I > S.Top_Index then
                I := S.Top_Index;
             else
                I := I - 1;
             end if;
          end;

          if I = 0 then
             Position.Container := null;
          end if;
       end Next;

       procedure Previous (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I >= S.Top_Index then
                I := 0;
                Position.Container := null;

             else
                I := I + 1;
             end if;
          end;
       end Previous;

    end Stacks.Bounded_G;

.. code:: ada

    with Ada.Unchecked_Deallocation;
    with System;  use type System.Address;

    package body Stacks.Unbounded_G is

       procedure Free is
          new Ada.Unchecked_Deallocation
         (Element_Array,
          Element_Array_Access);

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type)
       is
          R : Rep_Type renames Container.Rep;
          I : Natural renames R.Top_Index;

       begin
          if R.Elements = null then
             R.Elements := new Element_Array'(1 .. 1 => Item);
             I := 1;
             return;
          end if;

          if I = R.Elements'Last then
             declare
                X : Element_Array_Access := R.Elements;
                J : constant Positive := 2 * I;
                E : Element_Array_Access := new Element_Array (1 .. J);

             begin
                Copy : begin
                   E (1 .. I) := X.all;
                exception
                   when others =>
                      Free (E);
                      raise;
                end Copy;

                R.Elements := E;
                Free (X);
             end;
          end if;

          R.Elements (I + 1) := Item;
          I := I + 1;
       end Push;

       function Top
         (Container : Stack) return Element_Type
       is
          R : Rep_Type renames Container.Rep;

       begin
          return R.Elements (R.Top_Index);
       end Top;

       procedure Pop
         (Container : in out Stack)
       is
          R : Rep_Type renames Container.Rep;
          I : Natural renames R.Top_Index;

       begin
          I := I - 1;
       end Pop;

       function Length
         (Container : Stack) return Natural
       is (Container.Rep.Top_Index);

       procedure Clear (Container : in out Stack) is
       begin
          Container.Rep.Top_Index := 0;
       end Clear;

       procedure Assign
         (Target : in out Stack;
          Source : Stacks.Stack'Class)
       is
          --  C : Stacks.Cursor'Class := Source.Top_Cursor;
          C : Stacks.Cursor'Class := Top_Cursor (Source'Unchecked_Access);
          T : Rep_Type renames Target.Rep;
          L : constant Natural := Source.Length;

       begin
          if Target'Address = Source'Address then
             return;
          end if;

          Target.Clear;

          if L = 0 then
             return;
          end if;

          if T.Elements = null
            or else T.Elements'Length < L
          then
             declare
                X : Element_Array_Access := T.Elements;

             begin
                T.Elements := null;
                Free (X);
             end;

             T.Elements := new Element_Array (1 .. L);
          end if;

          for I in reverse 1 .. L loop
             T.Elements (I) := C.Element;
             C.Next;
          end loop;

          T.Top_Index := L;
       end Assign;

       procedure Adjust (Rep : in out Rep_Type) is
          X : constant Element_Array_Access := Rep.Elements;
          I : constant Natural := Rep.Top_Index;

       begin
          Rep.Elements := null;
          Rep.Top_Index := 0;

          if I > 0 then
             Rep.Elements := new Element_Array'(X (1 .. I));
             Rep.Top_Index := I;
          end if;
       end Adjust;

       procedure Finalize (Rep : in out Rep_Type) is
          X : Element_Array_Access := Rep.Elements;

       begin
          Rep.Elements := null;
          Rep.Top_Index := 0;

          Free (X);
       end Finalize;

       function Top_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class
       is
          R : Rep_Type renames Container.Rep;

       begin
          if R.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, R.Top_Index);
          end if;
       end Top_Cursor;

       function Bottom_Cursor
         (Container : not null access constant Stack)
         return Stacks.Cursor'Class
       is
          R : Rep_Type renames Container.Rep;

       begin
          if R.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, 1);
          end if;
       end Bottom_Cursor;

       function Has_Element (Position : Cursor) return Boolean is
         (Position.Index > 0);

       function Element
         (Position : Cursor) return Element_Type
       is
          R : Rep_Type renames Position.Container.Rep;
          I : constant Positive range 1 .. R.Top_Index := Position.Index;

       begin
          return R.Elements (I);
       end Element;

       procedure Next (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             R : Rep_Type renames Position.Container.Rep;
          begin
             if I > R.Top_Index then
                I := R.Top_Index;
             else
                I := I - 1;
             end if;
          end;

          if I = 0 then
             Position.Container := null;
          end if;
       end Next;

       procedure Previous (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             R : Rep_Type renames Position.Container.Rep;
          begin
             if I >= R.Top_Index then
                I := 0;
                Position.Container := null;

             else
                I := I + 1;
             end if;
          end;
       end Previous;

    end Stacks.Unbounded_G;

.. code:: ada run_button

    with Stacks;
    with Stacks.Bounded_G;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Test is

       package Int_Stacks           is new Stacks (Element_Type => Integer);
       package Bounded_G_Int_Stacks is new Int_Stacks.Bounded_G;
       use Bounded_G_Int_Stacks;

       S1, S2 : Stack (10);

    begin

       S1.Push (1);
       S1.Push (4);
       S1.Push (5);

       S2.Push (2);
       S2.Push (3);
       S2.Push (7);

       while S1.Length > 0 loop
          Put_Line (Integer'Image (S1.Top));
          S1.Pop;
       end loop;

       S1.Assign (S2);

       while S1.Length > 0 loop
          Put_Line (Integer'Image (S1.Top));
          S1.Pop;
       end loop;

    end Simple_Test;

:code-config:`reset_accumulator=True`

Classwide Operations, Iterators, and Generic Algorithms
-------------------------------------------------------

In the previous section, we used a stack class to demonstrate factory
functions (to construct iterator objects), and implemented an assignment
operation that dispatched on the type of the target stack. We mentioned in
passing that that operation could be implemented by dispatching on the
source stack, so let's show how to do that.

We reorder the parameters so that the :ada:`Source` stack is first in the
parameter list (so that it's the "distinguished receiver" of a
prefix-style call), and change its type from classwide to specific. We
also change the name of the operation from :ada:`Assign` to :ada:`Copy`,
per convention. The new declaration is as follows:

.. code-block:: ada

    procedure Copy
      (Source : Stack;
       Target : in out Stack'Class) is abstract;

In the earlier example, we had to populate the target stack in reverse, so
that the elements would be in the correct order. We were able to do that
because the operation was implemented by the specific type, and hence it
had direct access to the representation of the (target) stack. Here the
target type is classwide, so the only way to populate it is in forward
order, using :ada:`Push`. That means we'll have to iterate over the source
stack in reverse, so that the items are properly ordered in the target.

The bounded stack type is implemented as an array, so implementing
:ada:`Copy` is easy because the bottom of the stack begins at the
beginning of the array:

.. code-block:: ada

    procedure Copy
      (Source : Stack;  -- bounded stack (array-based)
       Target : in out Stacks.Stack'Class)
    is
    begin
       Target.Clear;

       for I in 1 .. Source.Top_Index loop    -- from bottom to top
          Target.Push (Source.Elements (I));  -- Elements is the array
       end loop;
    end Copy;

We also said in the earlier section that the operation need not be
primitive for the type. If we change the source stack's type to classwide,
then the operation itself becomes classwide:

.. code-block:: ada

    procedure Copy2  -- classwide op, not primitive
      (Source : Stack'Class;
       Target : in out Stack'Class);

If we make the type of the source stack classwide, then we'll need a
different way to iterate over items of the source stack in reverse order,
since we don't have access to its representation anymore.

To do that we'll amend the cursor type to include some additional
operations. First we'll add a new factory function, to construct a cursor
object that (initially) designates the element at the bottom of the stack:

.. code-block:: ada

    function Bottom_Cursor
      (Container : not null access constant Stack)
          return Cursor'Class is abstract;

We'll also need an operation to move the cursor to the element that
precedes the current item:

.. code-block:: ada

    procedure Previous (Position : in out Cursor) is abstract;

That gives us everything we need to turn :ada:`Copy` into a classwide
operation, so that it only needs to be implemented once:

.. code-block:: ada

    procedure Copy2
      (Source : Stack'Class;
       Target : in out Stack'Class)
    is
       C : Cursor'Class := Bottom_Cursor (Source'Access);

    begin
       Target.Clear;

       while C.Has_Element loop
          Target.Push (C.Element);
          C.Previous;
       end loop;
    end Copy2;

.. code:: ada

    generic
       type Element_Type is private;
       with function "=" (L, R : Element_Type) return Boolean is <>;
    package Stacks is
       pragma Pure;

       type Stack is abstract tagged null record;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type) is abstract;

       function Top
         (Container : Stack) return Element_Type is abstract;

       procedure Pop (Container : in out Stack) is abstract;

       function Length
         (Container : Stack) return Natural is abstract;

       procedure Clear (Container : in out Stack) is abstract;

       procedure Copy
         (Source : Stack;
          Target : in out Stack'Class) is abstract;

       procedure Copy2
         (Source : Stack'Class;
          Target : in out Stack'Class);

       type Cursor is abstract tagged null record;

       function Top_Cursor
         (Container : not null access constant Stack)
          return Cursor'Class is abstract;

       function Bottom_Cursor
         (Container : not null access constant Stack)
          return Cursor'Class is abstract;

       function Has_Element (Position : Cursor) return Boolean is abstract;

       function Element
         (Position : Cursor) return Element_Type is abstract;

       procedure Next (Position : in out Cursor) is abstract;
       procedure Previous (Position : in out Cursor) is abstract;

    end Stacks;

    generic
    package Stacks.Bounded_G is
       pragma Pure;

       type Stack (Capacity : Natural) is new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       function Top
         (Container : Stack) return Element_Type;

       procedure Pop
         (Container : in out Stack);

       function Length
         (Container : Stack) return Natural;

       procedure Clear (Container : in out Stack);

       procedure Copy
         (Source : Stack;
          Target : in out Stacks.Stack'Class);

       function Top_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class;

       function Bottom_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class;

       type Cursor is new Stacks.Cursor with private;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       function "=" (L, R : Element_Array) return Boolean is abstract;

       type Stack (Capacity : Natural) is new Stacks.Stack with record
          Elements  : Element_Array (1 .. Capacity);
          Top_Index : Natural := 0;
       end record;

       type Cursor is new Stacks.Cursor with record
          Container : access constant Stack;
          Index     : Natural := 0;
       end record;

       function Has_Element (Position : Cursor) return Boolean;

       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       procedure Previous (Position : in out Cursor);

    end Stacks.Bounded_G;

    private with Ada.Finalization;

    generic
    package Stacks.Unbounded_G is
       pragma Preelaborate;

       type Stack is new Stacks.Stack with private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       function Top
         (Container : Stack) return Element_Type;

       procedure Pop
         (Container : in out Stack);

       function Length
         (Container : Stack) return Natural;

       procedure Clear (Container : in out Stack);

       procedure Copy
         (Source : Stack;
          Target : in out Stacks.Stack'Class);

       function Top_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class;

       function Bottom_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class;

    private

       type Element_Array is array (Positive range <>) of Element_Type;

       function "=" (L, R : Element_Array) return Boolean is abstract;

       type Element_Array_Access is access Element_Array;

       type Rep_Type is new Ada.Finalization.Controlled with record
          Elements  : Element_Array_Access;
          Top_Index : Natural := 0;
       end record;

       overriding
       procedure Adjust (Rep : in out Rep_Type);

       overriding
       procedure Finalize (Rep : in out Rep_Type);

       type Stack is new Stacks.Stack with record
          Rep : Rep_Type;
       end record;

       type Cursor is new Stacks.Cursor with record
          Container : access constant Stack;
          Index     : Natural := 0;
       end record;

       function Has_Element (Position : Cursor) return Boolean;

       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       procedure Previous (Position : in out Cursor);

    end Stacks.Unbounded_G;

.. code:: ada

    with System;  use type System.Address;

    package body Stacks is

       procedure Copy2
         (Source : Stack'Class;
          Target : in out Stack'Class)
       is
          C : Cursor'Class := Bottom_Cursor (Source'Access);

       begin
          if Source'Address = Target'Address then
             return;
          end if;

          Target.Clear;

          while C.Has_Element loop
             Target.Push (C.Element);
             C.Previous;
          end loop;
       end Copy2;

    end Stacks;

.. code:: ada

    with System;  use type System.Address;

    package body Stacks.Bounded_G is

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type)
       is
          E : Element_Array renames Container.Elements;
          I : Natural renames Container.Top_Index;

       begin
          E (I + 1) := Item;
          I := I + 1;
       end Push;

       function Top
         (Container : Stack) return Element_Type
       is
       begin
          return Container.Elements (Container.Top_Index);
       end Top;

       procedure Pop
         (Container : in out Stack)
       is
          I : Natural renames Container.Top_Index;

       begin
          I := I - 1;
       end Pop;

       function Length
         (Container : Stack) return Natural
       is
       begin
          return Container.Top_Index;
       end Length;

       procedure Clear (Container : in out Stack) is
       begin
          Container.Top_Index := 0;
       end Clear;

       procedure Copy
         (Source : Stack;
          Target : in out Stacks.Stack'Class)
       is
       begin
          if Target'Address = Source'Address then
             return;
          end if;

          Target.Clear;

          for I in 1 .. Source.Top_Index loop
             Target.Push (Source.Elements (I));
          end loop;
       end Copy;

       function Top_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class
       is
       begin
          if Container.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, Container.Top_Index);
          end if;
       end Top_Cursor;

       function Bottom_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class
       is
       begin
          if Container.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, 1);
          end if;
       end Bottom_Cursor;

       function Has_Element (Position : Cursor) return Boolean is
       begin
          return Position.Index > 0;
       end Has_Element;

       function Element
         (Position : Cursor) return Element_Type
       is
          S : Stack renames Position.Container.all;
          I : constant Positive range 1 .. S.Top_Index := Position.Index;

       begin
          return S.Elements (I);
       end Element;

       procedure Next (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I > S.Top_Index then
                I := S.Top_Index;
             else
                I := I - 1;
             end if;
          end;

          if I = 0 then
             Position.Container := null;
          end if;
       end Next;

       procedure Previous (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I >= S.Top_Index then
                I := 0;
                Position.Container := null;

             else
                I := I + 1;
             end if;
          end;
       end Previous;

    end Stacks.Bounded_G;

.. code:: ada

    with Ada.Unchecked_Deallocation;
    with System;  use type System.Address;

    package body Stacks.Unbounded_G is

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Element_Array,
            Element_Array_Access);

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type)
       is
          R : Rep_Type renames Container.Rep;
          I : Natural renames R.Top_Index;

       begin
          if R.Elements = null then
             R.Elements := new Element_Array'(1 .. 1 => Item);
             I := 1;
             return;
          end if;

          if I = R.Elements'Last then
             declare
                X : Element_Array_Access := R.Elements;
                J : constant Positive := 2 * I;
                E : Element_Array_Access := new Element_Array (1 .. J);

             begin
                Copy : begin
                   E (1 .. I) := X.all;
                exception
                   when others =>
                      Free (E);
                      raise;
                end Copy;

                R.Elements := E;
                Free (X);
             end;
          end if;

          R.Elements (I + 1) := Item;
          I := I + 1;
       end Push;

       function Top
         (Container : Stack) return Element_Type
       is
          R : Rep_Type renames Container.Rep;

       begin
          return R.Elements (R.Top_Index);
       end Top;

       procedure Pop
         (Container : in out Stack)
       is
          R : Rep_Type renames Container.Rep;
          I : Natural renames R.Top_Index;

       begin
          I := I - 1;
       end Pop;

       function Length
         (Container : Stack) return Natural
       is
       begin
          return Container.Rep.Top_Index;
       end Length;

       procedure Clear (Container : in out Stack) is
       begin
          Container.Rep.Top_Index := 0;
       end Clear;

       procedure Copy
         (Source : Stack;
          Target : in out Stacks.Stack'Class)
       is
          S : Rep_Type renames Source.Rep;

       begin
          if Target'Address = Source'Address then
             return;
          end if;

          Target.Clear;

          for I in 1 .. S.Top_Index loop
             Target.Push (S.Elements (I));
          end loop;
       end Copy;

       procedure Adjust (Rep : in out Rep_Type) is
          X : constant Element_Array_Access := Rep.Elements;
          I : constant Natural := Rep.Top_Index;

       begin
          Rep.Elements := null;
          Rep.Top_Index := 0;

          if I > 0 then
             Rep.Elements := new Element_Array'(X (1 .. I));
             Rep.Top_Index := I;
          end if;
       end Adjust;

       procedure Finalize (Rep : in out Rep_Type) is
          X : Element_Array_Access := Rep.Elements;

       begin
          Rep.Elements := null;
          Rep.Top_Index := 0;

          Free (X);
       end Finalize;

       function Top_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class
       is
          R : Rep_Type renames Container.Rep;

       begin
          if R.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, R.Top_Index);
          end if;
       end Top_Cursor;

       function Bottom_Cursor
         (Container : not null access constant Stack)
          return Stacks.Cursor'Class
       is
          R : Rep_Type renames Container.Rep;

       begin
          if R.Top_Index = 0 then
             return Cursor'(null, 0);
          else
             return Cursor'(Container, 1);
          end if;
       end Bottom_Cursor;

       function Has_Element (Position : Cursor) return Boolean is
       begin
          return Position.Index > 0;
       end Has_Element;

       function Element
         (Position : Cursor) return Element_Type
       is
          R : Rep_Type renames Position.Container.Rep;
          I : constant Positive range 1 .. R.Top_Index := Position.Index;

       begin
          return R.Elements (I);
       end Element;

       procedure Next (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             R : Rep_Type renames Position.Container.Rep;
          begin
             if I > R.Top_Index then
                I := R.Top_Index;
             else
                I := I - 1;
             end if;
          end;

          if I = 0 then
             Position.Container := null;
          end if;
       end Next;

       procedure Previous (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             R : Rep_Type renames Position.Container.Rep;
          begin
             if I >= R.Top_Index then
                I := 0;
                Position.Container := null;

             else
                I := I + 1;
             end if;
          end;
       end Previous;

    end Stacks.Unbounded_G;

Note that we declared the classwide stack operation in the root package
(see the specification of the :ada:`Stacks` package), but it could have
just as easily been declared as a generic child procedure:

.. code:: ada

    generic
    procedure Stacks.Generic_Copy3
      (Source : Stack'Class;
       Target : in out Stack'Class);

.. code:: ada

    with System;  use type System.Address;

    procedure Stacks.Generic_Copy3
      (Source : Stack'Class;
       Target : in out Stack'Class)
    is
       C : Cursor'Class := Bottom_Cursor (Source'Access);

    begin
       if Source'Address = Target'Address then
          return;
       end if;

       Target.Clear;

       while C.Has_Element loop
          Target.Push (C.Element);
          C.Previous;
       end loop;
    end Stacks.Generic_Copy3;

Actually, we could move the operation out of the package hierarchy
entirely:

.. code:: ada

    with Stacks;

    generic
       with package Stack_Types is new Stacks (<>);
       use Stack_Types;
    procedure Generic_Stack_Copy4
      (Source : Stack'Class;
       Target : in out Stack'Class);

.. code:: ada

    with System;  use type System.Address;

    procedure Generic_Stack_Copy4
      (Source : Stack'Class;
       Target : in out Stack'Class)
    is
       C : Cursor'Class := Bottom_Cursor (Source'Access);

    begin
       if Source'Address = Target'Address then
          return;
       end if;

       Target.Clear;
       while C.Has_Element loop
          Target.Push (C.Element);
          C.Previous;
       end loop;
    end Generic_Stack_Copy4;

We can generalize this even more, such that the copy algorithm works for
any kind of stack:

.. code:: ada

    generic
       type Stack_Type (<>) is limited private;
       type Cursor_Type (<>) is private;
       type Element_Type (<>) is private;

       with function Bottom_Cursor
         (Stack : Stack_Type)
          return Cursor_Type is <>;
       with procedure Clear
         (Stack : in out Stack_Type) is <>;
       with procedure Push
         (Stack : in out Stack_Type;
          Item  : Element_Type) is <>;
       with function Has_Element
         (Cursor : Cursor_Type) return Boolean is <>;
       with function Element
         (Cursor : Cursor_Type) return Element_Type is <>;
       with procedure Previous
         (Cursor : in out Cursor_Type) is <>;

    procedure Generic_Stack_Copy5
      (Source : Stack_Type;
       Target : in out Stack_Type);

.. code:: ada

    with System;  use type System.Address;

    procedure Generic_Stack_Copy5
      (Source : Stack_Type;
       Target : in out Stack_Type)
    is
       C : Cursor_Type := Bottom_Cursor (Source);

    begin
       if Source'Address = Target'Address then
          return;
       end if;

       Clear (Target);
       while Has_Element (C) loop
          Push (Target, Element (C));
          Previous (C);
       end loop;
    end Generic_Stack_Copy5;

This illustrates the difference between the dynamic polymorphism of tagged
types and the static polymorphism of generics. There is no need for a
stack class anymore (having a dedicated copy operation that works only for
types in that class), since the generic algorithm works for any stack.
(This is exactly how the standard container library is designed. Container
types are tagged, but they are not members of a common class.)

Instantiating this operation on our stack type is easy, since the names of
the generic actual operations match the names of the generic formal
operations, so we don't need to specify them explicitly (since the generic
formals are marked as accepting a :ada:`<>` default).

First, we instantiate a stack of integer:

.. code:: ada

    with Stacks;
    pragma Elaborate_All (Stacks);

    package Integer_Stacks is new Stacks (Integer);
    pragma Pure (Integer_Stacks);

    with Stacks.Bounded_G;

    package Integer_Stacks.Bounded is new Integer_Stacks.Bounded_G;
    pragma Pure (Integer_Stacks.Bounded);

Unfortunately, using our original :ada:`Stacks` package doesn't work in
this case, as indicated in the compilation error:

.. code:: ada
    :class: ada-expect-compile-error

    with Integer_Stacks.Bounded; use Integer_Stacks.Bounded;
    with Generic_Stack_Copy5;

    procedure Test_Copy5 (S : Stack) is
       procedure Copy5 is
         new Generic_Stack_Copy5
           (Stack,
            Cursor,
            Integer);

       T : Stack (S.Length);

    begin
       Copy5 (Source => S, Target => T);
    end Test_Copy5;

We need to first rewrite our stack package:

.. code:: ada

    generic
       type Element_Type is private;

    package Stacks_Bounded is
       pragma Pure;

       type Stack (Capacity : Natural) is tagged limited private;

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type);

       function Top
         (Container : Stack) return Element_Type;

       procedure Pop
         (Container : in out Stack);

       function Length
         (Container : Stack) return Natural;

       procedure Clear (Container : in out Stack);

       type Cursor is private;

       function Top_Cursor
         (Container : Stack) return Cursor;

       function Bottom_Cursor
         (Container : Stack) return Cursor;

       function Has_Element (Position : Cursor) return Boolean;

       function Element
         (Position : Cursor) return Element_Type;

       procedure Next (Position : in out Cursor);
       procedure Previous (Position : in out Cursor);

    private

       type Element_Array is array (Positive range <>) of Element_Type;
       function "=" (L, R : Element_Array) return Boolean is abstract;

       type Stack (Capacity : Natural) is tagged limited record
          Elements  : Element_Array (1 .. Capacity);
          Top_Index : Natural := 0;
       end record;

       type Cursor is record
          Container : access constant Stack;
          Index     : Natural := 0;
       end record;

    end Stacks_Bounded;

This is the package body of :ada:`Stacks_Bounded`:

.. code:: ada

    with System;  use type System.Address;

    package body Stacks_Bounded is

       procedure Push
         (Container : in out Stack;
          Item      :        Element_Type)
       is
          E : Element_Array renames Container.Elements;
          I : Natural renames Container.Top_Index;

       begin
          E (I + 1) := Item;
          I := I + 1;
       end Push;

       function Top
         (Container : Stack) return Element_Type
       is
       begin
          return Container.Elements (Container.Top_Index);
       end Top;

       procedure Pop
         (Container : in out Stack)
       is
          I : Natural renames Container.Top_Index;

       begin
          I := I - 1;
       end Pop;

       function Length
         (Container : Stack) return Natural
       is
       begin
          return Container.Top_Index;
       end Length;

       procedure Clear (Container : in out Stack) is
       begin
          Container.Top_Index := 0;
       end Clear;

       function Top_Cursor (Container : Stack) return Cursor is
       begin
          if Container.Top_Index = 0 then
             return (null, 0);
          else
             return (Container'Unchecked_Access, Container.Top_Index);
          end if;
       end Top_Cursor;

       function Bottom_Cursor (Container : Stack) return Cursor is
       begin
          if Container.Top_Index = 0 then
             return (null, 0);
          else
             return (Container'Unchecked_Access, 1);
          end if;
       end Bottom_Cursor;

       function Has_Element (Position : Cursor) return Boolean is
       begin
          return Position.Index > 0;
       end Has_Element;

       function Element
         (Position : Cursor) return Element_Type
       is
          S : Stack renames Position.Container.all;
          I : constant Positive range 1 .. S.Top_Index := Position.Index;

       begin
          return S.Elements (I);
       end Element;

       procedure Next (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I > S.Top_Index then
                I := S.Top_Index;
             else
                I := I - 1;
             end if;
          end;

          if I = 0 then
             Position.Container := null;
          end if;
       end Next;

       procedure Previous (Position : in out Cursor) is
          I : Natural renames Position.Index;

       begin
          if I = 0 then
             return;
          end if;

          declare
             S : Stack renames Position.Container.all;
          begin
             if I >= S.Top_Index then
                I := 0;
                Position.Container := null;

             else
                I := I + 1;
             end if;
          end;
       end Previous;

    end Stacks_Bounded;

Now, we instantiate a stack of integer based on :ada:`Stacks_Bounded`:

.. code:: ada

    with Stacks_Bounded;
    pragma Elaborate_All (Stacks_Bounded);

    package Integer_Stacks_Bounded is new Stacks_Bounded (Integer);
    pragma Pure (Integer_Stacks_Bounded);

We can use this stack in our test application:

.. code:: ada

    with Integer_Stacks_Bounded; use Integer_Stacks_Bounded;
    with Generic_Stack_Copy5;

    procedure Test_Copy5 (S : Stack) is
       procedure Copy5 is
         new Generic_Stack_Copy5
           (Stack,
            Cursor,
            Integer);

       T : Stack (S.Length);

    begin
       Copy5 (Source => S, Target => T);
    end Test_Copy5;

But why stop there? We can write a generic copy algorithm for any kind of
container. We just need to generalize iteration a little, to mean "visit
these items in the way that makes sense for this source container," and
generalizing insertion, to mean "add this element in the way that makes
sense for this target container." The declaration would be:

.. code:: ada

    generic
       type Container_Type (<>) is limited private;
       type Cursor_Type (<>) is private;
       type Element_Type (<>) is private;

       with function First
         (Container : Container_Type)
          return Cursor_Type is <>;
       with procedure Clear
         (Container : in out Container_Type) is <>;
       with procedure Insert
         (Container : in out Container_Type;
          Item      : Element_Type) is <>;
       with function Has_Element
         (Cursor : Cursor_Type) return Boolean is <>;
       with function Element
         (Cursor : Cursor_Type) return Element_Type is <>;
       with procedure Advance
         (Cursor : in out Cursor_Type) is <>;

    procedure Generic_Copy6
      (Source : Container_Type;
       Target : in out Container_Type);

.. code:: ada

    private with Ada.Finalization;
    with System;  use type System.Address;

    procedure Generic_Copy6
      (Source : Container_Type;
       Target : in out Container_Type)
    is
       C : Cursor_Type := First (Source);

    begin
       if Source'Address = Target'Address then
          return;
       end if;

       Clear (Target);
       while Has_Element (C) loop
          Insert (Target, Element (C));
          Advance (C);
       end loop;
    end Generic_Copy6;

We can instantiate this using our stack type, but note that the generic
actuals no longer match the generic formals, so we need to specify them
explicitly:

.. code:: ada

    with Integer_Stacks_Bounded;  use Integer_Stacks_Bounded;
    with Generic_Copy6;

    procedure Test_Copy6 (S : Stack) is
       procedure Copy6 is
         new Generic_Copy6
           (Stack,
            Cursor,
            Integer,
            First   => Bottom_Cursor,
            Insert  => Push,
            Advance => Previous);

       T : Stack (S.Length);

    begin
       Copy6 (Source => S, Target => T);
    end Test_Copy6;

One assumption we've made here is that the source and target containers
have the same type. Suppose we would like to copy the items in a stack to,
say, an array. One approach would be to introduce another generic formal
container type (a *source container* type that is distinct from the
*target container* type), but there's another way. Consider the
implementation of the copy algorithm :ada:`Generic_Copy6` above.
Notice that the only thing we do with the source container is to use it to
construct a cursor. If we pass in the cursor directly, that eliminates any
mention of the source stack, which in turn allows the source and target
containers to be different types. Our algorithm now becomes:

.. code:: ada

    generic
       type Container_Type (<>) is limited private;
       type Cursor_Type (<>) is private;
       type Element_Type (<>) is private;

       with procedure Insert
         (Container : in out Container_Type;
          Item      : Element_Type) is <>;
       with function Has_Element
         (Cursor : Cursor_Type) return Boolean is <>;
       with function Element
         (Cursor : Cursor_Type) return Element_Type is <>;
       with procedure Advance
         (Cursor : in out Cursor_Type) is <>;
       with procedure Clear
         (Container : in out Container_Type) is null;

    procedure Generic_Copy7
      (Source : Cursor_Type;
       Target : in out Container_Type);

.. code:: ada

    with System;  use type System.Address;

    procedure Generic_Copy7
      (Source : Cursor_Type;
       Target : in out Container_Type)
    is
       C : Cursor_Type := Source;

    begin
       Clear (Target);
       while Has_Element (C) loop
          Insert (Target, Element (C));
          Advance (C);
       end loop;
    end Generic_Copy7;

We can now copy from an integer stack to an array like this:

.. code:: ada

    with Integer_Stacks_Bounded;  use Integer_Stacks_Bounded;
    with Generic_Copy7;

    procedure Test_Copy7 (S : in out Stack) is

       type Integer_Array is array (Positive range <>) of Integer;
       A : Integer_Array (1 .. S.Length);

    begin

       Copy_From_Stack_To_Array : declare
          I : Positive := A'First;

          procedure Insert
            (Container : in out Integer_Array;
             Item      : Integer)
          is
          begin
             Container (I) := Item;
             I := I + 1;
          end Insert;

          procedure Copy7 is
            new Generic_Copy7
              (Integer_Array,
               Cursor,
               Integer,
               Advance => Next);

       begin
          Copy7 (Source => S.Top_Cursor, Target => A);
       end Copy_From_Stack_To_Array;

       Copy_From_Array_To_Stack : declare
          function Has_Element (I : Natural) return Boolean is
          begin
             return I > 0;
          end Has_Element;

          function Element (I : Natural) return Integer is
          begin
             return A (I);
          end Element;

          procedure Advance (I : in out Natural) is
          begin
             I := I - 1;
          end Advance;

          procedure Copy7 is
            new Generic_Copy7
              (Stack,
               Natural,
               Integer,
               Insert => Push,
               Clear  => Clear);

       begin
          Copy7 (Source => A'Last, Target => S);
       end Copy_From_Array_To_Stack;

    end Test_Copy7;

The *target container* is just an array. The only special thing we need to
do is synthesize an insertion operation, to pass as the generic actual. We
can also use the same algorithm to go the other way, from an array to a
stack, as implemented in :ada:`Copy_From_Array_To_Stack` above.

Now the source container is an array, and the *cursor* is just the array
index (an integer subtype). We have the familiar problem of ensuring that
the target stack is populated in the correct order. As before, we simply
iterate over the array in reverse, by passing the index :ada:`S'Last` as
the initial cursor value, and then *advancing* the cursor by
decrementing the index value.

The algorithm can be generalized further still. In this final version, we
eliminate the generic formal element type. That means we'll need to modify
the generic formal :ada:`Insert` operation, by passing the source cursor
as a parameter instead of the source element. The declaration of the
generic algorithm now becomes:

.. code:: ada

    generic
       type Container_Type (<>) is limited private;
       type Cursor_Type (<>) is private;

       with procedure Insert
         (Target : in out Container_Type;
          Source : Cursor_Type) is <>;
       with function Has_Element
         (Cursor : Cursor_Type) return Boolean is <>;
       with procedure Advance
         (Cursor : in out Cursor_Type) is <>;
       with procedure Clear
         (Container : in out Container_Type) is null;

    procedure Generic_Copy8
      (Source : Cursor_Type;
       Target : in out Container_Type);

.. code:: ada

    with System;  use type System.Address;

    procedure Generic_Copy8
      (Source : Cursor_Type;
       Target : in out Container_Type)
    is
       C : Cursor_Type := Source;

    begin
       Clear (Target);
       while Has_Element (C) loop
          Insert (Target, C);
          Advance (C);
       end loop;
    end Generic_Copy8;

The algorithm is now agnostic about the mapping from cursor to element
(since it doesn't even know about elements), which is more flexible, since
it allows the client to choose whatever mechanism is the most efficient.
To use the new algorithm, all we need to do is make a slight change to the
generic actual :ada:`Insert` procedure, as follows:

.. code:: ada

    with Integer_Stacks_Bounded;  use Integer_Stacks_Bounded;
    with Generic_Copy8;

    procedure Test_Copy8 (S : in out Stack) is

       type Integer_Array is array (Positive range <>) of Integer;
       A : Integer_Array (1 .. S.Length);

    begin

       Copy_From_Stack_To_Array : declare
          I : Positive := A'First;

          procedure Insert
            (Target : in out Integer_Array;
             Source : Cursor)
          is
          begin
             Target (I) := Element (Source);
             I := I + 1;
          end Insert;

          procedure Copy8 is
            new Generic_Copy8
              (Integer_Array,
               Cursor,
               Advance => Next);

       begin
          Copy8 (Source => S.Top_Cursor, Target => A);
       end Copy_From_Stack_To_Array;

       Copy_From_Array_To_Stack : declare
          procedure Insert
            (Target : in out Stack;
             Source : Natural)
          is
          begin
             Target.Push (Item => A (Source));
          end Insert;

          function Has_Element (I : Natural) return Boolean is
          begin
             return I > 0;
          end Has_Element;

          procedure Advance (I : in out Natural) is
          begin
             I := I - 1;
          end Advance;

          procedure Copy8 is
            new Generic_Copy8
              (Stack,
               Natural,
               Clear => Clear);

       begin
          Copy8 (Source => A'Last, Target => S);
       end Copy_From_Array_To_Stack;

    end Test_Copy8;

The basic idea is that a generic algorithm can be used over a wide range
of containers (including array types). A cursor provides access to the
elements in a container, but as we've seen, once you have a cursor then
the container itself sort of disappears. From the point of view of a
generic algorithm, a container is merely a sequence of items.
