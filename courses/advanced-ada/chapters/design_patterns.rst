:orphan:

:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Design Patterns
===============

.. include:: ../../global.txt

Factory Functions
-----------------

.. sectionauthor:: Matthew Heaney

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

Scope Locks
-----------

Like the classical *monitor* concept on which they are based, protected
types provide mutually exclusive access to internal variables. Clients can
only access these variables indirectly, by means of a procedural
interface. This interface is very robust because mutually exclusive access
is provided automatically: users cannot forget to acquire the underlying
(logical) lock and cannot forget to release it, including when exceptions
occur. As a result, encapsulating actions within protected operations is
highly recommended.

However, applying a protected type and protected operations may not always
be feasible. For example, consider an existing sequential program that
makes calls to procedures and functions provided by a package. Inside the
package are variables that are manipulated by the procedures and
functions.

If more than one task is now going to be calling these subprograms, the
package-level variables will be subject to race conditions because they
are (indirectly) shared among the calling tasks. Moving the procedures and
functions into a protected object would provide the required mutual
exclusion, but it would require changes to both the package and the
callers. Additionally, the existing procedures and functions may perform
potentially blocking operations, such as I/O, that are prohibited from
within protected operations.

In such a case, the programmer must fall back to manually acquiring and
releasing an explicit lock. The result is essentially that of using
semaphores, a low-level and clearly much less robust approach. For
example, to ensure serial execution of the exported operations, one could
declare a lock at the package level, as shown below, and have each
operation acquire and release it:

.. code-block:: ada

    package body P is

       Mutex : Mutual_Exclusion;

       State : Integer := 0;

       procedure Operation_1 is
       begin
          Mutex.Seize;
          State := State + 1;  -- for example...
          Put_Line ("State is now" & State'Img);
          Mutex.Release;
       exception
          when others =>
             Mutex.Release;
             raise;
       end Operation_1;

       procedure Operation_2 is
       begin
          Mutex.Seize;
          State := State - 1;  -- for example...
          Put_Line ("State is now" & State'Img);
          Mutex.Release;
       exception
          when others =>
             Mutex.Release;
             raise;
       end Operation_2;

    end P;

The type :ada:`Mutual_Exclusion` is actually a subtype of a binary
semaphore abstraction available to users via the :ada:`GNAT.Semaphores`
package:

.. code-block:: ada

  subtype Mutual_Exclusion is Binary_Semaphore
    (Initially_Available => True,
     Ceiling             => Default_Ceiling);

See package :ada:`GNAT.Semaphores` for the details. You can assume it is
a protected type with classic semaphore semantics. We define a subtype to
ensure that all such objects are initially available, as required when
providing mutual exclusion.

Although we cannot eliminate the need for this lock, we can make the code
more robust by automatically acquiring and releasing it using an object of
a controlled type. Initialization will automatically acquire the lock and
finalization will automatically release it, including both when an
exception is raised and when the task is aborted.

.. admonition:: In other languages

    C++ programmers may be familiar with this technique under the name
    "Resource Acquisition Is Initialization" (RAII).

The idea is to define a limited controlled type that references a shared
lock using a discriminant. Objects of the type are then declared within
procedures and functions with a discriminant value designating the shared
lock declared within the package. Such a type is called a *scope lock*
because the elaboration of the enclosing declarative region |mdash| the
scope |mdash| is sufficient to acquire the referenced lock. The
subprogram's sequence of statements will not execute until the lock is
acquired, no matter how long that takes. When the procedure or function is
done, for any reason, finalization will release the lock. The resulting
user code is thus almost unchanged from the original sequential code:

.. code-block:: ada

    package body P is

       Mutex : aliased Mutual_Exclusion;

       State : Integer := 0;

       procedure Operation_1 is
          S : Scope_Lock (Mutex'Access);
       begin
          State := State + 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_1;

       procedure Operation_2 is
          S : Scope_Lock (Mutex'Access);
       begin
          State := State - 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_2;

    end P;

To define the :ada:`Scope_Lock` type, we declare it with a discriminant
designating a :ada:`Mutual_Exclusion` object:

.. code-block:: ada

      type Scope_Lock (Lock : access Mutual_Exclusion) is
         tagged limited private;

In the private part the type is fully declared as a controlled type
derived from :ada:`Ada.Finalization.Limited_Controlled`, as shown below.
We hide the fact that the type will be controlled because
:ada:`Initialize` and :ada:`Finalize` are never intended to be called
manually.

.. code-block:: ada

       type Scope_Lock (Lock : access Mutual_Exclusion) is
          new Ada.Finalization.Limited_Controlled with null record;

       overriding procedure Initialize (This : in out Scope_Lock);
       overriding procedure Finalize   (This : in out Scope_Lock);

Each overridden procedure simply references the semaphore object
designated by the formal parameter's discriminant:

.. code-block:: ada

      procedure Initialize (This : in out Scope_Lock) is
      begin
        This.Lock.Seize;
      end Initialize;

      procedure Finalize (This : in out Scope_Lock) is
      begin
        This.Lock.Release;
      end Finalize;

So as you can see, by combining controlled types with protected types one
can make simple work of providing mutually exclusive access when a
protected object is not an option. By taking advantage of the automatic
calls to :ada:`Initialize` and :ada:`Finalize`, the resulting user code is
much more robust and requires very little change.
