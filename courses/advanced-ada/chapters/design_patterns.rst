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
functions. For example:

.. code:: ada

    package P is

       procedure Operation_1;

       procedure Operation_2;

    end P;

    with Ada.Text_IO; use Ada.Text_IO;

    package body P is

       State : Integer := 0;

       procedure Operation_1 is
       begin
          State := State + 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_1;

       procedure Operation_2 is
       begin
          State := State - 1;  -- for example...
          Put_Line ("State is now" & State'Img);
       end Operation_2;

    end P;

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
declare a lock at the package level, and have each operation acquire and
release it. The lock can be implemented using a binary semaphore:

.. code:: ada

    package Semaphores is

       --  Simplified implementation
       --  See GNAT.Semaphores for complete package

       protected type Binary_Semaphore
         (Initially_Available : Boolean)
       is
          entry Seize;

          procedure Release;

       private
          Available : Boolean := Initially_Available;
       end Binary_Semaphore;

    end Semaphores;

    package body Semaphores is

       protected body Binary_Semaphore is

          entry Seize when Available is
          begin
             Available := False;
          end Seize;

          procedure Release is
          begin
             Available := True;
          end Release;

       end Binary_Semaphore;

    end Semaphores;

This semaphore is a simplified implementation based on the binary
semaphore that you can find in the package :ada:`GNAT.Semaphores`. You can
assume it is a protected type with classic semaphore semantics.

This is the updated package :ada:`P` that makes use of semaphores:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    with Semaphores;  use Semaphores;

    package body P is

       subtype Mutual_Exclusion is Binary_Semaphore
         (Initially_Available => True);

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
semaphore abstraction from the :ada:`Semaphores` package. We define a
subtype to ensure that all such objects are initially available, as
required when providing mutual exclusion.

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
scope |mdash| is sufficient to acquire the referenced lock.

To define the :ada:`Scope_Lock` type, we declare it with a discriminant
designating a :ada:`Mutual_Exclusion` object:

.. code:: ada

    with Semaphores;  use Semaphores;

    with Ada.Finalization;

    package Locks is

       subtype Mutual_Exclusion is Binary_Semaphore
         (Initially_Available => True);

       type Scope_Lock (Lock : access Mutual_Exclusion) is
         tagged limited private;

    private

       type Scope_Lock (Lock : access Mutual_Exclusion) is
         new Ada.Finalization.Limited_Controlled with null record;

       overriding procedure Initialize (This : in out Scope_Lock);
       overriding procedure Finalize   (This : in out Scope_Lock);

    end Locks;

In the private part the type is fully declared as a controlled type
derived from :ada:`Ada.Finalization.Limited_Controlled`, as shown below.
We hide the fact that the type will be controlled because
:ada:`Initialize` and :ada:`Finalize` are never intended to be called
manually.

Each overridden procedure simply references the semaphore object
designated by the formal parameter's discriminant:

.. code:: ada

    with Ada.Finalization;

    package body Locks is

       procedure Initialize (This : in out Scope_Lock) is
       begin
          This.Lock.Seize;
       end Initialize;

       procedure Finalize (This : in out Scope_Lock) is
       begin
          This.Lock.Release;
       end Finalize;

    end Locks;

The subprogram's sequence of statements will not execute until the lock is
acquired, no matter how long that takes. When the procedure or function is
done, for any reason, finalization will release the lock. The resulting
user code is thus almost unchanged from the original sequential code:

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    with Locks; use Locks;

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

So as you can see, by combining controlled types with protected types one
can make simple work of providing mutually exclusive access when a
protected object is not an option. By taking advantage of the automatic
calls to :ada:`Initialize` and :ada:`Finalize`, the resulting user code is
much more robust and requires very little change.

:code-config:`reset_accumulator=True`

Visitor
-------

Imagine that you have a UML model and you want to generate code from it.
A convenient approach is to have a *code generator* object, which has a
set of subprograms to handle each kind of UML element (one that generates
code for a class, one that generates code for an operation, etc.).

One way to implement this is by using a big series of *if* statements of
the form :ada:`if Obj in CClass'Class then`, which is rather inelegant and
inefficient.

Another approach is to use discriminated types. A *case* statement on the
discriminant is then efficient, and Ada will check that all discriminant
values are covered. The problem is that then you would need to use case
statements for all clients of the types in your application. Here, we
prefer to use tagged types, to take advantage of Ada's object-oriented
programming capabilities, so the *case* statement cannot be used.

Let's consider a specific example. Again, taking the UML example, assume
we have the following types. These are only very roughly similar to the
actual UML metamodel, but will be sufficient for our purposes. In practice,
these types would be automatically generated from the description of the
UML metamodel.

.. code:: ada

    limited with Visitors;

    package UML is

       -------------------
       --  NamedElement --
       -------------------

       type NamedElement is tagged null record;

       procedure Visit
          (Self        : in out NamedElement;
           The_Visitor : access Visitors.Visitor'Class);

       -------------
       --  CClass --
       -------------

       type CClass is new NamedElement with null record;

       overriding procedure Visit
          (Self        : in out CClass;
           The_Visitor : access Visitors.Visitor'Class);

       ---------------
       --  PPackage --
       ---------------

       type PPackage is new NamedElement with null record;

       overriding procedure Visit
          (Self        : in out PPackage;
           The_Visitor : access Visitors.Visitor'Class);
    end UML;

In addition, a visitor class is declared, which will be overridden by the
user code, for instance, to provide a code generator, a model checker, and
so on:

.. code:: ada

    with UML;  use UML;

    package Visitors is

       type Visitor is abstract tagged null record;

       procedure Visit_NamedElement
         (Self : in out Visitor;
          Obj  : in out NamedElement'Class) is null;
       --  No parent type, do nothing

       procedure Visit_CClass
         (Self : in out Visitor;
          Obj  : in out CClass'Class);

       procedure Visit_PPackage
         (Self : in out Visitor;
          Obj  : in out PPackage'Class);

    end Visitors;

    package body Visitors is

       procedure Visit_CClass
         (Self : in out Visitor;
          Obj  : in out CClass'Class) is
       begin
          --  In UML, a "Class" inherits from a "NamedElement".
          --  Concrete implementations of the visitor might want to work at the
          --  "NamedElement" level (so that their code applies to both a Class
          --  and a Package, for instance), rather than duplicate the work for each
          --  child of NamedElement. The default implementation here is to call the
          --  parent type's operation.

          Self.Visit_NamedElement (Obj);
       end Visit_CClass;

       procedure Visit_PPackage
         (Self : in out Visitor;
          Obj  : in out PPackage'Class) is
       begin
          Self.Visit_NamedElement (Obj);
       end Visit_PPackage;

    end Visitors;

We then need to add one primitive :ada:`Visit` operation to each of the
types created from the UML metamodel:

.. code:: ada

    with Visitors; use Visitors;

    package body UML is

       procedure Visit
          (Self        : in out NamedElement;
           The_Visitor : access Visitor'Class) is
       begin
          --  First dispatching was on "Self" (done by the compiler).
          --  Second dispatching is simulated here by calling the right
          --  primitive operation of V.

          The_Visitor.Visit_NamedElement (Self);
       end Visit;

       overriding procedure Visit
          (Self        : in out CClass;
           The_Visitor : access Visitor'Class) is
       begin
          The_Visitor.Visit_CClass (Self);
       end Visit;

       overriding procedure Visit
          (Self        : in out PPackage;
           The_Visitor : access Visitor'Class) is
       begin
          The_Visitor.Visit_PPackage (Self);
       end Visit;

    end UML;

All of the code described above is completely systematic, and as such
could and should be generated automatically as much as possible. The
:ada:`Visit` primitive operations should never be overridden in user code
in the usual case. On the other hand, the :ada:`Visit_...` primitives of
the visitor itself should be overridden when it makes sense. The default
implementation is provided just so the user has the choice at which level
to do the overriding.

Now let's see what a code generator would look like. We'll assume that we
are only interested, initially, in doing code generation for classes.
Other types of elements (such as packages) will call the default
implementation for their visitor (:ada:`Visit_PPackage`, for instance),
which then calls the visitor for its parent (:ada:`Visit_NamedElement`)
and so on, until we end up calling a :ada:`Visit` operation with a null
body. So nothing happens for those, and we don't need to deal with them
explicitly.

The code would be something like the following:

.. code:: ada

    with UML;      use UML;
    with Visitors; use Visitors;

    package Code_Generator_Pkg is

       type Code_Generator is new Visitor with null record;

       overriding procedure Visit_CClass
          (Self : in out Code_Generator; Obj : in out CClass'Class);

    end Code_Generator_Pkg;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Code_Generator_Pkg is

       overriding procedure Visit_CClass
          (Self : in out Code_Generator; Obj : in out CClass'Class) is
       begin
          Put_Line ("Visiting CClass");
       end Visit_CClass;

    end Code_Generator_Pkg;

.. code:: ada run_button

    with UML;                use UML;
    with Visitors;           use Visitors;
    with Code_Generator_Pkg; use Code_Generator_Pkg;

    procedure Main is

       Tmp1 : aliased NamedElement;
       Tmp2 : aliased CClass;
       Tmp3 : aliased PPackage;

       All_Model_Elements : array (Positive range <>) of
         access NamedElement'Class :=
           (Tmp1'Access,
            Tmp2'Access,
            Tmp3'Access);

       Gen  : aliased Code_Generator;

    begin

       for Element of All_Model_Elements loop  --  Pseudo code
          Element.Visit (Gen'Access);          --  Double dispatching
       end loop;

    end Main;

If we wanted to do model checking, we would create a type
:ada:`Model_Checker`, derived from :ada:`Visitor`, that overrides some of
the :ada:`Visit_...` operations. The body of :ada:`Main` would not change,
except for the type of :ada:`Gen`.

When using this in practice, there are a few issues to resolve. For
instance, the UML types need access to the :ada:`Visitor` type (because it
appears as a parameter in their operations). But a visitor also needs to
see the UML types for the same reason. One possibility is to put all the
types in the same package. Another is to use :ada:`limited with` to give
visibility on access types, and then pass an access to :ada:`Visitor'Class`
as a parameter to :ada:`Visit`, as we've implemented above.

:code-config:`reset_accumulator=True`
