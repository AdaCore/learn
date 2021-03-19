:orphan:

Design Patterns
===============

.. include:: ../../global.txt

.. role:: python(code)
   :language: python

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

.. code:: ada no_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Scope_Locks

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

This is the package specification of the UML metamodel example. The code
below also includes the specification of a visitor class, which will be
overridden by the user code, for instance, to provide a code generator, a
model checker, and so on:

.. code:: ada no_button project=Courses.Advanced_Ada.Design_Patterns.Visitor

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

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Visitor

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

.. code:: ada run_button project=Courses.Advanced_Ada.Design_Patterns.Visitor

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

    with UML;                use UML;
    with Visitors;           use Visitors;
    with Code_Generator_Pkg; use Code_Generator_Pkg;

    procedure Main is

       Tmp1 : aliased NamedElement;
       Tmp2 : aliased CClass;
       Tmp3 : aliased PPackage;

       All_Model_Elements : array (Positive range <>) of
         access NamedElement'Class := (Tmp1'Access,
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

Overridable Class Attributes
----------------------------

Most object-oriented programming languages provide a facility for
declaring variables that are shared by all objects of a given class. In
C++, these are called *static members* (and use the *static* keyword), and
similarly Python has the notion of *class attributes*.

Let's consider an example where this is useful. For instance, let's say we
want to define the notion of a block of text that is generated by
expanding a template (perhaps after we replace some parameters in that
template, as can be done with
`Ada Web Server <http://docs.adacore.com/aws-docs/aws/>`_'s templates
parser, for instance). Once we have computed those parameters, we might
want to generate multiple outputs (for instance HTML and CSV). Only the
template needs to change, not the computation of the parameters.

.. admonition:: In other languages

    Typically, such as in Python, the template could be implemented as a
    class  attribute of the :python:`Text_Block` class. We can then create
    templates that need the same information but have a different output
    simply by extending that class:

    .. code-block:: python

        class Text_Block(object):
            template = "filename.txt"
            def render (self):
                # ... compute some parameters
                # Then do template expansion
                print "processing %s" % self.__class__.template

        class Html_Block(Text_Block):
            template = "filename.html"

In this example, we chose to use a class attribute rather than the usual
instance attribute. This example comes from the implementation of
`GnatTracker <https://www.adacore.com/gt3userguide/>`_: in the web server,
we create a new instance of :ada:`Text_Block` for every request we have to
serve. For this, we use a registry that maps the URL to the class we need
to create. It is thus easier to create a new instance without specifying
the template name as a parameter, which would be required if the template
name was stored in the instance. Another reason (though not really
applicable here) is to save memory, which would be important in cases
where there are thousands of instances of the class.

.. admonition:: In other languages

    C++, like Ada, does not provide a way to override a static class
    member, so it would use a similar solution as described below.

Since Ada has no notion of an overridable class attribute, we'll model it
using a subprogram instead (the only way to get dispatching in Ada). The
important point here is that we want to be able to override the template
name in child classes, so we cannot use a simple constant in the package
spec or body.

.. code:: ada no_button project=Courses.Advanced_Ada.Design_Patterns.Text_Blocks

    package Text_Blocks is

       type Text_Block is tagged null record;

       function Template (Self : Text_Block) return String is
         ("filename.txt");

       function Render (Self : Text_Block) return String;

    end Text_Blocks;

Note that :ada:`Template` is a function that returns a constant, so we can
declare that directly in the spec as an expression function, and remove
the body altogether. This is a light syntax, and close to how one would do
it in Python, for example |mdash| except we use a function instead of a
variable to represent a class member.

Also note that the parameter :ada:`Self` of :ada:`Template` is only used
for dispatching, so that children of :ada:`Text_Block` can override this
function.

This is the package body with the implementation of :ada:`Render`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Design_Patterns.Text_Blocks

    package body Text_Blocks is

       function Render (Self : Text_Block) return String is
          T : constant String := Text_Block'Class (Self).Template;
       begin
          --  ..  prepare the parameters for template expansion
          --  ..  substitute in the template and return it
          return "processing " & T;
       end Render;

    end Text_Blocks;

A child of :ada:`Text_Block` would override :ada:`Template` using the same
notation:

.. code:: ada no_button project=Courses.Advanced_Ada.Design_Patterns.Text_Blocks

    package Text_Blocks.Html is

       type Html_Block is new Text_Block with null record;

       overriding function Template (Self : Html_Block) return String is
         ("filename.html");

    end Text_Blocks.Html;

This design pattern is the Ada equivalent of a Python class member.
Compared to Python, however, this approach is in fact more powerful,
because some of the children could provide a more complex body for
:ada:`Template`, so we are not limited to using the value of a simple
variable (as in Python). In fact, we can do this in the spec itself by
using a conditional expression:

.. code:: ada no_button project=Courses.Advanced_Ada.Design_Patterns.Text_Blocks

    package Text_Blocks.Selectable is

       type Selectable_Block is new Text_Block with record
          Use_Html : Boolean := True;
       end record;

       overriding function Template (Self : Selectable_Block) return String is
         (if Self.Use_Html then "filename.html" else "file2.json");

    end Text_Blocks.Selectable;

This is a test application that makes use of the packages above:

.. code:: ada run_button project=Courses.Advanced_Ada.Design_Patterns.Text_Blocks

    with Ada.Text_IO;            use Ada.Text_IO;
    with Text_Blocks;            use Text_Blocks;
    with Text_Blocks.Selectable; use Text_Blocks.Selectable;

    procedure Test_Blocks is

       B1 : Text_Block;
       B2 : Selectable_Block;

    begin

       Put_Line ("B1 : " & B1.Render);
       Put_Line ("B2 : " & B2.Render);

    end Test_Blocks;
