Controlled Types
================

.. include:: ../../../global.txt

.. _Adv_Ada_Controlled_Types_Overview:

Overview
--------

In this section, we introduce the concept of controlled types. We start with a
review of lifetime of objects and discuss how controlled types allow us to
control the initialization, post-copy (e.g. assignment) adjustment and
finalization of objects.

.. admonition:: Relevant topics

    - :arm22:`Assignment and Finalization <7-6>`


Lifetime of objects
~~~~~~~~~~~~~~~~~~~

We already talked about the
:wikipedia:`lifetime of objects <Variable_(computer_science)#Scope_and_extent>`
previously in the context of
:ref:`access types <Adv_Ada_Access_Types_Lifetime_of_Objects>`.
Again, we assume you understand the concept. In any case, let's quickly review
the typical lifetime of an object:

.. uml::
   :align: center
   :width: 100pt

    @startuml
    start
    :Create object A;
    :Use object A;
    :Finalize object A;
    stop
    @enduml

In simple terms, an object :ada:`A` is first created before we can make use of
it. When object :ada:`A` is about to get out of scope, it is finalized.
Note that finalization might not entail any actual code execution |mdash| but
it often does.

Let's analyze the lifetime of object :ada:`A` in a procedure :ada:`P`:

.. code-block:: ada

    procedure P is
       A : T;
    begin
       P2 (A);
    end P;

We could visualize the lifetime as follows:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        group block's declarative part
            Processing -> A ** : << create >>
        end
        group block's handled sequence of statements
            Processing -> A : << use >>
        end
        group block's end part
            Processing -> A !! : << finalize >>
        end
    @enduml

In other words, object :ada:`A` is created in the declarative part of :ada:`P`
and then it's used in :ada:`P`\'s sequence of statements. Finally, :ada:`A` is
finalized when :ada:`P` ends.


Initialization of objects
~~~~~~~~~~~~~~~~~~~~~~~~~

Typically, right after an object :ada:`A` is created, it is still uninitialized.
Therefore, we have to explicitly initialize it with a meaningful initial value
|mdash| or with the value returned by a function call, for example. Similarly,
when an object :ada:`A` is about to get out of scope, it is going to be
finalized (i.e. destroyed) and its contents are then lost forever.

As we know, for some standard Ada types, objects are initialized by default.
For example, objects of access types are initialized by default to :ada:`null`.
Likewise, we can declare
:ref:`types with default initial value <Adv_Ada_Default_Initial_Values>`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Overview.Default_Initialization

    pragma Ada_2022;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Int is new Integer
         with Default_Value => 42;

       I  : Int;
       AI : access Int;
    begin
       Put_Line ("I : "
                 & I'Image);
       Put_Line ("AI : "
                 & AI'Image);
    end Main;

In this case, we can visualize the lifetime of those objects as follows:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        group block's declarative part
            Processing -> A ** : << create >>
            Processing -> A : << initialize with \ndefault value >>
        end
        group block's handled sequence of statements
            Processing -> A : << use >>
        end
        group block's end part
            Processing -> A !! : << finalize >>
        end
    @enduml

Even though these default initialization methods provide some control over the
objects, they might not be enough in certain situations.
Also, we don't have any means to perform
useful operations right before an object gets out of scope.

.. admonition:: For further reading...

    In general, record types have a very good default initialization
    capability. They're the most common completion for private types, so the
    facility is often used. In this sense, default initialization is the first
    choice, as it's guaranteed and requires nothing of the client. In addition,
    it's cheap at run-time compared to controlled types.


.. _Adv_Ada_Controlled_Types_Overview_Controlled_Objects:

Controlled objects
~~~~~~~~~~~~~~~~~~

Controlled objects allow us to better control the initialization and
finalization of an object. For any controlled object :ada:`A`, an
:ada:`Initialize (A)` procedure is called right *after* the object is created,
and a :ada:`Finalize (A)` procedure is called right *before* the object is
actually finalized.

We can visualize the lifetime of controlled objects as follows:

.. uml::
   :align: center
   :width: 200pt

    @startuml
        actor Processing
        participant "object A" as A

        Processing -> A ** : << create >>
        Processing -> A : Initialize (A)
        Processing -> A : << use >>
        Processing -> A : Finalize (A)
        Processing -> A !! : << finalize >>
    @enduml

In the context of a block statement, the lifetime becomes:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        group block's declarative part
            Processing -> A ** : << create >>
            Processing -> A : Initialize (A)
        end
        group block's handled sequence of statements
            Processing -> A : << use >>
        end
        group block's end part
            Processing -> A : Finalize (A)
            Processing -> A !! : << finalize >>
        end
    @enduml

.. _Adv_Ada_Controlled_Types_Overview_Simple_Example:

Let's look at a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Overview.Simple_Example

    with Ada.Finalization;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize...");
       end Initialize;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize...");
       end Finalize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A : T;
       --
       --  This declaration roughly
       --  corresponds to:
       --
       --     A : T;
       --  begin
       --     Initialize (A);
       --
    begin
       Dummy (A);

       --  When A is about to get out of
       --  scope:
       --
       --  Finalize (A);
       --
    end Show_Controlled_Types;

When we run this application, we see the user messages indicating the calls to
:ada:`Initialize` and :ada:`Finalize`.

.. admonition:: For further reading...

   Note that if a controlled object isn't used in the application, the compiler
   might optimize it out. In this case, procedures :ada:`Initialize` and
   :ada:`Finalize` won't be called for this object, as it doesn't actually
   exist. You can see this effect by replacing the call to :ada:`Dummy (A)` in
   the :ada:`Show_Controlled_Types` procedure by a null statement (:ada:`null`).


.. _Adv_Ada_Controlled_Types_Overview_Adjustment:

Adjustment of controlled objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An assignment is a full bit-wise copy of the entire right-hand side to the
entire left-hand side. When copying controlled objects, however, we might
need to adjust the target object. This is made possible by overriding the
:ada:`Adjust` procedure, which is called
right after the copy to an object has been performed. (As we'll see later on,
:ref:`limited controlled types <Adv_Ada_Limited_Controlled_Types_Overview>`
do not offer an :ada:`Adjust` procedure.)

The :wikipedia:`deep copy <Object_copying#Deep_copy>` of objects is a typical
example where adjustments are necessary. When we assign an object :ada:`B` to
an object :ada:`A`, we're essentially doing a shallow copy. If we have
references to other objects in the source object :ada:`B`, those references
will be copied as well, so both target :ada:`A` and source :ada:`B` will be
referring to the same objects. When performing a deep copy, however, we want
the information from the dereferenced objects to be copied, not the references
themselves. Therefore, we have to first allocate new objects for the target
object :ada:`A` and copy the information from the original references |mdash|
the ones we copied from the source object :ada:`B` |mdash| to the new objects.
This kind of processing can be performed in the :ada:`Adjust` procedure.

As an example, let's extend the previous code example and override the
:ada:`Adjust` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Overview.Simple_Example_2

    with Ada.Finalization;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Adjust (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize...");
       end Initialize;

       procedure Adjust (E : in out T) is
       begin
          Put_Line ("Adjust...");
       end Adjust;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize...");
       end Finalize;

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A, B : T;
    begin
       Put_Line ("A := B");
       A := B;

       Dummy (A);
       Dummy (B);
    end Show_Controlled_Types;

When running this application, we see that the :ada:`Adjust` procedure is called
for object :ada:`A` |mdash| right after :ada:`B` is copied to :ada:`A` as part
of the :ada:`A := B` assignment. We discuss more
about this procedure :ref:`later on <Adv_Ada_Controlled_Types_Assignment>`.


.. _Adv_Ada_Limited_Controlled_Types_Overview:

Limited controlled types
~~~~~~~~~~~~~~~~~~~~~~~~

Ada offers controlled types in two flavors: nonlimited controlled types |mdash|
such as the ones we've seen so far |mdash| and limited controlled types. Both
types are declared in the :ada:`Ada.Finalization` package.

The only difference between these types is that limited controlled types don't
have an :ada:`Adjust` procedure that could be overridden, as limited types
:ref:`do not permit direct copies of objects to be made via assignments <Adv_Ada_Limited_Types_Assignments>`.
(Obviously, both controlled and limited controlled types provide
:ada:`Initialize` and :ada:`Finalize` procedures.)

The following table summarizes the information:

+-------------+---------------------------+-------------+----------+-----------+
| Type        | Name                      | Initialize  | Finalize | Adjust    |
+=============+===========================+=============+==========+===========+
| Nonlimited  | :ada:`Controlled`         | Yes         | Yes      | Yes       |
| Controlled  |                           |             |          |           |
+-------------+---------------------------+-------------+----------+-----------+
| Limited     | :ada:`Limited_Controlled` | Yes         | Yes      | Not       |
| controlled  |                           |             |          | available |
+-------------+---------------------------+-------------+----------+-----------+


Simple Example with ID
~~~~~~~~~~~~~~~~~~~~~~

Although the previous code examples indicated that :ada:`Initialize`,
:ada:`Finalize` and :ada:`Adjust` are called as we expect for controlled
objects, they didn't show us exactly how those objects are actually handled. In
this section, we discuss this by analyzing a code example that assigns a unique
ID to each controlled object.

Let's start with the complete code example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Overview.Simple_Example_With_Id

    with Ada.Finalization;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Show (E    : T;
                       Name : String);

    private

       protected Id_Gen is
          procedure New_Id (Id_Out : out Positive);
       private
          Id : Natural := 0;
       end Id_Gen;

       type T is new
         Ada.Finalization.Controlled with
       record
          Id : Positive;
       end record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Adjust (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       protected body Id_Gen is

          procedure New_Id (Id_Out : out Positive) is
          begin
             Id := Id + 1;
             Id_Out := Id;
          end New_Id;

       end Id_Gen;

       procedure Initialize (E : in out T) is
       begin
          Id_Gen.New_Id (E.Id);
          Put_Line ("Initialize: ID => "
                    & E.Id'Image);
       end Initialize;

       procedure Adjust (E : in out T) is
          Prev_Id : constant Positive := E.Id;
       begin
          Id_Gen.New_Id (E.Id);
          Put_Line ("Adjust:     ID => "
                    & E.Id'Image);
          Put_Line ("    (Previous ID => "
                    & Prev_Id'Image
                    & ")");
       end Adjust;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize:   ID => "
                    & E.Id'Image);
       end Finalize;

       procedure Show (E    : T;
                       Name : String) is
       begin
          Put_Line ("Obj. " & Name
                    & ": ID => "
                    & E.Id'Image);
       end Show;

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A, B : T;
       --
       --  Declaration corresponds to:
       --
       --  declare
       --     A, B : T;
       --  begin
       --     Initialize (A);
       --     Initialize (B);
       --  end;
    begin
       Put_Line ("--------");
       Show (A, "A");
       Show (B, "B");

       Put_Line ("--------");
       Put_Line ("A := B;");

       A := B;
       --  Statement corresponds to:
       --
       --  Finalize (A);
       --  A := B;
       --  Adjust (A);

       Put_Line ("--------");
       Show (A, "A");
       Show (B, "B");
       Put_Line ("--------");

       --  When A and B get out of scope::
       --
       --  Finalize (A);
       --  Finalize (B);
       --
    end Show_Controlled_Types;

In contrast to the previous versions of the :ada:`Simple_Controlled_Types`
package, type :ada:`T` now has an :ada:`Id` component. Moreover, we use a
protected object :ada:`Id_Gen` that provides us with a unique ID to keep track
of each controlled object. Basically, we assign an ID to each controlled object
(right after it is created) via the call to :ada:`Initialize`. Similarly, this
ID is updated via the calls to :ada:`Adjust`. Besides, we now have a :ada:`Show`
procedure that displays the ID of a controlled object.

When running the application, we see that the calls to :ada:`Initialize`,
:ada:`Adjust` and :ada:`Finalize` happen as expected. In addition, we see the
objects' ID, which we will now analyze in order to understand how each object is
actually handled.

First, we see the two calls to :ada:`Initialize` for objects :ada:`A` and
:ada:`B`. Object :ada:`A`\'s ID is 1, and object :ada:`B`\'s ID is 2. This is
later confirmed by the calls to :ada:`Show`.

The :ada:`A := B` assignment triggers two procedure calls: a call to
:ada:`Finalize (A)` and a call to :ada:`Adjust (A)`. In fact, this assignment
can be described as follows:

#. :ada:`Finalize (A)` is called before the actual copy;

#. :ada:`B`\'s data is copied to object :ada:`A`;

#. :ada:`Adjust (A)` is called after that copy.

We can confirm this via the object
ID: the object we handle in the call to :ada:`Finalize (A)` has an ID of 1, and
the object we handle in the call to :ada:`Adjust (A)` has an ID of 2 (which
originates from the copy of :ada:`B` to :ada:`A`) and is later changed
(*adjusted*) to 3. Again, we can verify the correct IDs by looking at the output
of the calls to :ada:`Show`.

Note that the call to :ada:`Finalize (A)` (before the copy of :ada:`B`\'s
data) indicates that the previous version of object :ada:`A` is being finalized,
i.e. it's as though the original object :ada:`A` is going to be destroyed and
its contents are going to be lost. Actually, the object's contents are just
overwritten, but the call to :ada:`Finalize` allows us to make proper
adjustments to the object before the previous information is lost.

Finally, the new version of object :ada:`A` (the one whose ID is 3) and object
:ada:`B` are finalized via the calls to :ada:`Finalize (A)` and
:ada:`Finalize (B)` before the :ada:`Show_Controlled_Types` procedure ends.


.. _Adv_Ada_Controlled_Types_Initialization:

Initialization
--------------

In this section, we cover some details about the initialization of controlled
types. Most of those details are related to the initialization order. In
principle, as stated in the Ada Reference Manual, ":ada:`Initialize` and other
initialization operations are done in an arbitrary order," except in the
situations that we describe later on.

.. admonition:: Relevant topics

    - :arm22:`Assignment and Finalization <7-6>`


.. _Adv_Ada_Controlled_Types_Initialization_Subcomponents:

Subcomponents
~~~~~~~~~~~~~

We've seen before that default initialization is a way of controlling the
initialization of arbitrary types. In the case of controlled types, the default
initialization of its subcomponents always takes places before the call to
:ada:`Initialize`.

Similarly, a controlled type might have subcomponents of controlled types.
These subcomponents are initialized by a call to the :ada:`Initialize`
procedure of each of those controlled types.

We can visualize the lifetime as follows:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        Processing -> A ** : << create >>
        Processing -> A : << initialize subcomponents of object A\n(with default values or calls to Initialize) >>
        Processing -> A : Initialize (A)
        Processing -> A : << use >>
        Processing -> A : Finalize (A)
        Processing -> A !! : << finalize >>
    @enduml

.. _Adv_Ada_Controlled_Types_Initialization_Subcomponents_Code_Example:

In order to see this effect, let's start by implementing two controlled types:
:ada:`Sub_1` and :ada:`Sub_2`:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    with Ada.Finalization;

    package Subs is

       type Sub_1 is tagged private;

       type Sub_2 is tagged private;

    private

       type Sub_1 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out Sub_1);

       type Sub_2 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out Sub_2);

    end Subs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Subs is

       procedure Initialize (E : in out Sub_1) is
       begin
          Put_Line ("Initialize: Sub_1...");
       end Initialize;

       procedure Initialize (E : in out Sub_2) is
       begin
          Put_Line ("Initialize: Sub_2...");
       end Initialize;

    end Subs;

Now, let's use those controlled types as components of a type :ada:`T`. In
addition, let's declare an integer component :ada:`I` with default
initialization. This is how the complete code looks like:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    with Ada.Finalization;

    with Subs; use Subs;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       function Default_Init return Integer;

       type T is new
         Ada.Finalization.Controlled with
       record
          S1 : Sub_1;
          S2 : Sub_2;
          I  : Integer := Default_Init;
       end record;

       overriding
       procedure Initialize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       function Default_Init return Integer is
       begin
          Put_Line ("Default_Init: Integer...");
          return 42;
       end Default_Init;

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy: T...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize: T...");
       end Initialize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A : T;
    begin
       Dummy (A);
    end Show_Controlled_Types;

When we run this application, we see that the :ada:`Sub_1` and :ada:`Sub_2`
components are initialized by calls to their respective :ada:`Initialize`
procedures, and the :ada:`I` component is initialized with its default value
(via a call to the :ada:`Default_Init` function). Finally, after all
subcomponents of type :ada:`T` have been initialized, the :ada:`Initialize`
procedure is called for the type :ada:`T` itself.

This diagram shows the initialization sequence:

.. uml::
    :align: center
    :width: 400pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.S1" as Sub_1
        participant "T.S2" as Sub_2
        participant "T.I" as I

        Processing -> type_t : << initialize subcomponents >>
        activate type_t
            type_t -> Sub_1 : Initialize (Sub_1)
            type_t -> Sub_2 : Initialize (Sub_2)
            type_t -> I : Default_Init (Integer)
        deactivate type_t
        Processing -> type_t : Initialize (T)
    @enduml


.. _Adv_Ada_Controlled_Types_Initialization_Components_Access_Disciminants:

Components with access discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Record types with access discriminants are a special case. In fact, according
to the Ada Reference Manual, "if an object has a component with an access
discriminant constrained by a
:ref:`per-object expression <Adv_Ada_Per_Object_Expressions>`,
:ada:`Initialize` is applied to this component after any components that do not
have such discriminants. For an object with several components with such a
discriminant, :ada:`Initialize` is applied to them in order of their component
declarations."

.. _Adv_Ada_Controlled_Types_Initialization_Subcomponents_Access_Discriminant_Code_Example:

Let's see a code example. First, we implement another package with controlled
types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    with Ada.Finalization;

    package Selections is

       type Selection is private;

       type Selection_1 (S : access Selection) is
         tagged private;

       type Selection_2 (S : access Selection) is
         tagged private;

    private

       type Selection is null record;

       type Selection_1 (S : access Selection) is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize
         (E : in out Selection_1);

       type Selection_2 (S : access Selection) is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize
         (E : in out Selection_2);

    end Selections;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Selections is

       procedure Initialize
         (E : in out Selection_1) is
       begin
          Put_Line ("Initialize: Selection_1...");
       end Initialize;

       procedure Initialize
         (E : in out Selection_2) is
       begin
          Put_Line ("Initialize: Selection_2...");
       end Initialize;

    end Selections;

In this example, we see the declaration of the :ada:`Selection_1` and
:ada:`Selection_2` types, which are controlled types with an access
discriminant of :ada:`Selection` type. Now, let's use these types in the
declaration of the :ada:`T` type from the
:ref:`previous example <Adv_Ada_Controlled_Types_Initialization_Subcomponents_Code_Example>`
and add two new components (:ada:`Sel_1` and :ada:`Sel_2`):

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    with Ada.Finalization;

    with Subs;       use Subs;
    with Selections; use Selections;

    package Simple_Controlled_Types is

       type T (S1 : access Selection;
               S2 : access Selection) is
         tagged private;

       procedure Dummy (E : T);

    private

       function Default_Init return Integer;

       type T (S1 : access Selection;
               S2 : access Selection) is new
         Ada.Finalization.Controlled with
       record
          Sel_1 : Selection_1 (S1);
          Sel_2 : Selection_2 (S2);
          S_1   : Sub_1;
          I     : Integer := Default_Init;
       end record;

       overriding
       procedure Initialize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       function Default_Init return Integer is
       begin
          Put_Line ("Default_Init: Integer...");
          return 42;
       end Default_Init;

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy: T...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize: T...");
       end Initialize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    with Selections;
    use  Selections;

    procedure Show_Controlled_Types is
       S1, S2 : aliased Selection;
       A : T (S1'Access, S2'Access);
    begin
       Dummy (A);
    end Show_Controlled_Types;

When running this example, we see that all other subcomponents |mdash| to be
more precise, those subcomponents that require initialization |mdash| are
initialized before the :ada:`Sub_1` and :ada:`Sub_2` components are initialized
via calls to their corresponding :ada:`Initialize` procedure. Note that,
although :ada:`Sub_1` and :ada:`Sub_2` are the last components to be
initialized, they are still initialized before the call to the
:ada:`Initialize` procedure of type :ada:`T`.

This diagram shows the initialization sequence:

.. uml::
    :align: center
    :width: 500pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.Sel_1" as Selection_1
        participant "T.Sel_2" as Selection_2
        participant "T.S_1" as Sub_1
        participant "T.I" as I

        Processing -> type_t : << initialize standard subcomponents >>
        activate type_t
            type_t -> Sub_1 : Initialize (Sub_1)
            type_t -> I : Default_Init (Integer)
        deactivate type_t

        Processing -> type_t : << initialize subcomponents \nwith access discriminant / per-object expression >>
        activate type_t
            type_t -> Selection_1 : Initialize (Selection_1)
            type_t -> Selection_2 : Initialize (Selection_2)
        deactivate type_t

        Processing -> type_t : Initialize (T)
    @enduml


Task activation
~~~~~~~~~~~~~~~

Components of task types also require special treatment. According to the Ada
Reference Manual, "for an allocator, any task activations follow all calls on
:ada:`Initialize`."

As always, let's analyze an example that illustrates this. First, we implement
another package called :ada:`Workers` with a simple task type:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    package Workers is

       task type Worker is
          entry Start;
          entry Stop;
       end Worker;

    end Workers;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Workers is

       task body Worker is

          function Init return Integer is
          begin
             Put_Line ("Activating Worker task...");
             return 0;
          end Init;

          I : Integer := Init;
       begin

          accept Start do
            Put_Line ("Worker.Start accepted...");
             I := I + 1;
          end Start;

          accept Stop do
            Put_Line ("Worker.Stop accepted...");
             I := I - 1;
          end Stop;
       end Worker;

    end Workers;

Let's extend the declaration of the :ada:`T` type from the
:ref:`previous example <Adv_Ada_Controlled_Types_Initialization_Subcomponents_Access_Discriminant_Code_Example>`
and declare a new component of :ada:`Worker` type. Note that we have to change
:ada:`T` to a limited controlled type because of this new component of task
type. This is the updated code:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Initialization.Controlled_Initialization

    with Ada.Finalization;

    with Subs;       use Subs;
    with Selections; use Selections;
    with Workers;    use Workers;

    package Simple_Controlled_Types is

       type T (S : access Selection) is
         tagged limited private;

       procedure Start_Work (E : T);
       procedure Stop_Work (E : T);

    private

       function Default_Init return Integer;

       type T (S : access Selection) is new
         Ada.Finalization.Limited_Controlled with
       record
          W     : Worker;
          Sel_1 : Selection_1 (S);
          S1    : Sub_1;
          I     : Integer := Default_Init;
       end record;

       overriding
       procedure Initialize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       function Default_Init return Integer is
       begin
          Put_Line ("Default_Init: Integer...");
          return 42;
       end Default_Init;

       procedure Start_Work (E : T) is
       begin
          --  Starting Worker task:
          E.W.Start;

       end Start_Work;

       procedure Stop_Work (E : T) is
       begin
          --  Stopping Worker task:
          E.W.Stop;
       end Stop_Work;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize: T...");
       end Initialize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    with Selections; use Selections;

    procedure Show_Controlled_Types is
       type T_Access is access T;

       S : aliased Selection;
       A : constant T_Access := new T (S'Access);
    begin
       Start_Work (A.all);
       Stop_Work (A.all);
    end Show_Controlled_Types;

When we run this application, we see that the :ada:`W` component is activated
only after all other subcomponents of type :ada:`T` have been initialized.

This diagram shows the initialization sequence:

.. uml::
    :align: center
    :width: 500pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.W" as Worker
        participant "T.Sel_1" as Selection_1
        participant "T.S_1" as Sub_1
        participant "T.I" as I

        Processing -> type_t : << initialize standard subcomponents >>
        activate type_t
            type_t -> Sub_1 : Initialize (Sub_1)
            type_t -> I : Default_Init (Integer)
        deactivate type_t

        Processing -> type_t : << initialize subcomponents \nwith access discriminant / per-object expression >>
        activate type_t
            type_t -> Selection_1 : Initialize (Selection_1)
        deactivate type_t

        Processing -> type_t : << activate task components >>
        activate type_t
            type_t -> Worker : << activate Worker >>
        deactivate type_t

        Processing -> type_t : Initialize (T)
    @enduml


.. _Adv_Ada_Controlled_Types_Assignment:

Assignment
----------

We already talked about
:ref:`adjustments <Adv_Ada_Controlled_Types_Overview_Adjustment>` previously.
As we already mentioned, an actual assignment is a full bit-wise copy of the
entire right-hand side to the entire left-hand side, so the adjustment (via a
call to :ada:`Adjust`) is a way to "work around" that, when necessary. In this
section, we'll look into some details about the adjustment of controlled types.

.. admonition:: Relevant topics

    - :arm22:`Assignment and Finalization <7-6>`


Assignment using anonymous object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :arm22:`Ada Reference Manual <7-6>` mentions that an anonymous object is
created during the assignment of objects of controlled type. A simple
:ada:`A := B` operation for nonlimited controlled types can be expanded to the
following illustrative code:

.. code-block:: ada

    procedure P is
       A, B: Some_Controlled_Type;
    begin
       --
       --  A := B;
       --
       B_To_A_Assignment : declare
          Anon_Obj : Some_Controlled_Type;
       begin
          Anon_Obj := B;
          Adjust (Anon_Obj);
          Finalize (A);
          A := Anon_Obj;
          Finalize (Anon_Obj);
       end B_To_A_Assignment;
    end P;

The first assignment happens to the anonymous object :ada:`Anon_Obj`. After the
adjustment of :ada:`Anon_Obj` and the finalization of the original version of
:ada:`A`, the actual assignment to :ada:`A` can take place |mdash| and
:ada:`Anon_Obj` can be discarded after it has been properly finalized. With
this strategy, we have a chance to finalize the original version of :ada:`A`
before the assignment overwrites the object.

Of course, this expanded code isn't really efficient, and the compiler has some
freedom to improve the performance of the generated machine code. Whenever
possible, it'll typically optimize the anonymous object out and build the
object in place. (The :arm22:`Ada Reference Manual <7-6>` describes the rules
when this is possible or not.)

Also, the :ada:`A := Anon_Obj` statement in the code above doesn't necessarily
translate to an actual assignment in the generated machine code. Typically, a
compiler may treat :ada:`Anon_Obj` as the new :ada:`A` and destroy the original
version of :ada:`A` (i.e. the object that used to be :ada:`A`). In this case,
the code becomes something like this:

.. code-block:: ada

    procedure P is
       A, B: Some_Controlled_Type;
    begin
       --
       --  A := B;
       --
       B_To_A_Assignment : declare
          Anon_Obj : Some_Controlled_Type;
       begin
          Anon_Obj := B;
          Finalize (A);
          Adjust (Anon_Obj);
          declare
             A : Some_Controlled_Type renames Anon_Obj;
          begin
             --  Now, we treat Anon_Obj as the new A.
             --  Further processing continues here...

          end;
       end B_To_A_Assignment;
    end P;

In some cases, the compiler is required to build the object in place. A typical
example is when an object of controlled type is initialized by assigning an
aggregate to it:

.. code-block:: ada

    C: constant Some_Controlled_Type :=
        (Ada.Finalization.Controlled with ...);
    --  C is built in place,
    --  no anonymous object is used here.

Also, it's possible that :ada:`Adjust` and :ada:`Finalize` aren't called at
all. Consider an assignment like this: :ada:`A := A;`. In this case, since the
object on both sides is the same, the compiler is allowed to simply skip the
assignment and not do anything.

For more details about possible optimizations and compiler behavior, please
refer to the :arm22:`Ada Reference Manual <7-6>` .

In general, the advice is simply: use :ada:`Adjust` and :ada:`Finalize` solely
for their intended purposes. In other words, don't implement extraneous
side-effects into those procedures, as they might not be called at run-time.


Adjustment of subcomponents
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In principle, the order in which components are adjusted is arbitrary. However,
adjustments of subcomponents will happen before the adjustment of the component
itself. The subcomponents must be adjusted before the enclosing object because
the semantics of the adjustment of the whole might depend on the states of the
parts (the subcomponents), so those states must already be in place.

Let's revisit a
:ref:`previous code example <Adv_Ada_Controlled_Types_Initialization_Subcomponents_Code_Example>`.
First, we override the :ada:`Adjust` procedure of the :ada:`Sub_1` and
:ada:`Sub_2` types from the :ada:`Subs` package.

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Adjustment.Controlled_Initialization

    with Ada.Finalization;

    package Subs is

       type Sub_1 is tagged private;

       type Sub_2 is tagged private;

    private

       type Sub_1 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out Sub_1);

       overriding
       procedure Adjust (E : in out Sub_1);

       overriding
       procedure Finalize (E : in out Sub_1);

       type Sub_2 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out Sub_2);

       overriding
       procedure Adjust (E : in out Sub_2);

       overriding
       procedure Finalize (E : in out Sub_2);

    end Subs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Subs is

       procedure Initialize (E : in out Sub_1) is
       begin
          Put_Line ("Initialize: Sub_1...");
       end Initialize;

       procedure Adjust (E : in out Sub_1) is
       begin
          Put_Line ("Adjust: Sub_1...");
       end Adjust;

       procedure Finalize (E : in out Sub_1) is
       begin
          Put_Line ("Finalize: Sub_1...");
       end Finalize;

       procedure Initialize (E : in out Sub_2) is
       begin
          Put_Line ("Initialize: Sub_2...");
       end Initialize;

       procedure Adjust (E : in out Sub_2) is
       begin
          Put_Line ("Adjust: Sub_2...");
       end Adjust;

       procedure Finalize (E : in out Sub_2) is
       begin
          Put_Line ("Finalize: Sub_2...");
       end Finalize;

    end Subs;

Next, we override the :ada:`Adjust` procedure of the :ada:`T` type from the
:ada:`Simple_Controlled_Types` package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Adjustment.Controlled_Initialization

    with Ada.Finalization;

    with Subs; use Subs;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       function Default_Init return Integer;

       type T is new
         Ada.Finalization.Controlled with
       record
          S1 : Sub_1;
          S2 : Sub_2;
          I  : Integer := Default_Init;
       end record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Adjust (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       function Default_Init return Integer is
       begin
          Put_Line ("Default_Init: Integer...");
          return 42;
       end Default_Init;

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy: T...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize: T...");
       end Initialize;

       procedure Adjust (E : in out T) is
       begin
          Put_Line ("Adjust: T...");
       end Adjust;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize: T...");
       end Finalize;

    end Simple_Controlled_Types;

Finally, this is the main application:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Adjustment.Controlled_Initialization

    with Ada.Text_IO; use Ada.Text_IO;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A, B : T;
    begin
       Dummy (A);

       Put_Line ("----------");
       Put_Line ("A := B");
       A := B;
       Put_Line ("----------");
    end Show_Controlled_Types;

When running this code, we see that the :ada:`S1` and :ada:`S2` components are
adjusted before the adjustment of the parent type :ada:`T` takes place.

This diagram shows the adjustment sequence:

.. uml::
    :align: center
    :width: 400pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.S1" as Sub_1
        participant "T.S2" as Sub_2
        participant "T.I" as I

        Processing -> type_t : << adjust subcomponents >>
        activate type_t
            type_t -> Sub_1 : Adjust (Sub_1)
            type_t -> Sub_2 : Adjust (Sub_2)
        deactivate type_t
        Processing -> type_t : Adjust (T)
    @enduml


.. _Adv_Ada_Finalization:

Finalization
------------

We mentioned finalization |mdash| and the :ada:`Finalize` procedure |mdash| at
the
:ref:`beginning of the chapter <Adv_Ada_Controlled_Types_Overview_Controlled_Objects>`.
In this section, we discuss the topic in more detail.

.. admonition:: Relevant topics

    - :arm22:`Assignment and Finalization <7-6>`
    - :arm22:`Completion and Finalization <7-6-1>`


Normal and abnormal completion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a subprogram has just executed its last statement, normal completion of
this subprogram has been reached. At this point, finalization starts. In the
case of controlled objects, this means that the :ada:`Finalize` procedure is
called for those objects. (As we've already seen
:ref:`an example of normal completion <Adv_Ada_Controlled_Types_Overview_Simple_Example>`
at the beginning of the chapter, we won't repeat it here, as we assume you are
already familiar with the concept.)

When an exception is raised or due to an abort, however, a subprogram has an
abnormal completion. We discuss more about exception handling and finalization
:ref:`later on <Adv_Ada_Controlled_Types_Exception_Handling>`.


Finalization via unchecked deallocation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When performing unchecked deallocation of a controlled type, the
:ada:`Finalize` procedure is called right before the actual memory for the
controlled object is deallocated.

Let's see a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Finalization.Unchecked_Deallocation

    with Ada.Finalization;
    with Ada.Unchecked_Deallocation;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

       type T_Access is access T;

       procedure Free (A : in out T_Access);

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Finalize (E : in out T);

       procedure Free_T_Access is
         new Ada.Unchecked_Deallocation
           (Object => T,
            Name   => T_Access);

       procedure Free (A : in out T_Access)
         renames Free_T_Access;

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy T...)");
       end Dummy;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize T...");
       end Finalize;

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A : T_Access := new T;
    begin
       Dummy (A.all);

       Free (A);
       --  At this point, Finalize (A.all)
       --  will be called before the actual
       --  deallocation.

       Put_Line ("We've just freed A.");
    end Show_Controlled_Types;

In this example, we see that a call to :ada:`Finalize` (for type :ada:`T`) is
triggered by the call to :ada:`Free` for the :ada:`A` object |mdash| at this
point, we haven't reached the end of the main procedure
(:ada:`Show_Controlled_Types`) yet. After the call to :ada:`Free`, the object
originally referenced by :ada:`A` has been completely finalized |mdash| and
deallocated.

When the main procedure completes (after the call to :ada:`Put_Line` in that
procedure), we would normally see the calls to :ada:`Finalize` for controlled
objects. However, at this point, we obviously don't have a second call to the
:ada:`Finalize` procedure for type :ada:`T`, as the object referenced by
:ada:`A` has already been finalized and freed.


Subcomponents
~~~~~~~~~~~~~

As we've seen in the section about
:ref:`initialization of subcomponents <Adv_Ada_Controlled_Types_Initialization_Subcomponents>`,
subcomponents of a controlled type are initialized by a call to their
corresponding :ada:`Initialize` procedure before the call to :ada:`Initialize`
for the parent controlled type. In the case of finalization, the reverse order
is applied: first, finalization of the parent type takes place, and then the
finalization of the subcomponents.

We can visualize the lifetime as follows:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        Processing -> A : << use >>
        Processing -> A : << completion >>
        Processing -> A : Finalize (A)
        Processing -> A : << finalize subcomponents of object A>>
        Processing -> A !! : << finalize >>
    @enduml

.. _Adv_Ada_Controlled_Types_Finalization_Subcomponents_Code_Example:

Let's show a code example by revisiting the previous implementation of the
controlled types :ada:`Sub_1` and :ada:`Sub_2`, and adapting it:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Finalization.Controlled_Initialization

    with Ada.Finalization;

    package Subs is

       type Sub_1 is tagged private;

       type Sub_2 is tagged private;

    private

       type Sub_1 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Finalize (E : in out Sub_1);

       type Sub_2 is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Finalize (E : in out Sub_2);

    end Subs;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Subs is

       procedure Finalize (E : in out Sub_1) is
       begin
          Put_Line ("Finalize: Sub_1...");
       end Finalize;

       procedure Finalize (E : in out Sub_2) is
       begin
          Put_Line ("Finalize: Sub_2...");
       end Finalize;

    end Subs;

Now, let's use those controlled types as components of a type :ada:`T`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Finalization.Controlled_Initialization

    with Ada.Finalization;

    with Subs; use Subs;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       type T is new
         Ada.Finalization.Controlled with
       record
          S1 : Sub_1;
          S2 : Sub_2;
       end record;

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy: T...)");
       end Dummy;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize: T...");
       end Finalize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Controlled_Types is
       A : T;
    begin
       Dummy (A);
    end Show_Controlled_Types;

When we run this application, we see that the :ada:`Finalize` procedure is
called for the type :ada:`T` itself |mdash| as the first step of the
finalization of type :ada:`T`. Then, the :ada:`Sub_2` and :ada:`Sub_1`
components are finalized by calls to their respective :ada:`Finalize`
procedures.

This diagram shows the finalization sequence:

.. uml::
    :align: center
    :width: 400pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.S1" as Sub_1
        participant "T.S2" as Sub_2

        Processing -> type_t : Finalize (T)
        Processing -> type_t : << finalize subcomponents >>
        activate type_t
            type_t -> Sub_2 : Finalize (Sub_2)
            type_t -> Sub_1 : Finalize (Sub_1)
        deactivate type_t
    @enduml


Components with access discriminants
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We already discussed the
:ref:`initialization of components with access discriminants constrained by a per-object expression <Adv_Ada_Controlled_Types_Initialization_Components_Access_Disciminants>`.
In the case of the finalization of such components, they are finalized before
any components that do not fall into this category |mdash| in the reverse order
of their component declarations |mdash| but after the finalization of the
parent type.

Let's revisit a
:ref:`previous code example <Adv_Ada_Controlled_Types_Initialization_Subcomponents_Access_Discriminant_Code_Example>`
and adapt it to demonstrate the finalization of components with access
discriminants. First, we implement another package with controlled types:

.. code:: ada compile_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Finalization.Controlled_Initialization

    with Ada.Finalization;

    package Selections is

       type Selection is private;

       type Selection_1 (S : access Selection) is
         tagged private;

       type Selection_2 (S : access Selection) is
         tagged private;

    private

       type Selection is null record;

       type Selection_1 (S : access Selection) is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Finalize
          (E : in out Selection_1);

       type Selection_2 (S : access Selection) is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Finalize
         (E : in out Selection_2);

    end Selections;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Selections is

       procedure Finalize
         (E : in out Selection_1) is
       begin
          Put_Line ("Finalize: Selection_1...");
       end Finalize;

       procedure Finalize
         (E : in out Selection_2) is
       begin
          Put_Line ("Finalize: Selection_2...");
       end Finalize;

    end Selections;

In this example, we see the declaration of the :ada:`Selection_1` and
:ada:`Selection_2` types, which are controlled types with an access
discriminant of :ada:`Selection` type. Now, let's use these types in the
declaration of a type :ada:`T` and add two new components |mdash| :ada:`Sel_1`
and :ada:`Sel_2`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Finalization.Controlled_Initialization

    with Ada.Finalization;

    with Subs;       use Subs;
    with Selections; use Selections;

    package Simple_Controlled_Types is

       type T (S1 : access Selection;
               S2 : access Selection) is
         tagged private;

       procedure Dummy (E : T);

    private

       type T (S1 : access Selection;
               S2 : access Selection) is new
         Ada.Finalization.Controlled with
       record
          Sel_1 : Selection_1 (S1);
          Sel_2 : Selection_2 (S2);
          S_1   : Sub_1;
       end record;

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy: T...)");
       end Dummy;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize: T...");
       end Finalize;

    end Simple_Controlled_Types;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    with Selections;
    use  Selections;

    procedure Show_Controlled_Types is
       S1, S2 : aliased Selection;
       A : T (S1'Access, S2'Access);
    begin
       Dummy (A);
    end Show_Controlled_Types;

When we run this example, we see that the :ada:`Finalize` procedure of type
:ada:`T` is called as the first step. Then, the :ada:`Finalize` procedure is
called for the components with an access discriminant constrained by a
:ref:`per-object expression <Adv_Ada_Per_Object_Expressions>` |mdash| in this
case, :ada:`Sel_2` and :ada:`Sel_1` (of :ada:`Selection_2` and
:ada:`Selection_1` types, respectively). Finally, the :ada:`Sub_1` component
is finalized.

This diagram shows the finalization sequence:

.. uml::
    :align: center
    :width: 500pt

    @startuml
        actor Processing
        participant "T" as type_t
        participant "T.Sel_1" as Selection_1
        participant "T.Sel_2" as Selection_2
        participant "T.S_1" as Sub_1

        Processing -> type_t : Finalize (T)

        Processing -> type_t : << finalize subcomponents \nwith access discriminant / per-object expression >>
        activate type_t
            type_t -> Selection_2 : Finalize (Selection_2)
            type_t -> Selection_1 : Finalize (Selection_1)
        deactivate type_t

        Processing -> type_t : << finalize standard subcomponents >>
        activate type_t
            type_t -> Sub_1 : Finalize (Sub_1)
        deactivate type_t

    @enduml


.. _Adv_Ada_Controlled_Types_Exception_Handling:

Controlled Types and Exception Handling
---------------------------------------

In the previous section, we mainly focused on the normal completion of
controlled types. However, when control is transferred out of the normal
execution path due to an abort or an exception being raised, we speak of
abnormal completion. In this section, we focus on those cases.

Let's start with a simple example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Exception_Handling.Simple_Exception
    :class: ada-run-expect-failure

    with Ada.Finalization;

    package Simple_Controlled_Types is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Simple_Controlled_Types is

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize...");
       end Initialize;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize...");
       end Finalize;

    end Simple_Controlled_Types;

    with Ada.Text_IO; use Ada.Text_IO;

    with Simple_Controlled_Types;
    use  Simple_Controlled_Types;

    procedure Show_Simple_Exception is
       A : T;

       function Int_Last return Integer is
         (Integer'Last);

       Cnt : Positive := Int_Last;
    begin
       Cnt := Cnt + 1;

       Dummy (A);

       Put_Line (Cnt'Image);

       --  When A is about to get out of
       --  scope:
       --
       --  Finalize (A);
       --
    end Show_Simple_Exception;

In this example, we're forcing an overflow to happen in the
:ada:`Show_Simple_Exception` by adding one to the integer variable :ada:`Cnt`,
which already has the value :ada:`Integer'Last`. The corresponding
:ref:`overflow check <Adv_Ada_Overflow_Check>` raises the
:ada:`Constraint_Error`.

However, *before* this exception is raised, the finalization of the controlled
object :ada:`A` is performed. In this sense, we have normal completion of the
controlled type |mdash| even though an exception is being raised.

.. admonition:: For further reading...

   We already talked about the
   :ref:`allocation check <Adv_Ada_Allocation_Check>`, which may raise a
   :ada:`Program_Error` exception. In the code example for that section, we
   used controlled types. Feel free to revisit the example.

.. admonition:: Relevant topics

    - :arm22:`Completion and Finalization <7-6-1>`


.. _Adv_Ada_Controlled_Types_Initialize_Exception:

Exception raising in Initialize
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If an exception is raised in the :ada:`Initialize` procedure, we have abnormal
completion. Let's see an example:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Exception_Handling.CT_Initialize_Exception
    :class: ada-run-expect-failure

    with Ada.Finalization;

    package CT_Initialize_Exception is

       type T is tagged private;

       procedure Dummy (E : T);

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end CT_Initialize_Exception;

    with Ada.Text_IO; use Ada.Text_IO;

    package body CT_Initialize_Exception is

       function Int_Last return Integer is
         (Integer'Last);

       Cnt : Positive := Int_Last;

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize...");
          Cnt := Cnt + 1;
       end Initialize;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize...");
       end Finalize;

    end CT_Initialize_Exception;

    with CT_Initialize_Exception;
    use  CT_Initialize_Exception;

    procedure Show_Initialize_Exception is
       A : T;
    begin
       Dummy (A);
    end Show_Initialize_Exception;

In the :ada:`Show_Initialize_Exception` procedure, we declare an object
:ada:`A` of controlled type :ada:`T`. As we know, this declaration triggers a
call to the :ada:`Initialize` procedure that we've implemented in the body of
the :ada:`CT_Initialize_Exception` package. In the :ada:`Initialize` procedure,
we're forcing an overflow to happen |mdash| by adding one to the :ada:`Cnt`
variable, which already has the :ada:`Integer'Last` value.

This is an example of abnormal completion, as the control is transferred out of
the :ada:`Initialize` procedure, and the corresponding :ada:`Finalize`
procedure is never called for object :ada:`A`.


Bounded errors of controlled types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Bounded errors are an important topic when talking about exception and
controlled types. In general, if an exception is raised in the :ada:`Adjust` or
:ada:`Finalize` procedure, this is considered a bounded error. If the bounded
error is detected, the :ada:`Program_Error` exception is raised.

Note that the original exception raised in the :ada:`Adjust` or :ada:`Finalize`
procedures could be any possible exception. For example, one of those
procedures could raise a :ada:`Constraint_Error` exception. However, the actual
exception that is raised at runtime is the :ada:`Program_Error` exception. This
is because the bounded error, which raises the :ada:`Program_Error` exception,
is more severe than the original exception coming from those procedures.

(The behavior is different when the :ada:`Adjust` or :ada:`Finalize` procedure
is called explicitly, as we'll see later.)

.. todo::

    - Add link to section on bounded errors (once it's available).

Not every exception raised during an operation on controlled types is
considered a bounded error. In fact, the case we've seen before, an
:ref:`exception raised in the Initialize procedure <Adv_Ada_Controlled_Types_Initialize_Exception>`
is not a bounded error.

Here's a code example of a :ada:`Constraint_Error` exception being raised in
the :ada:`Finalize` procedure:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Exception_Handling.CT_Finalize_Exception
    :class: ada-run-expect-failure

    with Ada.Finalization;

    package CT_Finalize_Exception is

       type T is tagged private;

       procedure Dummy (E : T);

       procedure Reset_Counter;

    private

       type T is new
         Ada.Finalization.Controlled
           with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Adjust (E : in out T);

       overriding
       procedure Finalize (E : in out T);

    end CT_Finalize_Exception;

    with Ada.Text_IO; use Ada.Text_IO;

    package body CT_Finalize_Exception is

       Cnt : Integer := Integer'Last;

       procedure Dummy (E : T) is
       begin
          Put_Line ("(Dummy...)");
       end Dummy;

       procedure Initialize (E : in out T) is
       begin
          Put_Line ("Initialize...");
       end Initialize;

       overriding
       procedure Adjust (E : in out T) is
       begin
          Put_Line ("Adjust...");
       end Adjust;

       procedure Finalize (E : in out T) is
       begin
          Put_Line ("Finalize...");
          Cnt := Cnt + 1;
       end Finalize;

       procedure Reset_Counter is
       begin
          Cnt := 0;
       end Reset_Counter;

    end CT_Finalize_Exception;

    with Ada.Text_IO; use Ada.Text_IO;

    with CT_Finalize_Exception;
    use  CT_Finalize_Exception;

    procedure Show_Finalize_Exception is
       A, B : T;
    begin
       Dummy (A);

       --  When A is about to get out of
       --  scope:
       --
       --  Finalize (A);
       --
    end Show_Finalize_Exception;

In this example, we're again forcing an overflow to happen (by adding one to
the integer variable :ada:`Cnt`), this time in the :ada:`Finalize` procedure.
When this procedure is implicitly called |mdash| when object :ada:`A` is about
to get out of scope in the :ada:`Show_Finalize_Exception` procedure |mdash|
the :ada:`Constraint_Error` exception is raised.

As we've just seen, having an exception be raised during an implicit call to
the :ada:`Finalize` procedure is a bounded error. Therefore, we see that the
:ada:`Program_Error` exception is raised at runtime instead of the original
:ada:`Constraint_Error` exception.

As we hinted in the beginning, when the :ada:`Adjust` or the :ada:`Finalize`
procedure is called *explicitly*, the exception raised in that procedure is
*not* considered a bounded error. In this case, the original exception is
raised.

To show an example of such an explicit call, let's first move the overriden
procedures for type :ada:`T` (:ada:`Initialize`, :ada:`Adjust` and
:ada:`Finalize`) out of the private part of the package
:ada:`CT_Finalize_Exception`, so they are now visible to clients. This allows
us to call the :ada:`Finalize` procedure explicitly:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Exception_Handling.CT_Finalize_Exception

    with Ada.Finalization;

    package CT_Finalize_Exception is

       type T is new
         Ada.Finalization.Controlled
       with null record;

       overriding
       procedure Initialize (E : in out T);

       overriding
       procedure Adjust (E : in out T);

       overriding
       procedure Finalize (E : in out T);

       procedure Dummy (E : T);

       procedure Reset_Counter;

    end CT_Finalize_Exception;

    with Ada.Text_IO; use Ada.Text_IO;

    with CT_Finalize_Exception;
    use  CT_Finalize_Exception;

    procedure Show_Finalize_Exception is
       A : T;
    begin
       Dummy (A);

       Finalize (A);

       Put_Line ("After Finalize");
    exception
       when Constraint_Error =>
          Put_Line
            ("Constraint_Error is being handled...");
          Reset_Counter;
    end Show_Finalize_Exception;

Now, we're calling the :ada:`Finalize` procedure explicitly in the
:ada:`Show_Finalize_Exception` procedure. As we know, due to the operation on
:ada:`I` in the :ada:`Finalize` procedure, the :ada:`Constraint_Error`
exception is raised in the procedure. Because we're handling this exception in
the :ada:`Show_Finalize_Exception` procedure, we see the corresponding user
message ("Constraint_Error is being handled...") at runtime.

(Note that in the exception handling block, we're calling the
:ada:`Reset_Counter` procedure. This prevents :ada:`Constraint_Error` from
being raised in the next call to :ada:`Finalize`.)


Memory allocation and exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a memory block is allocated for controlled types and a bounded error
occurs, there is no guarantee that this memory block will be deallocated.
Roughly speaking, the compiler has the freedom |mdash| but not the obligation
|mdash| to generate appropriate calls to :ada:`Finalize`, which may deallocate
memory blocks.

For example, we've seen that
:ref:`subcomponents of controlled type <Adv_Ada_Controlled_Types_Initialization_Subcomponents>`
of a controlled object :ada:`A` are initialized before the initialization of
object :ada:`A` takes place. Because memory might have been allocated for the
subcomponents, the compiler can insert code that attempts to finalize those
subcomponents, which in turn deallocates the memory blocks (if they were
allocated in the first place).

We can visualize this strategy in the following diagram:

.. uml::
   :align: center
   :width: 300pt

    @startuml
        actor Processing
        participant "object A" as A

        Processing -> A ** : << create >>
        Processing -> A : << initialize subcomponents of object A\n(Memory was allocated here!) >>
        Processing -> A : Initialize (A) \n << (An exception is raised here!) >>
        Processing -> A !! : << finalize subcomponents\n(Deallocate memory.) >>
    @enduml

This strategy (of finalizing subcomponents that haven't raised exceptions)
prevents memory leaks. However, this behavior very much depends on the compiler
implementation. The :arm22:`Ada Reference Manual <7-6-1>` delineates (in the
"Implementation Permissions" section) the cases where the compiler is allowed
|mdash| but not required |mdash| to finalize objects when exceptions are
raised.

Because the actual behavior isn't defined, custom implementation of
:ada:`Adjust` and :ada:`Finalize` procedures for controlled types should be
designed very carefully in order to avoid exceptions, especially when memory
is allocated in the :ada:`Initialize` procedure.


Applications of Controlled Types
--------------------------------

In this section, we discuss applications of controlled types. In this context,
it's important to remember that controlled types have an associated overhead,
which can become non-negligible depending in which context the controlled
objects are used. However, there are applications where utilizing controlled
types is the best approach.

(Note that this overhead we've just mentioned is not specific to Ada. In fact,
types similar to controlled types will be relatively expensive in any
programming language. As an example, destructors in C++ may require a similar
maintenance of state at run-time.)


Encapsulating access type handling
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Previously, when discussing
:ref:`design strategies for access types <Adv_Ada_Design_Strategies_Access_Types>`,
we saw an example on using
:ref:`limited controlled types to encapsulate access types <Adv_Ada_Controlled_Type_For_Access_Types>`.

A more generalized example is the one of an unbounded stack. Because it's
unbounded, it allows for increasing the stack's size *on demand*. We can
implement this kind of stack by using access types. Let's look at a simple
(unoptimized) implementation:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Applications.Unbounded_Stacks

    with Ada.Finalization;

    generic
       Default_Chunk_Size : Positive := 5;
       type Element is private;
    package Unbounded_Stacks is

       Stack_Underflow : exception;

       type Unbounded_Stack is private;

       procedure Push (S : in out Unbounded_Stack;
                       E :        Element);

       function Pop (S : in out Unbounded_Stack)
                     return Element;

       function Is_Empty (S : Unbounded_Stack)
                          return Boolean;

    private

       type Element_Array is
         array (Positive range <>) of
           Element;

       type Element_Array_Access is
         access Element_Array;

       type Unbounded_Stack is new
         Ada.Finalization.Controlled with
          record
             Chunk_Size : Positive
               := Default_Chunk_Size;
             Data       : Element_Array_Access;
             Top        : Natural := 0;
          end record;

       procedure Initialize
         (S : in out Unbounded_Stack);

       procedure Adjust
         (S : in out Unbounded_Stack);

       procedure Finalize
         (S : in out Unbounded_Stack);

    end Unbounded_Stacks;

    with Ada.Text_IO; use Ada.Text_IO;

    with Ada.Unchecked_Deallocation;

    package body Unbounded_Stacks is

       --
       --  LOCAL SUBPROGRAMS
       --

       procedure Free is
         new Ada.Unchecked_Deallocation
           (Object => Element_Array,
            Name   => Element_Array_Access);

       function Is_Full (S : Unbounded_Stack)
                         return Boolean is
       begin
          return S.Top = S.Data'Last;
       end Is_Full;

       procedure Reallocate_Data
         (To         : in out Element_Array_Access;
          From       :        Element_Array_Access;
          Max_Last   :        Positive;
          Valid_Last :        Positive) is
       begin
          To := new Element_Array (1 .. Max_Last);

          for I in 1 .. Valid_Last loop
             To (I) := From (I);
          end loop;
       end Reallocate_Data;

       procedure Increase_Size
         (S : in out Unbounded_Stack)
       is
          Old_Data : Element_Array_Access := S.Data;
          Old_Last : constant Positive
                     := Old_Data'Last;
          New_Last : constant Positive
                     := Old_Data'Last + S.Chunk_Size;
       begin
          Put_Line ("Increasing Unbounded_Stack "
                    & "(1 .. "
                    & Old_Last'Image
                    & ") to (1 .. "
                    & New_Last'Image
                    & ")");

          Reallocate_Data
            (To         => S.Data,
             From       => Old_Data,
             Max_Last   => New_Last,
             Valid_Last => S.Top);

          Free (Old_Data);
       end Increase_Size;

       --
       --  SUBPROGRAMS
       --

       procedure Push (S : in out Unbounded_Stack;
                       E :        Element) is
       begin
          if Is_Full (S) then
             Increase_Size (S);
          end if;

          S.Top := S.Top + 1;
          S.Data (S.Top) := E;
       end Push;

       function Pop (S : in out Unbounded_Stack)
                     return Element is
       begin
          return E : Element do
             if Is_Empty (S) then
                raise Stack_Underflow;
             end if;

             E := S.Data (S.Top);
             S.Top := S.Top - 1;
          end return;
       end Pop;

       function Is_Empty (S : Unbounded_Stack)
                          return Boolean is
       begin
          return S.Top = 0;
       end Is_Empty;

       --
       --  PRIVATE SUBPROGRAMS
       --

       procedure Initialize
         (S : in out Unbounded_Stack)
       is
          Last : constant Positive
                 := S.Chunk_Size;
       begin
          Put_Line ("Initializing Unbounded_Stack "
                    & "(1 .. "
                    & Last'Image
                    & ")");
          S.Data := new Element_Array
                          (1 .. S.Chunk_Size);
       end Initialize;

       procedure Allocate_Duplicate_Data
         (S : in out Unbounded_Stack)
       is
          Last : constant Positive
                 := S.Data'Last;
       begin
          Put_Line ("Duplicating data for new "
                    & "Unbounded_Stack (1 .. "
                    & Last'Image
                    & ")");

          Reallocate_Data
            (To         => S.Data,
             From       => S.Data,
             Max_Last   => Last,
             Valid_Last => S.Top);
       end Allocate_Duplicate_Data;

       procedure Adjust
         (S : in out Unbounded_Stack)
       is
       begin
          Put_Line ("Adjusting Unbounded_Stack...");
          Allocate_Duplicate_Data (S);
       end Adjust;

       procedure Finalize
         (S : in out Unbounded_Stack)
       is
          Last : constant Positive
                 := S.Data'Last;
       begin
          Put_Line ("Finalizing Unbounded_Stack "
                    & "(1 .. "
                    & Last'Image
                    & ")");
          if S.Data /= null then
            Free (S.Data);
          end if;
       end Finalize;

    end Unbounded_Stacks;

    with Ada.Text_IO; use Ada.Text_IO;

    with Unbounded_Stacks;

    procedure Show_Unbounded_Stack is

       package Unbounded_Integer_Stacks is new
         Unbounded_Stacks (Element => Integer);
       use Unbounded_Integer_Stacks;

       procedure Print_Pop_Stack
          (S    : in out Unbounded_Stack;
           Name :        String)
       is
          V : Integer;
       begin
          Put_Line ("STACK: " & Name);
          Put ("= ");
          while not Is_Empty (S) loop
             V := Pop (S);
             Put (V'Image & " ");
          end loop;
          New_Line;
       end Print_Pop_Stack;

       Stack   : Unbounded_Stack;
       Stack_2 : Unbounded_Stack;
    begin
       for I in 1 .. 10 loop
          Push (Stack, I);
       end loop;

       Stack_2 := Stack;

       for I in 11 .. 20 loop
          Push (Stack, I);
       end loop;

       Print_Pop_Stack (Stack, "Stack");
       Print_Pop_Stack (Stack_2, "Stack_2");

    end Show_Unbounded_Stack;

Let's first focus on the :ada:`Unbounded_Stack` type from the
:ada:`Unbounded_Stacks` package. The actual stack is implemented via the array
that we allocate for the :ada:`Data` component. The initial allocation takes
place in the :ada:`Initialize` procedure, which is called when an object of
:ada:`Unbounded_Stack` type is created. The corresponding deallocation of the
stack happens in the :ada:`Finalize` procedure.

In the :ada:`Push` procedure, we check whether the stack is full or not before
storing a new element into the stack. If the stack is full, we call the
:ada:`Increase_Size` procedure to *increase* the size of the array. This is
actually done by calling the :ada:`Reallocate_Data` procedure, which allocates
a new array for the stack and copies the original data to the new array.

Also, when copying an unbounded stack object to another object of this type, a
call to the :ada:`Adjust` procedure is triggered |mdash| we do this by the
assignment :ada:`Stack_2 := Stack` in the :ada:`Show_Unbounded_Stack`
procedure. In the :ada:`Adjust` procedure, we call the
:ada:`Allocate_Duplicate_Data` procedure to allocate a new array for the stack
data and copy the data from the original stack. (Internally, the
:ada:`Allocate_Duplicate_Data` procedure calls the :ada:`Reallocate_Data`
procedure, which we already mentioned.)

By encapsulating the access type handling in controlled types, we can ensure
that the access objects are handled correctly: no incorrect pointer usage or
memory leak can happen when we use this strategy.


Encapsulating file handling
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controlled types can be used to encapsulate file handling, so that files are
automatically created and closed. A common use-case is when a new file is
expected to be created or opened when we declare the controlled object, and
closed when the controlled object gets out of scope.

A simple example is the one of a logger, which we can use to write to a
logfile by simple calls to :ada:`Put_Line`:

.. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Applications.Logger main=show_logger.adb

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Finalization;

    package Loggers is

       type Logger (<>) is
         limited private;

       function Init (Filename : String)
                      return Logger;

       procedure Put_Line (L : Logger;
                           S : String);

    private

       type Logger is new
         Ada.Finalization.Limited_Controlled with
          record
             Logfile : File_Type;
          end record;

       procedure Finalize
         (L : in out Logger);

    end Loggers;

    package body Loggers is

       --
       --  SUBPROGRAMS
       --

       function Init (Filename : String)
                      return Logger is
       begin
          return L : Logger do
             Create (L.Logfile, Out_File, Filename);
          end return;
       end Init;

       procedure Put_Line (L : Logger;
                           S : String) is
       begin
          Put_Line ("Logger: Put_Line");
          Put_Line (L.Logfile, S);
       end Put_Line;

       --
       --  PRIVATE SUBPROGRAMS
       --

       procedure Finalize
         (L : in out Logger) is
       begin
          Put_Line ("Finalizing Logger...");
          if Is_Open (L.Logfile) then
             Close (L.Logfile);
          end if;
       end Finalize;

    end Loggers;

    with Loggers; use Loggers;

    procedure Some_Processing (Log : Logger) is
    begin
       Put_Line (Log, "Some processing...");
    end Some_Processing;

    with Loggers;         use Loggers;
    with Some_Processing;

    procedure Show_Logger is
       Log : constant Logger := Init ("report.log");
    begin
       Put_Line (Log, "Some info...");
       Some_Processing (Log);
    end Show_Logger;

The :ada:`Logger` type from the :ada:`Loggers` package has two subprograms:

- :ada:`Init`, which creates a logger object and creates a logfile
  *in the background*, and

- :ada:`Put_Line`, which writes a message to the logfile.

Note that we use the :ada:`(<>)` in the declaration of the :ada:`Logger` type
to ensure that clients call the :ada:`Init` function. This allows us to specify
the location of the logfile (as the :ada:`Filename` parameter).

Also, we can pass the logger to other subprograms and use it there. In this
example, we pass the logger to the :ada:`Some_Processing` procedure and there,
we the call :ada:`Put_Line` using the logger object.

Finally, as soon as the logger goes out of scope, the log is automatically
closed via the call to :ada:`Finalize`.

.. admonition:: For further reading...

    Instead of enforcing a call to :ada:`Init`, we could have overridden the
    :ada:`Initialize` procedure and opened the logfile there. This approach,
    however, would have prevented the client from specifying the location of
    the logfile in a simple way. Specifying the filename as a type discriminant
    wouldn't work because we cannot use a string as a discriminant |mdash| as
    we mentioned
    :ref:`in a previous chapter <Adv_Ada_Indefinite_Subtype_Discriminant>`,
    we cannot use indefinite subtypes as discriminants.

    If we had preferred this approach, we could generate a random name for the
    file in the :ada:`Initialize` procedure and store the file itself in a
    temporary directory indicated by the operating system. Alternatively, we
    could use the access to a string as a discriminant:

    .. code:: ada run_button project=Courses.Advanced_Ada.Resource_Management.Controlled_Types.Applications.Logger main=show_logger.adb

        with Ada.Text_IO; use Ada.Text_IO;
        with Ada.Finalization;

        package Loggers is

           type Logger (Filename : access String) is
             limited private;

           procedure Put_Line (L : Logger;
                               S : String);

        private

           type Logger (Filename : access String) is new
             Ada.Finalization.Limited_Controlled with
              record
                 Logfile : File_Type;
              end record;

           procedure Initialize
             (L : in out Logger);

           procedure Finalize
             (L : in out Logger);

        end Loggers;

        package body Loggers is

           --
           --  SUBPROGRAMS
           --

           procedure Put_Line (L : Logger;
                               S : String) is
           begin
              Put_Line ("Logger: Put_Line");
              Put_Line (L.Logfile, S);
           end Put_Line;

           --
           --  PRIVATE SUBPROGRAMS
           --

           procedure Initialize
             (L : in out Logger) is
           begin
              Create (L.Logfile,
                      Out_File,
                      L.Filename.all);
           end Initialize;

           procedure Finalize
             (L : in out Logger) is
           begin
              Put_Line ("Finalizing Logger...");
              if Is_Open (L.Logfile) then
                 Close (L.Logfile);
              end if;
           end Finalize;

        end Loggers;

        with Loggers;         use Loggers;
        with Some_Processing;

        procedure Show_Logger is
           Name : aliased String := "report.log";
           Log : Logger (Name'Access);
        begin
           Put_Line (Log, "Some info...");
           Some_Processing (Log);
        end Show_Logger;

    This approach works, but requires us to declare an aliased string
    (:ada:`Name`), which we can give access to in the declaration of the
    :ada:`Log` object.

By encapsulating the file handling in controlled types, we ensure that files
are properly opened when we want to use them, and that the files are closed
when they're not going to be used anymore.
