.. _Ada_For_Embedded_C_Dev_Hands_On_OOP:

Appendix A: Hands-On Object-Oriented Programming
================================================

.. include:: ../../../global.txt

The goal of this appendix is to present a hands-on view on how to translate a
system from C to Ada and improve it with object-oriented programming.

System Overview
---------------

Let's start with an overview of a simple system that we'll implement and use
below. The main system is called AB and it combines two systems A and B. System
AB is not supposed to do anything useful. However, it can serve as a good model
for the hands-on we're about to start.

This is a list of requirements for the individual systems A and B, and the
combined system AB:

- System A:

    - The system can be activated and deactivated.

        - During activation, the system's values are reset.

    - Its current value (in floating-point) can be retrieved.

        - This value is the average of the two internal floating-point values.

    - Its current state (activated or deactivated) can be retrieved.

- System B:

    - The system can be activated and deactivated.

        - During activation, the system's value is reset.

    - Its current value (in floating-point) can be retrieved.

    - Its current state (activated or deactivated) can be retrieved.

- System AB

    - The system contains an instance of system A and an instance of system B.

    - The system can be activated and deactivated.

        - System AB activates both systems A and B during its own activation.

        - System AB deactivates both systems A and B during its own
          deactivation.

    - Its current value (in floating-point) can be retrieved.

        - This value is the average of the current values of systems A and B.

    - Its current state (activated or deactivated) can be retrieved.

        - AB is only considered activated when both systems A and B are
          activated.

    - The system's health can be checked.

        - This check consists in calculating the absolute difference D between
          the current values of systems A and B and checking whether D is below
          a threshold of 0.1.

The source-code in the following section contains an implementation of these
requirements.

Non Object-Oriented Approach
----------------------------

In this section, we look into implementations (in both C and Ada) of system AB
that don't make use of object-oriented programming.

Starting point in C
~~~~~~~~~~~~~~~~~~~

Let's start with an implementation in C for the system described above:

[C]

.. code:: c manual_chop run_button project=Courses.Ada_For_Embedded_C_Dev.HandsOnOOP.System_AB_C

    !system_a.h
    typedef struct {
        float val[2];
        int   active;
    } A;

    void A_activate (A *a);

    int A_is_active (A *a);

    float A_value (A *a);

    void A_deactivate (A *a);

    !system_a.c
    #include "system_a.h"

    void A_activate (A *a)
    {
        int i;

        for (i = 0; i < 2; i++)
        {
            a->val[i] = 0.0;
        }
        a->active = 1;
    }

    int A_is_active (A *a)
    {
        return a->active == 1;
    }

    float A_value (A *a)
    {
        return (a->val[0] + a->val[1]) / 2.0;
    }

    void A_deactivate (A *a)
    {
        a->active = 0;
    }

    !system_b.h
    typedef struct {
        float val;
        int   active;
    } B;

    void B_activate (B *b);

    int B_is_active (B *b);

    float B_value (B *b);

    void B_deactivate (B *b);

    !system_b.c
    #include "system_b.h"

    void B_activate (B *b)
    {
        b->val    = 0.0;
        b->active = 1;
    }

    int B_is_active (B *b)
    {
        return b->active == 1;
    }

    float B_value (B *b)
    {
        return b->val;
    }

    void B_deactivate (B *b)
    {
        b->active = 0;
    }

    !system_ab.h
    #include "system_a.h"
    #include "system_b.h"

    typedef struct {
        A a;
        B b;
    } AB;

    void AB_activate (AB *ab);

    int AB_is_active (AB *ab);

    float AB_value (AB *ab);

    int AB_check (AB *ab);

    void AB_deactivate (AB *ab);

    !system_ab.c
    #include <math.h>
    #include "system_ab.h"

    void AB_activate (AB *ab)
    {
        A_activate (&ab->a);
        B_activate (&ab->b);
    }

    int AB_is_active (AB *ab)
    {
        return A_is_active(&ab->a) && B_is_active(&ab->b);
    }

    float AB_value (AB *ab)
    {
        return (A_value (&ab->a) + B_value (&ab->b)) / 2;
    }

    int AB_check (AB *ab)
    {
        const float threshold = 0.1;

        return fabs (A_value (&ab->a) - B_value (&ab->b)) < threshold;
    }

    void AB_deactivate (AB *ab)
    {
        A_deactivate (&ab->a);
        B_deactivate (&ab->b);
    }

    !main.c
    #include <stdio.h>
    #include "system_ab.h"

    void display_active (AB *ab)
    {
        if (AB_is_active (ab))
            printf ("System AB is active.\n");
        else
            printf ("System AB is not active.\n");
    }

    void display_check (AB *ab)
    {
        if (AB_check (ab))
            printf ("System AB check: PASSED.\n");
        else
            printf ("System AB check: FAILED.\n");
    }

    int main()
    {
        AB s;

        printf ("Activating system AB...\n");
        AB_activate (&s);

        display_active (&s);
        display_check (&s);

        printf ("Deactivating system AB...\n");
        AB_deactivate (&s);

        display_active (&s);
    }

Here, each system is implemented in a separate set of header and source-code
files. For example, the API of system AB is in :file:`system_ab.h` and its
implementation in :file:`system_ab.c`.

In the main application, we instantiate system AB and activate it. Then, we
proceed to display the activation state and the result of the system's health
check. Finally, we deactivate the system and display the activation state
again.

.. _Ada_For_Embedded_C_Dev_Initial_Translation_To_Ada:

Initial translation to Ada
~~~~~~~~~~~~~~~~~~~~~~~~~~

The direct implementation in Ada is:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.HandsOnOOP.System_AB_Ada

    package System_A is

       type Val_Array is array (Positive range <>) of Float;

       type A is record
          Val    : Val_Array (1 .. 2);
          Active : Boolean;
       end record;

       procedure A_Activate (E : in out A);

       function A_Is_Active (E : A) return Boolean;

       function A_Value (E : A) return Float;

       procedure A_Deactivate (E : in out A);

    end System_A;

    package body System_A is

       procedure A_Activate (E : in out A) is
       begin
          E.Val    := (others => 0.0);
          E.Active := True;
       end A_Activate;

       function A_Is_Active (E : A) return Boolean is
       begin
          return E.Active;
       end A_Is_Active;

       function A_Value (E : A) return Float is
       begin
          return (E.Val (1) + E.Val (2)) / 2.0;
       end A_Value;

       procedure A_Deactivate (E : in out A) is
       begin
          E.Active := False;
       end A_Deactivate;

    end System_A;

    package System_B is

       type B is record
          Val    : Float;
          Active : Boolean;
       end record;

       procedure B_Activate (E : in out B);

       function B_Is_Active (E : B) return Boolean;

       function B_Value (E : B) return Float;

       procedure B_Deactivate (E : in out B);

    end System_B;

    package body System_B is

       procedure B_Activate (E : in out B) is
       begin
          E.Val    := 0.0;
          E.Active := True;
       end B_Activate;

       function B_Is_Active (E : B) return Boolean is
       begin
          return E.Active;
       end B_Is_Active;

       function B_Value (E : B) return Float is
       begin
          return E.Val;
       end B_Value;

       procedure B_Deactivate (E : in out B) is
       begin
          E.Active := False;
       end B_Deactivate;

    end System_B;

    with System_A; use System_A;
    with System_B; use System_B;

    package System_AB is

       type AB is record
          SA : A;
          SB : B;
       end record;

       procedure AB_Activate (E : in out AB);

       function AB_Is_Active (E : AB) return Boolean;

       function AB_Value (E : AB) return Float;

       function AB_Check (E : AB) return Boolean;

       procedure AB_Deactivate (E : in out AB);

    end System_AB;

    package body System_AB is

       procedure AB_Activate (E : in out AB) is
       begin
          A_Activate (E.SA);
          B_Activate (E.SB);
       end AB_Activate;

       function AB_Is_Active (E : AB) return Boolean is
       begin
          return A_Is_Active (E.SA) and B_Is_Active (E.SB);
       end AB_Is_Active;

       function AB_Value (E : AB) return Float is
       begin
          return (A_Value (E.SA) + B_Value (E.SB)) / 2.0;
       end AB_Value;

       function AB_Check (E : AB) return Boolean is
          Threshold : constant := 0.1;
       begin
          return abs (A_Value (E.SA) - B_Value (E.SB)) < Threshold;
       end AB_Check;

       procedure AB_Deactivate (E : in out AB) is
       begin
          A_Deactivate (E.SA);
          B_Deactivate (E.SB);
       end AB_Deactivate;

    end System_AB;

    with Ada.Text_IO; use Ada.Text_IO;

    with System_AB;   use System_AB;

    procedure Main is

       procedure Display_Active (E : AB) is
       begin
          if AB_Is_Active (E) then
             Put_Line ("System AB is active");
          else
             Put_Line ("System AB is not active");
          end if;
       end Display_Active;

       procedure Display_Check (E : AB) is
       begin
          if AB_Check (E) then
             Put_Line ("System AB check: PASSED");
          else
             Put_Line ("System AB check: FAILED");
          end if;
       end Display_Check;

       S : AB;
    begin
       Put_Line ("Activating system AB...");
       AB_Activate (S);

       Display_Active (S);
       Display_Check (S);

       Put_Line ("Deactivating system AB...");
       AB_Deactivate (S);

       Display_Active (S);
    end Main;

As you can see, this is a direct translation that doesn't change much of the
structure of the original C code. Here, the goal was to simply translate the
system from one language to another and make sure that the behavior remains the
same.

Improved Ada implementation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

By analyzing this direct implementation, we may notice the following points:

- Packages :ada:`System_A`, :ada:`System_B` and :ada:`System_AB` are used to
  describe aspects of the same system. Instead of having three distinct
  packages, we could group them as child packages of a common parent package
  |mdash| let's call it :ada:`Simple`, since this system is supposed to be
  simple. This approach has the advantage of allowing us to later use the
  parent package to implement functionality that is common for all parts of the
  system.

- Since we have subprograms that operate on types :ada:`A`, :ada:`B` and
  :ada:`AB`, we should avoid exposing the record components by moving the
  type declarations to the private part of the corresponding packages.

- Since Ada supports subprogram overloading |mdash| as discussed in
  :ref:`this section from chapter 2 <Ada_For_Embedded_C_Dev_Overloading>` |mdash|, we don't need to
  have different names for subprograms with similar functionality. For example,
  instead of having :ada:`A_Is_Active` and :ada:`B_Is_Active`, we can simply
  name these functions :ada:`Is_Active` for both types :ada:`A` and :ada:`B`.

- Some of the functions |mdash| such as :ada:`A_Is_Active` and :ada:`A_Value`
  |mdash| are very simple, so we could simplify them with expression functions.

This is an update to the implementation that addresses all the points above:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.HandsOnOOP.System_AB_Ada_Enhanced

    package Simple
      with Pure
    is
    end Simple;

    package Simple.System_A is

       type A is private;

       procedure Activate (E : in out A);

       function Is_Active (E : A) return Boolean;

       function Value (E : A) return Float;

       procedure Finalize (E : in out A);

    private

       type Val_Array is array (Positive range <>) of Float;

       type A is record
          Val    : Val_Array (1 .. 2);
          Active : Boolean;
       end record;

    end Simple.System_A;

    package body Simple.System_A is

       procedure Activate (E : in out A) is
       begin
          E.Val    := (others => 0.0);
          E.Active := True;
       end Activate;

       function Is_Active (E : A) return Boolean is
          (E.Active);

       function Value (E : A) return Float is
       begin
          return (E.Val (1) + E.Val (2)) / 2.0;
       end Value;

       procedure Finalize (E : in out A) is
       begin
          E.Active := False;
       end Finalize;

    end Simple.System_A;

    package Simple.System_B is

       type B is private;

       procedure Activate (E : in out B);

       function Is_Active (E : B) return Boolean;

       function Value (E : B) return Float;

       procedure Finalize (E : in out B);

    private

       type B is record
          Val    : Float;
          Active : Boolean;
       end record;

    end Simple.System_B;

    package body Simple.System_B is

       procedure Activate (E : in out B) is
       begin
          E.Val    := 0.0;
          E.Active := True;
       end Activate;

       function Is_Active (E : B) return Boolean is
       begin
          return E.Active;
       end Is_Active;

       function Value (E : B) return Float is
         (E.Val);

       procedure Finalize (E : in out B) is
       begin
          E.Active := False;
       end Finalize;

    end Simple.System_B;

    with Simple.System_A; use Simple.System_A;
    with Simple.System_B; use Simple.System_B;

    package Simple.System_AB is

       type AB is private;

       procedure Activate (E : in out AB);

       function Is_Active (E : AB) return Boolean;

       function Value (E : AB) return Float;

       function Check (E : AB) return Boolean;

       procedure Finalize (E : in out AB);

    private

       type AB is record
          SA : A;
          SB : B;
       end record;

    end Simple.System_AB;

    package body Simple.System_AB is

       procedure Activate (E : in out AB) is
       begin
          Activate (E.SA);
          Activate (E.SB);
       end Activate;

       function Is_Active (E : AB) return Boolean is
          (Is_Active (E.SA) and Is_Active (E.SB));

       function Value (E : AB) return Float is
          ((Value (E.SA) + Value (E.SB)) / 2.0);

       function Check (E : AB) return Boolean is
          Threshold : constant := 0.1;
       begin
          return abs (Value (E.SA) - Value (E.SB)) < Threshold;
       end Check;

       procedure Finalize (E : in out AB) is
       begin
          Finalize (E.SA);
          Finalize (E.SB);
       end Finalize;

    end Simple.System_AB;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Simple.System_AB; use Simple.System_AB;

    procedure Main is

       procedure Display_Active (E : AB) is
       begin
          if Is_Active (E) then
             Put_Line ("System AB is active");
          else
             Put_Line ("System AB is not active");
          end if;
       end Display_Active;

       procedure Display_Check (E : AB) is
       begin
          if Check (E) then
             Put_Line ("System AB check: PASSED");
          else
             Put_Line ("System AB check: FAILED");
          end if;
       end Display_Check;

       S : AB;
    begin
       Put_Line ("Activating system AB...");
       Activate (S);

       Display_Active (S);
       Display_Check (S);

       Put_Line ("Deactivating system AB...");
       Finalize (S);

       Display_Active (S);
    end Main;

First Object-Oriented Approach
------------------------------

Until now, we haven't used any of the object-oriented programming features of
the Ada language. So we can start by analyzing the API of systems A and B and
deciding how to best abstract some of its elements using object-oriented
programming.

Interfaces
~~~~~~~~~~

The first thing we may notice is that we actually have two distinct sets of
APIs there:

- one API for activating and deactivating the system.

- one API for retrieving the value of the system.

We can use this distinction to declare two interface types:

- :ada:`Activation_IF` for the :ada:`Activate` and :ada:`Deactivate` procedures
  and the :ada:`Is_Active` function;

- :ada:`Value_Retrieval_IF` for the :ada:`Value` function.

This is how the declaration could look like:

.. code-block:: ada

    type Activation_IF is interface;

    procedure Activate (E : in out Activation_IF) is abstract;
    function Is_Active (E : Activation_IF) return Boolean is abstract;
    procedure Deactivate (E : in out Activation_IF) is abstract;

    type Value_Retrieval_IF is interface;

    function Value (E : Value_Retrieval_IF) return Float is abstract;

Note that, because we are declaring interface types, all operations on those
types must be abstract or, in the case of procedures, they can also be declared
:ada:`null`. For example, we could change the declaration of the procedures
above to this:

.. code-block:: ada

    procedure Activate (E : in out Activation_IF) is null;
    procedure Deactivate (E : in out Activation_IF) is null;

When an operation is declared abstract, we must override it for the type that
derives from the interface. When a procedure is declared :ada:`null`, it acts
as a do-nothing default. In this case, overriding the operation is optional for
the type that derives from this interface.

Base type
~~~~~~~~~

Since the original system needs both interfaces we've just described, we have
to declare another type that combines those interfaces. We can do this by
declaring the interface type :ada:`Sys_Base`, which serves as the base type for
systems A and B. This is the declaration:

.. code-block:: ada

    type Sys_Base is interface and Activation_IF and Value_Retrieval_IF;

Since the system activation functionality is common for both systems A and B,
we could implement it as part of :ada:`Sys_Base`. That would require changing
the declaration from a simple interface to an abstract record:

.. code-block:: ada

    type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF
      with null record;

Now, we can add the Boolean component to the record (as a private component)
and override the subprograms of the :ada:`Activation_IF` interface. This is the
adapted declaration:

.. code-block:: ada

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF with private;

       overriding procedure Activate (E : in out Sys_Base);
       overriding function Is_Active (E : Sys_Base) return Boolean;
       overriding procedure Deactivate (E : in out Sys_Base);

    private

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF with record
          Active : Boolean;
       end record;

Derived types
~~~~~~~~~~~~~

In the declaration of the :ada:`Sys_Base` type we've just seen, we're not
overriding the :ada:`Value` function |mdash| from the
:ada:`Value_Retrieval_IF` interface |mdash| for the :ada:`Sys_Base` type, so it
remains an abstract function for :ada:`Sys_Base`. Therefore, the
:ada:`Sys_Base` type itself remains abstract and needs be explicitly declared
as such.

We use this strategy to ensure that all types derived from :ada:`Sys_Base` need
to implement their own version of the :ada:`Value` function. For example:

.. code-block:: ada

   type A is new Sys_Base with private;

   overriding function Value (E : A) return Float;

Here, the :ada:`A` type is derived from the :ada:`Sys_Base` and it includes its
own version of the :ada:`Value` function by overriding it. Therefore,
:ada:`A` is not an abstract type anymore and can be used to declare objects:

.. code-block:: ada

    procedure Main is
       Obj : A;
       V   : Float;
    begin
       Obj.Activate;
       V := Obj.Value;
    end Main;

.. admonition:: Important

    Note that the use of the :ada:`overriding` keyword in the subprogram
    declaration is not strictly necessary. In fact, we could leave this keyword
    out, and the code would still compile. However, if provided, the compiler
    will check whether the information is correct.

    Using the :ada:`overriding` keyword can help to avoid bad surprises
    |mdash| when you *may think* that you're overriding a subprogram, but
    you're actually not. Similarly, you can also write :ada:`not overriding` to
    be explicit about subprograms that are new primitives of a derived type.
    For example:

    .. code-block:: ada

        not overriding function Check (E : AB) return Boolean;

We also need to declare the values that are used internally in systems A and B.
For system A, this is the declaration:

.. code-block:: ada

       type A is new Sys_Base with private;

       overriding function Value (E : A) return Float;

    private

       type Val_Array is array (Positive range <>) of Float;

       type A is new Sys_Base with record
          Val : Val_Array (1 .. 2);
       end record;

Subprograms from parent
~~~~~~~~~~~~~~~~~~~~~~~

In the previous implementation, we've seen that the :ada:`A_Activate` and
:ada:`B_Activate` procedures perform the following steps:

- initialize internal values;

- indicate that the system is active (by setting the :ada:`Active` flag to
  :ada:`True`).

In the implementation of the :ada:`Activate` procedure for the :ada:`Sys_Base`
type, however, we're only dealing with the second step. Therefore, we need to
override the :ada:`Activate` procedure and make sure that we initialize
internal values as well. First, we need to declare this procedure for type
:ada:`A`:

.. code-block:: ada

    type A is new Sys_Base with private;

    overriding procedure Activate (E : in out A);

In the implementation of :ada:`Activate`, we should call the :ada:`Activate`
procedure from the parent (:ada:`Sys_Base`) to ensure that whatever was
performed for the parent will be performed in the derived type as well. For
example:

.. code-block:: ada

   overriding procedure Activate (E : in out A) is
   begin
      E.Val := (others => 0.0);
      Sys_Base (E).Activate;    --  Calling Activate for Sys_Base type:
                                --  this call initializes the Active flag.
   end;

Here, by writing :ada:`Sys_Base (E)`, we're performing a view conversion.
Basically, we're telling the compiler to view :ada:`E` not as an object of
type :ada:`A`, but of type :ada:`Sys_Base`. When we do this, any operation
performed on this object will be done as if it was an object of
:ada:`Sys_Base` type, which includes calling the :ada:`Activate` procedure of
the :ada:`Sys_Base` type.

.. admonition:: Important

    If we write :ada:`T (Obj).Proc`, we're telling the compiler to call the
    :ada:`Proc` procedure of type :ada:`T` and apply it on :ada:`Obj`.

    If we write :ada:`T'Class (Obj).Proc`, however, we're telling the compiler
    to dispatch the call. For example, if :ada:`Obj` is of derived type
    :ada:`T2` and there's an overridden :ada:`Proc` procedure for type
    :ada:`T2`, then this procedure will be called instead of the :ada:`Proc`
    procedure for type :ada:`T`.

Type AB
~~~~~~~

While the implementation of systems A and B is almost straightforward, it gets
more interesting in the case of system AB. Here, we have a similar API, but we
don't need the activation mechanism implemented in the abstract type
:ada:`Sys_Base`. Therefore, deriving from :ada:`Sys_Base` is not the best
option. Instead, when declaring the :ada:`AB` type, we can simply use the same
interfaces as we did for :ada:`Sys_Base`, but keep it independent from
:ada:`Sys_Base`. For example:

.. code-block:: ada

       type AB is new Activation_IF and Value_Retrieval_IF with private;

    private

       type AB is new Activation_IF and Value_Retrieval_IF with record
          SA : A;
          SB : B;
       end record;

Naturally, we still need to override all the subprograms that are part of the
:ada:`Activation_IF` and :ada:`Value_Retrieval_IF` interfaces. Also, we need
to implement the additional :ada:`Check` function that was originally only
available on system AB. Therefore, we declare these subprograms:

.. code-block:: ada

    overriding procedure Activate (E : in out AB);
    overriding function Is_Active (E : AB) return Boolean;
    overriding procedure Deactivate (E : in out AB);

    overriding function Value (E : AB) return Float;

    not overriding function Check (E : AB) return Boolean;

Updated source-code
~~~~~~~~~~~~~~~~~~~

Finally, this is the complete source-code example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.HandsOnOOP.System_AB_Ada_OOP_1

    package Simple is

       type Activation_IF is interface;

       procedure Activate (E : in out Activation_IF) is abstract;
       function Is_Active (E : Activation_IF) return Boolean is abstract;
       procedure Deactivate (E : in out Activation_IF) is abstract;

       type Value_Retrieval_IF is interface;

       function Value (E : Value_Retrieval_IF) return Float is abstract;

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF
         with private;

       overriding procedure Activate (E : in out Sys_Base);
       overriding function Is_Active (E : Sys_Base) return Boolean;
       overriding procedure Deactivate (E : in out Sys_Base);

    private

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF
         with record
          Active : Boolean;
       end record;

    end Simple;

    package body Simple is

       overriding procedure Activate (E : in out Sys_Base) is
       begin
          E.Active := True;
       end Activate;

       overriding function Is_Active (E : Sys_Base) return Boolean is
          (E.Active);

       overriding procedure Deactivate (E : in out Sys_Base) is
       begin
          E.Active := False;
       end Deactivate;

    end Simple;

    package Simple.System_A is

       type A is new Sys_Base with private;

       overriding procedure Activate (E : in out A);

       overriding function Value (E : A) return Float;

    private

       type Val_Array is array (Positive range <>) of Float;

       type A is new Sys_Base with record
          Val : Val_Array (1 .. 2);
       end record;

    end Simple.System_A;

    package body Simple.System_A is

       procedure Activate (E : in out A) is
       begin
          E.Val := (others => 0.0);
          Sys_Base (E).Activate;
       end Activate;

       function Value (E : A) return Float is
          pragma Assert (E.Val'Length = 2);
       begin
          return (E.Val (1) + E.Val (2)) / 2.0;
       end Value;

    end Simple.System_A;

    package Simple.System_B is

       type B is new Sys_Base with private;

       overriding procedure Activate (E : in out B);

       overriding function Value (E : B) return Float;

    private

       type B is new Sys_Base with record
          Val : Float;
       end record;

    end Simple.System_B;

    package body Simple.System_B is

       procedure Activate (E : in out B) is
       begin
          E.Val := 0.0;
          Sys_Base (E).Activate;
       end Activate;

       function Value (E : B) return Float is
         (E.Val);

    end Simple.System_B;

    with Simple.System_A; use Simple.System_A;
    with Simple.System_B; use Simple.System_B;

    package Simple.System_AB is

       type AB is new Activation_IF and Value_Retrieval_IF with private;

       overriding procedure Activate (E : in out AB);
       overriding function Is_Active (E : AB) return Boolean;
       overriding procedure Deactivate (E : in out AB);

       overriding function Value (E : AB) return Float;

       not overriding function Check (E : AB) return Boolean;

    private

       type AB is new Activation_IF and Value_Retrieval_IF with record
          SA : A;
          SB : B;
       end record;

    end Simple.System_AB;

    package body Simple.System_AB is

       procedure Activate (E : in out AB) is
       begin
          E.SA.Activate;
          E.SB.Activate;
       end Activate;

       function Is_Active (E : AB) return Boolean is
         (E.SA.Is_Active and E.SB.Is_Active);

       procedure Deactivate (E : in out AB) is
       begin
          E.SA.Deactivate;
          E.SB.Deactivate;
       end Deactivate;

       function Value (E : AB) return Float is
         ((E.SA.Value + E.SB.Value) / 2.0);

       function Check (E : AB) return Boolean is
          Threshold : constant := 0.1;
       begin
          return abs (E.SA.Value - E.SB.Value) < Threshold;
       end Check;

    end Simple.System_AB;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Simple.System_AB; use Simple.System_AB;

    procedure Main is

       procedure Display_Active (E : AB) is
       begin
          if Is_Active (E) then
             Put_Line ("System AB is active");
          else
             Put_Line ("System AB is not active");
          end if;
       end Display_Active;

       procedure Display_Check (E : AB) is
       begin
          if Check (E) then
             Put_Line ("System AB check: PASSED");
          else
             Put_Line ("System AB check: FAILED");
          end if;
       end Display_Check;

       S : AB;
    begin
       Put_Line ("Activating system AB...");
       Activate (S);

       Display_Active (S);
       Display_Check (S);

       Put_Line ("Deactivating system AB...");
       Deactivate (S);

       Display_Active (S);
    end Main;

Further Improvements
--------------------

When analyzing the complete source-code, we see that there are at least two
areas that we could still improve.

Dispatching calls
~~~~~~~~~~~~~~~~~

The first issue concerns the implementation of the :ada:`Activate` procedure
for types derived from :ada:`Sys_Base`. For those derived types, we're
expecting that the :ada:`Activate` procedure of the parent must be called in
the implementation of the overriding :ada:`Activate` procedure. For example:

.. code-block:: ada

    package body Simple.System_A is

       procedure Activate (E : in out A) is
       begin
          E.Val := (others => 0.0);
          Activate (Sys_Base (E));
       end;

If a developer forgets to call that specific :ada:`Activate` procedure,
however, the system won't work as expected. A better strategy could be the
following:

- Declare a new :ada:`Activation_Reset` procedure for :ada:`Sys_Base` type.

- Make a dispatching call to the :ada:`Activation_Reset` procedure in the body
  of the :ada:`Activate` procedure (of the :ada:`Sys_Base` type).

- Let the derived types implement their own version of the
  :ada:`Activation_Reset` procedure.

This is a simplified view of the implementation using the points described
above:

.. code-block:: ada

    package Simple is

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF with
         private;

       not overriding procedure Activation_Reset (E : in out Sys_Base) is abstract;

    end Simple;

    package body Simple is

       procedure Activate (E : in out Sys_Base) is
       begin
          --  NOTE: calling "E.Activation_Reset" does NOT dispatch!
          --        We need to use the 'Class attribute here --- not using this
          --        attribute is an error that will be caught by the compiler.
          Sys_Base'Class (E).Activation_Reset;

          E.Active := True;
       end Activate;

    end Simple;

    package Simple.System_A is

       type A is new Sys_Base with private;

    private

       type Val_Array is array (Positive range <>) of Float;

       type A is new Sys_Base with record
          Val : Val_Array (1 .. 2);
       end record;

       overriding procedure Activation_Reset (E : in out A);

    end Simple.System_A;

    package body Simple.System_A is

       procedure Activation_Reset (E : in out A) is
       begin
          E.Val    := (others => 0.0);
       end Activation_Reset;

    end Simple.System_A;

An important detail is that, in the implementation of :ada:`Activate`, we use
:ada:`Sys_Base'Class` to ensure that the call to :ada:`Activation_Reset` will
dispatch. If we had just written :ada:`E.Activation_Reset` instead, then we
would be calling the :ada:`Activation_Reset` procedure of :ada:`Sys_Base`
itself, which is not what we actually want here. The compiler will catch the
error if you don't do the conversion to the class-wide type, because it would
otherwise be a statically-bound call to an abstract procedure, which is illegal
at compile-time.

Dynamic allocation
~~~~~~~~~~~~~~~~~~

The next area that we could improve is in the declaration of the system AB. In
the previous implementation, we were explicitly describing the two
components of that system, namely a component of type :ada:`A` and a component
of type :ada:`B`:

.. code-block:: ada

    type AB is new Activation_IF and Value_Retrieval_IF with record
       SA : A;
       SB : B;
    end record;

Of course, this declaration matches the system requirements that we presented
in the beginning. However, we could use strategies that make it easier to
incorporate requirement changes later on. For example, we could hide this
information about systems A and B by simply declaring an array of
components of type :ada:`access Sys_Base'Class` and allocate them dynamically
in the body of the package. Naturally, this approach might not be suitable for
certain platforms. However, the advantage would be that, if we wanted to
replace the component of type :ada:`B` by a new component of type :ada:`C`, for
example, we wouldn't need to change the interface. This is how the updated
declaration could look like:

.. code-block:: ada

    type Sys_Base_Class_Access is access Sys_Base'Class;
    type Sys_Base_Array is array (Positive range <>) of Sys_Base_Class_Access;

    type AB is limited new Activation_IF and Value_Retrieval_IF with record
       S_Array : Sys_Base_Array (1 .. 2);
    end record;

.. admonition:: Important

    Note that we're now using the :ada:`limited` keyword in the declaration of
    type :ada:`AB`. That is necessary because we want to prevent objects of
    type :ada:`AB` being copied by assignment, which would lead to two objects
    having the same (dynamically allocated) subsystems A and B internally. This
    change requires that both :ada:`Activation_IF` and
    :ada:`Value_Retrieval_IF` are declared limited as well.

The body of :ada:`Activate` could then allocate those components:

.. code-block:: ada

    procedure Activate (E : in out AB) is
    begin
       E.S_Array := (new A, new B);
       for S of E.S_Array loop
          S.Activate;
       end loop;
    end Activate;

And the body of :ada:`Deactivate` could deallocate them:

.. code-block:: ada

    procedure Deactivate (E : in out AB) is
       procedure Free is
         new Ada.Unchecked_Deallocation (Sys_Base'Class, Sys_Base_Class_Access);
    begin
       for S of E.S_Array loop
          S.Deactivate;
          Free (S);
       end loop;
    end Deactivate;

Limited controlled types
~~~~~~~~~~~~~~~~~~~~~~~~

Another approach that we could use to implement the dynamic allocation of
systems A and B is to declare :ada:`AB` as a limited controlled type |mdash|
based on the :ada:`Limited_Controlled` type of the :ada:`Ada.Finalization`
package.

The :ada:`Limited_Controlled` type includes the following operations:

- :ada:`Initialize`, which is called when objects of a type derived from the
  :ada:`Limited_Controlled` type are being created |mdash| by declaring an
  object of the derived type, for example |mdash|, and

- :ada:`Finalize`, which is called when objects are being destroyed |mdash|
  for example, when an object gets out of scope at the end of a subprogram
  where it was created.

In this case, we must override those procedures, so we can use them for dynamic
memory allocation. This is a simplified view of the update implementation:

.. code-block:: ada

    package Simple.System_AB is

       type AB is limited new Ada.Finalization.Limited_Controlled and
         Activation_IF and Value_Retrieval_IF with private;

       overriding procedure Initialize (E : in out AB);
       overriding procedure Finalize   (E : in out AB);

    end Simple.System_AB;

    package body Simple.System_AB is

       overriding procedure Initialize (E : in out AB) is
       begin
          E.S_Array := (new A, new B);
       end Initialize;

       overriding procedure Finalize   (E : in out AB) is
          procedure Free is
            new Ada.Unchecked_Deallocation (Sys_Base'Class, Sys_Base_Class_Access);
       begin
          for S of E.S_Array loop
             Free (S);
          end loop;
       end Finalize;

    end Simple.System_AB;

Updated source-code
~~~~~~~~~~~~~~~~~~~

Finally, this is the complete updated source-code example:

[Ada]

.. code:: ada run_button project=Courses.Ada_For_Embedded_C_Dev.HandsOnOOP.System_AB_Ada_OOP_2

    package Simple is

       type Activation_IF is limited interface;

       procedure Activate (E : in out Activation_IF) is abstract;
       function Is_Active (E : Activation_IF) return Boolean is abstract;
       procedure Deactivate (E : in out Activation_IF) is abstract;

       type Value_Retrieval_IF is limited interface;

       function Value (E : Value_Retrieval_IF) return Float is abstract;

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF with
         private;

       overriding procedure Activate (E : in out Sys_Base);
       overriding function Is_Active (E : Sys_Base) return Boolean;
       overriding procedure Deactivate (E : in out Sys_Base);

       not overriding procedure Activation_Reset (E : in out Sys_Base) is abstract;

    private

       type Sys_Base is abstract new Activation_IF and Value_Retrieval_IF with
          record
             Active : Boolean;
          end record;

    end Simple;

    package body Simple is

       procedure Activate (E : in out Sys_Base) is
       begin
          --  NOTE: calling "E.Activation_Reset" does NOT dispatch!
          --        We need to use the 'Class attribute:
          Sys_Base'Class (E).Activation_Reset;

          E.Active := True;
       end Activate;

       function Is_Active (E : Sys_Base) return Boolean is
          (E.Active);

       procedure Deactivate (E : in out Sys_Base) is
       begin
          E.Active := False;
       end Deactivate;

    end Simple;

    package Simple.System_A is

       type A is new Sys_Base with private;

       overriding function Value (E : A) return Float;

    private

       type Val_Array is array (Positive range <>) of Float;

       type A is new Sys_Base with record
          Val : Val_Array (1 .. 2);
       end record;

       overriding procedure Activation_Reset (E : in out A);

    end Simple.System_A;

    package body Simple.System_A is

       procedure Activation_Reset (E : in out A) is
       begin
          E.Val := (others => 0.0);
       end Activation_Reset;

       function Value (E : A) return Float is
          pragma Assert (E.Val'Length = 2);
       begin
          return (E.Val (1) + E.Val (2)) / 2.0;
       end Value;

    end Simple.System_A;

    package Simple.System_B is

       type B is new Sys_Base with private;

       overriding function Value (E : B) return Float;

    private

       type B is new Sys_Base with record
          Val : Float;
       end record;

       overriding procedure Activation_Reset (E : in out B);

    end Simple.System_B;

    package body Simple.System_B is

       procedure Activation_Reset (E : in out B) is
       begin
          E.Val := 0.0;
       end Activation_Reset;

       function Value (E : B) return Float is
         (E.Val);

    end Simple.System_B;

    with Ada.Finalization;

    package Simple.System_AB is

       type AB is limited new Ada.Finalization.Limited_Controlled and
         Activation_IF and Value_Retrieval_IF with private;

       overriding procedure Activate (E : in out AB);
       overriding function Is_Active (E : AB) return Boolean;
       overriding procedure Deactivate (E : in out AB);

       overriding function Value (E : AB) return Float;

       not overriding function Check (E : AB) return Boolean;

    private

       type Sys_Base_Class_Access is access Sys_Base'Class;
       type Sys_Base_Array is array (Positive range <>) of Sys_Base_Class_Access;

       type AB is limited new Ada.Finalization.Limited_Controlled and
         Activation_IF and Value_Retrieval_IF with record
          S_Array : Sys_Base_Array (1 .. 2);
       end record;

       overriding procedure Initialize (E : in out AB);
       overriding procedure Finalize   (E : in out AB);

    end Simple.System_AB;

    with Ada.Unchecked_Deallocation;

    with Simple.System_A; use Simple.System_A;
    with Simple.System_B; use Simple.System_B;

    package body Simple.System_AB is

       overriding procedure Initialize (E : in out AB) is
       begin
          E.S_Array := (new A, new B);
       end Initialize;

       overriding procedure Finalize   (E : in out AB) is
          procedure Free is
            new Ada.Unchecked_Deallocation (Sys_Base'Class, Sys_Base_Class_Access);
       begin
          for S of E.S_Array loop
             Free (S);
          end loop;
       end Finalize;

       procedure Activate (E : in out AB) is
       begin
          for S of E.S_Array loop
             S.Activate;
          end loop;
       end Activate;

       function Is_Active (E : AB) return Boolean is
         (for all S of E.S_Array => S.Is_Active);

       procedure Deactivate (E : in out AB) is
       begin
          for S of E.S_Array loop
             S.Deactivate;
          end loop;
       end Deactivate;

       function Value (E : AB) return Float is
         ((E.S_Array (1).Value + E.S_Array (2).Value) / 2.0);

       function Check (E : AB) return Boolean is
          Threshold : constant := 0.1;
       begin
          return abs (E.S_Array (1).Value - E.S_Array (2).Value) < Threshold;
       end Check;

    end Simple.System_AB;

    with Ada.Text_IO;      use Ada.Text_IO;

    with Simple.System_AB; use Simple.System_AB;

    procedure Main is

       procedure Display_Active (E : AB) is
       begin
          if Is_Active (E) then
             Put_Line ("System AB is active");
          else
             Put_Line ("System AB is not active");
          end if;
       end Display_Active;

       procedure Display_Check (E : AB) is
       begin
          if Check (E) then
             Put_Line ("System AB check: PASSED");
          else
             Put_Line ("System AB check: FAILED");
          end if;
       end Display_Check;

       S : AB;
    begin
       Put_Line ("Activating system AB...");
       Activate (S);

       Display_Active (S);
       Display_Check (S);

       Put_Line ("Deactivating system AB...");
       Deactivate (S);

       Display_Active (S);
    end Main;

Naturally, this is by no means the best possible implementation of system AB.
By applying other software design strategies that we haven't covered here, we
could most probably think of different ways to use object-oriented programming
to improve this implementation. Also, in comparison to the
:ref:`original implementation <Ada_For_Embedded_C_Dev_Initial_Translation_To_Ada>`, we recognize that
the amount of source-code has grown. On the other hand, we now have a system
that is factored nicely, and also more extensible.
