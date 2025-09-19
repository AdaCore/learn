Classes and Object Oriented Programming
-----------------------------------------

.. include:: ../../../global.txt

Primitive Subprograms
~~~~~~~~~~~~~~~~~~~~~~

Primitive subprograms in Ada are basically the subprograms that are eligible for inheritance / derivation. They are the equivalent of C++ member functions and Java instance methods. While in C++ and Java these subprograms are located within the nested scope of the type, in Ada they are simply declared in the same scope as the type. There's no syntactic indication that a subprogram is a primitive of a type.

The way to determine whether :ada:`P` is a primitive of a type :ada:`T` is if

    #. it is declared in the same scope as :ada:`T`, and
    #. it contains at least one parameter of type :ada:`T`, or returns a result of type :ada:`T`.

In C++ or Java, the self reference :cpp:`this` is implicitly declared. It may need to be explicitly stated in certain situations, but usually it's omitted. In Ada the self-reference, called the *controlling parameter*, must be explicitly specified in the subprogram parameter list. While it can be any parameter in the profile with any name, we'll focus on the typical case where the first parameter is used as the :cpp:`self` parameter. Having the controlling parameter listed first also enables the use of OOP prefix notation which is convenient.

A :cpp:`class` in C++ or Java corresponds to a :ada:`tagged type` in Ada. Here's an example of the declaration of an Ada tagged type with two parameters and some dispatching and non-dispatching primitives, with equivalent examples in C++ and Java:

[Ada]

.. code-block:: ada

   type T is tagged record
      V, W : Integer;
   end record;

   type T_Access is access all T;

   function F (V : T) return Integer;

   procedure P1 (V : access T);

   procedure P2 (V : T_Access);

[C++]

.. code-block:: cpp

   class T {
      public:
         int V, W;

         int F ();

         void P1 ();
   };

   void P2 (T * v);

[Java]

.. code-block:: java

   public class T {
         public int V, W;

         public int F () {};

         public void P1 () {};

         public static void P2 (T v) {};
   }

Note that :ada:`P2` is not a primitive of :ada:`T` |mdash| it does not have any parameters of type :ada:`T`. Its parameter is of type :ada:`T_Access`, which is a different type.

Once declared, primitives can be called like any subprogram with every necessary parameter specified, or called using prefix notation.  For example:

[Ada]

.. code-block:: ada

   declare
      V : T;
   begin
      V.P1;
   end;

[C++]

.. code-block:: cpp

   {
     T v;
     v.P1 ();
   }

[Java]

.. code-block:: java

   {
     T v = new T ();
     v.P1 ();
   }

Derivation and Dynamic Dispatch
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Despite the syntactic differences, derivation in Ada is similar to derivation (inheritance) in C++ or Java. For example, here is a type hierarchy where a child class overrides a method and adds a new method:

[Ada]

.. code-block:: ada

   type Root is tagged record
      F1 : Integer;
   end record;

   procedure Method_1 (Self : Root);

   type Child is new Root with record
      F2 : Integer;
   end record;

   overriding
   procedure Method_1 (Self : Child);

   procedure Method_2 (Self : Child);


[C++]

.. code-block:: cpp

   class Root {
      public:
         int f1;
         virtual void method1 ();
   };

   class Child : public Root {
      public:
         int f2;
         virtual void method1 ();
         virtual void method2 ();
   };

[Java]

.. code-block:: java

   public class Root {
      public int f1;
      public void method1 ();
   }

   public class Child extends Root {
      public int f2;
      @Override
      public void method1 ();
      public void method2 ();
   }

Like Java, Ada primitives on tagged types are always subject to dispatching; there is no need to mark them :cpp:`virtual`. Also like Java, there's an optional keyword :ada:`overriding` to ensure that a method is indeed overriding something from the parent type.

Unlike many other OOP languages, Ada differentiates between a reference to a specific tagged type, and a reference to an entire tagged type hierarchy. While :ada:`Root` is used to mean a specific type, :ada:`Root'Class` |mdash| a class-wide type |mdash| refers to either that type or any of its descendants. A method using a parameter of such a type cannot be overridden, and must be passed a parameter whose type is of any of :ada:`Root`'s descendants (including :ada:`Root` itself).

Next, we'll take a look at how each language finds the appropriate method to call within an OO class hierarchy; that is, their dispatching rules. In Java, calls to non-private instance methods are always dispatching. The only case where static selection of an instance method is possible is when calling from a method to the :java:`super` version.

In C++, by default, calls to virtual methods are always dispatching. One common mistake is to use a by-copy parameter hoping that dispatching will reach the real object. For example:

.. code-block:: cpp

   void proc (Root p) {
      p.method1 ();
   }

   Root * v = new Child ();

   proc (*v);


In the above code, :cpp:`p.method1()` will not dispatch. The call to :cpp:`proc` makes a copy of the :cpp:`Root` part of :cpp:`v`, so inside :cpp:`proc`, :cpp:`p.method1()` refers to the :cpp:`method1()` of the root object. The intended behavior may be specified by using a reference instead of a copy:

.. code-block:: cpp

   void proc (Root & p) {
      p.method1 ();
   }

   Root * v = new Child ();

   proc (*v);

In Ada, tagged types are always passed by reference but dispatching only occurs on class-wide types. The following Ada code is equivalent to the latter C++ example:

.. code-block:: ada

   declare
      procedure Proc (P : Root'Class) is
      begin
         P.Method_1;
      end;

      type Root_Access is access all Root'Class;
      V : Root_Access := new Child;
   begin
      Proc (V.all);
   end;

Dispatching from within primitives can get tricky. Let's consider a call to :ada:`Method_1` in the implementation of :ada:`Method_2`. The first implementation that might come to mind is:

.. code-block:: ada

   procedure Method_2 (P : Root) is
   begin
      P.Method_1;
   end;

However, :ada:`Method_2` is called with a parameter that is of the definite type :ada:`Root`. More precisely, it is a definite view of a child. So, this call is not dispatching; it will always call :ada:`Method_1` of :ada:`Root` even if the object passed is a child of :ada:`Root`. To fix this, a view conversion is necessary:

.. code-block:: ada

   procedure Method_2 (P : Root) is
   begin
      Root'Class (P).Method_1;
   end;

This is called "redispatching." Be careful, because this is the most common mistake made in Ada when using OOP. In addition, it's possible to convert from a class wide view to a definite view, and to select a given primitive, like in C++:

[Ada]

.. code-block:: ada

   procedure Proc (P : Root'Class) is
   begin
      Root (P).Method_1;
   end;

[C++]

.. code-block:: cpp

   void proc (Root & p) {
      p.Root::method1 ();
   }

Constructors and Destructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada does not have constructors and destructors in quite the same way as C++ and Java, but there is analagous functionality in Ada in the form of default initialization and finalization.

Default initialization may be specified for a record component and will occur if a variable of the record type is not assigned a value at initialization. For example:

.. code-block:: ada

   type T is tagged record
      F : Integer := Compute_Default_F;
   end record;

   function Compute_Default_F return Integer is
   begin
      Put_Line ("Compute");
      return 0;
   end Compute_Default_F;

   V1 : T;
   V2 : T := (F => 0);

In the declaration of :ada:`V1`, :ada:`T.F` receives a value computed by the subprogram :ada:`Compute_Default_F`. This is part of the default initialization. :ada:`V2` is initialized manually and thus will not use the default initialization.

For additional expressive power, Ada provides a type called :ada:`Ada.Finalization.Controlled` from which you can derive your own type. Then, by overriding the :ada:`Initialize` procedure you can create a constructor for the type:

.. code-block:: ada

   type T is new Ada.Finalization.Controlled with record
      F : Integer;
   end record;

   procedure Initialize (Self : in out T) is
   begin
      Put_Line ("Compute");
      Self.F := 0;
   end Initialize;

   V1 : T;
   V2 : T := (F => 0);

Again, this default initialization subprogram is only called for :ada:`V1`; :ada:`V2` is initialized manually. Furthermore, unlike a C++ or Java constructor, :ada:`Initialize` is a normal subprogram and does not perform any additional initialization such as calling the parent's initialization routines.

When deriving from :ada:`Controlled`, it's also possible to override the subprogram :ada:`Finalize`, which is like a destructor and is called for object finalization. Like :ada:`Initialize`, this is a regular subprogram. Do not expect any other finalizers to be automatically invoked for you.

Controlled types also provide functionality that essentially allows overriding the meaning of the assignment operation, and are useful for defining types that manage their own storage reclamation (for example, implementing a reference count reclamation strategy).

Encapsulation
~~~~~~~~~~~~~~~

While done at the class level for C++ and Java, Ada encapsulation occurs at the package level and targets all entities of the language, as opposed to only methods and attributes. For example:

[Ada]

.. code-block:: ada

   package Pck is
      type T is tagged private;
      procedure Method1 (V : T);
   private
      type T is tagged record
         F1, F2 : Integer;
      end record;
      procedure Method2 (V : T);
   end Pck;

[C++]

.. code-block:: cpp

   class T {
      public:
         virtual void method1 ();
      protected:
         int f1, f2;
         virtual void method2 ();
   };

[Java]

.. code-block:: java

   public class T {
      public void method1 ();
      protected int f1, f2;
      protected void method2 ();
   }

The C++ and Java code's use of :cpp:`protected` and the Ada code's use of :ada:`private` here demonstrates how to map these concepts between languages. Indeed, the private part of an Ada child package would have visibility of the private part of its parents, mimicking the notion of :cpp:`protected`. Only entities declared in the package body are completely isolated from access.

Abstract Types and Interfaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada, C++ and Java all offer similar functionality in terms of abstract classes, or pure virtual classes. It is necessary in Ada and Java to explicitly specify whether a tagged type or class is :ada:`abstract`, whereas in C++ the presence of a pure virtual function implicitly makes the class an abstract base class. For example:

[Ada]

.. code-block:: ada

  package P is

      type T is abstract tagged private;

      procedure Method (Self : T) is abstract;
   private
      type T is abstract tagged record
         F1, F2 : Integer;
      end record;

   end P;

[C++]

.. code-block:: cpp

   class T {
      public:
         virtual void method () = 0;
      protected:
         int f1, f2;
   };


[Java]

.. code-block:: java

   public abstract class T {
      public abstract void method1 ();
      protected int f1, f2;
   };

All abstract methods must be implemented when implementing a concrete type based on an abstract type.

Ada doesn't offer multiple inheritance the way C++ does, but it does support a Java-like notion of interfaces. An interface is like a C++ pure virtual class with no attributes and only abstract members. While an Ada tagged type can inherit from at most one tagged type, it may implement multiple interfaces. For example:

[Ada]

.. code-block:: ada

   type Root is tagged record
      F1 : Integer;
   end record;
   procedure M1 (Self : Root);

   type I1 is interface;
   procedure M2 (Self : I1) is abstract;

   type I2 is interface;
   procedure M3 (Self : I2) is abstract;

   type Child is new Root and I1 and I2 with record
      F2 : Integer;
   end record;

   -- M1 implicitly inherited by Child
   procedure M2 (Self : Child);
   procedure M3 (Self : Child);

[C++]

.. code-block:: cpp

   class Root {
      public:
         virtual void M1();
         int f1;
   };

   class I1 {
      public:
         virtual void M2 () = 0;
   };

   class I2 {
      public:
         virtual void M3 () = 0;
   };

   class Child : public Root, I1, I2 {
      public:
         int f2;
         virtual void M2 ();
         virtual void M3 ();
   };

[Java]

.. code-block:: java

   public class Root {
      public void M1();
      public int f1;
   }

   public interface I1 {
      public void M2 ();
   }

   public interface I2 {
      public void M3 ();
   }

   public class Child extends Root implements I1, I2 {
         public int f2;
         public void M2 ();
         public void M3 ();
   }

Invariants
~~~~~~~~~~~~

.. todo::
  *This section is not part of the OOP material and should be moved to a different chapter*


Any private type in Ada may be associated with a :ada:`Type_Invariant` contract. An invariant is a property of a type that must always be true after the return from of any of its primitive subprograms. (The invariant might not be maintained during the execution of the primitive subprograms, but will be true after the return.) Let's take the following example:

.. code-block:: ada

   package Int_List_Pkg is

      type Int_List (Max_Length : Natural) is private
        with Type_Invariant => Is_Sorted (Int_List);

      function Is_Sorted (List : Int_List) return Boolean;

      type Int_Array is array (Positive range <>) of Integer;

      function To_Int_List (Ints : Int_Array) return Int_List;

      function To_Int_Array (List : Int_List) return Int_Array;

      function "&" (Left, Right : Int_List) return Int_List;

      ... -- Other subprograms
   private

      type Int_List (Max_Length : Natural) is record
         Length : Natural;
         Data   : Int_Array (1..Max_Length);
      end record;


      function Is_Sorted (List : Int_List) return Boolean is
         (for all I in List.Data'First .. List.Length-1 =>
               List.Data (I) <= List.Data (I+1));

   end Int_List_Pkg;

   package body Int_List_Pkg is

      procedure Sort (Ints : in out Int_Array) is
      begin
         ... Your favorite sorting algorithm
      end Sort;

      function To_Int_List (Ints : Int_Array) return Int_List is
         List : Int_List :=
          (Max_Length => Ints'Length,
           Length     => Ints'Length,
           Data       => Ints);
      begin
         Sort (List.Data);
         return List;
      end To_Int_List;

      function To_Int_Array (List : Int_List) return Int_Array is
      begin
         return List.Data;
      end To_Int_Array;

      function "&" (Left, Right : Int_List) return Int_List is
         Ints : Int_Array := Left.Data & Right.Data;
      begin
         Sort (Ints);
         return To_Int_List (Ints);
      end "&";

      ... -- Other subprograms
   end Int_List_Pkg;

The :ada:`Is_Sorted` function checks that the type stays consistent. It will be called at the exit of every primitive above. It is permissible if the conditions of the invariant aren't met during execution of the primitive. In :ada:`To_Int_List` for example, if the source array is not in sorted order, the invariant will not be satisfied at the "begin",  but it will be checked at the end.
