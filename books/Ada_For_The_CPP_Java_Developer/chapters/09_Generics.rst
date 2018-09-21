Generics
----------

Ada, C++, and Java all have support for generics or templates, but on different sets of language entities. A C++ template can be applied to a class or a function. So can a Java generic. An Ada generic can be either a package or a subprogram.

Generic Subprograms
~~~~~~~~~~~~~~~~~~~~~

A feature that is similar across all three languages is the subprogram. To swap two objects:

[Ada]

.. code-block:: ada

   generic
      type A_Type is private;
   procedure Swap (Left, Right : in out A_Type) is
      Temp : A_Type := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;

[C++]

.. code-block:: cpp

   template <class AType>
   AType swap (AType & left, AType & right) {
      AType temp = left;
      left  = right;
      right = temp;
   }

[Java]

.. code-block:: java

   public <AType> void swap (AType left, AType right) {
      AType temp = left;
      left  = right;
      right = temp;
   }

And examples of using these:

[Ada]

.. code-block:: ada

   declare
      type R is record
         F1, F2 : Integer;
      end record;

      procedure Swap_R is new Swap (R);
      A, B : R;
   begin
      ...
      Swap_R (A, B);
   end;

[C++]

.. code-block:: cpp

   class R {
      public:
         int f1, f2;
   };

   R a, b;
   ...
   swap (a, b);

[Java]

.. code-block:: java

   public class R {
      public int f1, f2;
   }

   R a = new R(), b = new R();
   ...
   swap (a, b);

The C++ template and Java generic both become usable once defined. The Ada generic needs to be explicitly instantiated using a local name and the generic's parameters.

Generic Packages
~~~~~~~~~~~~~~~~~

Next, we're going to create a generic unit containing data and subprograms. In Java or C++, this is done through a class, while in Ada, it's a `generic package'. The Ada and C++ model is fundamentally different from the Java model. Indeed, upon instantiation, Ada and C++ generic data are duplicated; that is, if they contain global variables (Ada) or static attributes (C++), each instance will have its own copy of the variable, properly typed and independent from the others. In Java, generics are only a mechanism to have the compiler do consistency checks, but all instances are actually sharing the same data where the generic parameters are replaced by *java.lang.Object*. Let's look at the following example:

[Ada]

.. code-block:: ada

     generic
        type T is private;
     package Gen is
        type C is tagged record
           V : T;
        end record;

        G : Integer;
     end Gen;

[C++]

.. code-block:: cpp

   template <class T>
   class C{
      public:
        T v;
        static int G;
   };


[Java]

.. code-block:: java

   public class C <T> {
        public T v;
        public static int G;
   }

In all three cases, there's an instance variable (*v*) and a static variable (*G*). Let's now look at the behavior (and syntax) of these three instantiations:


[Ada]

.. code-block:: ada

   declare
      package I1 is new Gen (Integer);
      package I2 is new Gen (Integer);
      subtype Str10 is String (1..10);
      package I3 is new Gen (Str10);
   begin
      I1.G := 0;
      I2.G := 1;
      I3.G := 2;
   end;

[C++]

.. code-block:: cpp

   C <int>::G = 0;
   C <int>::G = 1;
   C <char *>::G = 2;


[Java]

.. code-block:: java

   C.G = 0;
   C.G = 1;
   C.G = 2;

In the Java case, we access the generic entity directly without using a parametric type. This is because there's really only one instance of *C*, with each instance sharing the same global variable *G*. In C++, the instances are implicit, so it's not possible to create two different instances with the same parameters. The first two assignments are manipulating the same global while the third one is manipulating a different instance. In the Ada case, the three instances are explicitly created, named, and referenced individually.

Generic Parameters
~~~~~~~~~~~~~~~~~~~~

Ada offers a wide variety of generic parameters which is difficult to translate into other languages. The parameters used during instantiation---and as a consequence those on which the generic unit may rely on---may be variables, types, or subprograms with certain properties. For example, the following provides a sort algorithm for any kind of array:

.. code-block:: ada

   generic
      type Component is private;
      type Index is (<>);
      with function "<" (Left, Right : Component) return Boolean;
      type Array_Type is array (Index range <>) of Component;
   procedure Sort (A : in out Array_Type);

The above declaration states that we need a type (*Component*), a discrete type (*Index*), a comparison subprogram (*"<"*), and an array definition (*Array_Type*). Given these, it's possible to write an algorithm that can sort any *Array_Type*. Note the usage of the **with** reserved word in front of the function name, to differentiate between the generic parameter and the beginning of the generic subprogram.

Here is a non-exhaustive overview of the kind of constraints that can be put on types:

.. code-block:: ada

   type T is private; -- T is a constrained type, such as Integer
   type T (<>) is private; -- T can be an unconstrained type, such as String
   type T is tagged private; -- T is a tagged type
   type T is new T2 with private; -- T is an extension of T2
   type T is (<>); -- T is a discrete type
   type T is range <>; -- T is an integer type
   type T is digits <>; -- T is a floating point type
   type T is access T2; -- T is an access type, T2 is its designated type
