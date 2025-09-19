Type System
-------------

.. include:: ../../../global.txt

Strong Typing
~~~~~~~~~~~~~~

One of the main characteristics of Ada is its strong typing (i.e., relative absence of implicit type conversions). This may take some getting used to. For example, you can't divide an integer by a float. You need to perform the division operation using values of the same type, so one value must be explicitly converted to match the type of the other (in this case the more likely conversion is from integer to float). Ada is designed to guarantee that what's done by the program is what's meant by the programmer, leaving as little room for compiler interpretation as possible. Let's have a look at the following example:

[Ada]

.. code-block:: ada

   procedure Strong_Typing is
      Alpha  : Integer := 1;
      Beta   : Integer := 10;
      Result : Float;
   begin
      Result := Float (Alpha) / Float (Beta);
   end Strong_Typing;

[C++]

.. code-block:: cpp

   void weakTyping () {
      int   alpha = 1;
      int   beta = 10;
      float result;

      result = alpha / beta;
   }

[Java]

.. code-block:: java


   void weakTyping () {
      int   alpha = 1;
      int   beta = 10;
      float result;

      result = alpha / beta;
   }

Are the three programs above equivalent? It may seem like Ada is just adding extra complexity by forcing you to make the conversion from Integer to Float explicit. In fact it significantly changes the behavior of the computation. While the Ada code performs a floating point operation :ada:`1.0 / 10.0` and stores 0.1 in :ada:`Result`, the C++ and Java versions instead store 0.0 in :cpp:`result`. This is because the C++ and Java versions perform an integer operation between two integer variables: :cpp:`1 / 10` is :cpp:`0`. The result of the integer division is then converted to a :cpp:`float` and stored. Errors of this sort can be very hard to locate in complex pieces of code, and systematic specification of how the operation should be interpreted helps to avoid this class of errors. If an integer division was actually intended in the Ada case, it is still necessary to explicitly convert the final result to :ada:`Float`:

.. code-block:: ada

   -- Perform an Integer division then convert to Float
   Result := Float (Alpha / Beta);

In Ada, a floating point literal must be written with both an integral and decimal part. :ada:`10` is not a valid literal for a floating point value, while :ada:`10.0` is.

Language-Defined Types
~~~~~~~~~~~~~~~~~~~~~~~~

The principal scalar types predefined by Ada are :ada:`Integer`, :ada:`Float`, :ada:`Boolean`, and :ada:`Character`. These correspond to :cpp:`int`, :cpp:`float`, :cpp:`bool`/:java:`boolean`, and :cpp:`char`, respectively. The names for these types are not reserved words; they are regular identifiers.

Application-Defined Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada's type system encourages programmers to think about data at a high level of abstraction. The compiler will at times output a simple efficient machine instruction for a full line of source code (and some instructions can be eliminated entirely). The careful programmer's concern that the operation really makes sense in the real world would be satisfied, and so would the programmer's concern about performance.

The next example below defines two different metrics: area and distance. Mixing these two metrics must be done with great care, as certain operations do not make sense, like adding an area to a distance. Others require knowledge of the expected semantics; for example, multiplying two distances. To help avoid errors, Ada requires that each of the binary operators :ada:`+`, :ada:`-`, :ada:`*`, and :ada:`/` for integer and floating-point types take operands of the same type and return a value of that type.

.. code-block:: ada

   procedure Main is
      type Distance is new Float;
      type Area is new Float;

      D1 : Distance := 2.0;
      D2 : Distance := 3.0;
      A  : Area;
   begin
      D1 := D1 + D2;        -- OK
      D1 := D1 + A;         -- NOT OK: incompatible types for "+" operator
      A  := D1 * D2;        -- NOT OK: incompatible types for ":=" assignment
      A  := Area (D1 * D2); -- OK
   end Main;

Even though the :ada:`Distance` and :ada:`Area` types above are just :ada:`Float`\s, the compiler does not allow arbitrary mixing of values of these different types. An explicit conversion (which does not necessarily mean any additional object code) is necessary.

The predefined Ada rules are not perfect; they admit some problematic cases (for example multiplying two :ada:`Distance`\s yields a :ada:`Distance`) and prohibit some useful cases (for example multiplying two :ada:`Distance`\s should deliver an :ada:`Area`). These situations can be handled through other mechanisms. A predefined operation can be identified as :ada:`abstract` to make it unavailable; overloading can be used to give new interpretations to existing operator symbols, for example allowing an operator to return a value from a type different from its operands; and more generally, GNAT has introduced a facility that helps perform dimensionality checking.

Ada enumerations work similarly to C++ and Java's :cpp:`enum`\s.

[Ada]

.. code-block:: ada

   type Day is
     (Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday);

[C++]

.. code-block:: cpp

   enum Day {
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday};

[Java]

.. code-block:: java

   enum Day {
      Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday}

But even though such enumerations may be implemented using a machine word, at the language level Ada will not confuse the fact that :ada:`Monday` is a :ada:`Day` and is not an :ada:`Integer`. You can compare a :ada:`Day` with another :ada:`Day`, though. To specify implementation details like the numeric values that correspond with enumeration values in C++ you include them in the original :cpp:`enum` statement:

[C++]

.. code-block:: cpp

   enum Day {
      Monday    = 10,
      Tuesday   = 11,
      Wednesday = 12,
      Thursday  = 13,
      Friday    = 14,
      Saturday  = 15,
      Sunday    = 16};

But in Ada you must use both a type definition for :ada:`Day` as well as a separate *representation clause* for it like:

[Ada]

.. code-block:: ada

   for Day use
     (Monday    => 10,
      Tuesday   => 11,
      Wednesday => 12,
      Thursday  => 13,
      Friday    => 14,
      Saturday  => 15,
      Sunday    => 16);

Type Ranges
~~~~~~~~~~~~

Contracts can be associated with types and variables, to refine values and define what are considered valid values. The most common kind of contract is a *range constraint* introduced with the :ada:`range` reserved word, for example:

.. code-block:: ada

   procedure Main is
      type Grade is range 0 .. 100;

      G1, G2  : Grade;
      N       : Integer;
   begin
      ...                -- Initialization of N
      G1 := 80;          -- OK
      G1 := N;           -- Illegal (type mismatch)
      G1 := Grade (N);   -- Legal, run-time range check
      G2 := G1 + 10;     -- Legal, run-time range check
      G1 := (G1 + G2)/2; -- Legal, run-time range check
   end Main;

In the above example, :ada:`Grade` is a new integer type associated with a range check. Range checks are dynamic and are meant to enforce the property that no object of the given type can have a value outside the specified range. In this example, the first assignment to :ada:`G1` is correct and will not raise a run-time exceprion. Assigning :ada:`N` to :ada:`G1` is illegal since :ada:`Grade` is a different type than :ada:`Integer`. Converting :ada:`N` to :ada:`Grade` makes the assignment legal, and a range check on the conversion confirms that the value is within :ada:`0 .. 100`.  Assigning :ada:`G1+10` to :ada:`G2` is legal since :ada:`+` for :ada:`Grade` returns a :ada:`Grade` (note that the literal :ada:`10` is interpreted as a :ada:`Grade` value in this context), and again there is a range check.

The final assignment illustrates an interesting but subtle point. The subexpression :ada:`G1 + G2` may be outside the range of :ada:`Grade`, but the final result will be in range. Nevertheless, depending on the representation chosen for :ada:`Grade`, the addition may overflow. If the compiler represents :ada:`Grade` values as signed 8-bit integers (i.e., machine numbers in the range :ada:`-128 .. 127`) then the sum :ada:`G1+G2` may exceed 127, resulting in an integer overflow. To prevent this, you can use explicit conversions and perform the computation in a sufficiently large integer type, for example:

.. code-block:: ada

      G1 := Grade ((Integer (G1) + Integer (G2)) / 2);

Range checks are useful for detecting errors as early as possible. However, there may be some impact on performance. Modern compilers do know how to remove redundant checks, and you can deactivate these checks altogether if you have sufficient confidence that your code will function correctly.

Types can be derived from the representation of any other type. The new derived type can be associated with new constraints and operations. Going back to the :ada:`Day` example, one can write:

.. code-block:: ada

   type Business_Day is new Day range Monday .. Friday;
   type Weekend_Day is new Day range Saturday .. Sunday;

Since these are new types, implicit conversions are not allowed. In this case, it's more natural to create a new set of constraints for the same type, instead of making completely new ones. This is the idea behind *subtypes* in Ada. A subtype is a type with optional additional constraints. For example:

.. code-block:: ada

   subtype Business_Day is Day range Monday .. Friday;
   subtype Weekend_Day is Day range Saturday .. Sunday;
   subtype Dice_Throw is Integer range 1 .. 6;

These declarations don't create new types, just new names for constrained ranges of their base types.

Generalized Type Contracts: Subtype Predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Range checks are a special form of type contracts; a more general method is provided by Ada subtype predicates, introduced in Ada 2012. A subtype predicate is a boolean expression defining conditions that are required for a given type or subtype. For example, the :ada:`Dice_Throw` subtype shown above can be defined in the following way:

.. code-block:: ada

   subtype Dice_Throw is Integer
      with Dynamic_Predicate => Dice_Throw in 1 .. 6;

The clause beginning with :ada:`with` introduces an Ada *aspect*, which is additional information provided for declared entities such as types and subtypes. The :ada:`Dynamic_Predicate` aspect is the most general form. Within the predicate expression, the name of the (sub)type refers to the current value of the (sub)type. The predicate is checked on assignment, parameter passing, and in several other contexts. There is a :ada:`Static_Predicate` form which introduce some optimization and constrains on the form of these predicates, outside of the scope of this document.

Of course, predicates are useful beyond just expressing ranges. They can be used to represent types with arbitrary constraints, in particular types with discontinuities, for example:

.. code-block:: ada

   type Not_Null is new Integer
      with Dynamic_Predicate => Not_Null /= 0;

   type Even is new Integer
      with Dynamic_Predicate => Even mod 2 = 0;


Attributes
~~~~~~~~~~~~

Attributes start with a single apostrophe ("tick"), and they allow you to query properties of, and perform certain actions on, declared entities such as types, objects, and subprograms. For example, you can determine the first and last bounds of scalar types, get the sizes of objects and types, and convert values to and from strings. This section provides an overview of how attributes work. For more information on the many attributes defined by the language, you can refer directly to the Ada Language Reference Manual.

The :ada:`'Image` and :ada:`'Value` attributes allow you to transform a scalar value into a :ada:`String` and vice-versa. For example:

.. code-block:: ada

   declare
      A : Integer := 99;
   begin
      Put_Line (Integer'Image (A));
      A := Integer'Value ("99");
   end;

Certain attributes are provided only for certain kinds of types. For example, the :ada:`'Val` and :ada:`'Pos` attributes for an enumeration type associates a discrete value with its position among its peers. One circuitous way of moving to the next character of the ASCII table is:

[Ada]

.. code-block:: ada

   declare
      C : Character := 'a';
   begin
      C := Character'Val (Character'Pos (C) + 1);
   end;

A more concise way to get the next value in Ada is to use the :ada:`'Succ` attribute:

.. code-block:: ada

   declare
      C : Character := 'a';
   begin
      C := Character'Succ (C);
   end;

You can get the previous value using the :ada:`'Pred` attribute. Here is the equivalent in C++ and Java:

[C++]

.. code-block:: cpp

   char c = 'a';
   c++;

[Java]

.. code-block:: java

   char c = 'a';
   c++;

Other interesting examples are the :ada:`'First` and :ada:`'Last` attributes which, respectively, return the first and last values of a scalar type. Using 32-bit integers, for instance, :ada:`Integer'First` returns -2\ :sup:`31` and :ada:`Integer'Last` returns 2\ :sup:`31` - 1.

Arrays and Strings
~~~~~~~~~~~~~~~~~~~~~

C++ arrays are pointers with offsets, but the same is not the case for Ada and Java. Arrays in the latter two languages are not interchangable with operations on pointers, and array types are considered first-class citizens. Arrays in Ada have dedicated semantics such as the availability of the array's boundaries at run-time. Therefore, unhandled array overflows are impossible unless checks are suppressed. Any discrete type can serve as an array index, and you can specify both the starting and ending bounds |mdash| the lower bound doesn't necessarily have to be 0. Most of the time, array types need to be explicitly declared prior to the declaration of an object of that array type.

Here's an example of declaring an array of 26 characters, initializing the values from ``'a'`` to ``'z'``:

[Ada]

.. code-block:: ada

   declare
      type Arr_Type is array (Integer range <>) of Character;
      Arr : Arr_Type (1 .. 26);
      C : Character := 'a';
   begin
      for I in Arr'Range loop
         Arr (I) := C;
         C := Character'Succ (C);
      end loop;
   end;

[C++]

.. code-block:: cpp

   char Arr [26];
   char C = 'a';

   for (int I = 0; I < 26; ++I) {
      Arr [I] = C;
      C = C + 1;
   }

[Java]

.. code-block:: java

   char [] Arr = new char [26];
   char C = 'a';

   for (int I = 0; I < Arr.length; ++I) {
      Arr [I] = C;
      C = C + 1;
   }

In C++ and Java, only the size of the array is given during declaration. In Ada, array index ranges are specified using two values of a discrete type. In this example, the array type declaration specifies the use of Integer as the index type, but does not provide any constraints (use :ada:`<>`, pronounced *box*, to specify "no constraints").  The constraints are defined in the object declaration to be 1 to 26, inclusive. Arrays have an attribute called :ada:`'Range`. In our example, :ada:`Arr'Range` can also be expressed as :ada:`Arr'First .. Arr'Last`; both expressions will resolve to :ada:`1 .. 26`. So the :ada:`'Range` attribute supplies the bounds for our :ada:`for` loop. There is no risk of stating either of the bounds incorrectly, as one might do in C++ where :cpp:`I <= 26` may be specified as the end-of-loop condition.

As in C++, Ada :ada:`String`\s are arrays of :ada:`Character`\s. The C++ or Java :cpp:`String` class is the equivalent of the Ada type :ada:`Ada.Strings.Unbounded_String` which offers additional capabilities in exchange for some overhead. Ada strings, importantly, are not delimited with the special character :cpp:`\'\\0\'` like they are in C++. It is not necessary because Ada uses the array's bounds to determine where the string starts and stops.

Ada's predefined :ada:`String` type is very straighforward to use:

.. code-block:: ada

   My_String : String (1 .. 26);

Unlike C++ and Java, Ada does not offer escape sequences such as :cpp:`\'\\n\'`. Instead, explicit values from the :ada:`ASCII` package must be concatenated (via the concatenation operator, :ada:`&`). Here for example, is how to initialize a line of text ending with a new line:

.. code-block:: ada

   My_String : String := "This is a line with a end of line" & ASCII.LF;

You see here that no constraints are necessary for this variable definition. The initial value given allows the automatic determination of :ada:`My_String`'s bounds.

Ada offers high-level operations for copying, slicing, and assigning values to arrays. We'll start with assignment. In C++ or Java, the assignment operator doesn't make a copy of the value of an array, but only copies the address or reference to the target variable. In Ada, the actual array contents are duplicated. To get the above behavior, actual pointer types would have to be defined and used.

[Ada]

.. code-block:: ada

   declare
      type Arr_Type is array (Integer range <>) of Integer;
      A1 : Arr_Type (1 .. 2);
      A2 : Arr_Type (1 .. 2);
   begin
      A1 (1) := 0;
      A1 (2) := 1;

      A2 := A1;
   end;

[C++]

.. code-block:: cpp

   int A1 [2];
   int A2 [2];

   A1 [0] = 0;
   A1 [1] = 1;

   for (int i = 0; i < 2; ++i) {
      A2 [i] = A1 [i];
   }


[Java]

.. code-block:: java

   int [] A1 = new int [2];
   int [] A2 = new int [2];

   A1 [0] = 0;
   A1 [1] = 1;

   A2 = Arrays.copyOf(A1, A1.length);

In all of the examples above, the source and destination arrays must have precisely the same number of elements. Ada allows you to easily specify a portion, or slice, of an array. So you can write the following:

[Ada]

.. code-block:: ada

   declare
      type Arr_Type is array (Integer range <>) of Integer;
      A1 : Arr_Type (1 .. 10);
      A2 : Arr_Type (1 .. 5);
   begin
      A2 (1 .. 3) := A1 (4 .. 6);
   end;

This assigns the 4th, 5th, and 6th elements of :ada:`A1` into the 1st, 2nd, and 3rd elements of :ada:`A2`. Note that only the length matters here: the values of the indexes don't have to be equal; they slide automatically.

Ada also offers high level comparison operations which compare the contents of arrays as opposed to their addresses:

[Ada]

.. code-block:: ada

   declare
      type Arr_Type is array (Integer range <>) of Integer;
      A1 : Arr_Type (1 .. 2);
      A2 : Arr_Type (1 .. 2);
   begin
      if A1 = A2 then

[C++]

.. code-block:: cpp

   int A1 [2];
   int A2 [2];

   bool eq = true;

   for (int i = 0; i < 2; ++i) {
      if (A1 [i] != A2 [i]) {
         eq = false;
      }
   }

   if (eq) {


[Java]

.. code-block:: java

   int [] A1 = new int [2];
   int [] A2 = new int [2];

   if (Arrays.equals (A1, A2)) {

You can assign to all the elements of an array in each language in different ways. In Ada, the number of elements to assign can be determined by looking at the right-hand side, the left-hand side, or both sides of the assignment. When bounds are known on the left-hand side, it's possible to use the :ada:`others` expression to define a default value for all the unspecified array elements. Therefore, you can write:

.. code-block:: ada

   declare
      type Arr_Type is array (Integer range <>) of Integer;
      A1 : Arr_Type := (1, 2, 3, 4, 5, 6, 7, 8, 9);
      A2 : Arr_Type (-2 .. 42) := (others => 0);
   begin
      A1 := (1, 2, 3, others => 10);

      -- use a slice to assign A2 elements 11 .. 19 to 1
      A2 (11 .. 19) := (others => 1);
   end;

Heterogeneous Data Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Ada, there's no distinction between :cpp:`struct` and :cpp:`class` as there is in C++. All heterogeneous data structures are :ada:`record`\s. Here are some simple records:

[Ada]

.. code-block:: ada

   declare
      type R is record
         A, B : Integer;
         C    : Float;
      end record;

      V : R;
   begin
      V.A := 0;
   end;


[C++]

.. code-block:: cpp

   struct R {
      int A, B;
      float C;
   };

   R V;
   V.A = 0;

[Java]

.. code-block:: java

   class R {
      public int A, B;
      public float C;
   }

   R V = new R ();
   V.A = 0;

Ada allows specification of default values for fields just like C++ and Java. The values specified can take the form of an ordered list of values, a named list of values, or an incomplete list followed by :ada:`others => <>` to specify that fields not listed will take their default values. For example:

.. code-block:: ada

   type R is record
      A, B : Integer := 0;
      C    : Float := 0.0;
   end record;

   V1 : R := (1, 2, 1.0);
   V2 : R := (A => 1, B => 2, C => 1.0);
   V3 : R := (C => 1.0, A => 1, B => 2);
   V4 : R := (C => 1.0, others => <>);

Pointers
~~~~~~~~~~

Pointers, references, and access types differ in significant ways across the languages that we are examining. In C++, pointers are integral to a basic understanding of the language, from array manipulation to proper declaration and use of function parameters. In Java, direct pointer manipulation is abstracted by the Java runtime. And in Ada, direct pointer manipulation is possible, but unlike C++, they are not required for basic usage with arrays and parameter passing.

We'll continue this section by explaining the difference between objects allocated on the stack and objects allocated on the heap using the following example:

[Ada]

.. code-block:: ada

   declare
      type R is record
         A, B : Integer;
      end record;

      V1, V2 : R;
   begin
      V1.A := 0;
      V2 := V1;
      V2.A := 1;
   end;

[C++]

.. code-block:: cpp

   struct R {
      int A, B;
   };

   R V1, V2;
   V1.A = 0;
   V2 = V1;
   V2.A = 1;

[Java]

.. code-block:: java

   public class R {
      public int A, B;
   }

   R V1, V2;
   V1 = new R ();
   V1.A = 0;
   V2 = V1;
   V2.A = 1;

There's a fundamental difference between the Ada and C++ semantics above and the semantics for Java. In Ada and C++, objects are allocated on the stack and are directly accessed. :ada:`V1` and :ada:`V2` are two different objects and the assignment statement copies the value of :ada:`V1` into :ada:`V2`. In Java, :java:`V1` and :java:`V2` are two *references* to objects of class :java:`R`. Note that when :java:`V1` and :java:`V2` are declared, no actual object of class :java:`R` yet exists in memory: it has to be allocated later with the :java:`new` allocator operator. After the assignment :java:`V2 = V1`, there's only one :java:`R` object in memory: the assignment is a reference assignment, not a value assignment. At the end of the Java code, :java:`V1` and :java:`V2` are two references to the same objects and the :java:`V2.A = 1` statement changes the field of that one object, while in the Ada and the C++ case :ada:`V1` and :ada:`V2` are two distinct objects.

To obtain similar behavior in Ada, you can use pointers. It can be done through Ada's *access type*:

[Ada]

.. code-block:: ada

   declare
      type R is record
         A, B : Integer;
      end record;
      type R_Access is access R;

      V1 : R_Access;
      V2 : R_Access;
   begin
      V1 := new R;
      V1.A := 0;
      V2 := V1;
      V2.A := 1;
   end;

[C++]

.. code-block:: cpp

   struct R {
      int A, B;
   };

   R * V1, * V2;
   V1 = new R ();
   V1->A = 0;
   V2 = V1;
   V2->A = 0;

For those coming from the Java world: there's no garbage collector in Ada, so objects allocated by the :ada:`new` operator need to be expressly freed.

Dereferencing is performed automatically in certain situations, for instance when it is clear that the type required is the dereferenced object rather than the pointer itself, or when accessing record members via a pointer. To explicitly dereference an access variable, append :ada:`.all`. The equivalent of :cpp:`V1->A` in C++ can be written either as :ada:`V1.A` or :ada:`V1.all.A`.

Pointers to scalar objects in Ada and C++ look like:

[Ada]

.. code-block:: ada

   procedure Main is
      type A_Int is access Integer;
      Var : A_Int := new Integer;
   begin
      Var.all := 0;
   end Main;

[C++]

.. code-block:: cpp

   int main (int argc, char *argv[]) {
     int * Var = new int;
     *Var = 0;
   }

An initializer can be specified with the allocation by appending ``'(value)``:

.. code-block:: ada

   Var : A_Int := new Integer'(0);

When using Ada pointers to reference objects on the stack, the referenced objects must be declared as being :ada:`aliased`. This directs the compiler to implement the object using a memory region, rather than using registers or eliminating it entirely via optimization. The access type needs to be declared as either :ada:`access all` (if the referenced object needs to be assigned to) or :ada:`access constant` (if the referenced object is a constant). The :ada:`'Access` attribute works like the C++ :cpp:`&` operator to get a pointer to the object, but with a "scope accessibility" check to prevent references to objects that have gone out of scope. For example:

[Ada]

.. code-block:: ada

   type A_Int is access all Integer;
   Var : aliased Integer;
   Ptr : A_Int := Var'Access;

[C++]

.. code-block:: cpp

   int Var;
   int * Ptr = &Var;

To deallocate objects from the heap in Ada, it is necessary to use a deallocation subprogram that accepts a specific access type. A generic procedure is provided that can be customized to fit your needs |mdash| it's called :ada:`Ada.Unchecked_Deallocation`. To create your customized deallocator (that is, to instantiate this generic), you must provide the object type as well as the access type as follows:

[Ada]

.. code-block:: ada

   with Ada.Unchecked_Deallocation;
   procedure Main is
      type Integer_Access is access all Integer;
      procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
      My_Pointer : Integer_Access := new Integer;
   begin
      Free (My_Pointer);
   end Main;

[C++]

.. code-block:: cpp

   int main (int argc, char *argv[]) {
     int * my_pointer = new int;
     delete my_pointer;
   }
