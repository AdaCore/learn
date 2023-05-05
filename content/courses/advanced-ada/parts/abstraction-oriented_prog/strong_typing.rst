Strong typing
=============

.. include:: ../../../global.txt

In this chapter, we discuss the advantages of strong typing and how it can
be used to avoid common implementation and maintenance issues.

Type-based security
-------------------

.. note::

    This section was originally written by Yannick Moy and published as
    `Gem #82: Type-Based Security 1 <https://www.adacore.com/gems/gem-82>`_ and
    `Gem #83: Type-Based Security 2 <https://www.adacore.com/gems/gem-83>`_.

The notions of tainted data and trusted data usually refer to data coming
from the user vs. data coming from the application. Tainting is viral, in
that any result of a computation where one of the operands is tainted
becomes tainted too.

Various C/C++ static analyzers provide checkers for tainted data that help
find bugs where data from the user serves to compute the size of an
allocation, so that an attacker could use this to trigger a buffer
overflow leading to an Elevation of Privilege (EoP) attack.

In Ada, the compiler can provide the guarantee that no such bugs have been
introduced by accident (although you can still bypass the rule if you
really want to, for example by using :ada:`Unchecked_Conversion` or
address clause overlays), provided different types are used for tainted
and trusted data, with no run-time penalty. This can be done with many
types of data, including basic types like integers.

Let's say tainted data is of an integer type. The basic idea is to derive
the trusted type from the tainted one, and to provide a function Value to
get to the raw data inside a trusted value, like the following:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint

    package Taint is

       type Trusted_Value is new Integer;

       function Value (V : Trusted_Value)
                       return Integer;
       pragma Inline (Value);

    end Taint;

Notice that the implementation of :ada:`Value` is just a type conversion:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint

    package body Taint is

       function Value (V : Trusted_Value)
                       return Integer is
       begin
          return Integer (V);
       end Value;

    end Taint;

Then, make sure the sensitive program uses trusted data:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint

    with Taint; use Taint;

    procedure Sensitive (X : Trusted_Value) is
    begin
       null; --  Do something sensitive with value X
    end Sensitive;

Let's try to pass in data from the user to the sensitive program:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint
    :class: ada-expect-compile-error

    with Taint;
    with Sensitive;

    procedure Main is

       procedure Bad (Some_Value : Integer) is
       begin
          Sensitive (Some_Value);
       end Bad;

       A : Integer := 0;
    begin
       Bad (A);
    end Main;

The compiler returns with a type error.

Now, this does not prevent us from doing useful computations on trusted
data as easily as on tainted data, including initialization with literals,
case statements, array indexing, etc.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint

    with Taint; use Taint;
    with Sensitive;

    procedure Main is
       Max_Value : constant := 100;
       X : Trusted_Value := Max_Value;
    begin
       X := X + 1; --  Perform any computations on X
       Sensitive (X);
    end Main;

Because :ada:`Trusted_Value` is a type derived from the tainted type
(:ada:`Integer`), all operations allowed on tainted data are also allowed
on trusted data, but operations mixing them are not allowed.

Be aware that nothing prevents the program itself from converting between
tainted data and trusted data freely, but this requires inserting an
explicit conversion, which can be spotted during code reviews.

To completely prevent such unintended conversions (say, to facilitate
maintenance), the type used for trusted data must be made private, so that
only the package which defines it can convert to and from it. With
:ada:`Trusted_Value` being private, we should also provide a corresponding
function for each literal which we used previously, as well as the
operations that we'd like to allow on trusted values (note that for
efficiency all operations could be inlined):

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint_2

    package Taint is

       type Trusted_Value is private;

       function Value (V : Trusted_Value)
                       return Integer;

       function Trusted_1 return Trusted_Value;
       function Trusted_100 return Trusted_Value;

       function "+" (V, W : Trusted_Value)
                     return Trusted_Value;

    private

       type Trusted_Value is new Integer;

    end Taint;

The new implementation is as expected:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint_2

    package body Taint is

       function Value (V : Trusted_Value)
                       return Integer is
       begin
          return Integer (V);
       end Value;

       function Trusted_1 return Trusted_Value is
       begin
          return 1;
       end Trusted_1;

       function Trusted_100 return Trusted_Value is
       begin
          return 100;
       end Trusted_100;

       function "+" (V, W : Trusted_Value)
                     return Trusted_Value is
       begin
          return Trusted_Value (Integer (V) +
                                Integer (W));
       end "+";

    end Taint;

Of course, the client now needs to be adapted to this new interface:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Taint_2

    with Taint; use Taint;
    procedure Sensitive (X : Trusted_Value);

    procedure Sensitive (X : Trusted_Value) is
    begin
       --  Missing implementation!
       null;
    end Sensitive;

    with Taint; use Taint;
    with Sensitive;

    procedure Good is
       X : Trusted_Value := Trusted_100;
    begin
       X := X + Trusted_1;
       --  ^^^^^^^^^^^^^^^
       --  Perform any computations on X

       Sensitive (X);
    end Good;

That's it! No errors can result in tainted data being accidentally passed
by the user where trusted data is expected, and future maintainers of the
code won't be tempted to insert conversions when the compiler complains.

Input validation consists of checking a set of properties on the input
which guarantee it is well-formed. This usually involves excluding a set
of ill-formed inputs (black-list) or matching the input against an
exhaustive set of well-formed patterns (white-list).

Here, we consider the task of validating an input for inclusion in an SQL
command. This is a well-known defense against SQL injection attacks, where
an attacker passes in a specially crafted string that is interpreted as a
command rather than a plain string when executing the initial SQL command.

The basic idea is to define a new type :ada:`SQL_Input` derived from type
:ada:`String`. Function :ada:`Validate` checks that the input is properly
validated and fails if not. Function :ada:`Valid_String` returns the raw
data inside a validated string, as follows:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.SQL_Input

    package Inputs is

       type SQL_Input is new String;

       function Validate (Input : String)
                          return SQL_Input;

       function Valid_String (Input : SQL_Input)
                              return String;

    end Inputs;

The implementation of :ada:`Validate` simply checks that the input string
does not contain a dangerous character before returning it as an
:ada:`SQL_Input`, while :ada:`Valid_String` is a simple type conversion:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.SQL_Input

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;

    package body Inputs is

       Dangerous_Characters : constant
         Character_Set := To_Set ("""*^';&><</");

       function Validate (Input : String)
                          return SQL_Input is
       begin
          if Index (Input,
                    Dangerous_Characters) /= 0
          then
             raise Constraint_Error
               with "Invalid input "
                    & Input
                    & " for an SQL query ";
          else
             return SQL_Input (Input);
          end if;
       end Validate;

       function Valid_String (Input : SQL_Input)
                              return String is
       begin
          return String (Input);
       end Valid_String;

    end Inputs;

Now, this does not prevent future uses of such type conversions in the
program, whether malicious or unintended. To guard against such
possibilities, we must make type :ada:`SQL_Input` private. To make sure we
do not ourselves inadvertently convert an input string into a valid one in
the implementation of package :ada:`Inputs`, we use this opportunity to
make :ada:`SQL_Input` a discriminated record parameterized by the
validation status.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.SQL_Input

    with Ada.Strings.Unbounded;
    use  Ada.Strings.Unbounded;

    package Inputs is

       type SQL_Input (<>) is private;

       function Validate (Input : String)
                          return SQL_Input;

       function Valid_String (Input : SQL_Input)
                              return String;

       function Is_Valid (Input : SQL_Input)
                          return Boolean;

    private

       type SQL_Input (Validated : Boolean) is
          record
             case Validated is
                when True =>
                   Valid_Input : Unbounded_String;
                when False =>
                   Raw_Input   : Unbounded_String;
             end case;
          end record;

    end Inputs;

Each time we access field :ada:`Valid_Input`, a discriminant check will be
performed to ensure that the operand of type :ada:`SQL_Input` has been
validated. Observe the use of :ada:`Unbounded_String` for the type of the
input component, which is more convenient and flexible than using a
constrained string.

Note in the implementation of :ada:`Validate`, that instead of raising an
exception when the string cannot be validated, as in the first
implementation, here we create corresponding validated or invalid input
values based on the result of the check against dangerous characters.
Also, an :ada:`Is_Valid` function has been added to allow clients to query
validity of an :ada:`SQL_Input` value.

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.SQL_Input

    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    with Ada.Strings.Maps;  use Ada.Strings.Maps;

    package body Inputs is

       Dangerous_Characters : constant
         Character_Set := To_Set ("""*^';&><</");

       function Validate (Input : String)
                          return SQL_Input is
          Local_Input : constant Unbounded_String :=
                          To_Unbounded_String (Input);
       begin
          if Index (Input,
                    Dangerous_Characters) /= 0
          then
             return (Validated   => False,
                     Raw_Input   => Local_Input);
          else
             return (Validated   => True,
                     Valid_Input => Local_Input);
          end if;
       end Validate;

       function Valid_String (Input : SQL_Input)
                              return String is
       begin
          return To_String (Input.Valid_Input);
       end Valid_String;

       function Is_Valid (Input : SQL_Input)
                          return Boolean is
       begin
          return Input.Validated;
       end Is_Valid;

    end Inputs;

That's it! As long as this interface is used, no errors can result in
improper input being interpreted as a command, while ensuring that future
maintainers of the code won't inadvertently be able to insert
inappropriate conversions.

Of course, this minimal interface does not really provide anything other
than the validation of the input. Simply having an :ada:`Is_Valid`
function to tell whether a string is valid input data would seem to give
you much the same functionality. However, you can now safely extend this
package with additional capabilities, such as transformations on valid SQL
inputs (for example, to optimize queries before sending them to the
database), or to resolve queries faster using a local cache, and so forth.
By using the private encapsulation, you are guaranteed that no client
package will tamper with the validity of the SQL inputs you are
manipulating.

Incidentally, the similar but distinct problem of input sanitization,
where possibly invalid data is transformed into something that is known
valid prior to use, can be handled in the same way.

Example: Table access
---------------------

In this section, we discuss an application that accesses a two-dimensional
table. We first look into a typical implementation, and then discuss how
to improve it with better use of strong typing.

Typical implementation
~~~~~~~~~~~~~~~~~~~~~~

Let's look at an application that declares a two-dimensional lookup table,
retrieves a value from it an displays this value.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Table_Access_1

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Tab_Access is

       Tab : array (1 .. 5, 1 .. 10) of Float
         := ((0.50, 0.73, 0.22, 0.66, 0.64,
              0.20, 0.73, 0.22, 0.66, 0.64),
             (0.60, 0.23, 0.56, 0.27, 0.72,
              0.36, 0.27, 0.18, 0.18, 0.08),
             (0.20, 0.56, 0.74, 0.43, 0.72,
              0.19, 0.46, 0.45, 0.25, 0.49),
             (0.75, 0.88, 0.29, 0.08, 0.17,
              0.96, 0.23, 0.83, 0.89, 0.97),
             (0.18, 0.97, 0.82, 0.86, 0.96,
              0.24, 0.84, 0.83, 0.14, 0.26));

       X, Y : Positive;
       V    : Float;

    begin
       X := 1;
       Y := 5;
       V := Tab (X, Y);

       Put_Line (Float'Image (V));
    end Show_Tab_Access;

In this application, we use :ada:`X` and :ada:`Y` as indices to access the
:ada:`Tab` table. We store the value in :ada:`V` and display it.

In principle, there is nothing wrong with this implementation. Also, we're
already making use of strong typing here, since accessing an invalid
position of the array (say :ada:`Tab (6, 25)`) raises an exception.
However, in this application, we're assuming that :ada:`X` always refers
to the first dimension, while :ada:`Y` refers to the second dimension.
What happens, however, if we write :ada:`Tab (Y, X)`? In the application
above, this would still work because :ada:`Tab (5, 1)` is in the table's
range. Even though this works fine here, it's not the expected behavior.
In the next section, we'll look into strategies to make better use of
strong typing to avoid this problem.

One could argue that the problem we've just described doesn't happen to
competent developers, who are expected to be careful. While this might be
true for the simple application we're discussing here, complex systems
can be much more complicated to understand: they might include multiple
tables and multiple indices for example. In this case, even competent
developers might make use of wrong indices to access tables. Fortunately,
Ada provides means to avoid this problem.


Using stronger typing
~~~~~~~~~~~~~~~~~~~~~

In the example above, we make use of the :ada:`Positive` type, which is
already a constrained type: we're avoiding accessing the :ada:`Tab` table
using an index with negative values or zero. But we still may use indices
that are out-of-range in the positive range, or switch the indices, as in
the :ada:`Tab (Y, X)` example we mentioned previously. These problems can
be avoided by defining range types for each dimension. This is the updated
implementation:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Table_Access_2

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Tab_Access is

       type X_Range is range 1 .. 5;
       type Y_Range is range 1 .. 10;

       Tab : array (X_Range, Y_Range) of Float
         := ((0.50, 0.73, 0.22, 0.66, 0.64,
              0.20, 0.73, 0.22, 0.66, 0.64),
             (0.60, 0.23, 0.56, 0.27, 0.72,
              0.36, 0.27, 0.18, 0.18, 0.08),
             (0.20, 0.56, 0.74, 0.43, 0.72,
              0.19, 0.46, 0.45, 0.25, 0.49),
             (0.75, 0.88, 0.29, 0.08, 0.17,
              0.96, 0.23, 0.83, 0.89, 0.97),
             (0.18, 0.97, 0.82, 0.86, 0.96,
              0.24, 0.84, 0.83, 0.14, 0.26));

       X : X_Range;
       Y : Y_Range;
       V : Float;

    begin
       X := 1;
       Y := 5;
       V := Tab (X, Y);

       Put_Line (Float'Image (V));
    end Show_Tab_Access;

Now, we not only avoid mistakes like :ada:`Tab (Y, X)`, but we also detect
them at compile time! This might decrease development time, since we don't
need to run the application in order to check for those issues.

Also, maintenance becomes easier as well. Because we're explicitly stating
the allowed ranges for :ada:`X` and :ada:`Y`, developers can know how to
avoid constraint issues when accessing the :ada:`Tab` table. We're also
formally indicating the expected behavior. For example, because we declare
:ada:`X` to be of :ada:`X_Range` type, and that type is used in the first
dimension of :ada:`Tab`, we're documenting |mdash| using the syntax of the
Ada language |mdash| that :ada:`X` is supposed to be used to access the
first dimension of :ada:`Tab`. Based on this information, developers that
need to maintain this application can immediately identify the purpose of
:ada:`X` and use the variable accordingly.


Example: Multiple indices
-------------------------

In this section, we discuss another example where the use of strong typing
is relevant. Let's consider an application with the following
requirements:

- The application receives the transmission of chunks of information.

  - Each chunk contains two floating-point coefficients.

  - Also, these chunks are received out of order, so that the chunk itself
    includes an index indicating its position in an ordered array.

- The application also receives a list of indices for the ordered array
  of chunks. This list |mdash| a so-called *selector* |mdash| is used to
  select two chunks from the array of ordered chunks.

- Due to external constraints, the application shall use the unordered
  array; creating an array of ordered chunks shall be avoided.

  - A function that returns an ordered array of chunks shall be available
    for testing purposes only.

  - A function that returns the selected chunks shall be available for
    testing purposes only.

  - A function that returns a mapping from the index of ordered chunks to
    the index of unordered chunks must be available.

For example, consider the following picture containing input chunks and a
selector:

.. graphviz:: strong_typing_graph_01.dot

By using the mapping, we can select the correct chunks from the input
(unordered) chunks. Also, we may create an array of ordered chunks for
testing purposes.

Let's skip the discussion whether the design used in this application is
good or not and assume that all requirements listed above are set on stone
and can't be changed.


Typical implementation
~~~~~~~~~~~~~~~~~~~~~~

This is a typical specification of the main package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering

    package Indirect_Ordering is

       type Chunk is record
          V1  : Float;
          V2  : Float;
          Idx : Positive;
       end record;

       type Selector is
         array (1 .. 2) of Positive;

       type Mapping is
         array (Positive range <>) of Positive;

       type Chunks is
         array (Positive range <>) of Chunk;

       function Get_Mapping (C : Chunks)
                             return Mapping;

    end Indirect_Ordering;

    package body Indirect_Ordering is

       function Get_Mapping (C : Chunks)
                             return Mapping is
       begin
          return Map : Mapping (C'Range) do
             for J in C'Range loop
                Map (C (J).Idx) := J;
             end loop;
          end return;
       end Get_Mapping;

    end Indirect_Ordering;

And this is a typical specification of the :ada:`Test` child package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering

    package Indirect_Ordering.Test is

       function Get_Ordered_Chunks (C : Chunks)
                                    return Chunks;

       function Get_Selected_Chunks (C : Chunks;
                                     S : Selector)
                                     return Chunks;

    end Indirect_Ordering.Test;

    package body Indirect_Ordering.Test is

       function Get_Ordered_Chunks (C : Chunks)
                                    return Chunks
       is
          Map : constant Mapping := Get_Mapping (C);
       begin
          return OC : Chunks (C'Range) do
             for I in OC'Range loop
                OC (I) := C (Map (I));
             end loop;
          end return;
       end Get_Ordered_Chunks;

       function Get_Selected_Chunks (C : Chunks;
                                     S : Selector)
                                     return Chunks
       is
          Map : constant Mapping := Get_Mapping (C);
       begin
          return SC : Chunks (S'Range) do
             for I in S'Range loop
                SC (I) := C (Map (S (I)));
             end loop;
          end return;
       end Get_Selected_Chunks;

    end Indirect_Ordering.Test;

Note that the information transmitted to the application might be
inconsistent due to errors in the transmission channel. For example, the
information from :ada:`Idx` (:ada:`Chunk` record) might be wrong. In a
real-world application, we should deal with those transmission errors.
However, for the discussion in this section, these problems are not
crucial, so that we can simplify the implementation by skipping error
handling.

Let's finally look at a test application that makes use of the package
we've just implemented. In order to simplify the discussion, we'll
initialize the array containing the unordered chunks and the selector
directly in the application instead of receiving input data from an
external source.

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering

    with Indirect_Ordering; use Indirect_Ordering;

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Indirect_Ordering is

       function Init_Chunks return Chunks is
          C : Chunks (1 .. 4);
       begin
          C (1) := (V1  => 0.70,
                    V2  => 0.72,
                    Idx => 3);
          C (2) := (V1  => 0.20,
                    V2  => 0.15,
                    Idx => 1);
          C (3) := (V1  => 0.40,
                    V2  => 0.74,
                    Idx => 2);
          C (4) := (V1  => 0.80,
                    V2  => 0.26,
                    Idx => 4);

          return C;
       end Init_Chunks;

       C  : Chunks            := Init_Chunks;
       S  : constant Selector := (2, 3);
       M  : constant Mapping  := Get_Mapping (C);

    begin
       --  Loop over selector using original chunks
       for I in S'Range loop
          declare
             C1 : Chunk := C (M (S (I)));
          begin
             Put_Line ("Selector #"
                       & Positive'Image (I)
                       & ": V1 = "
                       & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;

    end Show_Indirect_Ordering;

In this line of the test application, we retrieve the chunk using the
index from the selector:

.. code-block:: ada

    C1 : Chunk := C (M (S (I)));

Because :ada:`C` contains the unordered chunks and the index from :ada:`S`
refers to the ordered chunks, we need to map between the *ordered index*
and the *unordered index*. This is achieved by the mapping stored in
:ada:`M`.

If we'd use the ordered array of chunks, we could use the index from
:ada:`S` directly, as illustrated in the following procedure:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering

    with Indirect_Ordering;
    use  Indirect_Ordering;

    with Indirect_Ordering.Test;
    use  Indirect_Ordering.Test;

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Ordered_Chunk (C : Chunks;
                                     S : Selector)
    is
       OC : Chunks := Get_Ordered_Chunks (C);
    begin
       --  Loop over selector using ordered chunks
       for I in S'Range loop
          declare
             C1 : Chunk := OC (S (I));
          begin
             Put_Line ("Selector #"
                       & Positive'Image (I)
                       & ": V1 = "
                       & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;
    end Display_Ordered_Chunk;

In this relatively simple application, we're already dealing with 3
indices:

- The index of the unordered chunks.

- The index of the ordered chunks.

- The index of the selector array.

The use of the wrong index to access an array can be a common source of
issues. This becomes even more problematic when the application is
extended and new features are implemented: the amount of arrays might
increase and developers need to be especially careful not to use the
wrong index.

For example, a mistake that developers can make when using the package
above is to skip the mapping and access the array of unordered chunks
directly with the index from the selector |mdash| i.e. :ada:`C (S (I))` in
the test application above. Detecting this mistake requires extensive testing
and debugging, since both the array of unordered chunks and the array of
ordered chunks have the same range, so the corresponding indices can be
used interchangeably without raising constraint exceptions, even though
the behavior is not correct. Fortunately, we can use Ada's strong typing
to detect such issues in an early stage of the development.


Using stronger typing
~~~~~~~~~~~~~~~~~~~~~

In the previous implementation, we basically used the :ada:`Positive` type
for all indices. We can, however, declare individual types for each index
of the application. This is the updated specification of the main package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    package Indirect_Ordering is

       type Chunk_Index     is new Positive;
       type Ord_Chunk_Index is new Chunk_Index;

       type Chunk is record
          V1  : Float;
          V2  : Float;
          Idx : Ord_Chunk_Index;
       end record;

       type Selector_Index is range 1 .. 2;

       type Selector is
         array (Selector_Index) of Ord_Chunk_Index;

       type Mapping is
         array (Ord_Chunk_Index range <>) of
           Chunk_Index;

       type Chunks is
         array (Chunk_Index range <>) of Chunk;

       function Get_Mapping (C : Chunks)
                            return Mapping;

    end Indirect_Ordering;

By declaring these new types, we can avoid using the wrong index.
Moreover, we're documenting |mdash| using the syntax provided by the
language |mdash| which index is expected in each array or function from
the package.
This allows for better understanding of the package specification and
makes maintenance easier, as well as it helps when implementing new
features for the package.

This is the updated specification of the :ada:`Test` child package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    package Indirect_Ordering.Test is

       pragma Assertion_Policy
         (Dynamic_Predicate => Check);

       type Ord_Chunks is
         array (Ord_Chunk_Index range <>) of Chunk
           with Dynamic_Predicate =>
             (for all I in Ord_Chunks'Range =>
                Ord_Chunks (I).Idx = I);

       type Sel_Chunks is
         array (Selector_Index) of Chunk;

       function Get_Ordered_Chunks
         (C : Chunks)
          return Ord_Chunks;

       function Get_Selected_Chunks
         (C : Chunks;
          S : Selector)
          return Sel_Chunks;

    end Indirect_Ordering.Test;

Note that we also declared a separate type for the array of ordered
chunks: :ada:`Ord_Chunks`. This is needed because the arrays uses a
different index (:ada:`Ord_Chunk_Index`) and therefore can't be the same
type as :ada:`Chunks`. For the same reason, we declared a separate type
for the array of selected chunks: :ada:`Sel_Chunks`.

As a side note, we're now able to include a :ada:`Dynamic_Predicate` to
:ada:`Ord_Chunks` that verifies that the index stored in the each chunk
matches the corresponding index of its position in the ordered array.

We also had to add a new private package that includes a function that
retrieves the range of an array of :ada:`Chunk` type |mdash| which are of
:ada:`Chunk_Index` type |mdash| and converts the range using the
:ada:`Ord_Chunk_Index` type.

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    private package Indirect_Ordering.Cnvt is

       type Ord_Chunk_Range is record
          First : Ord_Chunk_Index;
          Last  : Ord_Chunk_Index;
       end record;

       function Get_Ord_Chunk_Range
         (C : Chunks)
          return Ord_Chunk_Range is
            ((Ord_Chunk_Index (C'First),
              Ord_Chunk_Index (C'Last)));

    end Indirect_Ordering.Cnvt;

This is needed for example in the :ada:`Get_Mapping` function, which has
to deal with indices of these two types. Although this makes the code a
little bit more verbose, it helps documenting the expected types in that
function.

This is the corresponding update to the body of the main package:

.. code:: ada no_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    with Indirect_Ordering.Cnvt;
    use  Indirect_Ordering.Cnvt;

    package body Indirect_Ordering is

       function Get_Mapping (C : Chunks)
                             return Mapping is
          R : constant Ord_Chunk_Range :=
                Get_Ord_Chunk_Range (C);
       begin
          return Map : Mapping (R.First .. R.Last) do
             for J in C'Range loop
                Map (C (J).Idx) := J;
             end loop;
          end return;
       end Get_Mapping;

    end Indirect_Ordering;

This is the corresponding update to the body of the :ada:`Test` child
package:

.. code:: ada compile_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    with Indirect_Ordering.Cnvt;
    use  Indirect_Ordering.Cnvt;

    package body Indirect_Ordering.Test is

       function Get_Ordered_Chunks
         (C : Chunks)
          return Ord_Chunks
       is
          Map : constant Mapping := Get_Mapping (C);
          R   : constant Ord_Chunk_Range :=
                  Get_Ord_Chunk_Range (C);
       begin
          return OC : Ord_Chunks (R.First .. R.Last)
          do
             for I in OC'Range loop
                OC (I) := C (Map (I));
             end loop;
          end return;
       end Get_Ordered_Chunks;

       function Get_Selected_Chunks
         (C : Chunks;
          S : Selector)
          return Sel_Chunks
       is
          Map : constant Mapping := Get_Mapping (C);
       begin
          return SC : Sel_Chunks do
             for I in S'Range loop
                SC (I) := C (Map (S (I)));
             end loop;
          end return;
       end Get_Selected_Chunks;

    end Indirect_Ordering.Test;

This is the updated test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Abstraction-Oriented_Prog.Strong_Typing.Indirect_Ordering_2

    with Indirect_Ordering; use Indirect_Ordering;

    with Ada.Text_IO; use  Ada.Text_IO;

    procedure Show_Indirect_Ordering is

       function Init_Chunks return Chunks is
          C : Chunks (1 .. 4);
       begin
          C (1) := (V1  => 0.70,
                    V2  => 0.72,
                    Idx => 3);
          C (2) := (V1  => 0.20,
                    V2  => 0.15,
                    Idx => 1);
          C (3) := (V1  => 0.40,
                    V2  => 0.74,
                    Idx => 2);
          C (4) := (V1  => 0.80,
                    V2  => 0.26,
                    Idx => 4);

          return C;
       end Init_Chunks;

       C  : Chunks            := Init_Chunks;
       S  : constant Selector := (2, 3);
       M  : constant Mapping  := Get_Mapping (C);

    begin
       --  Loop over selector using original chunks
       for I in S'Range loop
          declare
             C1 : Chunk := C (M (S (I)));
          begin
             Put_Line ("Selector #"
                       & Selector_Index'Image (I)
                       & ": V1 = "
                       & Float'Image (C1.V1));
          end;
       end loop;
       New_Line;

    end Show_Indirect_Ordering;

Apart from minor changes, the test application is basically still the
same. However, if we now change the following line:

.. code-block:: ada

    C1 : Chunk := C (M (S (I)));

to

.. code-block:: ada

    C1 : Chunk := C (S (I));

The compiler will gives us an error, telling us that it expected the
:ada:`Chunk_Index` type, but found the :ada:`Ord_Chunk_Index` instead.
By using Ada's strong typing, we're detecting issues at compile time
instead of having to rely on extensive testing and debugging to detect
them. Basically, this eliminates a whole category of potential bugs
and reduces development time. At the same time, we're improving the
documentation of the source-code and facilitating further improvements
to the application.


Discriminants
-------------

.. admonition:: Relevant topics

    - discriminants in the context of strong typing

.. todo::

    Complete section!
