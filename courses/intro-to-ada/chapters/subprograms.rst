Subprograms
===========

:code-config:`reset_accumulator=True`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++


Subprograms
-----------

So far, we have used procedures extensively, mostly to have a main body of code
to execute, and we have also seen a function or two. Those entities are
collectively known as *subprograms*.

There are two kinds of subprograms in Ada, *functions* and *procedures*. The
distinction between the two is that a function returns a value, and a procedure
does not.

.. code:: ada no_button

    package Week_3 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       function Get_Workload (Day : Days) return Natural;
       --  We declare (but don't define) a function with one
       --  parameter, returning a Natural value (a non-negative Integer)
    end Week_3;

As we saw earlier in the packages section, if you want to declare a subprogram
in a package, and have that subprogram available to be invoked from client
("with"ing) units, you need to do two things:

* Put its specification (name, parameters, result type if a function) in the
  package specifciation, along with any comments / documentation you wish to
  provide

* Put the full declaration of the subprogram (its body, or implementation) in
  the package body

Subprograms in Ada can, of course, have parameters. One syntactically important
note is that a subprogram which has no parameters does not have a parameter
section at all, for example:

.. code-block:: ada

   procedure Proc;

   function Func return Integer;

Here's another variation on the Week example:

.. code:: ada no_button

    package Week_4 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       function Get_Day_Name
          (Day : Days := Monday) return String;
       --                               ^ We can return any type,
       --                                 even indefinite ones
       --             ^ Default value for parameter
    end Week_4;

This example illustrates several points:

- Parameters can have default values. When calling the subprogram, you can
  then omit parameters if they have a default value. Unlike C/C++, a call to
  a subprogram without parameters does not include parentheses.

- The return type of a function can be any type; a function can return a value
  whose size is unknown at compile time. Likewise the parameters can be of
  any type.

.. admonition:: In other languages

    Returning variable size objects in languages lacking a garbage collector is
    a bit complicated implementation-wise, which is why C and C++ don't allow
    it, prefering to depend on explicit dynamic allocation / free from the user.

    The problem is that explicit storage management is unsafe as soon as you
    want to collect unused memory. Ada's ability to return variable size
    objects will remove one use case for dynamic allocation, and hence, remove
    one potential source of bugs from your programs.

    Rust follows the C/C++ model, but with safe pointer semantics.
    However, dynamic allocation is still used. Ada can benefit from
    an eventual performance edge because it can use any model.

    .. amiard: TODO: say less or say more

As we showed briefly above, if a subprogram declaration appears in a package
declaration then a subprogram body needs to be supplied in the package body.
For the ``Week`` package above, we could have the following body:

.. code:: ada no_button

    package body Week_4 is
       --  Implementation of the Get_Day_Name function
       function Get_Day_Name (Day : Days := Monday) return String is
       begin
          return
            (case Day is
             when Monday => "Monday",
             when Tuesday => "Tuesday",
             when Wednesday => "Wednesday",
             when Thursday => "Thursday",
             when Friday => "Friday",
             when Saturday => "Saturday",
             when Sunday => "Sunday");
       end Get_Day_Name;
    end Week_4;

(This example is for illustrative purposes only.  There is a built-in mechanism,
the 'Image attribute for scalar types, that returns the name (as a String) of
any element of an enumeration type.  For example Days'Image(Monday) is "MONDAY".)


Subprogram calls
~~~~~~~~~~~~~~~~

We can then call our subprogram this way:

.. code:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Week_4;

    procedure Show_Days is
    begin
       Put_Line (Week_4.Get_Day_Name);
       --             ^ Parameterless call, value of Day parameter is Monday
       for Day in Week_4.Days loop
          Put_Line (Week_4.Get_Day_Name (Day));
          --                           ^ Regular parameter passing
       end loop;

       Put_Line (Week_4.Get_Day_Name (Day => Week_4.Friday));
       --                           ^ Named parameter passing
    end Show_Days;

Ada allows you to name the parameters when you pass them, whether they have a
default or not. There are some rules:

- Positional parameters come first.
- A positional parameter cannot follow a named parameter.

.. ?? I don't understand the following sentence.  If the param has a
.. ?? default value then you can omit the parameter, it has nothing
.. ?? to do with the use of positional versus named

As a convention, people usually name parameters at the call site if the
function's corresponding parameters has a default value. However, it is also
perfectly acceptable to name every parameter if it makes the code clearer.

.. code:: ada no_button

    package Week_5 is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday, Saturday, Sunday);

       type Language is (English, Italian);

       function Get_Day_Name
         (Day : Days; Lang : Language := English) return String;
    end Week_5;

    with Week_5; use Week_5;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       Put_Line (Get_Day_Name (Monday, Lang => Italian));
    end Main;

Function calls
~~~~~~~~~~~~~~

An important feature of function calls in Ada is that the return value at a
call cannot be ignored; that is, a function call cannot be used as a statement.

If you want to call a function and do not need its result, you will still need
to explicitly store it in a local variable.

.. code:: ada
    :class: ada-expect-compile-error

    function Quadruple (I : Integer) return Integer is
        function Double (I : Integer) return Integer is
        begin
           return I * 2;
        end Double;

       Res : Integer := Double (Double (I));
       --               ^ Calling the double function
    begin
       Double (I);
       --  ERROR: cannot use call to function "Double" as a statement

       return Res;
    end Quadruple;

.. admonition:: In GNAT

    In GNAT, with all warnings activated, it becomes even harder to ignore the
    result of a function, because unused variables will be flagged. For
    example, this code would not be valid:

.. ?? This example might be confusing since out parameters have not been covered.
.. ?? It would be better to show an example where the function's side effect is on
.. ?? a non-local variable.  Maybe for the next version of the course.


    .. code:: ada
        :class: ada-syntax-only

        function Read_Int
           (Stream : Network_Stream; Result : out Integer) return Boolean;

        procedure Main is
            Stream : Network_Stream := Get_Stream;
            My_Int : Integer;
            B : Boolean := Read_Int (Stream, My_Int);  -- Warning here, B is never read
        begin
           null;
        end Main;

    You then have two solutions to silence this warning:

    - Either annotate the variable with pragma Unreferenced, thus:

    .. code-block:: ada

        B : Boolean := Read_Int (Stream, My_Int);
        pragma Unreferenced (B);

    - Or give the variable a name that contains any of the strings ``discard``
      ``dummy`` ``ignore`` ``junk`` ``unused`` (case insensitive)

Parameters modes
----------------

.. amiard TODO: Talk about early returns from procedures, and grouping
   parameters.
   Talk about the fact that order is unimportant with named parameters (with example)

So far we have seen that Ada is a safety-focused language. There are many ways
this is realized, but two important points are:

- Ada makes the user specify as much as possible about the behavior expected
  for the program, so that the compiler can warn or reject if there is an
  inconsistency.

- Ada provides a variety of techniques for achieving the generality and
  flexibility of pointers and dynamic memory management, but without the
  latter's drawbacks (such as memory leakage and dangling references).

Parameters modes are a feature that helps achieve the two design goals above. A
subprogram parameter can be specified with a mode, which is one of the
following:

+---------------+--------------------------------------------+
| :ada:`in`     | Parameter can only be read, not written    |
+---------------+--------------------------------------------+
| :ada:`out`    | Parameter can be written to, then read     |
+---------------+--------------------------------------------+
| :ada:`in out` | Parameter can be both read and written     |
+---------------+--------------------------------------------+

The default mode for parameters is :ada:`in`; so far, most of the examples
have been using :ada:`in` parameters.

.. admonition:: Historically

    Functions and procedures were originally more different in philosophy.
    Before Ada 2012, functions could only take "in" parameters.

Subprogram calls
----------------

In parameters
~~~~~~~~~~~~~

The first mode for parameters is the one we have been implicitly using so far.
Parameters passed using this mode cannot be modified, so that the following
program will cause an error:

.. code:: ada
    :class: ada-expect-compile-error

    procedure Swap (A, B : Integer) is
       Tmp : Integer;
    begin
       Tmp := A;

       --  Error: assignment to "in" mode parameter not allowed
       A := B;
       --  Error: assignment to "in" mode parameter not allowed
       B := Tmp;
    end Swap;

The fact that this is the default mode is in itself very important. It
means that a parameter will not be modified unless you explicitly specify
a mode in which modification is allowed.

In out parameters
~~~~~~~~~~~~~~~~~

To correct our code above, we can use an "in out" parameter.

.. code:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure In_Out_Params is
       procedure Swap (A, B : in out Integer) is
          Tmp : Integer;
       begin
          Tmp := A;
          A := B;
          B := Tmp;
       end Swap;

       A : Integer := 12;
       B : Integer := 44;
    begin
        Swap (A, B);
        Put_Line (Integer'Image (A)); --  Prints 44
    end In_Out_Params;

An in out parameter will allow read and write access to the object passed as
parameter, so in the example above, we can see that A is modified after the
call to Swap.

.. attention::

    While in out parameters look a bit like references in C++, or regular
    parameters in Java that are passed by-reference, the Ada language standard
    does not mandate "by reference" passing for in out parameters except for
    certain categories of types as will be explained later.

    In general, it is better to think of modes as higher level than by-value
    versus by-reference semantics. For the compiler, it means that an array
    passed as an in parameter might be passed by reference, because it is more
    efficient (which does not change anything for the user since the parameter
    is not assignable). However, a parameter of a discrete type will always be
    passed by copy, regardless of its mode (which is more efficient on most
    architectures).

Out parameters
~~~~~~~~~~~~~~

The "out" mode applies when the subprogram needs to write to a parameter that
might be uninitialized at the point of call. Reading the value of an out
parameter is permitted, but it should only be done after the subprogram has
assigned a value to the parameter. Out parameters behave a bit like return
values for functions.  When the subprogram returns, the actual parameter
(a variable) will have the value of the out parameter at the point of return.

.. admonition:: In other languages

    Ada doesn't have a tuple construct and does not allow returning multiple
    values from a subprogram (except by declaring a full-fledged record type).
    Hence, a way to return multiple values from a subprogram is to use out
    parameters.

For example, a procedure reading integers from the network could have one of
the following specifications:

.. code-block:: ada

    procedure Read_Int
       (Stream : Network_Stream; Success : out Boolean; Result : out Integer);

    function Read_Int
       (Stream : Network_Stream; Result : out Integer) return Boolean;

While reading an out variable before writing to it should, ideally, trigger an
error, imposing that as a rule would cause either inefficient run-time checks
or complex compile-time rules. So from the user's perspective an out parameter
acts like an uninitialized variable when the subprogram is invoked.

.. admonition:: In GNAT

    GNAT will detect simple cases of incorrect use of out parameters.
    For example, the compiler will emit a warning for the following program:

    .. code:: ada

        procedure Outp is
           procedure Foo (A : out Integer) is
              B : Integer := A; -- Warning on reference to uninitialized A
           begin
              A := B;
           end Foo;
        begin
           null;
        end Outp;

Nested subprograms
~~~~~~~~~~~~~~~~~~

As briefly mentioned earlier, Ada allows you to declare one subprogram inside
of another.

This is useful for two reasons:

- It lets you organize your programs in a cleaner fashion. If you need a
  subprogram only as a "helper" for another subprogram, then the principle of
  localization indicates that the helper subprogram should be declared nested.

- It allows you to share state easily in a controlled fashion, because the
  nested subprograms have access to the parameters, as well as any local
  variables, declared in the outer scope.

.. code:: ada
    :class: ada-run

    with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
    with Ada.Text_IO; use Ada.Text_IO;

    procedure Lists is

       type String_Array is array (Positive range <>) of Unbounded_String;

       procedure Show_List (Strings : String_Array) is
          Item_Number : Positive := 1;

          procedure Show_Item (Item : Unbounded_String) is
          begin
             Put_Line (Positive'Image (Item_Number)
                       & ". " & To_String (Item));
             Item_Number := Item_Number + 1;
          end Show_Item;

       begin
          for Item of Strings loop
             Show_Item (Item);
          end loop;
       end Show_List;

       List : String_Array :=
         (To_Unbounded_String ("This"),
          To_Unbounded_String ("is"),
          To_Unbounded_String ("a"),
          To_Unbounded_String ("list"));
    begin
       Show_List (List);
    end Lists;

Forward declaration of subprograms
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we saw earlier, a subprogram can be declared without being fully defined,
for example in a package specification. This is possible in general, and can be
useful if you need subprograms to be mutually recursive, as in the example
below:

.. ?? This example is rather contrived but I suspect that any realistic
.. ?? example would be in the context of recursive data structures and
.. ?? mutually dependent types, which have not been covered yet.

.. code:: ada
    :class: ada-run

    procedure Mutually_Recursive_Subprograms is
        procedure Compute_A (V : Natural);
        --  Forward declaration of Compute_A

        procedure Compute_B (V : Natural) is
        begin
           if V > 5 then
              Compute_A (V - 1);
              --  Call to Compute_A
           end if;
        end Compute_B;

        procedure Compute_A (V : Natural) is
        begin
           if V > 2 then
              Compute_B (V - 1);
              --  Call to Compute_B
           end if;
        end Compute_A;
    begin
       Compute_A (15);
    end Mutually_Recursive_Subprograms;
