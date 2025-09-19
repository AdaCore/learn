Functions and Procedures
-------------------------

.. include:: ../../../global.txt

General Form
~~~~~~~~~~~~~

Subroutines in C++ and Java are always expressed as functions (methods) which may or may not return a value. Ada explicitly differentiates between functions and procedures. Functions must return a value and procedures must not. Ada uses the more general term "subprogram" to refer to both functions and procedures.

Parameters can be passed in three distinct modes: :ada:`in`, which is the default, is for input parameters, whose value is provided by the caller and cannot be changed by the subprogram. :ada:`out` is for output parameters, with no initial value, to be assigned by the subprogram and returned to the caller. :ada:`in out` is a parameter with an initial value provided by the caller, which can be modified by the subprogram and returned to the caller (more or less the equivalent of a non-constant reference in C++). Ada also provides :ada:`access` parameters, in effect an explicit pass-by-reference indicator.

In Ada the programmer specifies how the parameter will be used and in general the compiler decides how it will be passed (i.e., by copy or by reference). (There are some exceptions to the "in general". For example, parameters of scalar types are always passed by copy, for all three modes.) C++ has the programmer specify how to pass the parameter, and Java forces primitive type parameters to be passed by copy and all other parameters to be passed by reference. For this reason, a 1:1 mapping between Ada and Java isn't obvious but here's an attempt to show these differences:

[Ada]

.. code-block:: ada

   procedure Proc
    (Var1 : Integer;
     Var2 : out Integer;
     Var3 : in out Integer);

   function Func (Var : Integer) return Integer;

   procedure Proc
    (Var1 : Integer;
     Var2 : out Integer;
     Var3 : in out Integer)
   is
   begin
      Var2 := Func (Var1);
      Var3 := Var3 + 1;
   end Proc;

   function Func (Var : Integer) return Integer
   is
   begin
      return Var + 1;
   end Func;

[C++]

.. code-block:: cpp

   void Proc
     (int Var1,
      int & Var2,
      int & Var3);

   int Func (int Var);

   void Proc
     (int Var1,
      int & Var2,
      int & Var3) {

      Var2 = Func (Var1);
      Var3 = Var3 + 1;
   }

   int Func (int Var) {
      return Var + 1;
   }

[Java]

.. code-block:: java

   public class ProcData {
      public int Var2;
      public int Var3;

      public void Proc (int Var1) {
         Var2 = Func (Var1);
         Var3 = Var3 + 1;
      }

      public static int Func (int Var) {
         return Var + 1;
      }
   }

The first two declarations for :ada:`Proc` and :ada:`Func` are specifications of the subprograms which are being provided later. Although optional here, it's still considered good practice to separately define specifications and implementations in order to make it easier to read the program. In Ada and C++, a function that has not yet been seen cannot be used. Here, :ada:`Proc` can call :ada:`Func` because its specification has been declared. In Java, it's fine to have the declaration of the subprogram later .

Parameters in Ada subprogram declarations are separated with semicolons, because commas are reserved for listing multiple parameters of the same type. Parameter declaration syntax is the same as variable declaration syntax, including default values for parameters. If there are no parameters, the parentheses must be omitted entirely from both the declaration and invocation of the subprogram.

Overloading
~~~~~~~~~~~~

Different subprograms may share the same name; this is called "overloading." As long as the subprogram signatures (subprogram name, parameter types, and return types) are different, the compiler will be able to resolve the calls to the proper destinations. For example:

.. code-block:: ada

   function Value (Str : String) return Integer;
   function Value (Str : String) return Float;

   V : Integer := Value ("8");

The Ada compiler knows that an assignment to :ada:`V` requires an :ada:`Integer`. So, it chooses the :ada:`Value` function that returns an :ada:`Integer` to satisfy this requirement.

Operators in Ada can be treated as functions too. This allows you to define local operators that override operators defined at an outer scope, and provide overloaded operators that operate on and compare different types. To express an operator as a function, enclose it in quotes:

[Ada]

.. code-block:: ada

   function "=" (Left : Day; Right : Integer) return Boolean;

[C++]

.. code-block:: cpp

   bool operator = (Day Left, int Right);

Subprogram Contracts
~~~~~~~~~~~~~~~~~~~~~

You can express the expected inputs and outputs of subprograms by specifying subprogram contracts. The compiler can then check for valid conditions to exist when a subprogram is called and can check that the return value makes sense. Ada allows defining contracts in the form of :ada:`Pre` and :ada:`Post` conditions; this facility was introduced in Ada 2012. They look like:

.. code-block:: ada

   function Divide (Left, Right : Float) return Float
      with Pre  => Right /= 0.0,
           Post => Divide'Result * Right < Left + 0.0001
                   and then Divide'Result * Right > Left - 0.0001;

The above example adds a :ada:`Pre` condition, stating that :ada:`Right` cannot be equal to 0.0. While the IEEE floating point standard permits divide-by-zero, you may have determined that use of the result could still lead to issues in a particular application. Writing a contract helps to detect this as early as possible. This declaration also provides a :ada:`Post` condition on the result.

Postconditions can also be expressed relative to the value of the input:

.. code-block:: ada

   procedure Increment (V : in out Integer)
      with Pre  => V < Integer'Last,
           Post => V = V'Old + 1;

:ada:`V'Old` in the postcondition represents the value that :ada:`V` had before entering :ada:`Increment`.
