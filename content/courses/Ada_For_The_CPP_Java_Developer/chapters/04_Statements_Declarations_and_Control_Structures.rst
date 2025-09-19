Statements, Declarations, and Control Structures
---------------------------------------------------

.. include:: ../../../global.txt

Statements and Declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following code samples are all equivalent, and illustrate the use of comments and working with integer variables:

[Ada]

.. code-block:: ada

   --
   --  Ada program to declare and modify Integers
   --
   procedure Main is
      --  Variable declarations
      A, B : Integer := 0;
      C    : Integer := 100;
      D    : Integer;
   begin
      --  Ada uses a regular assignment statement for incrementation.
      A := A + 1;

      --  Regular addition
      D := A + B + C;
   end Main;

[C++]

.. code-block:: cpp

   /*
    *  C++ program to declare and modify ints
    */
   int main(int argc, const char* argv[]) {
      //  Variable declarations
      int a = 0, b = 0, c = 100, d;

      //  C++ shorthand for incrementation
      a++;

      //  Regular addition
      d = a + b + c;
   }

[Java]

.. code-block:: java

   /*
    *  Java program to declare and modify ints
    */
   public class Main {
      public static void main(String [] argv) {
         //  Variable declarations
         int a = 0, b = 0, c = 100, d;

         //  Java shorthand for incrementation
         a++;

         //  Regular addition
         d = a + b + c;
      }
   }

Statements are terminated by semicolons in all three languages. In Ada, blocks of code are surrounded by the reserved words :ada:`begin` and :ada:`end` rather than by curly braces.  We can use both multi-line and single-line comment styles in the C++ and Java code, and only single-line comments in the Ada code.

Ada requires variable declarations to be made in a specific area called the *declarative part*, seen here before the :ada:`begin` keyword. Variable declarations start with the identifier in Ada, as opposed to starting with the type as in C++ and Java (also note Ada's use of the :ada:`:` separator). Specifying initializers is different as well: in Ada an initialization expression can apply to multiple variables (but will be evaluated separately for each), whereas in C++ and Java each variable is initialized individually. In all three languages, if you use a function as an initializer and that function returns different values on every invocation, each variable will get initialized to a different value.

Let's move on to the imperative statements. Ada does not provide :cpp:`++` or :cpp:`--` shorthand expressions for increment/decrement operations; it is necessary to use a full assignment statement. The :ada:`:=` symbol is used in Ada to perform value assignment. Unlike C++'s and Java's :cpp:`=` symbol, :ada:`:=` can not be used as part of an expression. So, a statement like :ada:`A := B := C;` doesn't make sense to an Ada compiler, and neither does a clause like :ada:`if A := B then ...`. Both are compile-time errors.

You can nest a block of code within an outer block if you want to create an inner scope:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main is
   begin
      Put_Line ("Before the inner block");

      declare
         Alpha : Integer := 0;
      begin
         Alpha := Alpha + 1;
         Put_Line ("Now inside the inner block");
      end;

      Put_Line ("After the inner block");
   end Main;

It is OK to have an empty declarative part or to omit the declarative part entirely |mdash| just start the inner block with :ada:`begin` if you have no declarations to make. However it is not OK to have an empty sequence of statements. You must at least provide a :ada:`null;` statement, which does nothing and indicates that the omission of statements is intentional.

Conditions
~~~~~~~~~~~~~

The use of the :ada:`if` statement:

[Ada]

.. code-block:: ada

   if Variable > 0 then
      Put_Line (" > 0 ");
   elsif Variable < 0 then
      Put_Line (" < 0 ");
   else
      Put_Line (" = 0 ");
   end if;

[C++]

.. code-block:: cpp

   if (Variable > 0)
      cout << " > 0 " << endl;
   else if (Variable < 0)
      cout << " < 0 " << endl;
   else
      cout << " = 0 " << endl;

[Java]

.. code-block:: java

   if (Variable > 0)
      System.out.println (" > 0 ");
   else if (Variable < 0)
      System.out.println (" < 0 ");
   else
      System.out.println (" = 0 ");

In Ada, everything that appears between the :ada:`if` and :ada:`then` keywords is the conditional expression |mdash| no parentheses required. Comparison operators are the same, except for equality (:ada:`=`) and inequality (:ada:`/=`). The English words :ada:`not`, :ada:`and`, and :ada:`or` replace the symbols :cpp:`!`, :cpp:`&`, and :cpp:`|`, respectively, for performing boolean operations.

It's more customary to use :cpp:`&&` and :cpp:`||` in C++ and Java than :cpp:`&` and :cpp:`|` when writing boolean expressions. The difference is that :cpp:`&&` and :cpp:`||` are short-circuit operators, which evaluate terms only as necessary, and :cpp:`&` and :cpp:`|` will unconditionally evaluate all terms. In Ada, :ada:`and` and :ada:`or` will evaluate all terms; :ada:`and then` and :ada:`or else` direct the compiler to employ short circuit evaluation.

Here are what switch/case statements look like:

[Ada]

.. code-block:: ada

   case Variable is
      when 0 =>
         Put_Line ("Zero");
      when 1 .. 9 =>
         Put_Line ("Positive Digit");
      when 10 | 12 | 14 | 16 | 18 =>
         Put_Line ("Even Number between 10 and 18");
      when others =>
         Put_Line ("Something else");
   end case;

[C++]

.. code-block:: cpp

   switch (Variable) {
      case 0:
         cout << "Zero" << endl;
         break;
      case 1: case 2: case 3: case 4: case 5:
      case 6: case 7: case 8: case 9:
         cout << "Positive Digit" << endl;
         break;
      case 10: case 12: case 14: case 16: case 18:
         cout << "Even Number between 10 and 18" << endl;
         break;
      default:
         cout << "Something else";
   }

[Java]

.. code-block:: java

   switch (Variable) {
      case 0:
         System.out.println ("Zero");
         break;
      case 1: case 2: case 3: case 4: case 5:
      case 6: case 7: case 8: case 9:
         System.out.println ("Positive Digit");
         break;
      case 10: case 12: case 14: case 16: case 18:
         System.out.println ("Even Number between 10 and 18");
         break;
      default:
         System.out.println ("Something else");
   }

In Ada, the :ada:`case` and :ada:`end case` lines surround the whole case statement, and each case starts with :ada:`when`. So, when programming in Ada, replace :cpp:`switch` with :ada:`case`, and replace :cpp:`case` with :ada:`when`.

Case statements in Ada require the use of discrete types (integers or enumeration types), and require all possible cases to be covered by :ada:`when` statements. If not all the cases are handled, or if duplicate cases exist, the program will not compile. The default case, :cpp:`default:` in C++ and Java, can be specified using :ada:`when others =>` in Ada.

In Ada, the :cpp:`break` instruction is implicit and program execution will never fall through to subsequent cases. In order to combine cases, you can specify ranges using :ada:`..` and enumerate disjoint values using :ada:`|` which neatly replaces the multiple :cpp:`case` statements seen in the C++ and Java versions.

Loops
~~~~~~~

In Ada, loops always start with the :ada:`loop` reserved word and end with :ada:`end loop`. To leave the loop, use :ada:`exit` |mdash| the C++ and Java equivalent being :cpp:`break`. This statement can specify a terminating condition using the :ada:`exit when` syntax. The :ada:`loop` opening the block can be preceded by a :ada:`while` or a :ada:`for`.

The :ada:`while` loop is the simplest one, and is very similar across all three languages:

[Ada]

.. code-block:: ada

   while Variable < 10_000 loop
      Variable := Variable * 2;
   end loop;

[C++]

.. code-block:: cpp

   while (Variable < 10000) {
      Variable = Variable * 2;
   }

[Java]

.. code-block:: java

  while (Variable < 10000) {
      Variable = Variable * 2;
  }

Ada's :ada:`for` loop, however, is quite different from that in C++ and Java. It always increments or decrements a loop index within a discrete range. The loop index (or "loop parameter" in Ada parlance) is local to the scope of the loop and is implicitly incremented or decremented at each iteration of the loop statements; the program cannot directly modify its value. The type of the loop parameter is derived from the range. The range is always given in ascending order even if the loop iterates in descending order. If the starting bound is greater than the ending bound, the interval is considered to be empty and the loop contents will not be executed. To specify a loop iteration in decreasing order, use the :ada:`reverse` reserved word. Here are examples of loops going in both directions:

[Ada]

.. code-block:: ada

   --  Outputs 0, 1, 2, ..., 9
   for Variable in 0 .. 9 loop
      Put_Line (Integer'Image (Variable));
   end loop;

   --  Outputs 9, 8, 7, ..., 0
   for Variable in reverse 0 .. 9 loop
      Put_Line (Integer'Image (Variable));
   end loop;

[C++]

.. code-block:: cpp

   //  Outputs 0, 1, 2, ..., 9
   for (int Variable = 0; Variable <= 9; Variable++) {
      cout << Variable << endl;
   }

   //  Outputs 9, 8, 7, ..., 0
   for (int Variable = 9; Variable >=0; Variable--) {
      cout << Variable << endl;
   }

[Java]

.. code-block:: java

   //  Outputs 0, 1, 2, ..., 9
   for (int Variable = 0; Variable <= 9; Variable++) {
      System.out.println (Variable);
   }

   //  Outputs 9, 8, 7, ..., 0
   for (int Variable = 9; Variable >= 0; Variable--) {
      System.out.println (Variable);
   }

Ada uses the :ada:`Integer` type's :ada:`'Image` attribute to convert a numerical value to a String. There is no implicit conversion between :ada:`Integer` and :ada:`String` as there is in C++ and Java. We'll have a more in-depth look at such attributes later on.

It's easy to express iteration over the contents of a container (for instance, an array, a list, or a map) in Ada and Java. For example, assuming that :ada:`Int_List` is defined as an array of Integer values, you can use:

[Ada]

.. code-block:: ada

   for I of Int_List loop
      Put_Line (Integer'Image (I));
   end loop;

[Java]

.. code-block:: java

   for (int i : Int_List) {
      System.out.println (i);
   }
