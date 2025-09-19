Compilation Unit Structure
----------------------------

.. include:: ../../../global.txt

C++ programming style usually promotes the use of two distinct files: header files used to define specifications (.h*, *.hxx*, *.hpp*), and implementation files which contain the executable code (*.c*, *.cxx*, *.cpp*). However, the distinction between specification and implementation is not enforced by the compiler and may need to be worked around in order to implement, for example, inlining or templates.

Java compilers expect both the implementation and specification to be in the same *.java* file. (Yes, design patterns allow using interfaces to separate specification from implementation to a certain extent, but this is outside of the scope of this description.)

Ada is superficially similar to the C++ case: Ada compilation units are generally split into two parts, the specification and the body. However, what goes into those files is more predictable for both the compiler and for the programmer. With GNAT, compilation units are stored in files with a *.ads* extension for specifications and with a *.adb* extension for implementations.

Without further ado, we present the famous "Hello World" in three languages:

[Ada]

.. code-block:: ada

   with Ada.Text_IO;
   use  Ada.Text_IO;

   procedure Main is
   begin
      Put_Line ("Hello World");
   end Main;

[C++]

.. code-block:: cpp

   #include <iostream>
   using namespace std;

   int main(int argc, const char* argv[]) {
      cout << "Hello World" << endl;
   }

[Java]

.. code-block:: java

   public class Main {
      public static void main(String [] argv) {
         System.out.println ("Hello World");
      }
   }

The first line of Ada we see is the :ada:`with` clause, declaring that the unit (in this case, the Main subprogram) will require the services of the package :ada:`Ada.Text_IO`. This is different from how :cpp:`#include` works in C++ in that it does not, in a logical sense, copy/paste the code of :ada:`Ada.Text_IO` into :ada:`Main`. The :ada:`with` clause directs the compiler to make the public interface of the :ada:`Ada.Text_IO` package visible to code in the unit (here :ada:`Main`) containing the :ada:`with` clause. Note that this construct does not have a direct analog in Java, where the entire CLASSPATH is always accessible. Also, the name ``Main`` for the main subprogram was chosen for consistency with C++ and Java style but in Ada the name can be whatever the programmer chooses.

The :ada:`use` clause is the equivalent of :cpp:`using namespace` in C++, or :java:`import` in Java (though it wasn't necessary to use :java:`import` in the Java example above). It allows you to omit the full package name when referring to :ada:`with`'ed units. Without the :ada:`use` clause, any reference to :ada:`Ada.Text_IO` items would have had to be fully qualified with the package name. The :ada:`Put_Line` line would then have read:

.. code-block:: ada

      Ada.Text_IO.Put_Line ("Hello World");

The word "package" has different meanings in Ada and Java. In Java, a package is used as a namespace for classes. In Ada, it's often a compilation unit. As a result Ada tends to have many more packages than Java. Ada package specifications ("package specs" for short) have the following structure:

.. code-block:: ada

   package Package_Name is

      -- public declarations

   private

      -- private declarations

   end Package_Name;

The implementation in a package body (written in a *.adb* file) has the structure:

.. code-block:: ada

   package body Package_Name is

      -- implementation

   end Package_Name;

The :ada:`private` reserved word is used to mark the start of the private portion of a package spec. By splitting the package spec into private and public parts, it is possible to make an entity available for use while hiding its implementation. For instance, a common use is declaring a :ada:`record` (Ada's :cpp:`struct`) whose fields are only visible to its package and not to the caller. This allows the caller to refer to objects of that type, but not to change any of its contents directly.

The package body contains implementation code, and is only accessible to outside code through declarations in the package spec.

An entity declared in the private part of a package in Ada is roughly equivalent to a protected member of a C++ or Java class.  An entity declared in the body of an Ada package is roughly equivalent to a private member of a C++ or Java class.
