Packages
----------

.. include:: ../../../global.txt

Declaration Protection
~~~~~~~~~~~~~~~~~~~~~~~

The package is the basic modularization unit of the Ada language, as is the class for Java and the header and implementation pair for C++. An Ada package contains three parts that, for GNAT, are separated into two files: *.ads* files contain public and private Ada specifications, and *.adb* files contain the implementation, or Ada bodies.

Java doesn't provide any means to cleanly separate the specification of methods from their implementation: they all appear in the same file. You can use interfaces to emulate having separate specifications, but this requires the use of OOP techniques which is not always practical.

Ada and C++ do offer separation between specifications and implementations out of the box, independent of OOP.

.. code-block:: ada

   package Package_Name is
      -- public specifications
   private
      -- private specifications
   end Package_Name;

   package body Package_Name is
      -- implementation
   end Package_Name;

Private types are useful for preventing the users of a package's types from depending on the types' implementation details. The :ada:`private` keyword splits the package spec into "public" and "private" parts. That is somewhat analogous to C++'s partitioning of the class construct into different sections with different visibility properties. In Java, the encapsulation has to be done field by field, but in Ada the entire definition of a type can be hidden. For example:

.. code-block:: ada

   package Types is
      type Type_1 is private;
      type Type_2 is private;
      type Type_3 is private;
      procedure P (X : Type_1);
      ...
   private
      procedure Q (Y : Type_1);
      type Type_1 is new Integer range 1 .. 1000;
      type Type_2 is array (Integer range 1 .. 1000) of Integer;
      type Type_3 is record
         A, B : Integer;
      end record;
   end Types;

Subprograms declared above the :ada:`private` separator (such as :ada:`P`) will be visible to the package user, and the ones below (such as :ada:`Q`) will not. The body of the package, the implementation, has access to both parts.

Hierarchical Packages
~~~~~~~~~~~~~~~~~~~~~~~

Ada packages can be organized into hierarchies. A child unit can be declared in the following way:

.. code-block:: ada

   -- root-child.ads

   package Root.Child is
      --  package spec goes here
   end Root.Child;

   -- root-child.adb

   package body Root.Child is
      --  package body goes here
   end Root.Child;

Here, :ada:`Root.Child` is a child package of :ada:`Root`. The public part of :ada:`Root.Child` has access to the public part of :ada:`Root`. The private part of :ada:`Child` has access to the private part of :ada:`Root`, which is one of the main advantages of child packages. However, there is no visibility relationship between the two bodies. One common way to use this capability is to define subsystems around a hierarchical naming scheme.

Using Entities from Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Entities declared in the visible part of a package specification can be made accessible using a :ada:`with` clause that references the package, which is similar to the C++ :cpp:`#include` directive. Visibility is implicit in Java: you can always access all classes located in your *CLASSPATH*. After a :ada:`with` clause, entities needs to be prefixed by the name of their package, like a C++ namespace or a Java package. This prefix can be omitted if a :ada:`use` clause is employed, similar to a C++ :cpp:`using namespace` or a Java :java:`import`.

[Ada]

.. code-block:: ada

   -- pck.ads

   package Pck is
      My_Glob : Integer;
   end Pck;

   -- main.adb

   with Pck;

   procedure Main is
   begin
      Pck.My_Glob := 0;
   end Main;

[C++]

.. code-block:: cpp

   // pck.h

   namespace pck {
      extern int myGlob;
   }

   // pck.cpp

   namespace pck {
      int myGlob;
   }

   // main.cpp

   #include "pck.h"

   int main (int argc, char ** argv) {
      pck::myGlob = 0;
   }

[Java]

.. code-block:: java

   // Globals.java

   package pck;

   public class Globals {
      public static int myGlob;
   }

   // Main.java

   public class Main {
      public static void main (String [] argv) {
         pck.Globals.myGlob = 0;
      }
   }
