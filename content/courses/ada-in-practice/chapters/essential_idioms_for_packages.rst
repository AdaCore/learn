.. _Ada_In_Practice_Essential_Design_Idioms_For_Packages:

Essential Design Idioms for Packages
====================================

.. include:: ../../global.txt


Motivation
----------

Packages, especially library packages, are modules, and as such are the
fundamental building blocks of Ada programs. There is no language-prescribed
way to use packages when designing an application, the language just specifies
what is legal. However, some legal approaches are more advisable than others.

Specifically, packages should exhibit high cohesion and loose coupling
:footcite:p:`1979:yourdon`.
Cohesion is the degree to which the declarations within a module are
related to one another, in the context of the problem being solved.
Unrelated entities should not be declared in the same module.  This allows
the reader to focus on one primary concept, which should be the subject of
the package. Coupling is the degree to which a module
depends upon other modules. Loose coupling enhances comprehension and
maintenance because it allows readers and future developers to examine and modify the
module in relative isolation.  Coupling and cohesion are interrelated:
higher cohesion tends to result in less coupling.


Implementation(s)
-----------------

Three idioms for packages were envisioned when the language was first designed.
They were introduced and described in detail in the Rationale document for the
initial language design :footcite:p:`1986:ichbiah`
and were further developed in Grady Booch's
book *Software Engineering with Ada* :footcite:p:`1983:booch`,
a foundational work on design with
the (sequential part of the) language. Booch added a fourth idiom, the Abstract
Data Machine, to the three described by the Rationale. These four idioms have
proven themselves capable of producing packages that exhibit high cohesion and
loose coupling, resulting in more comprehensible and maintainable source code.

These idioms pre-date later package facilities, such as private packages
and hierarchical packages. We describe idioms for those kinds of packages
separately.

Two of the simpler idioms are described here. The other two, that are more
commonly used, are described in two separate, dedicated entries within
this document.

Generic packages are not actually packages, but their instantiations are, so
these design idioms apply to generic packages as well.

Because these are idioms for modules, we differentiate them by what the
package declarations will contain. But as you will see, what they can
contain is a reflection of the degree of information hiding involved.


.. _Ada_In_Practice_Named_Collection_Of_Declarations:

Named Collection of Declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the first idiom, the package declaration can contain other declarations
only for the following:

    - Objects (constants and variables)
    - Types
    - Exceptions

The idea is to factor out common content required by multiple clients.
Declaring common content in one place and letting clients reference the one
unit makes the most sense.

For example, the following package declares several physical constants used in
a high-fidelity aircraft simulator. These constants are utilized throughout the
simulator code, so they are declared in one place and then referenced as
needed:

.. code-block:: ada

    package Physical_Constants is
       Polar_Radius   : constant  := 20_856_010.51; --  feet
       Equatorial_Radius : constant  := 20_926_469.20; --  feet
       Earth_Diameter : constant  :=
         2.0 * ((Polar_Radius + Equatorial_Radius)/2.0);
       Gravity  : constant  := 32.1740_4855_6430_4; --  feet/second**2
       Sea_Level_Air_Density   : constant  := 0.002378; --  slugs/foot**3
       Altitude_Of_Tropopause  : constant  := 36089.0; --  feet
       Tropopause_Temperature  : constant  := -56.5; --  degrees-C
    end Physical_Constants;

No information hiding is occurring when using this idiom.

Pros
^^^^

Packages designed with this idiom will have high cohesion and low coupling.

The idiom also enhances maintainability because changes to the values, if
necessary, need only be made in one place, although in this particular
example, we would hope that no such changes will be made.

Cons
^^^^

When a library package contains variable declarations, these variables comprise
global data. In this sense, *global* means potential visibility to multiple
clients. Global data should be avoided by default, because the effects of
changes are potentially pervasive, throughout the entire set of clients that
have visibility to it. In effect the developer must understand everything
before changing anything. The introduction of new bugs is a common result. But
if, for some compelling reason, the design really called for global data, this
idiom provides the way to declare it. Note also that global *constants*
are less problematic than variables because they can't be changed.


.. _Ada_In_Practice_Groups_Of_Related_Program_Units:

Groups of Related Program Units
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this idiom, the package can contain all of the declarations allowed by
the first idiom, but also contains declarations for operations. These are
usually subprograms but other kinds of declarations are also allowed such
as protected types and objects.  Hence these packages can contain:

    - Objects (constants and variables)
    - Types
    - Exceptions
    - Operations

Our intent is that the types declared in the package are used by the
operations declared in the package, typically in their formal parameters
and/or function return types. In this idiom, however, the types are not
private.

For example:

.. code-block:: ada

    package Linear_Algebra is
       type Vector is array (Positive range <>) of Real;
       type Matrix is array (Positive range <>, Positive range <>) of Real;
       function "+" (Left, Right : Vector) return Vector;
       function "*" (Left, Right : Vector) return Matrix;
       -- ...
    end Linear_Algebra;

In this example, :ada:`Vector` and :ada:`Matrix` are the types under
consideration. The type :ada:`Real` might be declared here too, but it might be
better declared in a
:ref:`Named Collection of Declarations <Ada_In_Practice_Named_Collection_Of_Declarations>`
package referenced in a with_clause. In any case, this package declares types
and subprograms that manipulate values of those types.

One might also declare variables in the package, but those should not be
the central purpose of the package. For example, perhaps we want to have a
variable whose value is used as the
default for some formal parameters. Clients can change the default for
subsequent calls by first assigning a different value to the variable, unlike a
hardcoded literal chosen by the developer. It would look like this:

.. code-block:: ada

       Default_Debounce_Time : Time_Span := Milliseconds (75);
       --  The default amount of time used to debounce an input pin.
       --  This value is tunable.

       procedure Await_Active
         (This : Discrete_Input;
          Debounce_Time : Time_Span := Default_Debounce_Time);

With this idiom, information hiding applies to the implementation of the
visible subprograms in the package body as well as any internal entities
declared in the body and used in implementing the visible subprograms.

As mentioned, these idioms apply to generic packages as well. For example,
a more realistic approach would be to make type :ada:`Real` be a generic
formal type:

.. code-block:: ada

    generic
       type Real is digits <>;
    package Linear_Algebra is
       type Vector is array (Positive range <>) of Real;
       type Matrix is array (Positive range <>, Positive range <>) of Real;
       function "+" (Left, Right : Vector) return Vector;
       function "*" (Left, Right : Vector) return Matrix;
       --  ...
    end Linear_Algebra;

Pros
^^^^

The types and the associated operations are grouped together and are hence
highly cohesive. Such packages usually can be loosely coupled as well.

Clients have all the language-defined operations available that the type
representations provide. In the case of :ada:`Vector` and :ada:`Matrix`,
clients have compile-time visibility to the fact they are array types.
Therefore, clients can manipulate :ada:`Vector` and :ada:`Matrix` values as
arrays: for example, they can create values via aggregates and use array
indexing to access specific components.

Cons
^^^^

Clients can write code that depends on the type's representation, and can be
relied upon to do so. Consequently, a change in the representation will
potentially require redeveloping the client code, which could be extensive and
expensive. That is a serious disadvantage.

However, compile-time visibility to the type representations may be necessary
to meet client expectations. For example, engineers expect to use indexing
with vectors and matrices. As of Ada 2012, developers can specify the meaning
of array indexing but the approach is fairly heavy.

Notes
-----

    1. The rules for what these idiomatic packages contain are not meant to be
       iron-clad; hybrids are possible but should be considered initially
       suspect and reviewed accordingly.

.. only:: builder_html

    .. rubric:: Bibliography

.. footbibliography::
