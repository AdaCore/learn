:prev_state: False

Introduction
============

.. include:: ../../global.txt

This course describes how to implement selected programming idioms in
the Ada language.

What is an idiom? Some would say that an idiom is a workaround for an
expressive deficiency in a programming language. That is not what we mean.

What we have in mind are answers to the question "In this situation,
how can one best use the Ada language to express an elegant solution?".

Design patterns [1]_ are intended to answer that question, and indeed
some would equate idioms with design patterns. But what we have in mind
is more general and does not focus on design patterns.

For example, :wikipedia:`Reference Counting <Reference_counting>` is a
well-known approach to tracking and managing the storage for objects and
is conceptually independent of the programming language. However,
reference counting is not a design pattern.

Likewise, :wikipedia:`Resource Acquisition Is Allocation (RAII)
<Resource_acquisition_is_initialization>`, :wikipedia:`type punning
<Type_punning>`, :wikipedia:`interface inheritance <Subtyping>`, and
:wikipedia:`implementation inheritance <Implementation_inheritance>` are
not design patterns.

Those are the kinds of situations and solutions we focus upon.

That said, we may refer to a design pattern to illustrate an idiom's
purpose and/or implementation. For example, in the idiom for controlling
object creation and initialization, the implementation approach happens
to be the same as for expressing a Singleton [1]_.

That's not to say that we could not have a design pattern as an idiom
solution. But if so, the purpose of the idiom entry would be to
illustrate some Ada programming technique, rather than the expression of
the design pattern itself.

In addition to language-independent situations, we also include
solutions for situations specific to the Ada language. These are not
deficiency workarounds, but rather, *best practices* in situations that
arise given the capabilities and semantics in the language.

For example, Ada directly supports tasks (threads) via a dedicated
construct consisting of local objects and a sequence of statements.
Tasks can also be defined as types, and then used to define components
for other composite types. As a result, there is an idiom showing how to
associate a task type with an enclosing composite type so that the task
components have visibility to the enclosing object's other components.

In all the idioms we want to apply the fundamental principles of
software engineering, especially those of abstraction and information
hiding. Therefore, we include an idiom for expressing abstractions as
types, with compile-time visibility control over the representation.
These are the well-known Abstract Data Types, something the Ada language
directly supports but using *building blocks* instead of a single
construct. For that same reason we include another idiom for defining
abstractions that manage global data (Abstract Data Machines). Most of
the idioms' solutions will be defined using these abstraction techniques
as their starting point.

Perhaps instead of *idiom* we should have used the term *cookbook,* but
although appropriate, that term didn't completely convey the intent either.

Assumptions
-----------

We assume the reader knows Ada to some degree, including some advanced topics.
For those lacking significant familiarity, we hope these solutions will at
least give a sense for how to apply the language. We direct such readers to the
:ref:`online Learn courses dedicated to the Ada language itself <Advanced_Ada_Course_Index>`.


Definitions
-----------

For the sake of avoiding duplication in the idiom entries, the following terms
are defined here. Note that the Ada Language Manual includes a glossary in
:arm22:`Section 1.3 <1-3>` (located in Annex N prior to Ada 2022). Some of the
following expand on the definitions found there.

Suppliers and Clients
~~~~~~~~~~~~~~~~~~~~~

*Suppliers* are software units that provide programming entities to other
software units, the users. These users are the *clients* of the supplied units.
The concept is simple and intuitive, but by defining these terms we can convey
these roles quickly in the idioms' discussions.

For example, a unit that defines a type and associated operations would be a
supplier. Client units could use that type to declare objects, and/or apply the
operations to such objects. The language-defined package :ada:`Ada.Text_IO` is
an example of a supplier. Similarly, the unit that defines a library, such as a
math library, is a supplier. Callers to the math library routines are the
clients. The generic package
:ada:`Ada.Numerics.Generic_Complex_Elementary_Functions`, once instantiated,
would be an example supplier. (Arguably, the generic package itself would be a
supplier to the client that instantiates it, but instantiation is the only
possibility in that narrow case. Only the routines in the instances can be
called.)

Betrand Meyer's book on OOP [2]_ limits these terms specifically to the case of
a type used in an object declaration. Our definitions cover that case but
others as well.

Units can be both suppliers and clients, because a given supplier's facility,
i.e., the interface and/or implementation, may be built upon the facilities
defined by other suppliers.

Compile-time Visibility
~~~~~~~~~~~~~~~~~~~~~~~

In the definitions of supplier and client above, we gave an example in which a
supplier's type was used by clients to declare objects of the type. For the
client to legally do so |mdash| that is, for the compiler to accept this usage
and process the code |mdash| the use of the supplier's type has to satisfy the
scope and visibility rules of the programming language.

Good implementations harness these visibility rules to adhere to the software
engineering principles of information hiding and abstraction, both of which
require that nothing of the implementation be made visible to clients unless
necessary. Compiler enforcement ensures rigorous adherence to those principles.

Therefore, modern languages provide some way to express this control. For
example, in Ada, a package can have both a *public* part and a *private* part.
Clients have no compile-time visibility to the private part, nor to the package
body, as both parts contain implementation artifacts. In class-oriented
languages, parts of the class can be marked as *public,* *private*, and
*protected* (the details depend on the specific language).

The idioms :ref:`Abstract Data Types <Ada_Idioms_Abstract_Data_Types>`
and :ref:`Abstract Data Types <Ada_Idioms_Abstract_Data_Machines>` are
prime examples used throughout the other idioms.

The idioms explored in :ref:`Fundamental Packages
<Ada_Idioms_Essential_Design_Idioms_For_Packages>` are largely
variations on expressing this control in Ada.

More details on the topic are provided in those idioms.

Views
~~~~~

In Ada, a *view* of an entity defines what the developer can legally do with
that entity. For example, the declaration of an object defines a view of that
object. The operations allowed by that view are determined by the type used to
declare the object: a signed integer type would allow signed integer numeric
operations, but not, say, bit-level operations, nor array indexing, and so on.
Furthermore, the view includes whether the object is a constant.

An entity can have more than one view, depending on where in the text of the
source code a view of that entity is considered. For example, let's say that
the integer object introduced above is in fact a variable. Within the scope of
that variable, we can refer to it by that name and update the value using
assignment statements. However, if we pass that variable as the argument to a
procedure call, within that subprogram (for that call) the view specifies a
different name for the argument, i.e., the formal parameter name. Moreover, if
that formal parameter is a mode-in parameter, within that procedure body the
view of the actual parameter is as if it is a constant rather than a variable.
No assignments via the formal parameter name are allowed because the view at
that point in the text |mdash| within that procedure body |mdash| doesn't allow
them, unlike the view outside the body.

As another example, consider a tagged type named :ada:`Parent`, and a type
derived from it via type extension, named :ada:`Child`. It is common for a
derived type to have either additional components, or additional operations, or
both. For a given object of the :ada:`Child` type, the view via type
:ada:`Child` allows the developer to refer to the extended components and/or
operations. But we can convert the :ada:`Child` object to a value of the
:ada:`Parent` type using what is known as a *view conversion*. With that
:ada:`Parent` view of the :ada:`Child` object, we can only refer to those
components and operations defined for the :ada:`Parent` type. The compiler
enforces this temporary view.

For further details about view conversions, please refer to that
:ref:`specific section of the Advanced Ada course <Adv_Ada_View_Conversion>`.

Views are a fundamental concept in Ada. Understanding them will greatly
facilitate understanding the rules of the language in general.


Partial and Full Views
~~~~~~~~~~~~~~~~~~~~~~

Like objects, types also can have more than one view, again determined by the
place in the program text that a view is considered. These views can be used to
apply information hiding and abstraction.

The declaration of a private type defines a *partial view* of a type that
reveals only some of its properties: the type name, primarily, but in
particular not the type's representation. For example:

.. code-block:: ada

    type Rotary_Encoder is private;

Private type declarations must occur in the *public part* of a package
declaration. Anything declared there is compile-time visible to clients of the
package so the type's name is visible, and potentially some other properties as
well. Clients can therefore declare objects of the type name, for example, but
must adhere to their partial view's affect on what is compile-time visible.

The private type's full representation must be specified within the
*private part* of that same package declaration. For example:

.. code-block:: ada

    type Rotary_Encoder is record ... end record;

Therefore, within that package private part and within the package body the
*full view* is available because full representation information is
compile-time visible in those regions. (Parts of child units have the full view
as well.) This view is necessary in those two regions of the package because
the representation details are required in order to implement the corresponding
operations, among other possibilities.

Because the clients only have the partial view they do not have compile-time
visibility to the type's internal representation. Consequently, the compiler
will not allow representation-specific references or operations in client code.
The resulting benefit is that clients are independent of the type's
representation and, therefore, it can be changed without requiring coding
changes in the clients. Clients need only be recompiled in that case.

This application of information hiding has real-world cost benefits because
changing client code can be prohibitively expensive. That's one reason why the
maintenance phase of a project is by far the most expensive phase. Another
reason is that *maintenance* is often a euphemism for new development. Either
way, change is involved.

As a result, when defining types, developers should use private types by
default, only avoiding them when they are not appropriate. Not using them
should be an explicit design choice, a line item in code reviews. Not defining
a major abstraction as a private type should be suspect, just as using a
:c:`struct` rather than a :c:`class` in C++ should be suspect in that case. (In
C++ anything a :c:`struct` contains is compile-time visible to clients by
default.)

For further details about type views, please refer to that
:ref:`specific section of the Advanced Ada course <Adv_Ada_Type_View>`.


Bibliography
------------

.. [1] Gamma, E., R. Helm, et al. (1995), pp. 127. Design Patterns: Elements of
       Reusable Object-Oriented Software. Reading, MA, Addison-Wesley
       Publishing Company.

.. [2] Meyer, B. (1997), pp. 182. Object-Oriented Software Construction,
       Prentice-Hall.
