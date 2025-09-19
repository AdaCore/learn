Conclusion
============

.. include:: ../../../global.txt

Although Ada's syntax might seem peculiar to C developers at first glance, it
was designed to increase readability and maintainability, rather than making it
faster to write in a condensed manner |mdash| as it is often the case in C.

Especially in the embedded domain, C developers are used to working at a very
low level, which includes mathematical operations on pointers, complex bit
shifts, and logical bitwise operations. C is well designed for such operations
because it was designed to replace Assembly language for faster, more efficient
programming.

Ada can be used to describe high level semantics and architectures. The beauty
of the language, however, is that it can be used all the way down to the lowest
levels of the development, including embedded Assembly code or bit-level data
management. However, although Ada supports bitwise operations such as masks and
shifts, they should be relatively rarely needed. When translating C code to
Ada, it's good practice to consider alternatives. In a lot of cases, these
operations are used to insert several pieces of data into a larger structure.
In Ada, this can be done by describing the structure layout at the type level
through representation clauses, and then accessing this structure as any other.
For example, we can interpret an arbitrary data type as a bit-field and perform
low-level operations on it.

Because Ada is a strongly typed language, it doesn't define any implicit type
conversions like C. If we try to compile Ada code that contains type
mismatches, we'll get a compilation error. Because the compiler prevents
mixing variables of different types without explicit type conversion, we can't
accidentally end up in a situation where we assume something will happen
implicitly when, in fact, our assumption is incorrect. In this sense, Ada's
type system  encourages programmers to think about data at a high level of
abstraction. Ada supports overlays and unchecked conversions as a way of
converting between unrelated data type, which are typically used for
interfacing with low-level elements such as registers.

In Ada, arrays aren't interchangeable with operations on pointers like in C.
Also, array types are considered first-class citizens and have dedicated
semantics such as the availability of the array's boundaries at run-time.
Therefore, unhandled array overflows are impossible unless checks are
suppressed. Any discrete type can serve as an array index, and we can specify
both the starting and ending bounds. In addition, Ada offers high-level
operations for copying, slicing, and assigning values to arrays.

Although Ada supports pointers, most situations that would require a pointer in
C do not in Ada. In the vast majority of the cases, indirect memory management
can be hidden from the developer and thus prevent many potential errors. In
C, pointers are typically used to pass references to subprograms, for example.
In contrast, Ada parameter modes indicate the flow of information to the
reader, leaving the means of passing that information to the compiler.

When translating pointers from C code to Ada, we need to assess whether they
are needed in the first place. Ada pointers (access types) should only be used
with complex structures that cannot be allocated at run-time. There are many
situations that would require a pointer in C, but do not in Ada. For example,
arrays |mdash| even when dynamically allocated |mdash|, results of functions,
passing of large structures as parameters, access to registers, etc.

Because of the absence of namespaces, global names in C tend to be very long.
Also, because of the absence of overloading, they can even encode type names in
their name. In Ada, a package is a namespace. Also, we can use the private part
of a package to declare private types and private subprograms. In fact, private
types are useful for preventing the users of those types from depending on the
implementation details. Another use-case is the prevention of package users
from accessing the package state/data arbitrarily.

Ada has a dedicated set of features for interfacing with other languages, so we
can easily interface with our existing C code before translating it to Ada.
Also, GNAT includes automatic binding generators. Therefore, instead of
re-writing the entire C code upfront, which isn't practical or
cost-effective, we can selectively translate modules from C to Ada.

When it comes to implementing concurrency and real time, Ada offers several
options. Ada provides high level constructs such as tasks and protected
objects to express concurrency and synchronization, which can be used when
running on top of an operating system such as Linux. On more constrained
systems, such as bare metal or some real-time operating systems, a subset of
the Ada tasking capabilities |mdash| known as the Ravenscar and Jorvik profiles
|mdash| is available. Though restricted, this subset also has nice properties,
in particular the absence of deadlock,the absence of priority inversion,
schedulability and very small footprint. On bare metal systems, this also
essentially means that Ada comes with its own real-time kernel. The advantage
of using the full Ada tasking model or the restricted profiles is to enhance
portability.

Ada includes many features typically used for embedded programming:

- Built-in support for handling interrupts, so we can process interrupts by
  attaching a handler |mdash| as a protected procedure |mdash| to it.

- Built-in support for handling both volatile and atomic data.

- Support for register overlays, which we can use to create a structure that
  facilitates manipulating bits from registers.

- Support for creating data streams for serialization of arbitrary information
  and transmission over a communication channel, such as a serial port.

- Built-in support for fixed-point arithmetic, which is an option when our
  target device doesn't have a floating-point unit or the result of
  calculations needs to be bit-exact.

Also, Ada compilers such as GNAT have built-in support for directly mixing Ada
and Assembly code.

Ada also supports contracts, which can be associated with types and variables
to refine values and define valid and invalid values. The most common kind of
contract is a *range constraint* |mdash| using the :ada:`range` reserved word.
Ada also supports contract-based programming in the form of preconditions and
postconditions. One typical benefit of contract-based programming is the
removal of defensive code in subprogram implementations.

It is common to see embedded software being used in a variety of configurations
that require small changes to the code for each instance. In C, variability is
usually achieved through macros and function pointers, the former being tied to
static variability and the latter to dynamic variability. Ada offers many
alternatives for both techniques, which aim at structuring possible variations
of the software. Examples of static variability in Ada are: genericity, simple
derivation, configuration pragma files, and configuration packages. Examples of
dynamic variability in Ada are: records with discriminants, variant records
|mdash| which may include the use of unions |mdash|, object orientation,
pointers to subprograms, and design by components using dynamic libraries.

There shouldn't be significant performance differences between code written in
Ada and code written in C |mdash| provided that they are semantically
equivalent. One reason is that the two languages are fairly similar in the way
they implement imperative semantics, in particular with regards to memory
management or control flow. Therefore, they should be equivalent on average.
However, when a piece of code in Ada is significantly slower than its
counterpart in C, this usually comes from the fact that, while the two pieces
of code appear to be semantically equivalent, they happen to be actually quite
different. Fortunately, there are strategies that we can use to improve the
performance and make it equivalent to the C version. These are some examples:

- Clever use of compilation switches, which might optimize the performance of
  an application significantly.

- Suppression of checks at specific parts of the implementation.

    - Although runtime checks are very useful and should be used as much as
      possible, they can also increase the overhead of implementations at
      certain hot-spots.

- Restriction of assertions to development code.

    - For example, we may use assertions in the debug version of the code and
      turn them off in the release version.

    - Also, we may use formal proof to decide which assertions we turn off in
      the release version. By formally proving that assertions will never fail
      at run-time, we can safely deactivate them.

Formal proof |mdash| a form of static analysis |mdash| can give strong
guarantees about checks, for all possible conditions and all possible inputs.
It verifies conditions prior to execution, even prior to compilation, so we can
remove bugs earlier in the development phase. This is far less expensive than
doing so later because the cost to fix bugs increases exponentially over the
phases of the project life cycle, especially after deployment. Preventing bug
introduction into the deployed system is the least expensive approach of all.

Formal analysis for proof can be achieved through the SPARK subset of the Ada
language combined with the :program:`gnatprove` verification tool. SPARK is a
subset encompassing most of the Ada language, except for features that preclude
proof.

In Ada, several common programming errors that are not already detected at
compile-time are detected instead at run-time, triggering *exceptions* that
interrupt the normal flow of execution. However, we may be able to prove that
the language-defined checks won't raise exceptions at run-time. This is known
as proving *Absence of Run-Time Errors*. Successful proof of these checks
is highly significant in itself. One of the major resulting benefits is that we
can deploy the final executable with checks disabled.

In many situations, the migration of C code to Ada is justified by an increase
in terms of integrity expectations, in which case it's expected that
development costs will raise. However, Ada is a more expressive, powerful
language, designed to reduce errors earlier in the life-cycle, thus reducing
costs. Therefore, Ada makes it possible to write very safe and secure software
at a lower cost than languages such as C.
