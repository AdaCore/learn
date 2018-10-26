:orphan:

Structure
---------

- [ ] Intro

    - [ ] List of chapters

- [ ] Packages

    - [ ] Child and nested units
    - [ ] Private packages
    - [ ] Partial dependencies
    - [ ] Elaboration

- [ ] Typing

    - [ ] Overloading (*material reuse*)

- [ ] Access types

    - [ ] Aliasing
    - [ ] Dereferencing
    - [ ] Implicit Dereferencing
    - [ ] Accessibility levels
    - [ ] Unchecked access
    - [ ] Nullability  (*material reuse*)
    - [ ] Anonymous access types

- [ ] Object-Oriented Programming / Tagged Types

    - [ ] Primitives
    - [ ] Interfaces  (*material reuse*)
    - [ ] Dynamic Polymorphism
    - [ ] References  (*material reuse*)
    - [ ] Controlled types
    - [ ] ``Ada.Tags`` package

- [ ] Limited types

    - [ ] Assignment and equality
    - [ ] Limited types as parameter
    - [ ] Initialization and function return
    - [ ] Limited record elements
    - [ ] Private implementation of limited types

- [ ] Advanced Privacy

    - [ ] Type view
    - [ ] Incomplete types

- [ ] Renaming

    - [ ] Renaming declarations
    - [ ] Renaming tricks

- [ ] Strong Typing

    - [ ] Type-Based Security (*material reuse*)
    - [x] Table access
    - [x] Multiple indices
    - [ ] Discriminants

- [ ] Exceptions

    - [ ] ``Ada.Exceptions`` package (*material reuse*)
    - [ ] Out and Uninitialized (*material reuse*)

- [ ] Contracts

    - [ ] Class-wide contracts
    - [ ] Common pitfalls

- [ ] Memory Management

    - [ ] Memory pools (*material reuse*)
    - [ ] Secondary stack

- [ ] Generics

    - [x] Formal packages
    - [x] Formal objects
    - [x] Formal access types
    - [ ] Generic interfaces
    - [x] Generic numeric types
    - [ ] Partial parameterization

- [ ] Freezing

    - [ ] Introduction
    - [ ] Freezing rules

- [ ] Containers

    - [ ] Linked lists
    - [ ] Trees
    - [ ] Queue containers
    - [ ] Holder container

- [ ] Tasking

    - Statements
        - [ ] Select statements
        - [ ] Guard expressions
        - [ ] Requeue instruction
        - [ ] Abort statements
        - [ ] ``select...then`` statement
    - [ ] Real-time programming (*material reuse*)
    - [ ] High-Performance (*material reuse*)
    - [ ] Task IDs and attributes
    - [ ] Task termination
    - [ ] Tasking and exceptions
    - [ ] Ravenscar profile

- [ ] File I/O

    - [ ] Efficient Stream I/O for Array Types (*material reuse*)
    - [ ] Container streaming

- [ ] Numerics

    - [ ] Modular types

- [ ] Design Patterns

    - [ ] Factory Functions (*material reuse*)
    - [ ] Scope Locks Idiom (*material reuse*)
    - [ ] Visitor Pattern (*material reuse*)
    - [ ] Overridable Class Attributes (*material reuse*)

    - Based on
      `list of design patterns <https://en.wikipedia.org/wiki/Software_design_pattern>`_
      and `list for C++ <https://en.wikibooks.org/wiki/C%2B%2B_Programming/Code/Design_Patterns>`_

        - **Note**: the goal is to identify equivalent patterns for Ada

        - Creational Patterns

            - [ ] Builder
            - [ ] Factory / Abstract Factory
            - [ ] Lazy initialization
            - [ ] Multiton
            - [ ] Object pool
            - [ ] Dependency Injection
            - [ ] Prototype
            - [ ] Resource acquisition is initialization (RAII)
            - [ ] Singleton

        - Structural Patterns

            - [ ] Adapter or Wrapper
            - [ ] Bridge
            - [ ] Composite
            - [ ] Decorator
            - [ ] Extension object
            - [ ] Façade
            - [ ] Flyweight
            - [ ] Front controller
            - [ ] Marker
            - [ ] Module (probably not worth mentioning)
            - [ ] Proxy
            - [ ] Twin
            - [ ] Mixin
            - [ ] Interface-based Programming (IBP)

        - Behavioral Patterns

            - [ ] Blackboard
            - [ ] Chain of Responsibility
            - [ ] Command
            - [ ] Interpreter
            - [ ] Iterator
            - [ ] Mediator
            - [ ] Memento
            - [ ] Null object
            - [ ] Observer or Publish/subscribe
            - [ ] Servant
            - [ ] Specification
            - [ ] State
            - [ ] Strategy
            - [ ] Template Method
            - [ ] Visitor
            - [ ] Model-View-Controller (MVC)

        - Concurrency patterns

            - [ ] Active Object
            - [ ] Balking
            - [ ] Binding properties
            - [ ] Compute kernel
            - [ ] Double-checked locking
            - [ ] Event-based asynchronous
            - [ ] Guarded suspension
            - [ ] Messaging design pattern (MDP)
            - [ ] Monitor object
            - [ ] Reactor
            - [ ] Read-write lock
            - [ ] Scheduler
            - [ ] Thread pool
            - [ ] Thread-specific storage

    - [ ] Ada-specific design patterns

        - Signature package

- [ ] Low-level programming

    - [ ] Data Representation (*material reuse*)

        - Include: ``Object_Size``, ``Value_Size``, ``Alignment``, ``T'Base``

    - [ ] Bit-fields
    - [ ] Enumeration representation clauses
    - [ ] Valid attribute
    - [ ] Shared variables
    - [ ] Address clauses
    - [ ] Overlays and conversions
    - [ ] ``System.Storage_Elements`` package
    - [ ] ``System.Address_To_Access_Conversions`` package
    - [ ] Interfaces package
    - [ ] Fat/thin/flat pointers for arrays
    - [ ] Inline Assembly

- [ ] Optimizations

    - [ ] Performance considerations

        - [ ] Vectors vs. arrays
        - [ ] Inlining
        - [ ] Copying / moving elements (compared to C++ move semantics)

    - [ ] Memory considerations

- [ ] Systems

    - [ ] ``Ada.Directories`` package
    - [ ] ``Ada.Environment_Variables`` package

- [ ] Real-Time Systems

    - [ ] The Real-Time Systems Annex

- [ ] Distributed Systems

    - [ ] The Distributed Systems Annex (*material reuse*)

- [ ] High-Integrity Systems

    - [ ] The High Integrity Systems Annex

- [ ] Pragmas, Aspects and Attributes

    - **Notes**

        - Include link to parts of the book where some of the elements below
          are explained; otherwise, explain them in this chapter

        - Point to the RM if an element is not explained neither in this
          chapter nor mentioned elsewhere in the book.

    - [ ] Pragmas

        - [ ] List of pragmas

    - [ ] Aspects and Attributes

        - [ ] List of aspects

            - Include link to parts of the book where some of the aspects
              are explained

        - [ ] List of attributes

            - Include link to parts of the book where some of the attribute
              are explained

- Interfacing with C/ C++

    - Interfacing with C

        - [x] Using unconstrained types

    - Interfacing with C++

        - [x] C++ symbol mangling
        - [x] C++ classes
        - [ ] C++ constructors (*material reuse*)

- [ ] Appendix

    - [ ] Incompatibilities between Ada standards

        - [ ] Incompatibilities between Ada 83 and Ada 95 (*material reuse*)
