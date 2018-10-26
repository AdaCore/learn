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
