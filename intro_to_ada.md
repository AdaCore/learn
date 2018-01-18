# Introduction to Ada

This document is a concise introduction to the Ada language, for people who
already have some experience with programming. It will go over the important
concepts of Ada in the order that made the most sense writing it.

# Introduction

## History

In the seventies, the American department of Defense suffered from an explosion
of the number of languages, each team using a different set of idioms/scripts.
The DOD decided to solve this by issuing a request for proposal to big
companies of the time. The proposal that was selected by the DOD in the end was
Jean Ichbiah's proposal, on behalf on HoneyWell bull.

The first standard for the language was issued in 1983, with revisions being
made in 1995, 2005 and 2012, each time adding major features to the language.

This tutorial will focus on Ada 2012 as a whole, rather than teach different
versions of the language.

## Ada today

Today, Ada is mainly used in real-time/safety critical systems, with a general
focus on embedded systems. While Ada is and can be used as a general purpose
language, it will really shine in low level applications:

- Embedded systems with low memory requirements/no garbage collector allowed.
- Direct interfacing with hardware.
- Soft or hard real-time systems.
- Low level systems programming.

This list is intentionally abstract. While today Ada has certain domains/niches
where it is used a lot, like Aerospace & Defense, civil aviation, public
transportation, etc. it also means that Ada can be a great language for other
applications in those abstract categories, such as:

- [Video game programming](https://github.com/AdaDoom3/AdaDoom3)
- [Real-time audio](http://www.electronicdesign.com/embedded-revolution/assessing-ada-language-audio-applications)
- [Kernel modules](http://www.nihamkin.com/tag/kernel.html)

This is a non-comprehensive list, that hopefully sheds light on which kind of
programming Ada is good at.

In terms of modern languages, the closest in terms of targets and level of
abstraction are probably [C++](https://fr.wikipedia.org/wiki/C%2B%2B)
and [Rust](https://www.rust-lang.org/en-US/).

# Imperative language

Ada is a multi-paradigm language, but at it's core, it contains a simple,
coherent procedural/imperative language akin to C or Pascal.

> One important distinction with a language like C is that statements and
> expressions are very clearly distinguished.

## Hello world

Let's go over a very simple imperative Ada program:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Greet is
begin
  Put_Line("Hello, World!");
end Greet;
```

If you compile that source with the GNAT compiler, you will get a pretty
unsurprising result.

```bash
 $ gprbuild greet.adb
using project file /home/amiard/libadalang/sandbox/x86_64-linux/gnat/install/share/gpr/_default.gpr
Compile
   [Ada]          greet.adb
Bind
   [gprbind]      greet.bexch
   [Ada]          greet.ali
Link
   [link]         greet.adb

 $ ./greet
Hello, World!
 %
```

## Imperative language - Loops

Let's go over a very simple imperative Ada program:

```ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Greet is
begin
   for I in 1 .. 10 loop
      Put_Line("Hello, World!");
   end loop;
end Greet;
```

There are several note-worthy

- I here denotes a constant that is only accessible in the
  loop.
- "1 .. 10" is a range.
- Put_Line is a procedure call. procedure is like a fn
  returning void in C/C++.

## Imperative language - If/Else
## Imperative language - Case statement

# Strongly typed language
## What is a type?
## Integers
## Enumerations
## Strong typing
## Subtypes

# Arrays
## Array type declaration
## Array index
## Indexation
## Shortcut for index
## Range attribute
## Unconstrained arrays
## Declaring arrays
## Predefined array type: String
## Declaring arrays (2)

# Modular/Structured programming
## Packages
## With-ing a package
## Using a package
## Package body

# Subprograms
## Subprograms
## Parameters modes
## Subprogram calls
## Function calls
## Mutually recursive subprograms
## Nested subprograms

# More about types
## Array
## Array slices
## Records
### - default values
### - Literals
### - Selection
## Access types (pointers)
## Dereferencing
## Allocation (by type)
## Allocation (by expression)
## Mutually recursive types
## More about records
## Records with discriminant
## Records with variant

# Privacy
## Private part
## Abstract data types
## Limited types

# Generics
## Generic declaration
## Generic body
## Generic instantiation
## Formal types
## Formal objects
## Formal subprograms

# Exceptions
## Exception declaration
## Raising an exception
## Handling an exception
## Predefined exceptions

# Tasking
## Simple task
## Simple synchronization
## Delay
## Synchronization: rendez-vous
## Cycling tasks
## Protected objects
## Protected objects: body
## Protected objects: entries
## Protected types

# Interfacing
## Type convention
## Foreign subprograms
## Foreign variables
## Multi-language project

# Object oriented programming
## Tagged types
## Classwide types
## Dispatching operations
## Interfaces

# Standard library
## Standard package
## Containers
## Dates & Times
## Strings
## Files and streams
## Dynamic allocation and reclamation
