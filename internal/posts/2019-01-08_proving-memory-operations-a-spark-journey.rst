:orphan:

:code-config:`run_button=False;prove_button=False;accumulate_code=True`
:code-config:`reset_accumulator=True`

Proving Memory Operations: A SPARK Journey
==========================================

.. include:: <isopub.txt>

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

by Quentin Ochem |mdash| Jan 08, 2019

Original post:
`AdaCore blog post <https://blog.adacore.com/proving-memory-operations-a-spark-journey>`_

The promise behind the SPARK language is the ability to formally
demonstrate properties in your code regardless of the input values that
are supplied |mdash| as long as those values satisfy specified
constraints. As such, this is quite different from static analysis tools
such as our CodePeer or the typical offering available for e.g. the C
language, which trade completeness for efficiency in the name of
pragmatism. Indeed, the problem they're trying to solve |mdash| finding
bugs in existing applications |mdash| makes it impossible to be complete.
Or, if completeness is achieved, then it is at the cost of massive amount
of uncertainties ("false alarms"). SPARK takes a different approach. It
requires the programmer to stay within the boundaries of a (relatively
large) Ada language subset and to annotate the source code with additional
information |mdash| at the benefit of being able to be complete (or sound)
in the verification of certain properties, and without inundating the
programmer with false alarms.

With this blog post, I'm going to explore the extent to which SPARK can
fulfill this promise when put in the hands of a software developer with no
particular affinity to math or formal methods. To make it more
interesting, I'm going to work in the context of a small low-level
application to gauge how SPARK is applicable to embedded device level
code, with some flavor of cyber-security.

The problem to solve and its properties
---------------------------------------

As a prelude, even prior to defining any behavior and any custom property
on this code, the SPARK language itself defines a default property, the
so-called absence of run-time errors. These include out of bounds access
to arrays, out of range assignment to variables, divisions by zero, etc.
This is one of the most advanced properties that traditional static
analyzers can consider. With SPARK, we're going to go much further than
that, and actually describe functional requirements.

Let's assume that I'm working on a piece of low level device driver whose
job is to set and move the boundaries of two secure areas in the memory: a
secure stack and a secure heap area. When set, these areas come with
specific registers that prevent non-secure code from reading the contents
of any storage within these boundaries. This process is guaranteed by the
hardware, and I'm not modeling this part. However, when the boundaries are
moved, the data that was in the old stack & heap but not in the new one is
now accessible. Unless it is erased, it can be read by non-secure code and
thus leak confidential information. Moreover, the code that changes the
boundaries is to be as efficient as possible, and I need to ensure that I
don't erase memory still within the secure area.

What I described above, informally, is the full functional behavior of
this small piece of code. This could be expressed as a boolean expression
that looks roughly like:

.. math::
        dataToErase = (oldStack \cup oldHeap) \cap
            \overline{newStack \cup newHeap}

Or in other words, the data to erase is everything that was in the
previous memory

.. math::
        (oldStack \cup oldHeap)

is not in the new memory

.. math::
        \overline{newStack \cup newHeap}

Another way to write the same property is to use a quantifier on every
byte of memory, and say that on every byte, if this byte is in the old
stack or the old heap but not in the new stack or the new heap, it should
be erased:

.. math::
        \forall b \in memory,
            ((isOldStack(b) \lor isOldHeap(b))
                \land \lnot (isNewStack(b) \lor isNewHeap(b))
                    \Longleftrightarrow isErased(b))


Which means that for all the bytes in the memory

.. math::
        \forall b \in memory,

what's in the old regions

.. math::
        (isOldStack(b) \lor isOldHeap(b))

but not in the new ones

.. math::
        \lnot (isNewStack(b) \lor isNewHeap(b))

has to be erased

.. math::
        \Longleftrightarrow isErased(b))

We will also need to demonstrate that the heap and the stack are disjoint.

Ideally, we'd like to have SPARK make the link between these two ways of
expressing things, as one may be easier to express than the other.

When designing the above properties, it became quite clear that I needed
some intermediary library with set operations, in order to be able to
express unions, intersections and complement operations. This will come
with its own set of lower-level properties to prove and demonstrate.

Let's now look at how to define the specification for this memory
information.

Memory Specification and Operations
-----------------------------------

The first step is to define some way to track the properties of the memory
- that is whether a specific byte is a byte of heap, of stack, and what
kind of other properties they can be associated with (like, has it been
erased?). The interesting point here is that the final program executable
should not have to worry about these values |mdash| not only would it be
expensive, it wouldn't be practical either. We can't easily store and
track the status of every single byte. These properties should only be
tracked for the purpose of statically verifying the required behavior.

There is a way in SPARK to designate code to be solely used for the
purpose of assertion and formal verification, through so-called "ghost
code". This code will not be compiled to binary unless assertions are
activated at run-time. But here we'll push this one step further by
writing ghost code that can't be compiled in the first place. This
restriction imposed on us will allow us to write assertions describing the
entire memory, which would be impossible to compile or run.

The first step is to model an address. To be as close as possible to the
actual way memory is defined, and to have access to Ada's bitwise
operations, we're going to use a modular type. It turns out that this
introduces a few difficulties: a modular type wraps around, so adding one
to the last value goes back to the first one. However, in order to prove
absence of run-time errors, we want to demonstrate that we never overflow.
To do that, we can define a precondition on the :ada:`+` and :ada:`-`
operators, with an initial attempt to define the relevant preconditions:

.. code-block:: ada

    function "+" (Left, Right : Address_Type) return Address_Type
    is (Left + Right)
    with Pre => Address_Type'Last - Left >= Right;

    function "-" (Left, Right : Address_Type) return Address_Type
    is (Left - Right)
    with Pre => Left >= Right;

The preconditions  verify that :ada:`Left + Right` doesn't exceed
:ada:`Address_Type'Last` (for :ada:`+`), and that :ada:`Left - Right` is
above zero (for :ada:`-`). Interestingly, we could have been tempted to
write the first precondition the following way:

.. code-block:: ada

    with Pre => Left + Right <= Address_Type'Last;

However, with wrap-around semantics inside the precondition itself, this
would always be true.

There's still a problem in the above code, due to the fact that :ada:`+`
is actually implemented with :ada:`+` itself (hence there's is an infinite
recursive call in the above code). The same goes for :ada:`-`. To avoid
that, we're going to introduce a new type :ada:`Address_Type_Base` to do
the computation without contracts, :ada:`Address_Type` being the actual
type in use. The full code, together with some size constants (assuming
32 bits), then becomes:

.. code-block:: ada

    Word_Size    : constant := 32;
    Memory_Size  : constant := 2 ** Word_Size;
    type Address_Type_Base is mod Memory_Size; -- 0 .. 2**Word_Size - 1
    type Address_Type is new Address_Type_Base;

    function "+" (Left, Right : Address_Type) return Address_Type
    is (Address_Type (Address_Type_Base (Left) + Address_Type_Base (Right)))
    with Pre => Address_Type'Last - Left >= Right;

    function "-" (Left, Right : Address_Type) return Address_Type
    is (Address_Type (Address_Type_Base (Left) - Address_Type_Base (Right)))
    with Pre => Left >= Right;

Armed with the above types, it's now time to get started on the modeling
of actual memory. We're going to track the status associated with every
byte. Namely, whether a given byte is part of the :ada:`Stack`, part of
the :ada:`Heap`, or neither; and whether that byte has been
:ada:`Scrubbed` (erased). The prover will reason on the entire memory.
However, the status tracking will never exist in the program itself |mdash|
it will just be too costly to keep all this data at run time. Therefore
we're going to declare all of these entities as :ada:`Ghost` (they are
here only for the purpose of contracts and assertions), and we will never
enable run-time assertions. The code looks like:

.. code-block:: ada

    type Byte_Property is record
       Stack    : Boolean;
       Heap     : Boolean;
       Scrubbed : Boolean;
    end record
    with Ghost;

    type Memory_Type is array (Address_Type) of Byte_Property
    with Ghost;

    Memory : Memory_Type
    with Ghost;

Here, :ada:`Memory` is a huge array declared as a global ghost variable.
We can't write executable code with it, but we can write contracts. In
particular, we can define a contract for a function that sets the heap
between two address values. As a precondition for this function, the lower
value has to be below or equal to the upper one. As a postcondition, the
property of the memory in the range will be set to :ada:`Heap`. The
specification looks like this:

.. code-block:: ada

    procedure Set_Heap (From, To : Address_Type)
      with
        Pre => To >= From,
        Post => (for all B in Address_Type =>
                   (if B in From .. To then
                          Memory (B).Heap
                        else
                          not Memory (B).Heap)),
      Global => (In_Out => Memory);

Note that I'm also defining a global here which is how :ada:`Memory` is
processed. Here it's modified, so :ada:`In_Out`.

While the above specification is correct, it's also incomplete. We're
defining what happens for the :ada:`Heap` property, but not the others.
What we expect here is that the rest of the memory is unmodified. Another
way to say this is that only the range :ada:`From .. To` is updated, the
rest is unchanged. This can be modelled through the :ada:`'Update`
attribute, and turn the postcondition into:

.. code-block:: ada

    Post => (for all B in Address_Type =>
       (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Heap => True)
        else Memory (B) = Memory'Old (B)'Update (Heap => False))),

Literally meaning "The new value of memory equals the old value of memory
(:ada:`Memory'Old`) with changes (:ada`'Update`) being :ada:`Heap = True`
from :ada:`From` to :ada:`To`, and :ada:`False` elsewhere".

Forgetting to mention what doesn't change in data is a common mistake when
defining contracts. It is also a source of difficulties to prove code, so
it's a good rule of the thumb to always consider what's unchanged when
checking these postconditions. Of course, the only relevant entities are
those accessed and modified by the subprogram. Any variable not accessed
is by definition unchanged.

Let's now get to the meat of this requirement. We're going to develop a
function that moves the heap and the stack boundaries, and scrubs all
that's necessary and nothing more. The procedure will set the new heap
boundaries between :ada:`Heap_From .. Heap_To`, and stack boundaries
between :ada:`Stack_From` and :ada:`Stack_To`, and is defined as such:

.. code-block:: ada

    procedure Move_Heap_And_Stack
       (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type);

Now remember the expression of the requirement from the previous section:

.. math::
        \forall b \in memory,
            ((isOldStack(b) \lor isOldHeap(b))
                \land \lnot (isNewStack(b) \lor isNewHeap(b))
                    \Longleftrightarrow isErased(b))

This happens to be a form that can be easily expressed as a quantifier, on
the :ada:`Memory` array described before:

.. code-block:: ada

    (for all B in Address_Type =>
       (((Memory'Old (B).Stack or Memory'Old (B).Heap)
             and then not
          (Memory (B).Stack or Memory (B).Heap))
       = Memory (B).Scrubbed));

The above is literally the transcription of the property above, checking
all bytes :ada:`B` in the address range, and then stating that if the old
memory is :ada:`Stack` or :ada:`Heap` and the new memory is not, then the
new memory is scrubbed, otherwise not. This contract is going to be the
postcondition of :ada:`Move_Heap_And_Stack`. It's the state of the system
after the call.

Interestingly, for this to work properly, we're going to need also to
verify the consistency of the values of :ada:`Heap_From`, :ada:`Heap_To`,
:ada:`Stack_From` and :ada:`Stack_To` |mdash| namely that the :ada:`From`
is below the :ada:`To` and that there's no overlap between heap and stack.
This will be the precondition of the call:

.. code-block:: ada

    ((Stack_From <= Stack_To and then Heap_From <= Heap_To) and then
     Heap_From not in Stack_From .. Stack_To and then
     Heap_To not in Stack_From .. Stack_To and then
     Stack_From not in Heap_From .. Heap_To and then
     Stack_To not in Heap_From .. Heap_To);

That's enough for now at this stage of the demonstration. We have
specified the full functional property to be demonstrated. Next step is to
implement, and prove this implementation.

This is the complete code so far:

.. code:: ada prove_button

    package Area_Math
      with SPARK_Mode => On
    is

       Word_Size    : constant := 32;
       Memory_Size  : constant := 2 ** Word_Size;
       type Address_Type_Base is mod Memory_Size;
       type Address_Type is new Address_Type_Base;

       function "+" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) + Address_Type_Base (Right)))
         with Pre => Address_Type_Base'Last - Address_Type_Base (Left) >= Address_Type_Base (Right);

       function "-" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) - Address_Type_Base (Right)))
         with Pre => Left >= Right;

    end Area_Math;

.. .. code:: ada spark-report-all
.. .. code:: ada spark-flow

.. code:: ada prove_button

    with Area_Math; use Area_Math;

    package Memory_Analysis
    with SPARK_Mode => On
    is

       type Byte_Property is record
          Stack    : Boolean;
          Heap     : Boolean;
          Scrubbed : Boolean;
       end record
       with Ghost;

       type Memory_Type is array (Address_Type) of Byte_Property with Ghost;

       Memory : Memory_Type with Ghost;

       procedure Set_Heap (From, To : Address_Type)
         with
           Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Heap => True) else Memory (B) = Memory'Old (B)'Update (Heap => False))),
         Global => (In_Out => Memory);

       procedure Move_Heap_And_Stack
          (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type);

    end Memory_Analysis;

Low-level Set Library
---------------------

In order to implement the service, it would actually be useful to have a
lower level library that implements set of ranges, together with
:ada:`Union`, :ada:`Intersect` and :ada:`Complement` functions. This
library will be called to determine which regions to erase |mdash| it will
also be at the basis of the proof. Because this is going to be used at
run-time, we need a very efficient implementation. A set of ranges is
going to be defined as an ordered disjoint list of ranges. Using a record
with discriminant, it looks like:

.. code-block:: ada

    type Area is record
       From, To : Address_Type;
    end record
    with Predicate => From <= To;

    type Area_Array is array(Natural range <>) of Area;

    type Set_Base (Max : Natural) is record
       Size  : Natural;
       Areas : Area_Array (1 .. Max);
    end record;

Note that I call this record :ada:`Set_Base`, and not :ada:`Set`. Bear
with me.

You may notice above already a first functional predicate. In the area
definition, the fields :ada:`From` and :ada:`To` are described such as
:ada:`From` is always below :ada:`To`. This is a check very similar to an
Ada range check in terms of where it applies |mdash| but on a more
complicated property. For :ada:`Set`, I'm also going to express the
property *I* described before, that :ada:`Areas` are ordered (which can can
be expressed as the fact that the :ada:`To` value of an element *N* is
below the :ada:`From` value of the element *N + 1*) and disjoint (the
:ada:`From` of element *N* minus the :ada:`To` of element *N + 1* is at
least 1). There's another implicit property to be specified which is that
the field :ada:`Size` is below or equal to the :ada:`Max` size of the
array. Being able to name and manipulate this specific property has some
use, so I'm going to name it in an expression function:

.. code-block:: ada

    function Is_Consistent (S : Set_Base) return Boolean is
       (S.Size <= S.Max and then
          ((for all I in 1 .. S.Size - 1 =>
             S.Areas (I).To < S.Areas (I + 1).From and then
             S.Areas (I + 1).From - S.Areas (I).To > 1)));

Now comes the predicate. If I were to write:

.. code-block:: ada

    type Set_Base (Max : Natural) is record
       Size  : Natural;
       Areas : Area_Array (1 .. Max);
    end record
    with Predicate => Is_Consistent (Set_Base);

I would have a recursive predicate call. Predicates are checked in
particular when passing parameters, so :ada:`Is_Consistent` would check
the predicate of :ada:`Set_Base`, which is a call to :ada:`Is_Consistent`,
which would then check the predicate and so on. To avoid this, the
predicate is actually applied to a subtype:

.. code-block:: ada

    subtype Set is Set_Base
    with Predicate => Is_Consistent (Set);

As it will later turn out, this property is very fundamental to the
ability for proving other properties. At this stage, it's already nice to
see some non-trivial property being expressed, namely that the structure
is compact (it doesn't waste space by having consecutive areas that could
be merged into one, or said otherwise, all areas are separated by at least
one excluded byte).

The formal properties expressed in the next steps will be defined in the
form of inclusion |mdash| if something is included somewhere then it may
or may not be included somewhere else. This inclusion property is expressed
as a quantifier over all the ranges. It's not meant to be run by the
program, but only for the purpose of property definition and proof. The
function defining that a given byte is included in a set of memory ranges
can be expressed as follows:

.. code-block:: ada

    function Includes (B : Address_Type; S : Set) return Boolean
    is (for some I in 1 .. S.Size =>
        B in S.Areas (I).From .. S.Areas (I).To)
    with Ghost;

Which means that for all the areas in the set :ada:`S`, :ada:`B` is
included in the set if :ada:`B` is included in at least one (some) of the
areas in the set.

I'm now going to declare a constructor :ada:`Create` together with three
operators, :ada:`or`, :ada:`and`, :ada:`not` which will respectively
implement union, intersection and complement. For each of those, I need to
provide some expression of the maximum size of the set before and after
the operation, as well as the relationship between what's included in the
input and in the output.

The specification of the function :ada:`Create` is straightforward. It
takes a range as input, and creates a set where all elements within this
range are contained in the resulting set. This reads:

.. code-block:: ada

    function Create (From, To : Address_Type) return Set
    with Pre => From <= To,
       Post => Create'Result.Max = 1
          and then Create'Result.Size = 1
          and then (for all B in Address_Type =>
             Includes (B, Create'Result) = (B in From .. To));

Note that interestingly, the internal implementation of the :ada:`Set`
isn't exposed by the property computing the inclusion. I'm only stating
what should be included without giving details on how it should be
included. Also note that as in many other places, this postconditon isn't
really something we'd like to execute (that would be possibly a long loop
to run for large area creation). However, it's a good way to model our
requirement.

Let's carry on with the :ada:`not`. A quick reasoning shows that at worst,
the result of :ada:`not` is one area bigger than the input. We'll need a
precondition checking that the :ada:`Size` can indeed be incremented (it
does not exceed the last value of the type). The postcondition is that
this :ada:`Size` has been potentially incremented, that values that were
not in the input :ada:`Set` are now in the resulting one and vice-versa.
The operator with its postcondition and precondition reads:

.. code-block:: ada

    function "not" (S : Set) return Set
    with
       Pre => Positive'Last - S.Size > 0,
       Post =>
          (for all B in Address_Type =>
              Includes (B, "not"'Result) /= Includes (B, S))
           and then "not"'Result.Size <= S.Size + 1;

The same reasoning can be applied to :ada:`and` and :ada:`or`, which leads
to the following specifications:

.. code-block:: ada

    function "or" (S1, S2 : Set) return Set
    with
       Pre => Positive'Last - S1.Size - S2.Size >= 0,
       Post => "or"'Result.Size <= S1.Size + S2.Size
          and Is_Consistent ("or"'Result)
          and (for all B in Address_Type =>
             (Includes (B, "or"'Result)) =
                ((Includes (B, S1) or Includes (B, S2))));

    function "and" (S1, S2 : Set) return Set
    with
       Pre => Positive'Last - S1.Size - S2.Size >= 0,
       Post => "and"'Result.Size <= S1.Size + S2.Size
          and (for all B in Address_Type =>
             (Includes (B, "and"'Result)) =
                ((Includes (B, S1) and Includes (B, S2))));

Of course at this point, one might be tempted to first prove the library
and then the user code. And indeed I was tempted and fell for it. However,
as this turned out to be a more significant endeavor, let's start by
looking at the user code.

This is the complete code so far:

.. code:: ada prove_button

    package Area_Math
    with SPARK_Mode => On
    is

       Word_Size    : constant := 32;
       Memory_Size  : constant := 2 ** Word_Size;
       type Address_Type_Base is mod Memory_Size;
       type Address_Type is new Address_Type_Base;

       function "+" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) + Address_Type_Base (Right)))
       with Pre => Address_Type_Base'Last - Address_Type_Base (Left) >= Address_Type_Base (Right);

       function "-" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) - Address_Type_Base (Right)))
       with Pre => Left >= Right;

       --
       -- NEW for this section
       --

       type Area is record
          From, To : Address_Type;
       end record
         with Predicate => From <= To;

       type Area_Array is array (Natural range <>) of Area;

       type Set_Base (Max : Natural) is record
          Size  : Natural;
          Areas : Area_Array (1 .. Max);
       end record;

       subtype Set is Set_Base
         with Predicate => Is_Consistent (Set);

       function Is_Consistent (S : Set_Base) return Boolean is
         (S.Size <= S.Max and then
            ((for all I in 1 .. S.Size - 1 =>
                   S.Areas (I).To < S.Areas (I + 1).From and then
              S.Areas (I + 1).From - S.Areas (I).To > 1)));

       function Includes (B : Address_Type; S : Set) return Boolean
       is (for some I in 1 .. S.Size =>
              B in S.Areas (I).From .. S.Areas (I).To)
       with Ghost;

       function Create (From, To : Address_Type) return Set
         with Pre => From <= To,
         Post => Create'Result.Max = 1
         and then Create'Result.Size = 1
         and then (for all B in Address_Type =>
                     Includes (B, Create'Result) = (B in From .. To));

       function "not" (S : Set) return Set
         with
           Pre => Positive'Last - S.Size > 0,
           Post =>
             (for all B in Address_Type =>
                Includes (B, "not"'Result) /= Includes (B, S))
             and then "not"'Result.Size <= S.Size + 1;

       function "or" (S1, S2 : Set) return Set
         with
           Pre => Positive'Last - S1.Size - S2.Size >= 0,
           Post => "or"'Result.Size <= S1.Size + S2.Size
           and Is_Consistent ("or"'Result)
         and (for all B in Address_Type =>
                (Includes (B, "or"'Result)) =
              ((Includes (B, S1) or Includes (B, S2))));

       function "and" (S1, S2 : Set) return Set
         with
           Pre => Positive'Last - S1.Size - S2.Size >= 0,
           Post => "and"'Result.Size <= S1.Size + S2.Size
           and (for all B in Address_Type =>
                  (Includes (B, "and"'Result)) =
                ((Includes (B, S1) and Includes (B, S2))));

    end Area_Math;

:ada:`Move_Heap_And_Stack` |mdash| the "easy" part
--------------------------------------------------

Armed with the :ada:`Set` library, implementing the move function is
relatively straightforward. We're using other services to get the heap and
stack boundaries, then creating the set, using the proper operators to
create the list to scrub, and then scrubbing pieces of memory one by one,
then finally setting the new heap and stack pointers.

.. code-block:: ada

    procedure Move_Heap_And_Stack
       (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
    is
       Prev_Heap_From, Prev_Heap_To,
       Prev_Stack_From, Prev_Stack_To : Address_Type;
    begin
       Get_Stack_Boundaries (Prev_Stack_From, Prev_Stack_To);
       Get_Heap_Boundaries (Prev_Heap_From, Prev_Heap_To);

       declare
          Prev : Set := Create (Prev_Heap_From, Prev_Heap_To) or
                        Create (Prev_Stack_From, Prev_Stack_To);
          Next : Set := Create (Heap_From, Heap_To) or
                        Create (Stack_From, Stack_To);
          To_Scrub : Set := Prev and not Next;
       begin
          for I in 1 .. To_Scrub.Size loop
             Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);
          end loop;

          Set_Stack (Stack_From, Stack_To);
          Set_Heap (Heap_From, Heap_To);
       end;
    end Move_Heap_And_Stack;

Now let's dive into the proof. As a disclaimer, the proofs we're going to
do for now on are hard. One doesn't need to go this far to take advantage
of SPARK. As a matter of fact, defining requirements formally is already
taking good advantage of the technology. Most people only prove data flow
or absence of run-time errors which is already a huge win. The next level
is some functional key properties. We're going one level up and entirely
proving all the functionalities. The advanced topics that are going to be
introduced in this section, such as lemma and loop invariants, are mostly
needed for these advanced levels.

The first step is to reset the knowledge we have on the scrubbing state of
the memory. Remember that all of the memory state is used to track the
status of the memory, but it does not correspond to any real code. To
reset the flag, we're going to create a special ghost procedure, whose
sole purpose is to set these flags:

.. code-block:: ada

    procedure Reset_Scrub
    with
       Post =>
          (for all B in Address_Type =>
             Memory (B) = Memory'Old(B)'Update (Scrubbed => False)),
       Ghost;

In theory, it is not absolutely necessary to provide an implementation to
this procedure if it's not meant to be compiled. Knowing what it's
supposed to do is good enough here. However, it can be useful to provide a
ghost implementation to describe how it would operate. The implementation
is straightforward:

.. code-block:: ada

    procedure Reset_Scrub is
    begin
       for B in Address_Type'Range loop
          Memory (B).Scrubbed := False;
       end loop;
    end Reset_Scrub;

We're going to hit now our first advanced proof topic. While extremely
trivial, the above code doesn't prove, and the reason why it doesn't is
because it has a loop. Loops are something difficult for provers and as of
today, they need help to break them down into sequential pieces. While the
developer sees a loop, SPARK sees three different pieces of code to prove,
connected by a so-called loop invariant which summarizes the behavior of
the loop:

.. code-block:: ada

    procedure Reset_Scrub is
       begin
          B := Address_Type'First;
             Memory (B).Scrubbed := False;
             --  [loop invariant]

    --  Then:

             --  [loop invariant]
             exit when B = Address_Type'Last;
    	 B := B + 1;
             Memory (B).Scrubbed := False;
             --  [loop invariant]

    --  And eventually:

             --  [loop invariant]
       end loop;
    end;

The difficulty is now about finding this invariant that is true on all
these sections of the code, and that ends up proving the postcondition. To
establish those, it's important to look at what needs to be proven at the
end of the loop. Here it would be that the entire array has
:ada:`Scrubbed = False`, and that the other fields still have the same
value as at the entrance of the loop (expressed using the attribute
:ada:`'Loop_Entry`):

.. code-block:: ada

    (for all X in Address_Type =>
       Memory (X) = Memory'Loop_Entry(X)'Update (Scrubbed => False))

Then to establish the loop invariant, the question is how much of this is
true at each step of the loop. The answer here is that this is true up
until the value :ada:`B`.

The loop invariant then becomes:

.. code-block:: ada

    (for all X in Address_Type'First .. B =>
       Memory (X) = Memory'Loop_Entry(X)'Update (Scrubbed => False))

Which can be inserted in the code:

.. code-block:: ada

    procedure Reset_Scrub is
    begin
       for B in Address_Type'Range loop
          Memory (B).Scrubbed := False;
          pragma Loop_Invariant
            (for all X in Address_Type'First .. B =>
                Memory (X) =
                Memory'Loop_Entry(X)'Update (Scrubbed => False))
       end loop;
    end Reset_Scrub;

Back to the main code. We can now insert a call to :ada:`Reset_Scrub`
before performing the actual scrubbing. This will not do anything in the
actual executable code, but will tell the prover to consider that the
ghost values are reset. Next, we have another loop scrubbing subsection:

.. code-block:: ada

    for I in 1 .. To_Scrub.Size loop
       Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);
    end loop;

Same as before, the question is here what is the property true at the end
of the loop, and breaking this property into what's true at each
iteration. The property true at the end is that everything included in the
:ada:`To_Scrub` set has been indeed scrubbed:

.. code-block:: ada

    (for all B in Address_Type =>
     Includes (B, To_Scrub) = Memory (B).Scrubbed);

This gives us the following loop with its invariant:

.. code-block:: ada

    for I in 1 .. To_Scrub.Size loop
       Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);

       -- everything until the current area is properly scrubbed
       pragma Loop_Invariant
         (for all B in Address_Type'First .. To_Scrub.Areas (I).To =>
          Includes (B, To_Scrub) = Memory (B).Scrubbed);
    end loop;

So far, this should be enough. Establishing these loop invariants may look
a little bit intimidating at first, but with a little bit of practise they
become rapidly straightforward.

This is the complete code so far:

.. code:: ada spark-flow

    with Area_Math; use Area_Math;

    package Memory_Analysis
    with SPARK_Mode => On
    is

       type Byte_Property is record
          Stack    : Boolean;
          Heap     : Boolean;
          Scrubbed : Boolean;
       end record
         with Ghost;

       type Memory_Type is array (Address_Type) of Byte_Property with Ghost;

       Memory : Memory_Type with Ghost;

       procedure Set_Heap (From, To : Address_Type)
         with
           Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Heap => True) else Memory (B) = Memory'Old (B)'Update (Heap => False))),
         Global => (In_Out => Memory);

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type);

       procedure Reset_Scrub
         with
           Post =>
             (for all B in Address_Type =>
                Memory (B) = Memory'Old (B)'Update (Scrubbed => False)),
             Ghost;

    end Memory_Analysis;

.. code:: ada prove_button

    with Interfaces.C;

    package body Memory_Analysis
    with SPARK_Mode => On
    is

       --  Assume these variables are defined in C source-code
       FromHeap  : Interfaces.C.unsigned;
       ToHeap    : Interfaces.C.unsigned;
       FromStack : Interfaces.C.unsigned;
       ToStack   : Interfaces.C.unsigned;

       procedure Set_Heap
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          FromHeap := Interfaces.C.unsigned (From);
          ToHeap := Interfaces.C.unsigned (From);
       end Set_Heap;

       procedure Get_Heap_Boundaries
         (From, To : out Address_Type)
       is
       begin
          From := Address_Type (FromHeap);
          To := Address_Type (ToHeap);
       end Get_Heap_Boundaries;

       procedure Get_Stack_Boundaries
         (From, To : out Address_Type)
       is
       begin
          From := Address_Type (FromStack);
          To := Address_Type (ToStack);
       end Get_Stack_Boundaries;

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
       is
          Prev_Heap_From, Prev_Heap_To,
          Prev_Stack_From, Prev_Stack_To : Address_Type;
       begin
          Get_Stack_Boundaries (Prev_Stack_From, Prev_Stack_To);
          Get_Heap_Boundaries (Prev_Heap_From, Prev_Heap_To);

          declare
             Prev     : Set := Create (Prev_Heap_From, Prev_Heap_To) or
               Create (Prev_Stack_From, Prev_Stack_To);
             Next     : Set := Create (Heap_From, Heap_To) or
               Create (Stack_From, Stack_To);
             To_Scrub : Set := Prev and not Next;
          begin
             for I in 1 .. To_Scrub.Size loop
                --  Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);

                -- everything until the current area is properly scrubbed
                pragma Loop_Invariant
                  (for all B in Address_Type'First .. To_Scrub.Areas (I).To =>
                       Includes (B, To_Scrub) = Memory (B).Scrubbed);
             end loop;

             --  Set_Stack (Stack_From, Stack_To);
             Set_Heap (Heap_From, Heap_To);
          end;
       end Move_Heap_And_Stack;

       procedure Reset_Scrub is
       begin
          for B in Address_Type'Range loop
             Memory (B).Scrubbed := False;
             pragma Loop_Invariant
               (for all X in Address_Type'First .. B =>
                  Memory (X) =
                    Memory'Loop_Entry (X)'Update (Scrubbed => False));
          end loop;
       end Reset_Scrub;

    end Memory_Analysis;

Unfortunately, this is not enough.

Implementing Unproven Interaction with Registers
------------------------------------------------

As this is a low level code, some data will not be proven in the SPARK
sense. Take for example the calls that read the memory boundaries:

.. code-block:: ada

    procedure Get_Heap_Boundaries (From, To : out Address_Type)
      with Post => (for all B in Address_Type =>
                      (Memory (B).Heap = (B in From .. To)))
                    and then From <= To;

    procedure Get_Stack_Boundaries (From, To : out Address_Type)
      with Post => (for all B in Address_Type =>
                      (Memory (B).Stack = (B in From .. To)))
                    and then From <= To;

The values :ada:`From` and :ada:`To` are possibly coming from registers.
SPARK wouldn't necessary be able to make the link with the post condition
just because the data is outside of its analysis. In this case, it's
perfectly fine to just tell SPARK about a fact without having to prove it.
There are two ways to do this, one is to deactivate SPARK from the entire
subprogram:

.. code-block:: ada

    procedure Get_Heap_Boundaries
      (From, To : out Address_Type)
      with SPARK_Mode => Off
    is
    begin
       -- code
    end Get_Heap_Boundaries;

In this case, SPARK will just assume that the postcondition is correct.
The issue is that there's no SPARK analysis on the entire subprogram,
which may be too much. An alternative solution is just to state the fact
that the postcondition is true at the end of the subprogram:

.. code-block:: ada

    procedure Get_Heap_Boundaries
      (From, To : out Address_Type)
    is
    begin
       -- code

       pragma Assume (for all B in Address_Type =>
                        (Memory (B).Heap = (B in From .. To)));
    end Get_Heap_Boundaries;

In this example, to illustrate the above, the registers will be modeled as
global variables read and written from C |mdash| which is outside of SPARK
analysis as registers would be.

:ada:`Move_Heap_And_Stack` |mdash| the "hard" part
--------------------------------------------------

Before diving into what's next and steering away readers from ever doing
proof, let's step back a little. We're currently set to doing the hardest
level of proof |mdash| platinum. That is, fully proving a program's functional
behavior. There is a lot of benefit to take from SPARK prior to reaching
this stage. The subset of the language alone provides more analyzable
code. Flow analysis allows you to easily spot uninitialized data. Run-time
errors such as buffer overflow are relatively easy to clear out, and even
simple gold property demonstration is reachable by most software engineers
after a little bit of training.

Full functional proof |mdash| that is, complex property demonstration
|mdash| is hard. It is also not usually required. But if this is what you
want to do, there's a fundamental shift of mindset required. As it turns
out, it took me a good week to understand that. For a week as I was trying
to build a proof from bottom to top, adding various assertions left and
right, trying to make things fits the SPARK expectations. To absolutely no
results.

And then, in the midst of despair, the apple fell on my head.

The prover is less clever than you think. It's like a kid learning maths.
It's not going to be able to build a complex demonstration to prove the
code by itself. Asking it to prove the code is not the right approach. The
right approach is to build the demonstration of the code correctness, step
by step, and to prove that this demonstration is correct. This
demonstration is a program in itself, with its own subprograms, its own
data and control flow, its own engineering and architecture. What the
prover is going to do is to prove that the demonstration of the code is
correct, and as the demonstration is linked to the code, the code happens
to be indirectly proven correct as well.

Now reversing the logic, how could I prove this small loop? One way to
work that out is to describe scrubbed and unscrubbed areas one by one:

- On the first area to scrub, if it doesn't start at the beginning of
  the memory, everything before the start has not been scrubbed.

- When working on any area :ada:`I` beyond the first one, everything
  between the previous area :ada:`I - 1` and the current one has not been
  scrubbed

- At the end of an iteration, everything beyond the current area
  :ada:`I` is unscrubbed.

- At the end of the last iteration, everything beyond the last area is
  unscrubbed

The first step to help the prover is to translate all of these into
assertions, and see if these steps are small enough for the demonstration
to be proven |mdash| and for the demonstration to indeed prove the code.
At this stage, it's not a bad idea to describe the assertion in the loop
as loop invariants as we want the information to be carried from one loop
iteration to the next. This leads to the following code:

.. code-block:: ada

    for I in 1 .. To_Scrub.Size loop
       Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);

       pragma Loop_Invariant
         (if I > 1 then (for all B in Address_Type range
              To_Scrub.Areas (I - 1).To + 1 .. To_Scrub.Areas (I).From - 1 =>
             not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (if To_Scrub.Areas (1).From > Address_Type'First
          then (for all B in Address_Type'First .. To_Scrub.Areas (1).From - 1 =>
               not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (if To_Scrub.Areas (I).To < Address_Type'Last then
              (for all B in To_Scrub.Areas (I).To + 1 .. Address_Type'Last =>
                      not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (for all B in Address_Type'First .. To_Scrub.Areas (I).To =>
              Includes (B, To_Scrub) = Memory (B).Scrubbed);
    end loop;

    pragma Assert
      (if To_Scrub.Size >= 1
       and then To_Scrub.Areas (To_Scrub.Size).To < Address_Type'Last then
           (for all B in
                To_Scrub.Areas (To_Scrub.Size).To + 1 .. Address_Type'Last =>
                   not Memory (B).Scrubbed));

Results are not too bad at first sight. Out of the 5 assertions, only 2
don't prove. This may mean that they're wrong |mdash| this may also mean
that SPARK needs some more help to prove.

Let's look at the first one in more details:

.. code-block:: ada

    pragma Loop_Invariant
       (if To_Scrub.Areas (1).From > Address_Type'First
        then (for all B in Address_Type'First .. To_Scrub.Areas (1).From - 1 =>
             not Memory (B).Scrubbed));

Now if we were putting ourselves in the shoes of SPARK. The prover doesn't
believe that there's nothing scrubbed before the first element. Why would
that be the case? None of these bytes are in the :ada:`To_Scrub` area,
right? Let's check. To investigate this, the technique is to add
assertions to verify intermediate steps, pretty much like what you'd do
with the debugger. Let's add an assertion before:

.. code-block:: ada

    pragma Assert
      (if To_Scrub.Areas (1).From > Address_Type'First
       then (for all B in Address_Type'First .. To_Scrub.Areas (1).From - 1 =>
            not Includes (B, To_Scrub)));

That assertion doesn't prove. But what would this be true? Recall that we
have a consistency check for all sets which is supposed to be true at this
point, defined as:

.. code-block:: ada

    function Is_Consistent (S : Set_Base) return Boolean is
       (S.Size <= S.Max and then
          ((for all I in 1 .. S.Size - 1 =>
             S.Areas (I).To < S.Areas (I + 1).From and then
             S.Areas (I + 1).From - S.Areas (I).To > 1)));

So looking at the above, if all areas are after the first one, there
should be nothing before the first one. If :ada:`Is_Consistent` is true
for :ada:`To_Scrub`, then the assertion ought to be true. Yet SPARK
doesn't believe us.

When reaching this kind of situation, it's good practise to factor out the
proof. The idea is to create a place where we say "given only these
hypotheses, can you prove this conclusion?". Sometimes, SPARK is getting
lost in the wealth of information available, and just reducing the number
of hypothesis to consider to a small number is enough for get it to figure
out something.

Interestingly, this activity of factoring out a piece of proof is very
close to what you'd do for a regular program. It's also easier for the
developer to understand small pieces of code than a large flat program.
The prover is no better than that.

These factored out proofs are typically referred to as lemmas. They are
Ghost procedures that prove a postcondition from a minimal precondition.
For convention, we'll call them all Lemma something. The Lemma will look
like:

.. code-block:: ada

    procedure Lemma_Nothing_Before_First (S : Set) with
       Ghost,
       Pre => Is_Consistent (S),
       Post =>
          (if S.Size = 0 then (for all B in Address_Type => not Includes (B, S))
             elsif S.Areas (1).From > Address_Type'First then
             (for all B in Address_Type'First .. S.Areas (1).From - 1 =>
                  not Includes (B, S)));

Stating that if :ada:`S` is consistent, then either it's null (nothing is
included) or all elements before the first :ada:`From` are not included.

Now let's see if reducing the scope of the proof is enough. Let's just add
an empty procedure:

.. code-block:: ada

    procedure Lemma_Nothing_Before_First (S : Set) is
    begin
       null;
    end Lemma_Nothing_Before_First;

Still no good. That was a good try though. Assuming we believe to be
correct here (and we are), the game is now to demonstrate to SPARK how to
go from the hypotheses to the conclusion.

To do so we need to take into account one limitation of SPARK |mdash| it
doesn't do induction. This has a significant impact on what can be deduced
from one part of the hypothesis:

.. code-block:: ada

    (for all I in 1 .. S.Size - 1 =>
       S.Areas (I).To < S.Areas (I + 1).From and then
       S.Areas (I + 1).From - S.Areas (I).To > 1)

If all elements :ada:`I` are below the :ada:`I + 1` element, then I would
like to be able to check that all :ada:`I` are below all the :ada:`I + N`
elements after it. This ability to jump from proving a one by one property
to a whole set is called induction. This happens to be extremely hard to
do for state-of-the-art provers. Here lies our key. We're going to
introduce a new lemma that goes from the same premise, and then
demonstrates that it means that all the areas after a given one are
greater:

.. code-block:: ada

    procedure Lemma_Order (S : Set) with
      Ghost,
      Pre => (for all I in 1 .. S.Size - 1 =>
                S.Areas (I).To < S.Areas (I + 1).From),
      Post => (for all I in 1 .. S.Size - 1 =>
                 (for all J in I + 1 .. S.Size =>
                      S.Areas (I).To < S.Areas (J).From));

And we're going to write the demonstration as a program:

.. code-block:: ada

    procedure Lemma_Order (S : Set)
    is
    begin
       if S.Size = 0 then
          return;
       end if;

       for I in 1 .. S.Size - 1 loop
          for J in I + 1 .. S.Size loop
             pragma Assert (S.Areas (J - 1).To < S.Areas (J).From);
             pragma Loop_Invariant (for all R in I + 1 .. J =>
                                      S.Areas (I).To < S.Areas (R).From);
          end loop;

          pragma Loop_Invariant
            ((for all R in 1 .. I =>
                  (for all T in R + 1 .. S.Size =>
                     S.Areas (R).To < S.Areas (T).From)));
       end loop;
    end Lemma_Order;

As you can see here, for each area :ada:`I`, we're checking that the area
:ada:`I + [1 .. Size]` are indeed greater. This happens to prove trivially
with SPARK. We can now prove :ada:`Lemma_Nothing_Before_First` by applying
the lemma :ada:`Lemma_Order`. To apply a lemma, we just need to call it as
a regular function call. Its hypotheses (precondition) will be checked by
SPARK, and its conclusion (postcondition) added to the list of hypotheses
available to prove:

.. code-block:: ada

    procedure Lemma_Nothing_Before_First (S : Set) is
    begin
       Lemma_Order (S);
    end Lemma_Nothing_Before_First;

This now proves trivially. Back to the main loop, applying the lemma
:ada:`Lemma_Nothing_Before_First` looks like:

.. code-block:: ada

    for I in 1 .. To_Scrub.Size loop
       Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);
       Lemma_Nothing_Before_First (To_Scrub);

       pragma Loop_Invariant
         (if I > 1 then
            (for all B in Address_Type range
                 To_Scrub.Areas (I - 1).To + 1 .. To_Scrub.Areas (I).From - 1 =>
                  not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (if To_Scrub.Areas (1).From > Address_Type'First
          then (for all B in Address_Type'First .. To_Scrub.Areas (1).From - 1 =>
               not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (if To_Scrub.Areas (I).To < Address_Type'Last then
              (for all B in To_Scrub.Areas (I).To + 1 .. Address_Type'Last =>
                      not Memory (B).Scrubbed));

       pragma Loop_Invariant
         (for all B in Address_Type'First .. To_Scrub.Areas (I).To =>
              Includes (B, To_Scrub) = Memory (B).Scrubbed);
    end loop;

    pragma Assert
      (if To_Scrub.Size >= 1
       and then To_Scrub.Areas (To_Scrub.Size).To < Address_Type'Last then
           (for all B in
                To_Scrub.Areas (To_Scrub.Size).To + 1 .. Address_Type'Last =>
                   not Memory (B).Scrubbed));

And voila! One more loop invariant now proving properly.

This is the complete code so far:

.. code:: ada

    package Area_Math.Lemma
    with SPARK_Mode
    is
       -- Lemma are used for two purposes:
       --   Manually demonstrating an implication between two truths (a Pre and a Post)
       --   Reducing the scope of hypothesis need to automatically run such demonstration
       --      (Lemma will only consider the pre and predicates).

       procedure Lemma_Order (S : Set) with
         Ghost,
         Pre => (for all I in 1 .. S.Size - 1 => S.Areas (I).To < S.Areas (I + 1).From),
         Post =>
           (for all I in 1 .. S.Size - 1 => (for all J in I + 1 .. S.Size => S.Areas (I).To < S.Areas (J).From));

       procedure Lemma_Last_Is_Included (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => S.Size = 0 or else (for all B in S.Areas (S.Size).From .. S.Areas (S.Size).To => Includes (B, S));

       procedure Lemma_Nothing_In_Between (S : Set; Left : Natural) with
         Ghost,
         Pre =>
           Is_Consistent (S) and
           Left in 1 .. S.Size - 1,
         Post => (for all B in S.Areas (Left).To + 1 .. S.Areas (Left + 1).From - 1 => not Includes (B, S));

       procedure Lemma_Nothing_Before_First (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => (if S.Size = 0 then (for all B in Address_Type => not Includes (B, S))
                    elsif S.Areas (1).From > Address_Type'First then
                      (for all B in Address_Type'First .. S.Areas (1).From - 1 => not Includes (B, S)));


       procedure Lemma_Nothing_Beyond_Last (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => (if S.Size = 0 then (for all B in Address_Type => not Includes (B, S))
                    elsif S.Areas (S.Size).To < Address_Type'Last then
                      (for all B in S.Areas (S.Size).To + 1 .. Address_Type'Last => not Includes (B, S)));

    end Area_Math.Lemma;

.. code:: ada prove_button

    package body Area_Math.Lemma
    with SPARK_Mode
    is

       procedure Lemma_Order
         (S : Set)
       is
       begin
          if S.Size = 0 then
             return;
          end if;

          for I in 1 .. S.Size - 1 loop
             for J in I + 1 .. S.Size loop
                pragma Assert (S.Areas (J - 1).To < S.Areas (J).From);
                pragma Loop_Invariant (for all R in I + 1 .. J => S.Areas (I).To < S.Areas (R).From);
             end loop;

             pragma Loop_Invariant
               ((for all R in 1 .. I => (for all T in R + 1 .. S.Size => S.Areas (R).To < S.Areas (T).From)));
          end loop;
       end Lemma_Order;

       procedure Lemma_Last_Is_Included (S : Set)
       is
       begin
          null;
       end Lemma_Last_Is_Included;

       procedure Lemma_Nothing_In_Between (S : Set; Left : Natural) is
       begin
          Lemma_Order (S);

          for K in 1 .. S.Size loop
             pragma Loop_Invariant (for all B in S.Areas (Left).To + 1 .. S.Areas (Left + 1).From - 1 =>
                                       not (for some I in 1 .. K => B in S.Areas (I).From .. S.Areas (I).To));
          end loop;
       end Lemma_Nothing_In_Between;

       procedure Lemma_Nothing_Before_First (S : Set) is
       begin
          Lemma_Order (S);
       end Lemma_Nothing_Before_First;

       procedure Lemma_Nothing_Beyond_Last (S : Set) is
       begin
          Lemma_Order (S);
       end Lemma_Nothing_Beyond_Last;

    end Area_Math.Lemma;

.. code:: ada

    with Area_Math; use Area_Math;

    package Memory_Analysis
    with SPARK_Mode => On
    is

       type Byte_Property is record
          Stack    : Boolean;
          Heap     : Boolean;
          Scrubbed : Boolean;
       end record
       with Ghost;

       type Memory_Type is array (Address_Type) of Byte_Property with Ghost;

       Memory : Memory_Type with Ghost;

       procedure Set_Heap (From, To : Address_Type)
         with
           Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Heap => True) else Memory (B) = Memory'Old (B)'Update (Heap => False))),
         Global => (In_Out => Memory);

       procedure Set_Stack (From, To : Address_Type)
         with Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Stack => True) else Memory (B) = Memory'Old (B)'Update (Stack => False))),
           Global => (In_Out => Memory);

       procedure Scrub (From, To : Address_Type)
         with Pre => To >= From,
         Post =>
           (for all B in Address_Type =>
                  (if B in From .. To then Memory (B) = Memory'Old(B)'Update (Scrubbed => True)
                       else Memory (B) = Memory'Old(B))),
           Global => (In_Out => Memory);

       procedure Get_Heap_Boundaries (From, To : out Address_Type)
         with Post => (for all B in Address_Type => (Memory (B).Heap = (B in From .. To)))
         and then From <= To;

       procedure Get_Stack_Boundaries (From, To : out Address_Type)
         with Post => (for all B in Address_Type => (Memory (B).Stack = (B in From .. To)))
         and then From <= To;
    --
       function Valid_Heap_And_Stack_Area
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type) return Boolean
       is
         ((Stack_From <= Stack_To and Heap_From <= Heap_To) and then
          Heap_From not in Stack_From .. Stack_To and then
          Heap_To not in Stack_From .. Stack_To and then
          Stack_From not in Heap_From .. Heap_To and then
          Stack_To not in Heap_From .. Heap_To);

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
         with Pre => Valid_Heap_And_Stack_Area (Heap_From, Heap_To, Stack_From, Stack_To),
           Post =>
           (for all B in Address_Type =>
              (Memory (B).Scrubbed = ((Memory'Old (B).Heap or Memory'Old (B).Stack) and then not (Memory (B).Heap or Memory (B).Stack))));

    end Memory_Analysis;

.. code:: ada prove_button

    with Interfaces.C; use Interfaces.C;

    with Area_Math.Lemma; use Area_Math.Lemma;

    package body Memory_Analysis
    with SPARK_Mode => On
    is

       --  Assume these variables are defined in C source-code
       FromHeap  : Interfaces.C.unsigned;
       ToHeap    : Interfaces.C.unsigned;
       FromStack : Interfaces.C.unsigned;
       ToStack   : Interfaces.C.unsigned;

       --------------
       -- Set_Heap --
       --------------

       procedure Set_Heap
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          FromHeap := Interfaces.C.unsigned (From);
          ToHeap := Interfaces.C.unsigned (From);
       end Set_Heap;

       ---------------
       -- Set_Stack --
       ---------------

       procedure Set_Stack
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          FromStack := Interfaces.C.unsigned (From);
          ToStack := Interfaces.C.unsigned (From);
       end Set_Stack;

       -----------
       -- Scrub --
       -----------

       procedure Scrub
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          -- TODO: This is not implemented in the context of this example
          null;
       end Scrub;

       -------------------------
       -- Get_Heap_Boundaries --
       -------------------------

       procedure Get_Heap_Boundaries
         (From, To : out Address_Type)
         with SPARK_Mode => Off
       is
          FromHeap : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromHeap";

          ToHeap : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromHeap";
       begin
          From := Address_Type (FromHeap);
          To := Address_Type (ToHeap);
       end Get_Heap_Boundaries;

       --------------------------
       -- Get_Stack_Boundaries --
       --------------------------

       procedure Get_Stack_Boundaries
         (From, To : out Address_Type)
         with SPARK_Mode => Off
       is
          FromStack : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromStack";

          ToStack : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromStack";
       begin
          From := Address_Type (FromStack);
          To := Address_Type (ToStack);
       end Get_Stack_Boundaries;

       -------------------------
       -- Move_Heap_And_Stack --
       -------------------------

       procedure Reset_Scrub
         with Post =>
           (for all B in Address_Type'Range =>
              Memory (B) = Memory'Old(B)'Update (Scrubbed => False)),
         Ghost;

       procedure Reset_Scrub is
          Old_Memory : Memory_Type := Memory;
       begin
          for B in Address_Type'Range loop
             Memory (B).Scrubbed := False;

             pragma Loop_Invariant
               (for all B2 in Address_Type'First .. B =>
                  Memory (B2) = Memory'Loop_Entry(B2)'Update (Scrubbed => False));
          end loop;
       end Reset_Scrub;


       procedure Lemma_No_Overlap (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
         with Pre => Valid_Heap_And_Stack_Area (Heap_From, Heap_To, Stack_From, Stack_To),
         Post => (for all B in Heap_From .. Heap_To => B not in Stack_From .. Stack_To)
         and (for all B in Stack_From .. Stack_To => B not in Heap_From .. Heap_To),
           Ghost
       is
       begin
          null;
       end Lemma_No_Overlap;

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
       is
          Prev_Heap_From, Prev_Heap_To, Prev_Stack_From, Prev_Stack_To : Address_Type;
       begin
          Get_Stack_Boundaries (Prev_Stack_From, Prev_Stack_To);
          Get_Heap_Boundaries (Prev_Heap_From, Prev_Heap_To);

          Reset_Scrub;

          declare
             Prev : Set := Create (Prev_Heap_From, Prev_Heap_To) or Create (Prev_Stack_From, Prev_Stack_To);
             Next : Set := Create (Heap_From, Heap_To) or Create (Stack_From, Stack_To);
             To_Scrub : Set := Prev and not Next;
          begin
             Lemma_Nothing_Beyond_Last (To_Scrub);

             for I in 1 .. To_Scrub.Size loop
                Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);

                if I = 1 then
                   Lemma_Nothing_Before_First (To_Scrub);
                else
                   Lemma_Nothing_In_Between (To_Scrub, I - 1);
                end if;

                pragma Loop_Invariant
                  (if To_Scrub.Areas (I).To < Address_Type'Last then
                        (for all B in To_Scrub.Areas (I).To + 1 .. Address_Type'Last => not Memory (B).Scrubbed));
                pragma Loop_Invariant
                  (for all B in Address_Type'First .. To_Scrub.Areas (I).To => Includes (B, To_Scrub) = Memory (B).Scrubbed);
             end loop;

             Lemma_Nothing_Beyond_Last (To_Scrub);
             Lemma_No_Overlap (Heap_From, Heap_To, Stack_From, Stack_To);

             Set_Stack (Stack_From, Stack_To);
             Set_Heap (Heap_From, Heap_To);
          end;
       end Move_Heap_And_Stack;

    end Memory_Analysis;

More madness, in a nutshell
---------------------------

At this point, it's probably not worth diving into all the details of this
small subprogram |mdash| the code is available
`here <https://github.com/AdaCore/SPARK_memory>`_. There's just more of
the same.

The size of this small function is relatively reasonable. Now let's give
some insights on a much more difficult problem: the :ada:`Set` library.

A generous implementation brings about 250 lines of code (it could
actually be less if condensed, but let's start with this). That's a little
bit less than a day of work for implementation and basic testing.

For the so called silver level |mdash| that is absence of run-time errors,
add maybe around 50 lines of assertions and half a day of work. Not too
bad.

For gold level, I decided to prove one key property. :ada:`Is_Consistent`,
to be true after each operator. Maybe a day of work was needed for that
one. Add another 150 lines of assertions maybe. Still reasonable.

Platinum is about completely proving the functionality of my subprogram.
And that proved (pun intended) to be a much much more difficult
experience. See this
`link <https://blog.adacore.com/verifythis-challenge-in-spark>`_
and this
`link <https://blog.adacore.com/applied-formal-logic-searching-in-strings>`_
for other similar experiences.

This is the complete source-code:

.. code:: ada

    package Area_Math
      with SPARK_Mode => On
    is

       Word_Size    : constant := 32;
       Memory_Size  : constant := 2 ** Word_Size;
       type Address_Type_Base is mod Memory_Size;
       type Address_Type is new Address_Type_Base;

       function "+" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) + Address_Type_Base (Right)))
         with Pre => Address_Type_Base'Last - Address_Type_Base (Left) >= Address_Type_Base (Right);

       function "-" (Left, Right : Address_Type) return Address_Type
       is (Address_Type (Address_Type_Base (Left) - Address_Type_Base (Right)))
         with Pre => Left >= Right;

       type Area is record
          From, To : Address_Type;
       end record
         with Predicate => From <= To;

       type Area_Array is array (Natural range <>) of Area;

       type Set (Max : Natural) is record
          Size  : Natural;
          Areas : Area_Array (1 .. Max);
       end record
         with Predicate => Set.Size <= Max;

       Empty_Set : constant Set := (Max => 0, Size => 0, Areas => (others => (0, 0)));
       Full_Set : constant Set := (Max => 1, Size => 1, Areas => (others => (Address_Type'First, Address_Type'Last)));

       function Is_Consistent (S : Set) return Boolean is
         ((for all I in 1 .. S.Size - 1 => S.Areas (I).To < S.Areas (I + 1).From and then S.Areas (I + 1).From - S.Areas (I).To > 1));

       function Create (From, To : Address_Type) return Set
         with Pre => From <= To,
         Post => Create'Result.Max = 1
         and then Create'Result.Size = 1
         and then (for all B in Address_Type => Includes (B, Create'Result) = (B in From .. To));

       procedure Put_Line (S : Set);

       function Includes (B : Address_Type; S : Set) return Boolean
       is (for some I in 1 .. S.Size => B in S.Areas (I).From .. S.Areas (I).To)
         with Ghost;

       function "or" (S1, S2 : Set) return Set
         with Pre =>
            Is_Consistent (S1) and
            Is_Consistent (S2) and
            Positive'Last - S1.Size - S2.Size >= 0,
         Post => "or"'Result.Size <= S1.Size + S2.Size
         and Is_Consistent ("or"'Result)
         and (for all B in Address_Type => (Includes (B, "or"'Result)) = ((Includes (B, S1) or Includes (B, S2))));

       function "and" (S1, S2 : Set) return Set
         with Pre =>
            Is_Consistent (S1) and
            Is_Consistent (S2) and
            Positive'Last - S1.Size - S2.Size >= 0,
         Post => "and"'Result.Size <= S1.Size + S2.Size
         and Is_Consistent ("and"'Result)
         and (for all B in Address_Type => (Includes (B, "and"'Result)) = ((Includes (B, S1) and Includes (B, S2))));

       function "not" (S : Set) return Set
         with Pre =>
           Positive'Last - S.Size > 0
             and Is_Consistent (S),
         Post =>
           Is_Consistent ("not"'Result) and then
           (for all B in Address_Type => Includes (B, S) /= Includes (B, "not"'Result)) and then
           "not"'Result.Size <= S.Size + 1 and then
           "not"'Result.Max <= S.Size + 1;

    end Area_Math;

.. code:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Strings.Fixed;
    with Ada.Strings; use Ada.Strings;

    with Area_Math.Lemma; use Area_Math.Lemma;

    package body Area_Math
    with SPARK_Mode => On
    is
       pragma Unevaluated_Use_Of_Old (Allow);

       procedure Append (S : in out Set; A : Area) with
         Pre =>
           Is_Consistent (S)
           and S.Max - S.Size >= 1
           and (if S.Size > 0 then A.From >= S.Areas (S.Size).From and then A.To >= S.Areas (S.Size).To),
         Post =>
           Is_Consistent (S)
           and (if S.Size'Old = 0 then S.Size = 1
                elsif A.From > S'Old.Areas (S'Old.Size).To and then A.From - S'Old.Areas (S'Old.Size).To > 1 then S.Size - S.Size'Old = 1
                else S.Size = S.Size'Old)
           and (if A.From > Address_Type'First then
                (for all B in Address_Type'First .. A.From - 1 =>
                     Includes (B, S) = Includes (B, S'Old)))
           and (for all B in A.From .. A.To => Includes (B, S))
           and (for all B in Address_Type => (if not Includes (B, S'Old) and Includes (B, S) then B in A.From .. A.To))
           and (if A.To < Address_Type'Last then (for all B in A.To + 1 .. Address_Type'Last => not Includes (B, S)))
           and S.Areas (S.Size).From <= A.From
           and S.Areas (S.Size).To = A.To;

       procedure Append (S : in out Set; A : Area) is
          Old : constant Set := S with Ghost;
       begin
          Lemma_Order (S);

          if S.Size = 0 then
             S.Size := S.Size + 1;
             S.Areas (S.Size) := A;

             Lemma_Order (S);
             Lemma_Last_Is_Included (S);

             for I in 1 .. S.Size - 1 loop
                pragma Loop_Invariant (S.Areas (1 .. Old.Size) = Old.Areas (1 .. Old.Size));
                pragma Loop_Invariant (for all B in Address_Type'First .. S.Areas (I).To => Includes (B, S) = Includes (B, Old));
             end loop;
          elsif A.From > S.Areas (S.Size).To and then A.From - S.Areas (S.Size).To > 1 then
             S.Size := S.Size + 1;
             S.Areas (S.Size) := A;

             Lemma_Order (S);
             Lemma_Last_Is_Included (S);

             for I in 1 .. S.Size - 1 loop
                pragma Loop_Invariant (S.Areas (1 .. Old.Size) = Old.Areas (1 .. Old.Size));
                pragma Loop_Invariant (for all B in Address_Type'First .. S.Areas (I).To => Includes (B, S) = Includes (B, Old));
             end loop;
          else
             S.Areas (S.Size).To := A.To;

             Lemma_Order (S);
             Lemma_Last_Is_Included (S);

             for I in 1 .. S.Size - 1 loop
                pragma Loop_Invariant (S.Areas (1 .. Old.Size - 1) = Old.Areas (1 .. Old.Size - 1));
                pragma Loop_Invariant (for all B in Address_Type'First .. S.Areas (I).To => Includes (B, S) = Includes (B, Old));
             end loop;

             pragma Assert (S.Areas (Old.Size).From = Old.Areas (Old.Size).From);
             pragma Assert (S.Areas (Old.Size).To >= Old.Areas (Old.Size).To);
             pragma Assert (for all B in S.Areas (Old.Size).From .. Old.Areas (Old.Size).To => Includes (B, S));
             pragma Assert (for all B in S.Areas (Old.Size).From .. Old.Areas (Old.Size).To => Includes (B, Old));
             pragma Assert (for all B in S.Areas (Old.Size).From .. Old.Areas (Old.Size).To => Includes (B, S) = Includes (B, Old));
             pragma Assert (S.Size = Old.Size);
          end if;
       end Append;

       function Create (From, To : Address_Type) return Set is
       begin
          return (Max => 1, Size => 1, Areas => (1 => (From, To)));
       end Create;

       procedure Put_Line (S : Set)
         with SPARK_Mode => Off
       is
       begin
          Put ("{");

          for I in 1 .. S.Size loop
             if I > 1 then
                Put (", ");
             end if;

             Put (Ada.Strings.Fixed.Trim(S.Areas (I).From'Img, Both) & " .. " & Ada.Strings.Fixed.Trim(S.Areas(I).To'Img, Both));
          end loop;

          Put_Line ("}");
       end Put_Line;

       -----------
       -- "or" --
       -----------

      function "or"
         (S1, S2 : Set)
          return Set
       is
          Result : Set (S1.Size + S2.Size);
          It1, It2 : Integer := 1;

          function Is_Computed (S1, S2 : Set; From, To : Address_Type) return Boolean is
            (for all B in From .. To => Includes (B, Result) = ((Includes (B, S1) or Includes (B, S2))))
          with Ghost;

          function Is_Computed (S1, S2 : Set; To : Address_Type) return Boolean is
            (for all B in Address_Type'First .. To => Includes (B, Result) = ((Includes (B, S1) or Includes (B, S2))))
              with Ghost;

          procedure Lemma_One_If_Not_The_Other (S1, S2 : Set; From, To : Address_Type)
            with Pre => Is_Computed (S1, S2, From, To),
            Post => (for all B in From .. To => (if Includes (B, Result) and not Includes (B, S1) then Includes (B, S2)))
            and (for all B in From .. To => (if Includes (B, Result) and not Includes (B, S2) then Includes (B, S1))),
            Ghost
          is
          begin
             null;
          end Lemma_One_If_Not_The_Other;

          procedure Combine (S1, S2 : Set; It1 : Integer; It2 : Integer)
            with Pre =>
              -- Silver
            It1 in 1 .. S1.Size
            and then It2 in 1 .. S2.Size
            and then Natural'Last - Result.Size >= 1
            and then Result.Max - Result.Size >= 1

              -- Gold
            and then Is_Consistent (S1)
            and then Is_Consistent (S2)
            and Then Is_Consistent (Result)

              -- Platinium
            and then (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1))
            and then (if Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From)
            and then (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To))
            and then (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2))),

            Post =>
              Result.Size > 0
              and then Result.Size >= Result.Size'Old
              and then Result.Size - Result.Size'Old in 0 .. 1
              and then Is_Consistent (Result)

              -- Platinium
              and then Is_Computed (S1, S2, S1.Areas (It1).To)
              and then (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To))
              and then (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2)))
              and then (for all B in Address_Type => (if Includes (B, Result'Old) then Includes (B, Result)))
              and then Result.Areas (Result.Size).From <= S1.Areas (It1).From;

          procedure Combine (S1, S2 : Set; It1 : Integer; It2 : Integer) is
             Initial_Size : constant Natural := Result.Size with Ghost;

             function Is_Computed (From, To : Address_Type) return Boolean is
               (Is_Computed (S1, S2, From, To))
                 with Ghost;

             function Is_Computed (To : Address_Type) return Boolean is
               (Is_Computed (S1, S2, To))
                 with Ghost;

             Old : Set := Result with Ghost;

          begin
             Lemma_Order (S1);
             Lemma_Order (S2);
             Lemma_Order (Result);

             if Result.Size = 0 then
                -- First element, to be added unconditionally

                Append (Result, S1.Areas (It1));
                Lemma_Last_Is_Included (Result);

                pragma Assert (Is_Computed (S1, S2, S1.Areas (It1).To));
                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));
             elsif S1.Areas (It1).To <= Result.Areas (Result.Size).To then
                -- It1 is already included in Result

                pragma Assert (Is_Computed (S1, S2, S1.Areas (It1).To));
                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));
             else
                -- In all other cases, append the element (extend if needed).

                Append (Result, S1.Areas (It1));
                Lemma_Last_Is_Included (Result);
                pragma Assert (Is_Computed (S1, S2, S1.Areas (It1).To));

                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));
             end if;
          end Combine;

          function Invariant_In_Range (S1, S2 : Set; It1, It2 : Integer) return Boolean is
            (
               (It1 in 1 .. S1.Size) and then
               (It2 in 1 .. S2.Size) and then
               (Natural'Last - S1.Size - S2.Size >= 0) and then
               (Natural'Last - Result.Size >= (S1.Size - It1) + (S2.Size - It2))
            )
              with Ghost;

          function Partial_Invariant (S1, S2 : Set; It1, It2 : Integer) return Boolean is
            (
               -- Silver
               Invariant_In_Range (S1, S2, It1, It2) and then
               --(Result.Max - Result.Size > (S1.Size - It1) + (S2.Size - It2)) and then

               -- Gold
               (Is_Consistent (S1)) and then
               (Is_Consistent (S2)) and then
               (Is_Consistent (Result)) and then

               -- Partial result, if set to True then coming from one or the other input
               (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2))) and then

               (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To)) and then
               (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To)) and then

               (if S1.Areas (It1).From > Address_Type'First
                and then S1.Areas (It1).From <= S2.Areas (It2).From
                then Is_Computed (S1, S2, S1.Areas (It1).From - 1)) and then

               (if S1.Areas (It1).From <= S2.Areas (It2).From and Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From)
            )
              with Ghost;

          function Invariant (S1, S2 : Set; It1, It2 : Integer) return Boolean is
            (
               -- Silver
               Invariant_In_Range (S1, S2, It1, It2) and then
               --(Result.Max - Result.Size > (S1.Size - It1) + (S2.Size - It2)) and then

               -- Gold
               (Is_Consistent (S1)) and then
               (Is_Consistent (S2)) and then
               (Is_Consistent (Result)) and then

               -- Partial result, if set to True then coming from one or the other input
               (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2))) and then

               (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To)) and then
               (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To)) and then

               (if S1.Areas (It1).From > Address_Type'First
                and then S1.Areas (It1).From <= S2.Areas (It2).From
                then Is_Computed (S1, S2, S1.Areas (It1).From - 1)) and then

               (if S1.Areas (It1).From <= S2.Areas (It2).From and Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From) and then

               (if S2.Areas (It2).From > Address_Type'First
                and then S2.Areas (It2).From <= S1.Areas (It1).From
                then Is_Computed (S1, S2, S2.Areas (It2).From - 1)) and then

               (if S2.Areas (It2).From <= S1.Areas (It1).From and Result.Size > 0 then S2.Areas (It2).From >= Result.Areas (Result.Size).From)
            )
              with Ghost;

          function Invariant_S_Finished (S1, S2 : Set; It1, It2 : Integer) return Boolean is
            (
               Invariant_In_Range (S1, S2, It1, It2) and then
               (It1 = S1.Size) and then
               (Is_Computed (S1, S2, S1.Areas (S1.Size).To))
            )
              with Ghost;

          function Has_Result_Space_Left (S1, S2 : Set; It1, It2 : Integer; Space : Natural) return Boolean is
            (Invariant_In_Range (S1, S2, It1, It2) and then Result.Max - Result.Size - Space >= (S1.Size - It1) + (S2.Size - It2))
              with Ghost,
              Pre => Space in 1 .. 2;

          procedure Lemma_Invariant_Commutative (S1, S2 : Set; It1, It2 : Integer)
            with Pre => Invariant (S1, S2, It1, It2),
            Post => Invariant (S2, S1, It2, It1),
            Ghost
          is
          begin
             null;
          end Lemma_Invariant_Commutative;

          procedure Lemma_Partial_To_Complete_Work
            with Pre => Is_Consistent (S1) and then
            Is_Consistent (S2) and then
            Invariant_S_Finished (S1, S2, It1, It2) and then
            Invariant_S_Finished (S2, S1, It2, It1) and then
            (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2))),
            Post => Is_Computed (S1, S2, Address_Type'Last),
            Ghost
          is
          begin
             for B in Address_Type'Range loop
                Lemma_Nothing_Beyond_Last (S1);
                Lemma_Nothing_Beyond_Last (S2);
                pragma Loop_Invariant (Is_Computed (S1, S2, B));
             end loop;
          end Lemma_Partial_To_Complete_Work;

          procedure Combine_And_Increment (S1, S2 : Set; It1 : in out Integer; It2 : Integer; End_It1 : out Boolean; End_It2 : Boolean)
            with Pre => Has_Result_Space_Left (S1, S2, It1, It2, 1) and then Partial_Invariant (S1, S2, It1, It2)

            and then (if End_It2 then Invariant_S_Finished (S2, S1, It2, It1) and then (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1)))
            and then (End_It2 or else S1.Areas (It1).From <= S2.Areas (It2).From)
            and then (if Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From)
            and then (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1)),

            Post =>
              (if End_It2 then
                 Invariant_S_Finished (S2, S1, It2, It1)
                 and then (if Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From)
                 and then (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1)))

              and then

                (if End_It1 then
                   Invariant_S_Finished (S1, S2, It1, It2) and then
                   (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1)) and then
                   (if Has_Result_Space_Left (S1, S2, It1, It2, 2)'Old then Has_Result_Space_Left (S1, S2, It1, It2, 1))
                 else (if Has_Result_Space_Left (S1, S2, It1, It2, 2)'Old then Has_Result_Space_Left (S1, S2, It1, It2, 2)
                      else Has_Result_Space_Left (S1, S2, It1, It2, 1)))

              and then

                (if    not End_It1 and not End_It2 then Invariant (S1, S2, It1, It2)
                 elsif not End_It1 and     End_It2 then Partial_Invariant (S1, S2, It1, It2)
                 elsif     End_It1 and not End_It2 then Partial_Invariant (S2, S1, It2, It1)
                 elsif     End_It1 and     End_It2 then Partial_Invariant (S1, S2, It1, It2)) -- Then add the case if everything is done?

              and then

                (if End_It1 and not End_It2 then S2.Areas (It2).From >= Result.Areas (Result.Size).From);

          procedure Combine_And_Increment (S1, S2 : Set; It1 : in out Integer; It2 : Integer; End_It1 : out Boolean; End_It2 : Boolean) is
          begin
             Combine (S1, S2, It1, It2);
             pragma Assert (if End_It2 then Invariant_S_Finished (S2, S1, It2, It1));

             Lemma_Order (S1);
             Lemma_Order (S2);
             Lemma_Order (Result);

             if It1 = S1.Size then
                pragma Assert (if not End_It2 then S2.Areas (It2).From >= Result.Areas (Result.Size).From);

                if S2.Areas (It2).From <= S1.Areas (It1).To then
                   pragma Assert (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                elsif It2 > 1 then
                   Lemma_Nothing_In_Between (S2, It2 - 1);
                   pragma Assert (for all B in S1.Areas (It1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S1));
                   pragma Assert (Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                else
                   pragma Assert (for all B in S1.Areas (It1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S1));
                   pragma Assert (Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                end if;

                pragma Assert (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1));

                pragma Assert (if not End_It2 then Partial_Invariant (S2, S1, It2, It1));

                End_It1 := True;
                return;
             end if;

             End_It1 := False;

             It1 := It1 + 1;

             pragma Assert (Is_Computed (S1, S2, S1.Areas (It1 - 1).To));
             pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));

             pragma Assert (for all B in Address_Type => (if Includes (B, Result) then Includes (B, S1) or Includes (B, S2)));

             if S1.Areas (It1).From <= S2.Areas (It2).From then
                Lemma_Nothing_In_Between (S1, It1 - 1);
                pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => not Includes (B, S1));

                if It2 > 1 then
                   pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => (if Includes (B, Result) then Includes (B, S2)));
                   Lemma_Nothing_In_Between (S2, It2 - 1);
                   pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => (if not Includes (B, Result) then not Includes (B, S2)));
                   pragma Assert (Is_Computed (S1, S2, S1.Areas (It1).From - 1));
                end if;

                pragma Assert (Is_Computed (S1, S2, S1.Areas (It1).From - 1));
             else
                Lemma_Nothing_In_Between (S1, It1 - 1);
                pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => not Includes (B, S1));

                if S2.Areas (It2).From <= S1.Areas (It1 - 1).To then
                   pragma Assert (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                elsif It2 > 1 then
                   Lemma_Nothing_In_Between (S2, It2 - 1);
                   pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S1));
                   pragma Assert (Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                else
                   pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S1));
                   pragma Assert (Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                end if;

                pragma Assert (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1));
             end if;

             pragma Assert (if not End_It2 then Invariant (S1, S2, It1, It2) else Partial_Invariant (S1, S2, It1, It2));
          end Combine_And_Increment;

          End_It1 : Boolean := False;
          End_It2 : Boolean := False;
       begin
          Result.Size := 0;
          Result.Areas := (others => (others => 0));

          if S1.Size = 0 and then S2.Size = 0 then
             return Empty_Set;
          elsif S1.Size = 0 then
             return S2;
          elsif S2.Size = 0 then
             return S1;
          end if;

          Lemma_Order (S1);
          Lemma_Order (S2);

          pragma Assert
            (if S1.Areas (It1).From > Address_Type'First
             and then S1.Areas (It1).From <= S2.Areas (It2).From
             then Is_Computed (S1, S2, S1.Areas (It1).From - 1));

          pragma Assert
            (if S2.Areas (It2).From > Address_Type'First
             and then S2.Areas (It2).From <= S1.Areas (It1).From
             then Is_Computed (S1, S2, S2.Areas (It2).From - 1));

          loop
             pragma Loop_Invariant (Invariant (S1, S2, It1, It2));
             pragma Loop_Invariant (not End_It1 and then not End_It2);
             pragma Loop_Invariant (Has_Result_Space_Left (S1, S2, It1, It2, 2));
             pragma Loop_Invariant (Result.Size <= Result.Max);  -- BUG? Do I really need this invariant while it's an actual type predicate?

             if S1.Areas (It1).From <= S2.Areas (It2).From then
                Combine_And_Increment (S1, S2, It1, It2, End_It1, End_It2);
                exit when End_It1;
             else
                Combine_And_Increment (S2, S1, It2, It1, End_It2, End_It1);
                exit when End_It2;
                Lemma_Invariant_Commutative (S2, S1, It2, It1);
             end if;
          end loop;

          if End_It2 then
             loop
                pragma Loop_Invariant (Partial_Invariant (S1, S2, It1, It2));
                pragma Loop_Invariant (Invariant_S_Finished (S2, S1, It2, It1));
                pragma Loop_Invariant (if Result.Size > 0 then S1.Areas (It1).From >= Result.Areas (Result.Size).From);
                pragma Loop_Invariant (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1));
                pragma Loop_Invariant (Has_Result_Space_Left (S1, S2, It1, It2, 1));

                Combine_And_Increment (S1, S2, It1, It2, End_It1, End_It2);
                exit when End_It1;
             end loop;
          else
             pragma Assert (End_It1);

             loop
                pragma Loop_Invariant (Partial_Invariant (S2, S1, It2, It1));
                pragma Loop_Invariant (Invariant_S_Finished (S1, S2, It1, It2));
                pragma Loop_Invariant (if Result.Size > 0 then S2.Areas (It2).From >= Result.Areas (Result.Size).From);
                pragma Loop_Invariant (if S2.Areas (It2).From > Address_Type'First then Is_Computed (S1, S2, S2.Areas (It2).From - 1));
                pragma Loop_Invariant (Has_Result_Space_Left (S1, S2, It1, It2, 1));

                Combine_And_Increment (S2, S1, It2, It1, End_It2, End_It1);
                exit when End_It2;
             end loop;
          end if;

          Lemma_Order (S1);
          Lemma_Order (S2);
          Lemma_Order (Result);

          Lemma_Partial_To_Complete_Work;
          pragma Assert (Is_Computed (S1, S2, Address_Type'Last));

          return Result;
       end "or";

       -----------
       -- "and" --
       -----------

      function "and"
         (S1, S2 : Set)
          return Set
       is
          Result : Set (S1.Size + S2.Size);
          It1, It2 : Integer := 1;

          function Is_Computed (S1, S2 : Set; From, To : Address_Type) return Boolean is
            (for all B in From .. To => Includes (B, Result) = ((Includes (B, S1) and Includes (B, S2))))
          with Ghost;

          function Is_Computed (S1, S2 : Set; To : Address_Type) return Boolean is
            (for all B in Address_Type'First .. To => Includes (B, Result) = ((Includes (B, S1) and Includes (B, S2))))
          with Ghost;

          procedure Combine (S1, S2 : Set; It1 : Integer; It2 : in out Integer)
            with Pre =>
              -- Silver
            It1 in 1 .. S1.Size
            and then It2 in 1 .. S2.Size
            and then Natural'Last - Result.Size >= S2.Size - It2
            and then Result.Max - Result.Size > S2.Size - It2

              -- Gold
            and then Is_Consistent (S1)
            and then Is_Consistent (S2)
            and Then Is_Consistent (Result)
            and then S1.Areas (It1).From <= S2.Areas (It2).From
            and then (if Result.Size > 0 then Result.Areas (Result.Size).To <= S1.Areas (It1).To)
            and then (if Result.Size > 0 then Result.Areas (Result.Size).To <= S2.Areas (It2).To)
            and then (if Result.Size > 0 then
                        (Result.Areas (Result.Size).To < Address_Type'Last
                         and then Result.Areas (Result.Size).To + 1 < S2.Areas (It2).From))

              -- Platinium
             and then (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1, S2, S1.Areas (It1).From - 1))
             and then (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To))
             and then (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To)
             and then (if It2 = 1 then Result.Size = 0),

            Post =>
              Result.Size >= Result.Size'Old
              and then Result.Size - Result.Size'Old in It2 - It2'Old .. It2 - It2'Old + 1
              and then It2 in 1 .. S2.Size
              and then Is_Consistent (Result)
              and then (if Result.Size > 0 then Result.Areas (Result.Size).To <= S1.Areas (It1).To)
              and then (if Result.Size > 0 then Result.Areas (Result.Size).To <= S2.Areas (It2).To)

              -- Platinium
              and then Is_Computed (S1, S2, S1.Areas (It1).To)
              and then (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To))
              and then (if Result.Size > Result.Size'Old then
                          It2 > It2'Old
                          or else It2 = S2.Size
                        or else S2.Areas (It2).To > S1.Areas (It1).To) -- ADDED
              and then (It1 = S1.Size or else (if S1.Areas (It1 + 1).From <= S2.Areas (It2).From and It2 = 1 then Result.Size = 0))
              and then (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2).To)
              and then (-- There are three cases to enumerate here
                          -- case 1, It1 = S1.Size, end of the computation
                          (It1 = S1.Size) or else
                          -- case 2, the last It2 element is at least partially included, we know we're going to switch It1 and It2
                          (S1.Areas (It1 + 1).From > S2.Areas (It2).From) or else
                          -- case 3, the last It2 element is excluded, beyond the It1 point, we don't know if we're
                          -- going to switch but we can ensure the postcondition
                          (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To)
                     );

          procedure Combine (S1, S2 : Set; It1 : Integer; It2 : in out Integer) is
             Initial_Size : constant Natural := Result.Size with Ghost;

             function Is_Computed (From, To : Address_Type) return Boolean is
               (Is_Computed (S1, S2, From, To))
                 with Ghost;

             function Is_Computed (To : Address_Type) return Boolean is
               (Is_Computed (S1, S2, To))
                 with Ghost;

          begin
             Lemma_Order (S1);
             Lemma_Order (S2);
             Lemma_Order (Result);

             loop
                -- Silver
                pragma Loop_Invariant (Result.Max - Result.Size > S2.Size - It2);
                pragma Loop_Invariant (Result.Size - Result.Size'Loop_Entry >= 0);
                pragma Loop_Invariant (It2 <= S2.Size);
                pragma Loop_Invariant (Result.Size - Result.Size'Loop_Entry = It2 - It2'Loop_Entry);
                pragma Loop_Invariant (It2 in It2'Loop_Entry .. It2'Loop_Entry + (Result.Size - Result.Size'Loop_Entry));

                -- Gold
                pragma Loop_Invariant (Result.Size = 0 or else S2.Areas (It2).From > Address_Type'First);
                pragma Loop_Invariant (Result.Size = 0 or else Result.Areas (Result.Size).To < S2.Areas (It2).From - 1);
                pragma Loop_Invariant (Result.Size = 0 or else Result.Areas (Result.Size).To <= S1.Areas (It1).To);
                pragma Loop_Invariant (Is_Consistent (Result));

                -- Platinium
                pragma Loop_Invariant (if It2 > 1 then Is_Computed (S2.Areas (It2 - 1).To));
                pragma Loop_Invariant (if S1.Areas (It1).From > Address_Type'First then Is_Computed (S1.Areas (It1).From - 1));
                pragma Loop_Invariant (Result.Size = 0 or else (if It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To));
                pragma Loop_Invariant (if It2 = 1 then Result.Size = 0);
                pragma Loop_Invariant (It2 >= 1);
                pragma Loop_Invariant (if Result.Size > Result.Size'Loop_Entry then It2 > It2'Loop_Entry);
                pragma Loop_Invariant (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2).To);

                Lemma_Order (Result);
                Lemma_Order (S1);
                Lemma_Order (S2);

                pragma Assert (if It2 > 1 then (for all B in S2.Areas (It2 - 1).To + 1 .. Address_Type'Last => not Includes (B, Result)));

                if S2.Areas (It2).From in S1.Areas(It1).From .. S1.Areas (It1).To then
                   -- there is an intersect

                   -- check that everything is computed until the point of From - 1.

                   if It2 > 1 then
                      pragma Assert (Is_Computed (S2.Areas (It2 - 1).To));
                      pragma Assert (for all B in S2.Areas (It2 - 1).To + 1 .. Address_Type'Last => not Includes (B, Result));
                      Lemma_Nothing_In_Between (S2, It2 - 1);
                      pragma Assert (for all B in S2.Areas (It2 - 1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S2));
                      pragma Assert (Is_Computed (S2.Areas (It2 - 1).To + 1, S2.Areas (It2).From - 1));
                      pragma Assert (Is_Computed (S2.Areas (It2).From - 1));
                   elsif S2.Areas (It2).From > Address_Type'First then
                      pragma Assert (for all B in Address_Type'First .. S2.Areas (It2).From - 1 => not Includes (B, Result));
                      pragma Assert (for all B in Address_Type'First .. S2.Areas (It2).From - 1 => not Includes (B, S2));
                      pragma Assert (Is_Computed (S2.Areas (It2).From - 1));
                   end if;

                   if S2.Areas (It2).To in S1.Areas(It1).From .. S1.Areas(It1).To  then
                      -- The secondary region is all included in the primary one, this is a intersect

                      Append (Result, (S2.Areas (It2).From,  S2.Areas (It2).To));
                      Lemma_Last_Is_Included (Result);

                      pragma Assert (for all B in S2.Areas (It2).From .. S2.Areas (It2).To => Includes (B, Result));
                      pragma Assert (for all B in S2.Areas (It2).From .. S2.Areas (It2).To => Includes (B, S2));
                      pragma Assert (for all B in S2.Areas (It2).From .. S2.Areas (It2).To => Includes (B, S1));
                      pragma Assert (Is_Computed (S2.Areas (It2).From, S2.Areas (It2).To));

                      pragma Assert (Is_Computed (S2.Areas (It2).To));

                      if It2 = S2.Size then
                         if S2.Areas (S2.Size).To < Address_Type'Last then
                            pragma Assert (for all B in S2.Areas (It2).To + 1 .. Address_Type'Last => not Includes (B, S2));
                            pragma Assert (for all B in S2.Areas (It2).To + 1 .. Address_Type'Last => not Includes (B, Result));
                         end if;

                         pragma Assert (Is_Computed (S1.Areas (It1).To));
                         exit;
                      end if;

                      It2 := It2 + 1;

                   else
                      -- Only the beginning is included

                      Append (Result, (S2.Areas (It2).From,  S1.Areas (It1).To));
                      Lemma_Last_Is_Included (Result);

                      pragma Assert (for all B in S2.Areas (It2).From .. S1.Areas (It1).To => Includes (B, Result));
                      pragma Assert (for all B in S2.Areas (It2).From .. S1.Areas (It1).To => Includes (B, S2));
                      pragma Assert (for all B in S2.Areas (It2).From .. S1.Areas (It1).To => Includes (B, S1));
                      pragma Assert (Is_Computed (S2.Areas (It2).From, S1.Areas (It1).To));

                      pragma Assert (Is_Computed (S1.Areas (It1).To));

                      pragma Assert (It1 = S1.Size or else S1.Areas (It1 + 1).From > S2.Areas (It2).From);

                      exit;
                   end if;
                else
                   -- there is no intersect

                   pragma Assert (S2.Areas (It2).From > S1.Areas (It1).To);

                   if It2 > 1 then
                      Lemma_Nothing_In_Between (S2, It2 - 1);
                      pragma Assert (Is_Computed (S1.Areas (It1).To));
                   else
                      pragma Assert (Is_Computed (S1.Areas (It1).To));
                   end if;

                   pragma Assert (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To);

                   exit;
                end if;

                Lemma_Order (Result);
             end loop;

             pragma Assert (if Result.Size > 0 then Result.Areas (Result.Size).To <= S1.Areas (It1).To);
          end Combine;

       begin
          Result.Size := 0;
          Result.Areas := (others => (others => 0));

          if S1.Size = 0 or else S2.Size =  0 then
             return Empty_Set;
          end if;

          Lemma_Order (S1);
          Lemma_Order (S2);

          pragma Assert
            (if S1.Areas (It1).From > Address_Type'First then
                 (for all B in Address_Type'First .. S1.Areas (It1).From - 1 => (Includes (B, Result)) = ((Includes (B, S1) and Includes (B, S2)))));

          pragma Assert
            (if S2.Areas (It2).From > Address_Type'First then
                 (for all B in Address_Type'First .. S2.Areas (It2).From - 1 => (Includes (B, Result)) = ((Includes (B, S1) and Includes (B, S2)))));

          loop
             -- Silver
             pragma Loop_Invariant (It1 in 1 .. S1.Size);
             pragma Loop_Invariant (It2 in 1 .. S2.Size);
             pragma Loop_Invariant (Natural'Last - Result.Size >= (S1.Size - It1) + (S2.Size - It2));
             pragma Loop_Invariant (Result.Max - Result.Size > (S1.Size - It1) + (S2.Size - It2));

             -- Gold
             pragma Loop_Invariant (Is_Consistent (S1));
             pragma Loop_Invariant (Is_Consistent (S2));
             pragma Loop_Invariant (Is_Consistent (Result));
             pragma Loop_Invariant (if Result.Size > 0 then Result.Areas (Result.Size).To <= S1.Areas (It1).To);
             pragma Loop_Invariant (if Result.Size > 0 then Result.Areas (Result.Size).To <= S2.Areas (It2).To);
             pragma Loop_Invariant (if Result.Size > 0 then Result.Areas (Result.Size).To < Address_Type'Last);
             pragma Loop_Invariant (if Result.Size > 0 and then It1 <= S1.Size and then It2 <= S2.Size then
                              (if S2.Areas (It2).From < S1.Areas (It1).From then Result.Areas (Result.Size).To + 1 < S1.Areas (It1).From
                               else Result.Areas (Result.Size).To + 1 < S2.Areas (It2).From));

             pragma Loop_Invariant
               (if S1.Areas (It1).From < S2.Areas (It2).From and then S1.Areas (It1).From > Address_Type'First then
                    (for all B in Address_Type'First .. S1.Areas (It1).From - 1 => (Includes (B, Result)) = ((Includes (B, S1) and Includes (B, S2))))
                elsif S2.Areas (It2).From > Address_Type'First then
                    (for all B in Address_Type'First .. S2.Areas (It2).From - 1 => (Includes (B, Result)) = ((Includes (B, S1) and Includes (B, S2)))));

             pragma Loop_Invariant (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To));
             pragma Loop_Invariant (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));

             pragma Loop_Invariant (if S1.Areas (It1).From <= S2.Areas (It2).From and It2 = 1 then Result.Size = 0); -- NOT NEEDED?
             pragma Loop_Invariant (if S2.Areas (It2).From <= S1.Areas (It1).From and It1 = 1 then Result.Size = 0); -- NOT NEEDED?

             pragma Loop_Invariant (if S1.Areas (It1).From <= S2.Areas (It2).From then (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To));
             pragma Loop_Invariant (if S2.Areas (It2).From <= S1.Areas (It1).From then (if Result.Size > 0 and then It1 > 1 then Result.Areas (Result.Size).To <= S1.Areas (It1 - 1).To));

             if S1.Areas (It1).From <= S2.Areas (It2).From then
                Combine (S1, S2, It1, It2);

                Lemma_Order (S1);
                Lemma_Order (S2);
                Lemma_Order (Result);

                if It1 = S1.Size then
                   if S1.Areas (It1).To < Address_Type'Last then
                      pragma Assert (if Result.Size > 0 then Result.Areas (Result.Size).To <= S1.Areas (It1).To);
                      pragma Assert (if Result.Size > 0 then Result.Areas (Result.Size).To <= S2.Areas (It2).To);

                      pragma Assert (for all B in S1.Areas (It1).To + 1 .. Address_Type'Last => not Includes (B, S1));
                      Lemma_Nothing_Beyond_Last (Result);
                      pragma Assert (for all B in S1.Areas (It1).To + 1 .. Address_Type'Last => not Includes (B, Result));
                      pragma Assert (for all B in S1.Areas (It1).To + 1 .. Address_Type'Last => Includes (B, Result) = (Includes (B, S1) and Includes (B, S2)));
                   end if;

                   pragma Assert (for all B in Address_Type'First .. S1.Areas (S1.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));
                   pragma Assert (for all B in Address_Type'First .. S2.Areas (S2.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));

                   exit;
                end if;

                It1 := It1 + 1;

                Lemma_Nothing_In_Between (S1, It1 - 1);
                pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => not Includes (B, S1));
                Lemma_Nothing_Beyond_Last (Result);
                pragma Assert (for all B in S1.Areas (It1 - 1).To + 1 .. S1.Areas (It1).From - 1 => not Includes (B, Result));
                pragma Assert (for all B in Address_Type'First .. S1.Areas (It1).From - 1 => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));

                pragma Assert (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To));
                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));

                -- START TRY
                pragma Assert (if S1.Areas (It1).From <= S2.Areas (It2).From then (if Result.Size > 0 and then It2 > 1 then Result.Areas (Result.Size).To <= S2.Areas (It2 - 1).To));
                pragma Assert (if S2.Areas (It2).From <= S1.Areas (It1).From then (if Result.Size > 0 and then It1 > 1 then Result.Areas (Result.Size).To <= S1.Areas (It1 - 1).To));
                -- END TRY
             else
                Combine (S2, S1, It2, It1);

                Lemma_Order (S1);
                Lemma_Order (S2);
                Lemma_Order (Result);

                pragma Assert (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To));
                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));

                if It2 = S2.Size then

                   if S2.Areas (It2).To < Address_Type'Last then
                      pragma Assert (for all B in S2.Areas (It2).To + 1 .. Address_Type'Last => not Includes (B, S2));
                      pragma Assert (for all B in S2.Areas (It2).To + 1 .. Address_Type'Last => not Includes (B, Result));
                      pragma Assert (for all B in S2.Areas (It2).To + 1 .. Address_Type'Last => Includes (B, Result) = (Includes (B, S1) and Includes (B, S2)));
                   end if;

                   pragma Assert (for all B in Address_Type'First .. S1.Areas (S1.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));
                   pragma Assert (for all B in Address_Type'First .. S2.Areas (S2.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));

                   exit;
                end if;

                It2 := It2 + 1;

                Lemma_Nothing_In_Between (S2, It2 - 1);
                pragma Assert (for all B in S2.Areas (It2 - 1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, S2));
                Lemma_Nothing_Beyond_Last (Result);
                pragma Assert (for all B in S2.Areas (It2 - 1).To + 1 .. S2.Areas (It2).From - 1 => not Includes (B, Result));
                pragma Assert (for all B in Address_Type'First .. S2.Areas (It2).From - 1 => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));

                pragma Assert (if It1 > 1 then Is_Computed (S1, S2, S1.Areas (It1 - 1).To));
                pragma Assert (if It2 > 1 then Is_Computed (S1, S2, S2.Areas (It2 - 1).To));
             end if;
          end loop;

          Lemma_Order (S1);
          Lemma_Order (S2);
          Lemma_Order (Result);

          pragma Assert (for all B in Address_Type'First .. S1.Areas (S1.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));
          pragma Assert (for all B in Address_Type'First .. S2.Areas (S2.Areas'Last).To => (Includes (B, Result)) = (Includes (B, S1) and Includes (B, S2)));

          pragma Assert (for all B in Address_Type => (Includes (B, Result)) = ((Includes (B, S1) and Includes (B, S2))));

          return Result;
       end "and";

       -----------
       -- "not" --
       -----------

       function "not" (S : Set) return Set
       is
          Result : Set (S.Size + 1);
       begin
          if S.Size = 0 then
             Lemma_Last_Is_Included (Full_Set);
             pragma Assert (for all B in 0 .. Address_Type'Last => not Includes (B, S));
             pragma Assert (for all B in 0 .. Address_Type'Last => Includes (B, Full_Set));
             pragma Assert (for all B in 0 .. Address_Type'Last => Includes (B, Full_Set) /= Includes (B, S));
             return Full_Set;
          end if;

          Result.Size := 0;
          Result.Areas := (others => (others => 0));

          Lemma_Order (S);

          if S.Areas (1).From /= Address_Type'First then
             Result.Size := Result.Size + 1;
             Result.Areas (Result.Size) := (Address_Type'First, S.Areas (1).From - 1);

             Lemma_Order (Result);
             Lemma_Last_Is_Included (Result);

             pragma Assert (for all B in Result.Areas (Result.Size).From .. Result.Areas (Result.Size).To => Includes (B, Result));
             pragma Assert (for all B in Result.Areas (Result.Size).From .. Result.Areas (Result.Size).To => not Includes (B, S));

             pragma Assert (for all B in Result.Areas (Result.Size).To + 1.. S.Areas (1).To => not Includes (B, Result));
             pragma Assert (for all B in Result.Areas (Result.Size).To + 1 .. S.Areas (1).To => Includes (B, S));

             pragma Assert (for all B in 0 .. S.Areas (1).To => Includes (B, Result) /= Includes (B, S));
          end if;

          pragma Assert (Is_Consistent (Result));

          Lemma_Order (Result);

          pragma Assert (for all B in 0 .. S.Areas (1).To => Includes (B, Result) /= Includes (B, S));

          for I in 1 .. S.Size - 1 loop
             Lemma_Order (S);
             Lemma_Order (Result);

             Result.Size := Result.Size + 1;
             Result.Areas (Result.Size) := (S.Areas (I).To + 1, S.Areas (I + 1).From - 1);

             Lemma_Order (Result);
             Lemma_Last_Is_Included (Result);
             Lemma_Nothing_In_Between (S, I);

             -- Assertions needed for platinium
             pragma Assert (for all B in Result.Areas (Result.Size).From .. Result.Areas (Result.Size).To => Includes (B, Result));
             pragma Assert (for all B in Result.Areas (Result.Size).From .. Result.Areas (Result.Size).To => not Includes (B, S));

             pragma Assert (for all B in Result.Areas (Result.Size).To + 1.. S.Areas (I + 1).To => not Includes (B, Result));
             pragma Assert (for all B in Result.Areas (Result.Size).To + 1 .. S.Areas (I + 1).To => Includes (B, S));

             pragma Assert (for all B in Result.Areas (Result.Size).From.. S.Areas (I + 1).To => Includes (B, Result) /= Includes (B, S));

             -- Silver, no RTE
             pragma Loop_Invariant (Result.Size <= I + 1);

             -- Gold, Is_Consistent
             pragma Loop_Invariant (Result.Size in Result.Areas'Range);
             pragma Loop_Invariant (Result.Areas (Result.Size).To < S.Areas (I + 1).From);
             pragma Loop_Invariant (Is_Consistent (S));
             pragma Loop_Invariant (Is_Consistent (Result));

             -- Platinium, S = not E
             pragma Loop_Invariant (for all B in 0 .. S.Areas (I + 1).To  => Includes (B, Result) /= Includes (B, S));
          end loop;

          -- Gold

          Lemma_Order (Result);
          Lemma_Order (S);

          -- Platinium
          pragma Assert (for all B in 0 .. S.Areas (S.Size).To  => Includes (B, Result) /= Includes (B, S));

          if S.Areas (S.Size).To /= Address_Type'Last then
             pragma Assert (if S.Size > 1 then Result.Areas (Result.Size).To < S.Areas (S.Size).From);

             Result.Size := Result.Size + 1;
             Result.Areas (Result.Size) := (S.Areas (S.Size).To + 1, Address_Type'Last);
             Lemma_Order (Result);
             Lemma_Last_Is_Included (Result);

             -- Gold
             pragma Assert (Is_Consistent (Result));

             -- Platinium

             -- The prover needs a bit of help believing that the previous areas have not been touched
             pragma Assert (if S.Size > 1 then (for all B in 0 .. S.Areas (S.Size).To => Includes (B, Result) /= Includes (B, S))); -- ???
             pragma Assert (if S.Size = 1 then (for all B in 0 .. S.Areas (S.Size).To => Includes (B, Result) /= Includes (B, S)));

             pragma Assert (for all B in Result.Areas (Result.Size).From .. Address_Type'Last => Includes (B, Result));
             pragma Assert (for all B in Result.Areas (Result.Size).From .. Address_Type'Last => not Includes (B, S));

             pragma Assert (for all B in Result.Areas (Result.Size).From .. Address_Type'Last => Includes (B, Result) /= Includes (B, S));
             pragma Assert (for all B in 0 .. S.Areas (S.Size).To => Includes (B, Result) /= Includes (B, S));

             pragma Assert (for all B in 0 .. Address_Type'Last => Includes (B, Result) /= Includes (B, S));
          end if;

          pragma Assert (Is_Consistent (Result));

          pragma Assert (for all B in 0 .. Address_Type'Last => Includes (B, Result) /= Includes (B, S));

          return Result;
       end "not";

    end Area_Math;

.. code:: ada

    package Area_Math.Lemma
    with SPARK_Mode
    is
       -- Lemma are used for two purposes:
       --   Manually demonstrating an implication between two truths (a Pre and a Post)
       --   Reducing the scope of hypothesis need to automatically run such demonstration
       --      (Lemma will only consider the pre and predicates).

       procedure Lemma_Order (S : Set) with
         Ghost,
         Pre => (for all I in 1 .. S.Size - 1 => S.Areas (I).To < S.Areas (I + 1).From),
         Post =>
           (for all I in 1 .. S.Size - 1 => (for all J in I + 1 .. S.Size => S.Areas (I).To < S.Areas (J).From));

       procedure Lemma_Last_Is_Included (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => S.Size = 0 or else (for all B in S.Areas (S.Size).From .. S.Areas (S.Size).To => Includes (B, S));

       procedure Lemma_Nothing_In_Between (S : Set; Left : Natural) with
         Ghost,
         Pre =>
           Is_Consistent (S) and
           Left in 1 .. S.Size - 1,
         Post => (for all B in S.Areas (Left).To + 1 .. S.Areas (Left + 1).From - 1 => not Includes (B, S));

       procedure Lemma_Nothing_Before_First (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => (if S.Size = 0 then (for all B in Address_Type => not Includes (B, S))
                    elsif S.Areas (1).From > Address_Type'First then
                      (for all B in Address_Type'First .. S.Areas (1).From - 1 => not Includes (B, S)));


       procedure Lemma_Nothing_Beyond_Last (S : Set) with
         Ghost,
         Pre => Is_Consistent (S),
         Post => (if S.Size = 0 then (for all B in Address_Type => not Includes (B, S))
                    elsif S.Areas (S.Size).To < Address_Type'Last then
                      (for all B in S.Areas (S.Size).To + 1 .. Address_Type'Last => not Includes (B, S)));

    end Area_Math.Lemma;

.. code:: ada

    package body Area_Math.Lemma
    with SPARK_Mode
    is

       procedure Lemma_Order
         (S : Set)
       is
       begin
          if S.Size = 0 then
             return;
          end if;

          for I in 1 .. S.Size - 1 loop
             for J in I + 1 .. S.Size loop
                pragma Assert (S.Areas (J - 1).To < S.Areas (J).From);
                pragma Loop_Invariant (for all R in I + 1 .. J => S.Areas (I).To < S.Areas (R).From);
             end loop;

             pragma Loop_Invariant
               ((for all R in 1 .. I => (for all T in R + 1 .. S.Size => S.Areas (R).To < S.Areas (T).From)));
          end loop;
       end Lemma_Order;

       procedure Lemma_Last_Is_Included (S : Set)
       is
       begin
          null;
       end Lemma_Last_Is_Included;

       procedure Lemma_Nothing_In_Between (S : Set; Left : Natural) is
       begin
          Lemma_Order (S);

          for K in 1 .. S.Size loop
             pragma Loop_Invariant (for all B in S.Areas (Left).To + 1 .. S.Areas (Left + 1).From - 1 =>
                                       not (for some I in 1 .. K => B in S.Areas (I).From .. S.Areas (I).To));
          end loop;
       end Lemma_Nothing_In_Between;

       procedure Lemma_Nothing_Before_First (S : Set) is
       begin
          Lemma_Order (S);
       end Lemma_Nothing_Before_First;

       procedure Lemma_Nothing_Beyond_Last (S : Set) is
       begin
          Lemma_Order (S);
       end Lemma_Nothing_Beyond_Last;

    end Area_Math.Lemma;

.. code:: ada

    with Area_Math; use Area_Math;

    package Memory_Analysis
    with SPARK_Mode => On
    is

       type Byte_Property is record
          Stack    : Boolean;
          Heap     : Boolean;
          Scrubbed : Boolean;
       end record
       with Ghost;

       type Memory_Type is array (Address_Type) of Byte_Property with Ghost;

       Memory : Memory_Type with Ghost;

       procedure Set_Heap (From, To : Address_Type)
         with
           Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Heap => True) else Memory (B) = Memory'Old (B)'Update (Heap => False))),
         Global => (In_Out => Memory);

       procedure Set_Stack (From, To : Address_Type)
         with Pre => To >= From,
           Post => (for all B in Address_Type => (if B in From .. To then Memory (B) = Memory'Old (B)'Update (Stack => True) else Memory (B) = Memory'Old (B)'Update (Stack => False))),
           Global => (In_Out => Memory);

       procedure Scrub (From, To : Address_Type)
         with Pre => To >= From,
         Post =>
           (for all B in Address_Type =>
                  (if B in From .. To then Memory (B) = Memory'Old(B)'Update (Scrubbed => True)
                       else Memory (B) = Memory'Old(B))),
           Global => (In_Out => Memory);

       procedure Get_Heap_Boundaries (From, To : out Address_Type)
         with Post => (for all B in Address_Type => (Memory (B).Heap = (B in From .. To)))
         and then From <= To;

       procedure Get_Stack_Boundaries (From, To : out Address_Type)
         with Post => (for all B in Address_Type => (Memory (B).Stack = (B in From .. To)))
         and then From <= To;
    --
       function Valid_Heap_And_Stack_Area
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type) return Boolean
       is
         ((Stack_From <= Stack_To and Heap_From <= Heap_To) and then
          Heap_From not in Stack_From .. Stack_To and then
          Heap_To not in Stack_From .. Stack_To and then
          Stack_From not in Heap_From .. Heap_To and then
          Stack_To not in Heap_From .. Heap_To);

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
         with Pre => Valid_Heap_And_Stack_Area (Heap_From, Heap_To, Stack_From, Stack_To),
           Post =>
           (for all B in Address_Type =>
              (Memory (B).Scrubbed = ((Memory'Old (B).Heap or Memory'Old (B).Stack) and then not (Memory (B).Heap or Memory (B).Stack))));

    end Memory_Analysis;

.. code:: ada prove_button

    with Interfaces.C; use Interfaces.C;

    with Area_Math.Lemma; use Area_Math.Lemma;

    package body Memory_Analysis
    with SPARK_Mode => On
    is

       --  Assume these variables are defined in C source-code
       FromHeap  : Interfaces.C.unsigned;
       ToHeap    : Interfaces.C.unsigned;
       FromStack : Interfaces.C.unsigned;
       ToStack   : Interfaces.C.unsigned;

       --------------
       -- Set_Heap --
       --------------

       procedure Set_Heap
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          FromHeap := Interfaces.C.unsigned (From);
          ToHeap := Interfaces.C.unsigned (From);
       end Set_Heap;

       ---------------
       -- Set_Stack --
       ---------------

       procedure Set_Stack
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          FromStack := Interfaces.C.unsigned (From);
          ToStack := Interfaces.C.unsigned (From);
       end Set_Stack;

       -----------
       -- Scrub --
       -----------

       procedure Scrub
         (From, To : Address_Type)
         with SPARK_Mode => Off
       is
       begin
          -- TODO: This is not implemented in the context of this example
          null;
       end Scrub;

       -------------------------
       -- Get_Heap_Boundaries --
       -------------------------

       procedure Get_Heap_Boundaries
         (From, To : out Address_Type)
         with SPARK_Mode => Off
       is
          FromHeap : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromHeap";

          ToHeap : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromHeap";
       begin
          From := Address_Type (FromHeap);
          To := Address_Type (ToHeap);
       end Get_Heap_Boundaries;

       --------------------------
       -- Get_Stack_Boundaries --
       --------------------------

       procedure Get_Stack_Boundaries
         (From, To : out Address_Type)
         with SPARK_Mode => Off
       is
          FromStack : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromStack";

          ToStack : Interfaces.C.unsigned
            with Import,
            Convention => C,
            External_Name => "fromStack";
       begin
          From := Address_Type (FromStack);
          To := Address_Type (ToStack);
       end Get_Stack_Boundaries;

       -------------------------
       -- Move_Heap_And_Stack --
       -------------------------

       procedure Reset_Scrub
         with Post =>
           (for all B in Address_Type'Range =>
              Memory (B) = Memory'Old(B)'Update (Scrubbed => False)),
         Ghost;

       procedure Reset_Scrub is
          Old_Memory : Memory_Type := Memory;
       begin
          for B in Address_Type'Range loop
             Memory (B).Scrubbed := False;

             pragma Loop_Invariant
               (for all B2 in Address_Type'First .. B =>
                  Memory (B2) = Memory'Loop_Entry(B2)'Update (Scrubbed => False));
          end loop;
       end Reset_Scrub;

       procedure Lemma_No_Overlap (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
         with Pre => Valid_Heap_And_Stack_Area (Heap_From, Heap_To, Stack_From, Stack_To),
         Post => (for all B in Heap_From .. Heap_To => B not in Stack_From .. Stack_To)
         and (for all B in Stack_From .. Stack_To => B not in Heap_From .. Heap_To),
           Ghost
       is
       begin
          null;
       end Lemma_No_Overlap;

       procedure Move_Heap_And_Stack
         (Heap_From, Heap_To, Stack_From, Stack_To : Address_Type)
       is
          Prev_Heap_From, Prev_Heap_To, Prev_Stack_From, Prev_Stack_To : Address_Type;
       begin
          Get_Stack_Boundaries (Prev_Stack_From, Prev_Stack_To);
          Get_Heap_Boundaries (Prev_Heap_From, Prev_Heap_To);

          Reset_Scrub;

          declare
             Prev : Set := Create (Prev_Heap_From, Prev_Heap_To) or Create (Prev_Stack_From, Prev_Stack_To);
             Next : Set := Create (Heap_From, Heap_To) or Create (Stack_From, Stack_To);
             To_Scrub : Set := Prev and not Next;
          begin
             Lemma_Nothing_Beyond_Last (To_Scrub);

             for I in 1 .. To_Scrub.Size loop
                Scrub (To_Scrub.Areas (I).From, To_Scrub.Areas (I).To);

                if I = 1 then
                   Lemma_Nothing_Before_First (To_Scrub);
                else
                   Lemma_Nothing_In_Between (To_Scrub, I - 1);
                end if;

                pragma Loop_Invariant
                  (if To_Scrub.Areas (I).To < Address_Type'Last then
                        (for all B in To_Scrub.Areas (I).To + 1 .. Address_Type'Last => not Memory (B).Scrubbed));
                pragma Loop_Invariant
                  (for all B in Address_Type'First .. To_Scrub.Areas (I).To => Includes (B, To_Scrub) = Memory (B).Scrubbed);
             end loop;

             Lemma_Nothing_Beyond_Last (To_Scrub);
             Lemma_No_Overlap (Heap_From, Heap_To, Stack_From, Stack_To);

             Set_Stack (Stack_From, Stack_To);
             Set_Heap (Heap_From, Heap_To);
          end;
       end Move_Heap_And_Stack;

    end Memory_Analysis;

.. code:: ada run_button prove_button

    with Ada.Text_IO; use Ada.Text_IO;
    with Memory_Analysis; use Memory_Analysis;
    with Area_Math; use Area_Math;

    procedure Memory_Main
      with SPARK_Mode => On
    is
       Heap_Start, Heap_End : Address_Type;
       Stack_Start, Stack_End : Address_Type;
    begin
       Set_Heap (16#00A0_0000#, 16#00AF_FFFF#);
       Set_Stack (16#00B0_0000#, 16#00BF_FFFF#);

       Stack_Start := Address_Type'Value (Get_Line);
       Stack_End := Address_Type'Value (Get_Line);

       Heap_Start := Address_Type'Value (Get_Line);
       Heap_End := Address_Type'Value (Get_Line);

       if not Valid_Heap_And_Stack_Area (Stack_Start, Stack_End, Heap_Start, Heap_End) then
          return;
       end if;

       Move_Heap_And_Stack (Stack_Start, Stack_End, Heap_Start, Heap_End);
    end Memory_Main;

As a disclaimer, I am an experienced Ada developer but had relatively little
experience with proof. I also selected a problem relatively hard |mdash|
the quantified properties and the :ada:`Set` structure are quite
different, and proving quantifiers is known to be hard for provers to
start with.With that in mind, the solution I came up with spreads over
almost a thousand lines of code |mdash| and consumed about a week and a
half of effort.

I'm also
`linking <https://github.com/AdaCore/SPARK_memory/tree/master/src_alternate>`_
here a solution that my colleague Claire Dross came up with. She's one of
our most senior expert in formal proof, and within a day could prove the
two most complex operators in about 300 lines of code (her implementation
is also more compact than mine).

The above raises a question |mdash| is it really worth it? Silver
absolutely does |mdash| it is difficult to bring a case against spending a
little bit more effort in exchange for the absolute certainty of never
having a buffer overflow or a range check error. There's no doubt that in
the time I spent in proving this, I would have spent much more in
debugging either testing, or worse, errors in the later stages should this
library be integrated in an actual product. Gold is also a relatively
strong case. The fact that I only select key properties allows only
concentrating on relatively easy stuff, and the confidence of the fact
that they are enforced and will never have to be tested clearly outweighs
the effort.

I also want to point out that the platinum effort is well worth it on the
user code in this example. While it looks tedious at first sight, getting
these properties right is relatively straightforward, and allows gaining
confidence on something that can't be easily tested; that is, a property
on the whole memory.

Now the question remains |mdash| is it worth the effort on the :ada:`Set`
library, to go from maybe two days of code + proof to around a week and a
half?

I can discuss it either way but having to write 700 lines of code to
demonstrate to the prover that what I wrote is correct keeps haunting me.
Did I really have these 700 lines of reasoning in my head when I developed
the code? Did I have confidence in the fact that each of those was
logically linked to the next? To be fair, I did find errors in the code
when writing those, but the code wasn't fully tested when I started the
proof. Would the test have found all the corner cases? How much time would
such a corner case take to debug if found in a year? (see this blog post
for some insights on hard to find bugs removed by proof).

Some people who safely certify software against e.g. avionics & railway
standards end up writing 10 times more lines of tests than code |mdash|
all the while just verifying samples of potential data. In that situation,
provided that the properties under test can be modelled by SPARK
assertions and that they fit what the prover knows how to do, going
through this level of effort is a very strong case.

Anything less is open for debate. I have to admit, against all odds, it
was a lot of fun and I would personally be looking forward to taking the
challenge again. Would my boss allow me to is a different question. It all
boils down to the cost of a failure versus the effort to prevent said
failure. Being able to make an enlightened decision might be the most
valuable outcome of having gone through the effort.
