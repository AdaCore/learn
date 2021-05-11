Interacting with Devices
========================

.. include:: ../../global.txt


Memory-Mapped Abstractions
--------------------------

Earlier we said that we could query the address of some object, and we 
also showed how to use that result to specify the address of some other 
object. We used that capability to create an overlay, in which two 
objects are used to refer to the memory shared by those objects. As we 
indicated in that discussion, you would not use the same type for each 
object |mdash| the point, after all, is to provide a view of the shared 
underlying memory cells that is not already available otherwise. Each 
distinct type, in other words, would provide a set of operations 
providing some required functionality. 

For example, here's an overlay of a (presumably) 32-bit integer type and 
a 32-bit array type: 

.. code-block:: ada

   type Bits32 is array (0 .. 31) of Boolean
      with Component_Size => 1;

   X : Integer;
   Y : Bits32 with Address => X'Address;
   
Because one view is as an integer and the 
other as an array, we can access that memory using different operations. 
Via the array object Y we can access individual bits of the memory 
shared with X. Via the integer X, we can do arithmetic on the contents 
of that memory. 

Very often, though, there is only on Ada object that we place at 
some specific address. That's because the object is meant to be the 
software interface to some memory-mapped hardware device. In this 
scenario we don't have two overlaid Ada objects, we just have one. The 
other "object" is the hardware device. Since they are at the same memory 
locations, accessing the Ada object accesses the device. 

For a real-world but nonetheless simple example, suppose we have a 
rotary switch on the front of our embedded computer. Rotating the switch 
selects among a limited number of values. This switch allows humans to 
provide some very simple input to the software running on the computer. 
In the application this example is taken from, the rotary switch 
identified the specific computer among several in a rack of computers 
and other boards, each computer having been given a different rotary 
switch value. 

.. code-block:: ada

   Rotary_Switch : Unsigned_8 with
     Address => System.Storage_Elements.To_Address (16#FFC0_0801#);

We declare the object and also specify the address, but not by querying 
some entity. We already know the address from the hardware 
documentation. But we cannot simply use an integer address literal from 
that documentation because type Address is almost always a private type. 
We need a way to compose an Address value from an integer value. The 
package :ada:`System.Storage_Elements` defines an integer representation 
for Address values, among other useful things, and a way to convert 
those integer values to Address values. The function :ada:`To_Address` 
does that conversion. 

As a result, in the Ada code, reading the value of the variable 
Rotary_Switch reads the number on the actual hardware switch. 
 
Note that if you specify the wrong address, it is hard to say what 
happens. Likewise, it is an error for an Address clause to disobey the 
object's Alignment. The error cannot be detected at compile time, in 
general, because the Address is not necessarily known at compile time. 
There's no requirement for a run-time check for the sake of efficiency, 
since efficiency seems paramount here. Consequently, this misuse of 
Address clauses is just like any other misuse of Address clauses |mdash| 
execution of the code is erroneous, meaning all bets are off. You need 
to know what you're doing. 

What about writing to the variable? Is that meaningful? In this case, 
no. But for some other device it could be meaningful, certainly. It just 
depends on the hardware. But in this case, writing to the Rotary_Switch 
variable would have no effect, which could be confusing to programmers. 
It looks like a variable, after all. We wouldn't declare it as a constant 
because the human user could rotate the switch, resulting in a different 
value read. Therefore, we hid the Ada variable behind a function, 
obviating the entire question. Clients of the function can then use it 
for whatever purpose they require, e.g., as the unique identifier for a 
computer in a rack. 

Let's talk about the type we use to represent the memory-mapped device. 
That type defines the view |mdash| hence the operations |mdash| we have 
for the device. 

We choose the type for the representative Ada variable based on the 
interface of the hardware mapped to the memory. If the interface is a 
single monolithic register, for example, then an integer, either signed 
or unsigned, and of the necessary size, will suffice. But suppose the 
interface is several bytes wide, and some of the bytes have different 
purposes from the others? In that case, a record type is the obvious 
solution, with distinct record components dedicated to the different 
bytes of the hardware interface. We could use individual bits too, of 
course, if that's what the hardware does. Ada is particularly good at 
this fine-degree of representation because record components of any 
types can be specified in the layout, down to the bit level, within the 
record. 


volatile
atomic
atomic_components
volatile_full_access

benefit of bit-mapped record fields versus bit-patterns with unsigned types




.. todo::

    Complete section!


Dynamically Converting Addresses
--------------------------------

.. todo::

    Complete section!


General-Purpose Code Generators
-------------------------------

.. todo::

    Complete section!


Pragma :ada:`Atomic`
--------------------

.. todo::

    Complete section!


Pragma :ada:`Atomic_Components`
-------------------------------

.. todo::

    Complete section!
