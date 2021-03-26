:prev_state: False

Introduction
============

:code-config:`run_button=True;prove_button=False;accumulate_code=False`
:code-config:`reset_accumulator=True`

.. include:: ../../global.txt

This is a course about embedded systems programming. Embedded systems 
are everywhere today, including, for example, the thermostats that 
control a building's temperature, the power-steering controller in 
modern automobiles, and the control systems in charge of jet 
engines. (A jet engine controller, when designed to have absolute 
control, is known as a Full Authority Digital Engine Controller, or 
FADEC for short. If they fail, very bad things can happen.) 

Clearly, much can depend on these systems operating correctly. Using Ada 
can help you get it right, and for less cost than other languages, if 
you use it well. Many industrial organizations developing critical 
embedded software use Ada for that reason. Our goal is to get you 
started in using it well. 

The course is based on the assumption that you know some of the Ada 
language already, preferably even some of the more advanced concepts. 
You don't need to know how to use Ada constructs for embedded systems, 
of course, but you do need to know at least the language basics. 
If you need that introduction, see the course  
:doc:`Introduction to Ada <courses/intro-to-ada/index>`.

We also assume that you already have some programming experience so we 
won't cover CS-101. 

Ideally, you also have some experience with low-level programming, 
because we will focus on "how to do it in Ada." If you do, feel free to 
gloss over the introductory material. If not, don't worry. We will cover 
enough for the course to be of value in any case. 


So, what will we actually cover?
--------------------------------

We will introduce you to using Ada to do low level programming, such as 
how to specify the layout of types, how to map variables of those types 
to specific addresses, when and how to do unchecked programming (and 
how not to), and how to determine the validity of incoming data, e.g., 
data from sensors that are occasionally faulty. 

We will discuss development using more than Ada alone, nowadays a quite common 
approach. Specifically, how to interface with code and data written in 
other languages, and how (and why) to work with assembly language. 

Embedded systems work with embedded devices, such as A/D converters, 
timers, and so forth. Frequently these devices are mapped into the 
target memory address space. We will cover how to define and interact 
with these memory-mapped devices. 

Finally, we will show how to handle interrupts in Ada, using portable 
constructs. 

Definitions
-----------

Before we go any further, what do we mean by "embedded system" anyway? 
It's time to be specific. We're talking about a computer that is part of 
a larger system, in which the capability to compute is not the larger 
system's primary function. These computers are said to be "embedded" in 
the larger system: the enclosing thermostat controlling the temperature, 
the power steering controller in the enclosing automobile, and the FADEC 
embedded in the enclosing aircraft. So these are not stand-alone 
computers available general purpose application execution. 

As such, embedded systems typically have reduced resources available, 
especially power, which means reduced processor speed and reduced memory 
on-board. For an example at the small end of the spectrum, consider the 
computer embedded in a wearable device: it must run for a long time on a 
very little battery, with comparatively little memory available. But 
that's often true of bigger systems too, such as systems on aircraft 
where power (and heat) are directly limiting factors. 

As a result, developing embedded systems software can be more difficult 
than general application development, not to mention that this software 
is potentially safety-critical. 

Ada is known for use in very large, very long-lived projects (e.g., 
deployed for decades), but it can also be used for very small systems 
with tight resource constraints. We'll show you how. 

We used the term "computer" above. You already know what that means, but 
you may be thinking of your laptop or something like that, where the 
processor, memory, and devices are all distinct, separate components. 
That can be the case for embedded systems too, albeit in a different 
form-factor such as rack-mounted boards. However, be sure to expand your 
definition to include the notion of a system-on-chip (SoC), in which the 
processor, memory, and various useful devices are all on a single chip. 
Embedded systems don't necessarily involve SoC computers but they 
frequently do. The techniques and information in this course work on any 
of these kinds of computer. 

Down To The Bare Metal
----------------------

Ada has always had facilities designed specifically for embedded 
systems. The language includes constructs for directly manipulating 
hardware, for example, and direct interaction with assembly language. 
These constructs are as effective as those of any high-level programming 
language (yes, including C). These constructs are expressively powerful, 
well-specified (so there are few surprises), efficient, and portable 
(within reason). 

We say "within reason" because portability is a difficult goal for 
embedded systems. That's because the hardware is so much a part of the 
application itself, rather than being abstracted away as in a 
general-purpose application. That said, the hardware details can be 
managed in Ada so that portability is maximized to the extent possible 
for the application. 

Strictly speaking, not all software can or should be absolutely 
portable! If a specific device is required, well, the program won't work 
with some other device. But tot he extent possible portability is 
obviously a good thing. 

Separation Principle
--------------------

There is a language design principle underlying the Ada facilities 
intended for implementing embedded software. This design principle 
directly affects how the language is used, and therefore, the 
portability and readability of the resulting application code. 

This language design principle is known as the "separation principle." 
What's being separated? The low-level, less portable aspects of some 
piece of code are separated from the usage of that piece of code. 

Don't confuse this with hiding unnecessary implementation details via 
compile-time visibility control (i.e., information hiding and 
encapsulation). That certainly should be done too. Instead, because of 
the separation principle, we specify the low-level properties of 
something once, when we declare it. From then on, we can use regular Ada 
code to interact with it. That way the bulk of the code -- the usage -- 
is like any other Ada code, and doesn't propagate the low-level details 
all over the client code. This greatly simplifies usage and 
understandability as well as easing porting to new hardware-specific 
aspects. You change things in one place, rather than everywhere. 

For example, consider a device mapped to the memory address space of the 
processor. To interact with the device we interact with one or more 
memory cells. Reading input from the device amounts to reading the value 
at the associated memory location. Likewise, sending output to the 
device amounts to writing to that location. 

To represent this device mapping we declare a variable of an appropriate 
type and specify the starting address the object should occupy. (There 
are other ways too, but for a single, statically mapped object this is 
the simplest approach.) We'd want to specify some other characteristics 
as well, but let's focus on the address. 

.. image:: images/memory-mapped-device.png
  :width: 600
  :alt: Memory-mapped device with Ada object declared at specific address

If the hardware presents an interface consisting of multiple fields 
within individual memory cells, we can use a record type instead of a 
single unsigned type representing a single word. Ada allows us to 
specify the exact record layout, own to the individual bit level, for 
any type we may need to use for the record components. Then when we 
declare the object we use that record type, again specifying the 
starting address. Then we can just refer to the object's record 
components as usual, having the compiler compute the address offsets 
required to access the components representing the individual hardware 
fields. 

Note that we aren't saying that other languages cannot do this too. Many 
can, using good programming practices. What we're saying is that those 
practices are designed into the Ada way of doing it. 

The Ada Drivers Library
-----------------------

Speaking of memory-mapped devices and SoC computers, there is a library 
of freely-available device drivers in Ada. Known as the Ada Driver 
Library (ADL), it supports many devices on a number of vendors' 
products. Device drivers for timers, I2C, SPI, A/D and D/A converters, 
DMA, General Purpose I/O, LCD displays, sensors, and other devices are 
included. The ADL is available on GitHub for both non-proprietary and 
commercial use here: https://github.com/AdaCore/Ada_Drivers_Library. 

An extensive description of a project using the ADL is available here:
https://blog.adacore.com/making-an-rc-car-with-ada-and-spark
