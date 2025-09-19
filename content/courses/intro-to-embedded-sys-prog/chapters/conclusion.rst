:next_state: False

Conclusion
==========

.. include:: ../../../global.txt


In the introduction to this course, we defined an "embedded system" as a
computer that is part of a larger system, in which the capability to
compute is not the larger system's primary function. These computers are
said to be "embedded" in the larger system. That, in itself, sets this kind
of programming apart from the more typical host-oriented programming. But
the context also implies fewer resources are available, especially memory
and electrical power, as well as processor power. Add to those limitations
a frequent reliability requirement and you have a demanding context for
development.

Using Ada can help you in this context, and for less cost than other
languages, if you use it well. Many industrial organizations developing
critical embedded software use Ada for that reason. Our goal in this course
was to get you started in using it well.

To that end, we spent a lot of time talking about how to use Ada to do low
level programming, such as how to specify the layout of types, how to map
variables of those types to specific addresses, when and how to do
unchecked programming (and how not to), and how to determine the validity
of incoming data. Ada has a lot of support for this activity so there was
much to explore.

Likewise, we examined development using Ada in combination with other
languages, a not uncommon approach. Specifically, we saw how
to interface with code and data written in other languages, and how (and
why) to work with assembly language. Development in just one language is
becoming less common over time so these were important aspects to know.

One of the more distinctive activities of embedded programming involves
interacting with the outside world via embedded devices, such as A/D
converters, timers, actuators, sensors, and so forth. (This can be one
of the more entertaining activities as well.) We covered how to
interact with these memory-mapped devices using representation
specifications, data structures that simplified the functional code, and
time-honored aspects of software engineering, including abstract data types.

Finally, we explored how to handle interrupts in Ada, another
distinctive part of embedded systems programming. As we saw, Ada has
extensive support for handling interrupts, using the same building
blocks |mdash| protected objects |mdash| used in concurrent programming.
These constructs provide a way to handle interrupts that is as portable
as possible, in what is otherwise a very hardware-specific
endeavor.

In the course, we mentioned a library of freely-available
device drivers in Ada known as the Ada Driver Library (ADL). The ADL is
a good resource for learning how Ada can be used to develop software for
embedded systems using real-world devices and processors. Becoming
familiar with it would be a good place to go next. Contributing to it
would be even better! The ADL is available on GitHub for both
non-proprietary and commercial use here:
https://github.com/AdaCore/Ada_Drivers_Library.
