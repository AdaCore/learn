Handling Interrupts
===================

.. include:: ../../global.txt

Background
----------

External devices typically are much slower than the internal
processor(s) executing the application. The speed difference is usually
sufficient for the application to make significant progress doing
something else while the external device performs the function
requested. That's a good thing, because the application often imposes a
heavy load on the internal processor. Offloading as much as possible
means we can achieve our requirements with a less powerful, less
power-hungry processor.

It may be that the device itself isn't the greatest bottleneck. Maybe
the external environment just takes a relatively long time to enter the
desired state.

For example, maybe an external device has been designed to wait
passively for something to happen in the external world, and only on the
infrequent occurrence of that event should be application be notified.
Perhaps a switch is to be toggled on certain circumstances, or an
intruder detected.

Maybe the application has set up a device to do something that results
in an extended sequence of events that occur repeatedly, such as the
arrival of incoming character data from a serial (UART) port. The
arrival of each character take a long time relative to the processor,
even though it seems quick to humans. And of course an incoming message
may or may not arrive frequently.

As a consequence, there's a good chance that the internal processor
should not wait on the external device. But then, how does the external
device notify the processor that the device has something for the
application to process?

Before we answer that, there's another wrinkle to consider. When the
notification from the external device arrives, it may very well require
a timely response from the internal processor. Think back to that serial
port with a UART again. The UART is responsible for handling the
arriving characters as individual incoming bits on the line. When all
the bits for a single character have arrived, what happens next depends
on the software design. In the simplest case the internal processor
copies the single character from the UART to an internal buffer and then
goes back to doing something else while the next full character arrives
in the UART. The response to the UART must be fairly quick because the
next incoming character's bits are arriving. The internal processor must
get the current character before it is overwritten by the next arriving
character, otherwise we'll lose valid characters. So we can say that the
response to the notification from the external device must often be very
quick.

Now, ideally in the UART case, we would further offload the work from
the internal processor. Instead of having the processor copy each
arriving character from the UART into an application buffer, we would
have another external device |mdash| a direct memory access (DMA) device
|mdash| copy the arriving characters from the UART to the buffer. A DMA
device copies data from one location to another, in this case from the
address of the UART's one-character memory-mapped register to the
address of the application buffer in memory. The copy is performed by
the DMA hardware so it is extremely fast.

In either approach we need to recognize the end of a message so that the
application can be notified that a complete message is ready for
processing. That's usually a matter of the communication protocol, but
in either case we would need to notify the processor of the message
arrival.

Therefore, the general requirement is for an external device to be able
to asynchronously notify the internal processor, and for the
notification to be implemented in such a way that the beginning of the
response can be sufficiently and predictably quick.

Fortunately computers already have such a mechanism: interrupts. The
details vary considerably with the hardware architecture, but the
overall idea is independent of the ISA: an external event can trigger a
response from the processor by becoming "active." The current state of
the application is temporarily stored, and then an interrupt response
routine, known as an "interrupt handler" is executed. Upon completion of
the handler the original state of the application is restored and the
application continues execution. The time between the interrupt becoming
active and the start of the responding handler execution is known as
"interrupt latency."

Multiple interrupt sources are usually possible. Many of the external
devices will have a specific interrupt that they generate, although some
will be shared.

Hardware interrupts typically have priorities assigned. These priorities
are applied when multiple interrupts are triggered at the same time, to
define the order in which the interrupts are presented and the handlers
invoked.

There are several aspects of interrupt and interrupt handling worth
discussing but this is an introduction we will stop here, having covered
enough to explain the Ada interrupt handling facilities. One very
important point to make about these facilities is that they are highly
portable, so they don't require extensive changes when moving to an new
target computer.

Before we go into the facility details, there's one more general issue
to mention. Sometimes we do want the application to wait for the
external device. When would that be the case? To answer that we need to
introduce another term. The act of saving and restoring the state of the
interrupted application software is known as "context switching."
Naturally, we want the time required for context switching to be a small
as possible. But if the time for the device to finish is approximately
that of the context switching, the application might as well wait for
the device.

To implement the waiting, the application code can simply enter a loop,
exiting only when some external device status flag indicates completion
of the function. (You could have an upper bound on the loop, or a
timeout, if necessary.) That active waiting, via looping, is known as
"polling." As mentioned earlier, polling only makes sense for very fast
device interactions. That's a comparatively rare situation though, so
polling should not be your default design assumption. Besides, active
polling consumes power. On an embedded platform, conserving power is
often important.


Protected Procedure Handlers
----------------------------

.. todo::

    Complete section!


Interrupt Subsystem Model
-------------------------

Package :ada:`Ada.Interrupts`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Package :ada:`Ada.Interrupts.Names`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Complete section!


Associating Handlers With Interrupts
------------------------------------

.. todo::

    Complete section!


Implicit Elaboration Model
--------------------------

.. todo::

    Complete section!


Explicit Model
--------------

.. todo::

    Complete section!


Common Design Idioms
--------------------

.. todo::

    Complete section!

