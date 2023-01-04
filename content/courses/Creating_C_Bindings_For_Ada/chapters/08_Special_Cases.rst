***************
Special Cases
***************

.. include:: ../../global.txt

===============
Void Pointers
===============

In the C language, many parameters and function return values are
declared using :c:`void *`. This allows the programmer to pass/return
the address of an object, and either manipulate memory directly,
or use this address as the location of an object to be manipulated.

In Ada, we try to stay away from manipulating addresses because the
compiler will not know the underlying type structure of the object, and
therefore cannot enforce strong typing.

When translating :c:`void *` to an Ada construct, we first need to know 
what the C usage is. Most often, it is the address of an object, but the
structure of the object may change based on other criteria (example: a
message whose content changes based on header information). In some other
cases, it's the address of some object that we have no control over, we're
just passing it around (such as a file pointer).

In the second case, where we are just pasing around an object we don't have
information about, we can use :ada:`System.Address` for the underlying type,
but we should "hide" it from the client by declaring the type :ada:`private`.
This way, the client can still create an object, but cannot treat it as a
memory location. In addition, if you are concerned about aliasing, you could
even make the type :ada:`limited`, which would prevent the client from even
making a copy of the object.

.. code-block:: c
  :caption: C Header File

  void IncreaseFlow ( void *pipe, unsigned amount );
 
.. code-block:: Ada
  :caption: Ada Thin Binding

  with Interfaces.C;
  with System;
  package Void_Pointer_H is
     procedure Increase_Flow ( Pipe : System.Address;
                               Amount : Interfaces.C.Unsigned)
     with Import => Standard.True,
          Convention => C,
          External_Name => "IncreaseFlow";
  end Void_Pointer_H;
 
.. code-block:: Ada
  :caption: Ada Thick Binding

  with Interfaces.C;
  with System;
  package Void_Pointer is
     type Pipe_T is limited private;
     type Amount_T is new Interfaces.C.unsigned range 1 .. 1_000;
     procedure Increase_Flow
       (Pipe   : System.Address;
        Amount : Interfaces.C.unsigned);
  private
     type Pipe_T is new System.Address;
  end Void_Pointer;

----------------------------
Void Pointers via Generics
----------------------------

TBD
