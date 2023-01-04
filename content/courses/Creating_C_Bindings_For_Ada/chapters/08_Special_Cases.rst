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
     procedure Increase_Flow (Pipe   : System.Address;
                              Amount : Interfaces.C.unsigned);
  private
     type Pipe_T is new System.Address;
  end Void_Pointer;

----------------------------
Void Pointers via Generics
----------------------------

In many cases, :c:`void *` is used to map an address to an object
that can be of varying size. A typical example is a mechanism that
sends a message (basically some number of bytes), but that message
may have different content based on some criteria.

.. code-block:: c
  :caption: C Header File

  struct OpenPipeT {
     unsigned pipeId;
     unsigned requestor;
     };
  struct ChangeFlowT {
     unsigned pipeId;
     unsigned requestor;
     short percentage;
     };
  struct ClosePipeT {
     unsigned pipeId;
     unsigned requestor;
     };
  enum CommandT { OPEN, CHANGE, CLOSE };
  void setPipeCommand ( enum CommandT command,
                        void *content );

As you will notice above, :c:`setPipeCommand` takes a :ada:`void *`
parameter. When we call the function, we would create an object
of the appropriate type and pass the address into the subprogram.

.. code-block:: c

  struct OpenPipeT message = { 1, 2 };
  setPipeCommand ( OPEN, &message );

The concern in this case (regardless of language), is there is
no verification that the contents at the address is a valid
message, or even is a legal address.

.. code-block:: Ada
  :caption: Ada Thin Binding

  with Interfaces.C;
  with System;
  package Message_H is
     type Open_Pipe_T is record
        Pipe_Id   : Interfaces.C.unsigned;
        Requestor : Interfaces.C.unsigned;
     end record with Convention => C_Pass_By_Copy;
     type Change_Flow_T is record
        Pipe_Id    : Interfaces.C.unsigned;
        Requestor  : Interfaces.C.unsigned;
        Percentage : Interfaces.C.short;
     end record with Convention => C_Pass_By_Copy;
     type Close_Pipe_T is record
        Pipe_Id   : Interfaces.C.unsigned;
        Requestor : Interfaces.C.unsigned;
     end record with Convention => C_Pass_By_Copy;
     type Command_T is (Open, Change, Close) with Convention => C;
     procedure Set_Pipe_Command (Command : Command_T;
                                 Content : System.Address)
       with Import => Standard.True,
            Convention => C,
            External_Name => "setPipeCommand";
  end Message_H;

The thin binding of the header file will allow us to do the same -
create an object, and pass the address of the object into
:ada:`Set_Pipe_Command`.

.. code-block:: Ada

   procedure Open is
      Message : Open_Pipe_T := (1, 2);
   begin
      Set_Pipe_Command (Open, Message'address);
   end Open;

But this still leaves the concern about random addresses.

If we were writing this in Ada to begin with, one solution would be to
use generics (either a subprogram or package) to implement different versions
of :ada:`Set_Pipe_Command` that would take a parameter of the appropriate
message type and have it call the thin binding, remove the ability of the
clients to abuse the address parameter.

.. code-block:: Ada
  :caption: Ada Thick Binding

   generic
      Command : Command_T;
      type Content_T is private;
   procedure Generic_Set_Pipe_Command (Content : Content_T);

   procedure Generic_Set_Pipe_Command (Content : Content_T) is
   begin
      Set_Pipe_Command (Command, Content'address);
   end Generic_Set_Pipe_Command;

.. code-block:: Ada
  :caption: Ada Thick Binding Usage

   procedure Open_Command is new Generic_Set_Pipe_Command
     (Open, Open_Pipe_T);
   procedure Change_Flow_Command is new Generic_Set_Pipe_Command
     (Change, Change_Flow_T);
   procedure Close_Command is new Generic_Set_Pipe_Command
     (Close, Close_Pipe_T);

   procedure Example is
      Open_Pipe : Open_Pipe_T := (1, 2);
      Change_Flow : Change_Flow_T := (1, 2, 3);
      Close_Pipe : Close_Pipe_T := (1, 2);
   begin
      Open_Command (Open_Pipe);
      Change_Flow_Command (Change_Flow);
      Close_Command (Close_Pipe);
   end Example;

