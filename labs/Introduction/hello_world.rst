.. meta::
:author: AdaCore

:prev_state: False
:next_state: False

:code-config:`accumulate_code=False;reset_accumulator=True`

"Hello World!" in Ada
======================

In this lab we will learn to print strings to stdout with the Ada.Text_IO package and to read input strings from stdin with the Ada.Command_Line package.

Task
-----

Print "Hello World" on a single line and then print the provided string on stdin to stdout.

Input Format
------------

Stdin will have a list of strings. 

Output Format
-------------

Print :ada:`Hello World!` on the first line, and the string from stdin on the second line.

Sample Input
------------

:ada:`Welcome to the Ada programming language.`

Sample Output
-------------

:ada:`Hello World!`
:ada:`Welcome to the Ada programming language.`

--------------


.. code:: ada lab=Introduction_HelloWorld

   --  START LAB IO BLOCK
   in 0: Welcome to the Ada programming language.
   out 0: Hello World! Welcome to the Ada programming language.
   in 1: ABC123
   out 1: Hello World! ABC123
   in 2: 
   out 2: Hello World!
   --  END LAB IO BLOCK

   with Ada.Command_Line; use Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;

   procedure Main
   is
   begin
      Put_Line ("Hello World!");
      if Argument_Count > 0 then
         for Arg in 1 .. Argument_Count - 1 loop
            -- fill in this loop to complete the lab
            null;
         end loop;
      end if;
   end Main; 
