.. include:: ../../global.txt

Big Numbers
===========

.. note::

    Big numbers are supported by

    * GNAT Community Edition 2020
    * GCC 11
    * GCC 10 (draft, no user defined literals)

Ada 2022 introduces big integers and big real types.

Big Integers
------------

The package :ada:`Ada.Numerics.Big_Numbers.Big_Integers` contains
a type :ada:`Big_Integer` and corresponding operation like comparison
(:ada:`=`, :ada:`<`, :ada:`>`, :ada:`<=`, :ada:`>=`), arithmetic
(:ada:`+`, :ada:`-`, :ada:`*`, :ada:`/`, :ada:`rem`, :ada:`mod`,
:ada:`abs`, :ada:`**`), :ada:`Min`, :ada:`Max` and
:ada:`Greatest_Common_Divisor`. The type has :ada:`Integer_Literal`
and :ada:`Put_Image` aspects redefined. So you can use it in a natural
way.

.. code-block:: ada

   Ada.Text_IO.Put_Line (Big_Integer'Image(2 ** 256));

.. code-block::

   115792089237316195423570985008687907853269984665640564039457584007913129639936

Tiny RSA implementation
-----------------------

Now we can implement the
`RSA algorithm <https://en.wikipedia.org/wiki/RSA_(cryptosystem)>`_
in a few lines of code. The main operation of RSA is **(mᵈ) mod n**.
But you can't just write :ada:`m ** d`, because these are really big
numbers and the result doesn't fit the memory. However if you keep
intermediate result by :ada:`mod n` during mᵈ calculation then it will
work. Let's write this operation as a function:

.. code-block:: ada

   --  Calculate M ** D mod N

   function Power_Mod (M, D, N : Big_Integer) return Big_Integer is

      function Is_Odd (X : Big_Integer) return Boolean is
        (X mod 2 /= 0);

      Result : Big_Integer := 1;
      Exp    : Big_Integer := D;
      Mult   : Big_Integer := M mod N;
   begin
      while Exp /= 0 loop
         --  Loop invariant is Power_Mod'Result = Result * Mult**Exp mod N
         if Is_Odd (Exp) then
            Result := (Result * Mult) mod N;
         end if;

         Mult := Mult ** 2 mod N;
         Exp := Exp / 2;
      end loop;

      return Result;
   end Power_Mod;

Let's check this with the example from
`Wikipedia <https://en.wikipedia.org/wiki/RSA_(cryptosystem)>`_.
In the example the `public key` is (n = 3233, e = 17) and the message
is m = 65. The encrypted message is
mᵉ mod n = 65¹⁷ mod 3233 = 2790 = c.

.. code-block:: ada

   Ada.Text_IO.Put_Line (Power_Mod (M => 65, D => 17, N => 3233)'Image);

.. code-block::

   2790

To decrypt it with the public key (n = 3233, d = 413) we need to
calculate cᵈ mod n = 2790⁴¹³ mod 3233

.. code-block:: ada

   Ada.Text_IO.Put_Line (Power_Mod (M => 2790, D => 413, N => 3233)'Image);

.. code-block::

   65

So :ada:`65` is the original message m. Easy!

Complete code snippet:

.. code:: ada run_button project=Courses.Ada_2022_Whats_New.Big_Numbers_Tiny_RSA

   pragma Ada_2022;

   with Ada.Text_IO;
   with Ada.Numerics.Big_Numbers.Big_Integers;
   use  Ada.Numerics.Big_Numbers.Big_Integers;

   procedure Main is

      --  Calculate M ** D mod N

      function Power_Mod (M, D, N : Big_Integer) return Big_Integer is

         function Is_Odd (X : Big_Integer) return Boolean is
            (X mod 2 /= 0);

            Result : Big_Integer := 1;
            Exp    : Big_Integer := D;
            Mult   : Big_Integer := M mod N;
         begin
            while Exp /= 0 loop
               --  Loop invariant is Power_Mod'Result = Result * Mult**Exp mod N
               if Is_Odd (Exp) then
                  Result := (Result * Mult) mod N;
               end if;

               Mult := Mult ** 2 mod N;
               Exp := Exp / 2;
            end loop;

            return Result;
         end Power_Mod;

   begin
      Ada.Text_IO.Put_Line (Big_Integer'Image(2 ** 256));
      --  Encrypt:
      Ada.Text_IO.Put_Line (Power_Mod (M => 65, D => 17, N => 3233)'Image);
      --  Decrypt:
      Ada.Text_IO.Put_Line (Power_Mod (M => 2790, D => 413, N => 3233)'Image);
   end Main;

Big Reals
---------

Besides Big_Integer, Ada 2022 provides `Big Reals`_.

.. _`Big Reals`: http://www.ada-auth.org/standards/2xaarm/html/AA-A-5-7.html

.. note::

   Don't use Big_Numbers for cryptography because it is vulnerable to
   timing side-channels attacks.

References:
-----------

* `ARM A.5.6 Big Integers`_
* `ARM A.5.7 Big Reals`_
* AI12-0208-1_

 .. _`ARM A.5.6 Big Integers`: http://www.ada-auth.org/standards/2xaarm/html/AA-13-4.html
 .. _`ARM A.5.7 Big Reals`: http://www.ada-auth.org/standards/2xaarm/html/AA-A-5-7.html
 .. _AI12-0208-1: http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-0208-1.TXT
