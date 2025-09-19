Exceptions
------------

.. include:: ../../../global.txt

Exceptions are a mechanism for dealing with run-time occurrences that are rare, that usually correspond to errors (such as improperly formed input data), and whose occurrence causes an unconditional transfer of control.

.. todo::

   *This chapter needs some additional material, for example on how exception propagation works.  Or at least just say that it is similar to Java and C++*

Standard Exceptions
~~~~~~~~~~~~~~~~~~~~~

Compared with Java and C++, the notion of an Ada exception is very simple. An exception in Ada is an object whose "type" is :ada:`exception`, as opposed to classes in Java or any type in C++. The only piece of user data that can be associated with an Ada exception is a String.  Basically, an exception in Ada can be raised, and it can be handled; information associated with an occurrence of an exception can be interrogated by a handler.

Ada makes heavy use of exceptions especially for data consistency check failures at run time. These include, but are not limited to, checking against type ranges and array boundaries, null pointers, various kind of concurrency properties, and functions not returning a value.  For example, the following piece of code will raise the exception :ada:`Constraint_Error`:

.. code-block:: ada

   procedure P is
      V : Positive;
   begin
      V := -1;
   end P;

In the above code, we're trying to assign a negative value to a variable that's declared to be positive. The range check takes place during the assignment operation, and the failure raises the :ada:`Constraint_Error` exception at that point. (Note that the compiler may give a warning that the value is out of range, but the error is manifest as a run-time exception.) Since there is no local handler, the exception is propagated to the caller; if :ada:`P` is the main procedure, then the program will be terminated.

Java and C++ can :cpp:`throw` and :cpp:`catch` exceptions when :cpp:`try`\ing code. All Ada code is already implicitly within :cpp:`try` blocks, and exceptions are :cpp:`raise`\d and handled.

[Ada]

.. code-block:: ada

   begin
      Some_Call;
   exception
      when Exception_1 =>
         Put_Line ("Error 1");
      when Exception_2 =>
         Put_Line ("Error 2");
      when others =>
         Put_Line ("Unknown error");
   end;

[C++]

.. code-block:: cpp

   try {
      someCall ();
   } catch (Exception1) {
      cout << "Error 1" << endl;
   } catch (Exception2) {
      cout << "Error 2" << endl;
   } catch (...) {
      cout << "Unknown error" << endl;
   }

[Java]

.. code-block:: java

   try {
      someCall ();
   } catch (Exception1 e1) {
      System.out.println ("Error 1");
   } catch (Exception2 e2) {
      System.out.println ("Error 2");
   } catch (Throwable e3) {
      System.out.println ("Unknown error");
   }

Raising and throwing exceptions is permissible in all three languages.

Custom Exceptions
~~~~~~~~~~~~~~~~~~~

Custom exception declarations resemble object declarations, and they can be created in Ada using the :ada:`exception` keyword:

.. code-block:: ada

   My_Exception : exception;

Your exceptions can then be raised using a :ada:`raise` statement, optionally accompanied by a message following the :ada:`with` reserved word:

[Ada]

.. code-block:: ada

   raise My_Exception with "Some message";

[C++]

.. code-block:: cpp

   throw My_Exception ("Some message");

[Java]

.. code-block:: java

   throw new My_Exception ("Some message");

Language defined exceptions can also be raised in the same manner:

.. code-block:: ada

   raise Constraint_Error;
