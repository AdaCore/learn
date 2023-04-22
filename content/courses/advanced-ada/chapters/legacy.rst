Legacy features
===============

.. include:: ../../global.txt

Nested packages
---------------

Nested packages, as the name suggests, are declared within a parent
package. This contrasts with child packages, which are declared independently.
For example, this would be a nested package for the :ada:`Week` package:

.. code:: ada no_button project=Courses.Advanced_Ada.Packages.Nested_Packages

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

       package Nested is

          function Get_First_Of_Week return String;

       end Nested;

    end Week;

In contrast to child packages, we don't write
:ada:`package body Week.Nested is ...` to implement the package body of
:ada:`Nested`. Instead, the package body of :ada:`Nested` is *nested* in
the package body of the parent package :ada:`Week`:

.. code:: ada no_button project=Courses.Advanced_Ada.Packages.Nested_Packages

    package body Week is

       package body Nested is

          function Get_First_Of_Week return String is
          begin
             return Mon;
          end Get_First_Of_Week;

       end Nested;

    end Week;

We can now use elements from :ada:`Week.Nested` in a test application:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Nested_Packages
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;

    procedure Main is
       use Week.Nested;
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
    end Main;

Note that we cannot access the :ada:`Week.Nested` package directly using
:ada:`with Week.Nested` because :ada:`Nested` is actually part of :ada:`Week`,
not a child package. We can, however, still write :ada:`use Week.Nested`
|mdash| as we did in the example above.

Visibility
~~~~~~~~~~

Let's now discuss visibility of nested packages. Because the body of nested
packages is part of the body of their parent, nested packages have the same
visibility as their parent package. Let's rewrite the previous example using
nested packages to illustrate this:

.. code:: ada no_button project=Courses.Advanced_Ada.Packages.Visibility

    package Book is

       Title : constant String :=
                 "Visible for my children";

       function Get_Title return String;

       function Get_Author return String;

       package Additional_Operations is

          function Get_Extended_Title return String;

          function Get_Extended_Author return String;

       end Additional_Operations;

    end Book;

Now, because :ada:`Author` is declared before the body of the nested package
:ada:`Additional_Operations`, we can use it in the implementation of the
:ada:`Get_Extended_Author` function:

.. code:: ada no_button project=Courses.Advanced_Ada.Packages.Visibility

    package body Book is

       Author : constant String :=
                 "Author not visible for my children";

       function Get_Title return String is
       begin
          return Title;
       end Get_Title;

       function Get_Author return String is
       begin
          return Author;
       end Get_Author;

       package body Additional_Operations is

          function Get_Extended_Title return String
          is
          begin
             return "Book Title: " & Title;
          end Get_Extended_Title;

          function Get_Extended_Author return String
          is
          begin
             return "Book Author: " & Author;
          end Get_Extended_Author;

       end Additional_Operations;

    end Book;

This is the test application in this case:

.. code:: ada run_button project=Courses.Advanced_Ada.Packages.Visibility
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Book;

    procedure Main is
       use Book.Additional_Operations;
    begin
       Put_Line (Get_Extended_Title);
       Put_Line (Get_Extended_Author);
    end Main;


:ada:`separate` compilation
---------------------------

.. admonition:: Relevant topics

    - :arm:`Subunits of Compilation Units <10-1-3>`

.. todo::

    Complete section!
