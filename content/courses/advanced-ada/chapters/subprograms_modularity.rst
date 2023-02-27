Subprograms and Modularity
==========================

.. include:: ../../global.txt

.. _Adv_Ada_Private_Subprograms:

Private subprograms
-------------------

We've seen :ref:`previously <Adv_Ada_Private_Packages>` that we can declare
private packages. Because packages and subprograms can both be library units,
we can declare private subprograms as well. We do this by using the
:ada:`private` keyword. For example:

.. code:: ada compile_button project=Courses.Advanced_Ada.Subprograms.Private_Test_Procedure

    private procedure Test;

    procedure Test is
    begin
       null;
    end Test;

Such a subprogram as the one above isn't really useful. For example, we cannot
write a with clause that refers to the :ada:`Test` procedure, as it's not
visible anywhere:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Private_Test_Procedure
    :class: ada-expect-compile-error

    with Test;

    procedure Show_Test is
    begin
       Test;
    end Show_Test;

As expected, since :ada:`Test` is private, we get a compilation error because
this procedure cannot be referred in the :ada:`Show_Test` procedure.

A more useful example is to declare private subprograms of a package. For
example:

.. code:: ada run_button project=Courses.Advanced_Ada.Subprograms.Private_Package_Procedure

    package Data_Processing is

       type Data is private;

       procedure Process (D : in out Data);

    private

        type Data is null record;

    end Data_Processing;

    with Data_Processing.Calculate;

    package body Data_Processing is

       procedure Process (D : in out Data) is
       begin
          Calculate (D);
       end Process;

    end Data_Processing;

    private procedure Data_Processing.Calculate (D : in out Data);

    procedure Data_Processing.Calculate (D : in out Data) is
    begin
       --  Dummy implementation...
       null;
    end Data_Processing.Calculate;

    with Data_Processing; use Data_Processing;

    procedure Test_Data_Processing is
       D : Data;
    begin
       Process (D);
    end Test_Data_Processing;

In this example, we declare :ada:`Calculate` as a private procedure of the
:ada:`Data_Processing` package. Therefore, it's visible in that package (but
not in the :ada:`Test_Data_Processing` procedure).

.. admonition:: In the Ada Reference Manual

    - :arm:`10.1.1 Compilation Units - Library Units <10-1-1>`
    - :arm:`10.1.2 Context Clauses - With Clauses <10-1-2>`
