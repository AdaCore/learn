.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

Engine Test Front-end
=====================
:code-config:`run_button=False;prove_button=False;accumulate_code=False`

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

.. role:: cpp(code)
   :language: c++

This page is used to test the engine (front-end to backend interaction)

.. admonition:: How it works

    The test.js script reads the JSON descriptor listed before each test editor and simulates "pressing" the listed button. It then compares the response from the backend with the known 'test_expects' field. Insert new tests before the Test Results section at the end of the document.

Simple Ada Run Button Test
--------------------------

.. container:: test_descriptor

   .. raw:: html

      <div class="test_name">Simple Ada Run Button Test</div>
      <div class="test_expects"><div class="output_area"><div class="output_info">Run...</div><div class="output_line"></div><div class="output_line"></div><div class="output_line">Hello, World!</div><div class="output_line"></div><div class="output_success">Success!</div></div></div>
      <div class="test_exercises">Run</div>

   .. code:: ada run_button

       with Ada.Text_IO;

       procedure Greet is
       begin
          --  Print "Hello, World!" to the screen
          Ada.Text_IO.Put_Line ("Hello, World!");
       end Greet;

Simple C Run Button Test
------------------------

.. container:: test_descriptor

   .. raw:: html

      <div class="test_name">Simple C Run Button Test</div>
      <div class="test_expects"><div class="output_area"><div class="output_info">Run...</div><div class="output_line"></div><div class="output_line"></div><div class="output_line">Hello, World!</div><div class="output_success">Success!</div></div></div>
      <div class="test_exercises">Run</div>

   .. code:: c run_button

      !main.c
      #include <stdio.h>

      int main()
      {
         printf("Hello, World!");
      }


Accumulated Code Test
---------------------

.. container:: test_descriptor

   .. raw:: html

      <div class="test_name">Accumulated Code Test</div>
      <div class="test_expects"><div class="output_area"><div class="output_info">Run...</div><div class="output_line"></div><div class="output_line"></div><div class="output_line">&lt;Stack, items: [  1  2  3  4]&gt;</div><div class="output_line"></div><div class="output_success">Success!</div></div></div>
      <div class="test_exercises">Run</div>

   :code-config:`reset_accumulator=True;accumulate_code=True`

   .. code:: ada no_button

       package Var_Size_Record_2 is
           type Items_Array is array (Positive range <>) of Integer;

           type Growable_Stack (Max_Len : Natural) is record
           --                   ^ Discriminant. Cannot be modified once initialized.
              Items : Items_Array (1 .. Max_Len);
              Len   : Natural := 0;
           end record;
           --  Growable_Stack is an indefinite type (like an array)
       end Var_Size_Record_2;

   .. code:: ada run_button

       with Var_Size_Record_2; use Var_Size_Record_2;
       with Ada.Text_IO; use Ada.Text_IO;

       procedure Main is
          procedure Print_Stack (G : Growable_Stack) is
          begin
             Put ("<Stack, items: [");
             for I in G.Items'Range loop
                exit when I > G.Len;
                Put (" " & Integer'Image (G.Items (I)));
             end loop;
             Put_Line ("]>");
          end Print_Stack;

          S : Growable_Stack :=
            (Max_Len => 128, Items => (1, 2, 3, 4, others => <>), Len => 4);
       begin
          Print_Stack (S);
       end Main;

   :code-config:`reset_accumulator=True;accumulate_code=False`

Examine SPARK Test
------------------

.. container:: test_descriptor

   .. raw:: html

      <div class="test_name">Examine SPARK Test</div>
      <div class="test_expects"><div class="output_area"><div class="output_info">Examine...</div><div class="output_line"></div><div class="output_line"></div><div class="output_line">Phase 1 of 2: generation of Global contracts ...</div><div class="output_line"></div><div class="output_line">Phase 2 of 2: analysis of data and information flow ...</div><div class="output_line"></div><div class="output_msg">show_uninitialized.adb:7:21: warning: "Max" may be referenced before it has a value</div><div class="output_line"></div><div class="output_msg">show_uninitialized.adb:7:21: medium: "Max" might not be initialized</div><div class="output_line"></div><div class="output_msg">show_uninitialized.adb:11:14: medium: "Max" might not be initialized</div><div class="output_line"></div><div class="output_line">gnatprove: unproved check messages considered as errors</div><div class="output_line"></div><div class="output_line"></div><div class="output_info">3 errors.</div></div></div>
      <div class="test_exercises">Examine</div>

   .. code:: ada prove_flow_button

       package Show_Uninitialized is

          type Array_Of_Naturals is array (Integer range <>) of Natural;

          function Max_Array (A : Array_Of_Naturals) return Natural;

       end Show_Uninitialized;

       package body Show_Uninitialized is

          function Max_Array (A : Array_Of_Naturals) return Natural is
             Max : Natural;
          begin
             for I in A'Range loop
                if A (I) > Max then -- Here Max may not be initialized
                   Max := A (I);
                end if;
             end loop;
             return Max;
          end Max_Array;

       end Show_Uninitialized;

Prove SPARK Test
----------------

.. container:: test_descriptor

   .. raw:: html

      <div class="test_name">Prove SPARK Test</div>
      <div class="test_expects"><div class="output_area"><div class="output_info">Prove...</div><div class="output_line"></div><div class="output_line"></div><div class="output_line">Phase 1 of 2: generation of Global contracts ...</div><div class="output_line"></div><div class="output_line">Phase 2 of 2: flow analysis and proof ...</div><div class="output_line"></div><div class="output_msg">show_runtime_errors.adb:5:12: medium: overflow check might fail (e.g. when I = -2147483648 and J = -1)</div><div class="output_line"></div><div class="output_msg">show_runtime_errors.adb:5:12: medium: array index check might fail (e.g. when A'First = 1)</div><div class="output_line"></div><div class="output_msg">show_runtime_errors.adb:5:22: medium: range check might fail</div><div class="output_line"></div><div class="output_msg">show_runtime_errors.adb:5:22: medium: overflow check might fail (e.g. when P = -1 and Q = 0)</div><div class="output_line"></div><div class="output_msg">show_runtime_errors.adb:5:22: medium: divide by zero might fail (e.g. when Q = 0)</div><div class="output_line"></div><div class="output_line">gnatprove: unproved check messages considered as errors</div><div class="output_line"></div><div class="output_line"></div><div class="output_info">5 errors.</div></div></div>
      <div class="test_exercises">Prove</div>

   .. code:: ada prove_button

       package Show_Runtime_Errors is

          type Nat_Array is array (Integer range <>) of Natural;

          procedure Update (A : in out Nat_Array; I, J, P, Q : Integer);

       end Show_Runtime_Errors;

       package body Show_Runtime_Errors is

          procedure Update (A : in out Nat_Array; I, J, P, Q : Integer) is
          begin
             A (I + J) := P / Q;
          end Update;

       end Show_Runtime_Errors;

-----------------

Test Results
------------

.. container:: test_results

    placeholder text
