Standard library: Files and streams
===================================

.. include:: ../../../global.txt

Ada provides different approaches for file input/output (I/O):

- *Text I/O*, which supports file I/O in text format, including the display of
  information on the console.

- *Sequential I/O*, which supports file I/O in binary format written in a
  sequential fashion for a specific data type.

- *Direct I/O*, which supports file I/O in binary format for a specific data
  type, but also supporting access to any position of a file.

- *Stream I/O*, which supports I/O of information for multiple data types,
  including objects of unbounded types, using files in binary format.

This table presents a summary of the features we've just seen:

+----------------+------------+------------+----------------+
| File I/O       | Format     | Random     | Data types     |
| option         |            | access     |                |
+================+============+============+================+
| Text I/O       | text       |            | string type    |
+----------------+------------+------------+----------------+
| Sequential I/O | binary     |            | single type    |
+----------------+------------+------------+----------------+
| Direct I/O     | binary     | |check|    | single type    |
+----------------+------------+------------+----------------+
| Stream I/O     | binary     | |check|    | multiple types |
+----------------+------------+------------+----------------+

In the following sections, we discuss details about these I/O approaches.

Text I/O
--------

In most parts of this course, we used the :ada:`Put_Line` procedure to display
information on the console. However, this procedure also accepts a
:ada:`File_Type` parameter. For example, you can select between standard
output and standard error by setting this parameter explicitly:

.. code:: ada run_button project=Courses.Intro_To_Ada.Standard_Library.Show_Std_Text_Out

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Std_Text_Out is
    begin
       Put_Line (Standard_Output, "Hello World #1");
       Put_Line (Standard_Error,  "Hello World #2");
    end Show_Std_Text_Out;

You can also use this parameter to write information to any text file.  To
create a new file for writing, use the :ada:`Create` procedure, which
initializes a :ada:`File_Type` element that you can later pass to :ada:`Put_Line`
(instead of, e.g., :ada:`Standard_Output`). After you finish writing
information, you can close the file by calling the :ada:`Close` procedure.

You use a similar method to read information from a text file.  However,
when opening the file, you must specify that it's an input file
(:ada:`In_File`) instead of an output file. Also, instead of calling the
:ada:`Put_Line` procedure, you call the :ada:`Get_Line` function to read
information from the file.

Let's see an example that writes information into a new text file and then
reads it back from the same file:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Simple_Text_File_IO
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Text_File_IO is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Put_Line (F, "Hello World #2");
       Put_Line (F, "Hello World #3");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Simple_Text_File_IO;

In addition to the :ada:`Create` and :ada:`Close` procedures, the standard
library also includes a :ada:`Reset` procedure, which, as the name implies,
resets (erases) all the information from the file. For example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Text_File_Reset
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Reset is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Reset (F);
       Put_Line (F, "Hello World #2");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Text_File_Reset;

By running this program, we notice that, although we've written the first
string (:ada:`"Hello World #1"`) to the file, it has been erased because of the
call to :ada:`Reset`.

In addition to opening a file for reading or writing, you can also open an
existing file and append to it.  Do this by calling the :ada:`Open` procedure
with the :ada:`Append_File` option.

When calling the :ada:`Open` procedure, an exception is raised if the
specified file isn't found.  Therefore, you should handle exceptions in
that context. The following example deletes a file and then tries to open
the same file for reading:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Text_File_Input_Except
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Input_Except is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       --  Open output file and delete it
       Create (F, Out_File, File_Name);
       Delete (F);

       --  Try to open deleted file
       Open (F, In_File, File_Name);
       Close (F);
    exception
       when Name_Error =>
          Put_Line ("File does not exist");
       when others =>
          Put_Line
            ("Error while processing input file");
    end Show_Text_File_Input_Except;

In this example, we create the file by calling :ada:`Create` and then
delete it by calling :ada:`Delete`. After the call to :ada:`Delete`, we can
no longer use the :ada:`File_Type` element. After deleting the file, we
try to open the non-existent file, which raises a :ada:`Name_Error`
exception.

Sequential I/O
--------------

The previous section presented details about text file I/O. Here, we
discuss doing file I/O in binary format. The first package we'll explore is
the :ada:`Ada.Sequential_IO` package. Because this package is a generic
package, you need to instantiate it for the data type you want to use for
file I/O. Once you've done that, you can use the same procedures we've seen
in the previous section: :ada:`Create`, :ada:`Open`, :ada:`Close`, :ada:`Reset` and
:ada:`Delete`. However, instead of calling the :ada:`Get_Line` and :ada:`Put_Line`
procedures, you'd call the :ada:`Read` and :ada:`Write` procedures.

In the following example, we instantiate the :ada:`Ada.Sequential_IO`
package for floating-point types:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Seq_Float_IO
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Float_IO is
       package Float_IO is
         new Ada.Sequential_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String :=
                     "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line
               (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Seq_Float_IO;

We use the same approach to read and write complex information. The
following example uses a record that includes a Boolean and a
floating-point value:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Seq_Rec_IO
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Rec_IO is
       type Num_Info is record
          Valid : Boolean := False;
          Value : Float;
       end record;

       procedure Put_Line (N : Num_Info) is
       begin
          if N.Valid then
             Ada.Text_IO.Put_Line
               ("(ok,     "
                & Float'Image (N.Value) & ")");
          else
             Ada.Text_IO.Put_Line
               ("(not ok,  -----------)");
          end if;
       end Put_Line;

       package Num_Info_IO is new
         Ada.Sequential_IO (Num_Info);
       use Num_Info_IO;

       F         : Num_Info_IO.File_Type;
       File_Name : constant String :=
                     "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  (True,  1.5));
       Write (F,  (False, 2.4));
       Write (F,  (True,  6.7));
       Close (F);

       declare
          Value : Num_Info;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Put_Line (Value);
          end loop;
          Close (F);
       end;
    end Show_Seq_Rec_IO;

As the example shows, we can use the same approach we used for
floating-point types to perform file I/O for this record. Once we
instantiate the :ada:`Ada.Sequential_IO` package for the record type, file
I/O operations are performed the same way.

Direct I/O
----------

Direct I/O is available in the :ada:`Ada.Direct_IO` package. This mechanism
is similar to the sequential I/O approach just presented, but allows us to
access any position in the file. The package instantiation and most
operations are very similar to sequential I/O.  To rewrite the
:ada:`Show_Seq_Float_IO` application presented in the previous section to use
the :ada:`Ada.Direct_IO` package, we just need to replace the instances of
the :ada:`Ada.Sequential_IO` package by the :ada:`Ada.Direct_IO`
package. This is the new source code:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Dir_Float_IO
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_IO is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String :=
                     "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line
               (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_IO;

Unlike sequential I/O, direct I/O allows you to access any position in
the file. However, it doesn't offer an option to append information to
a file. Instead, it provides an :ada:`Inout_File` mode allowing reading
and writing to a file via the same :ada:`File_Type` element.

To access any position in the file, call the :ada:`Set_Index` procedure to set
the new position / index.  You can use the :ada:`Index` function to retrieve
the current index. Let's see an example:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Dir_Float_In_Out_File
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_In_Out_File is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String :=
                     "float_file.bin";
    begin
       --  Open file for input / output
       Create (F, Inout_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);

       --  Set index to previous position
       --  and overwrite value
       Set_Index (F, Index (F) - 1);
       Write (F,  7.7);

       declare
          Value : Float;
       begin
          --  Set index to start of file
          Set_Index (F, 1);

          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line
               (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_In_Out_File;

By running this example, we see that the file contains ``7.7``, rather than
the previous ``6.7`` that we wrote.  We overwrote the value by changing the
index to the previous position before doing another write.

In this example we used the :ada:`Inout_File` mode. Using that mode, we just
changed the index back to the initial position before reading from the file
(:ada:`Set_Index (F, 1)`) instead of closing the file and reopening it for
reading.

Stream I/O
----------

All the previous approaches for file I/O in binary format (sequential and
direct I/O) are specific for a single data type (the one we instantiate
them with).  You can use these approaches to write objects of a single data
type that may be an array or record (potentially with many fields), but if
you need to create and process files that include different data types, or
any objects of an unbounded type, these approaches are not
sufficient. Instead, you should use stream I/O.

Stream I/O shares some similarities with the previous approaches.  We still
use the :ada:`Create`, :ada:`Open` and :ada:`Close` procedures. However, instead of
accessing the file directly via a :ada:`File_Type` element, you use a
:ada:`Stream_Access` element. To read and write information, you use the
:ada:`'Read` or :ada:`'Write` attributes of the data types you're reading
or writing.

Let's look at a version of the :ada:`Show_Dir_Float_IO` procedure from the
previous section that makes use of stream I/O instead of direct I/O:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_Float_Stream
    :class: ada-run

    with Ada.Text_IO;

    with Ada.Streams.Stream_IO;
    use  Ada.Streams.Stream_IO;

    procedure Show_Float_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String :=
                     "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Float'Write (S, 1.5);
       Float'Write (S, 2.4);
       Float'Write (S, 6.7);

       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          S := Stream (F);

          while not End_Of_File (F) loop
             Float'Read (S, Value);
             Ada.Text_IO.Put_Line
               (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Float_Stream;

After the call to :ada:`Create`, we retrieve the corresponding
:ada:`Stream_Access` element by calling the :ada:`Stream` function. We then
use this stream to write information to the file via the :ada:`'Write`
attribute of the :ada:`Float` type. After closing the file and
reopening it for reading, we again retrieve the corresponding
:ada:`Stream_Access` element and processed to read information from the
file via the :ada:`'Read` attribute of the :ada:`Float` type.

You can use streams to create and process files containing different data
types within the same file.  You can also read and write unbounded data
types such as strings. However, when using unbounded data types you must
call the :ada:`'Input` and :ada:`'Output` attributes of the unbounded data
type: these attributes write information about bounds or discriminants in
addition to the object's actual data.

The following example shows file I/O that mixes both strings of
different lengths and floating-point values:

.. code:: ada no_button project=Courses.Intro_To_Ada.Standard_Library.Show_String_Stream
    :class: ada-run

    with Ada.Text_IO;

    with Ada.Streams.Stream_IO;
    use  Ada.Streams.Stream_IO;

    procedure Show_String_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String :=
                     "float_file.bin";

       procedure Output (S  : Stream_Access;
                         FV : Float;
                         SV : String) is
       begin
          String'Output (S, SV);
          Float'Output (S,  FV);
       end Output;

       procedure Input_Display (S : Stream_Access) is
          SV : String := String'Input (S);
          FV : Float  := Float'Input (S);
       begin
          Ada.Text_IO.Put_Line (Float'Image (FV)
                                & " --- " & SV);
       end Input_Display;

    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Output (S, 1.5, "Hi!!");
       Output (S, 2.4, "Hello world!");
       Output (S, 6.7, "Something longer here...");

       Close (F);

       Open (F, In_File, File_Name);
       S := Stream (F);

       while not End_Of_File (F) loop
          Input_Display (S);
       end loop;
       Close (F);

    end Show_String_Stream;

When you use Stream I/O, no information is written into the file indicating
the type of the data that you wrote.  If a file contains data from
different types, you must reference types in the same order when reading a
file as when you wrote it. If not, the information you get will be
corrupted. Unfortunately, strong data typing doesn't help you in this
case. Writing simple procedures for file I/O (as in the example above) may
help ensuring that the file format is consistent.

Like direct I/O, stream I/O support also allows you to access any location
in the file. However, when doing so, you need to be extremely careful that
the position of the new index is consistent with the data types you're
expecting.
