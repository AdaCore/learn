.. code:: ada compile_button project=Training_Material.Fundamentals_Of_Ada.Program_Structure.private_children
   :class: ada-run

   package Os is
      type File_T is private;
      function Open (Name : String) return File_T;
      procedure Write (File : File_T; Str : String);
      procedure Close (File : File_T);
   private
      type File_T is new Integer;
   end Os;

   private package Os.Uart is
      type Device_T is private;
      function Open (Name : String) return Device_T;
      procedure Write (Device : Device_T; Str : String);
      procedure Close (Device : Device_T);
   private
      type Device_T is new Integer;
   end Os.Uart;

   private with Os.Uart; -- references only in private section
   private package Os.Serial is
      type Comport_T is private;
      procedure Initialize (Comport : in out Comport_T);
   private
      type Comport_T is record
         Device : Uart.Device_T;
      end record;
   end Os.Serial;

   package body Os is
      function Open (Name : String) return File_T is (1);
      procedure Write (File : File_T; Str : String) is null;
      procedure Close (File : File_T) is null;
   end Os;

   package body Os.Uart is
      function Open (Name : String) return Device_T is (1);
      procedure Write (Device : Device_T; Str : String) is null;
      procedure Close (Device : Device_T) is null;
   end Os.Uart;

   package body Os.Serial is
      procedure Initialize (Comport : in out Comport_T) is null;
   end Os.Serial;
