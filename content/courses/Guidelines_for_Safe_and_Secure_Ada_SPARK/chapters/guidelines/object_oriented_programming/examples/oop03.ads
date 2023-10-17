package Oop03 is

   package Noncompliant is
      type Shape_T is tagged private;
      procedure Set_Name (Shape : Shape_T; Name  : String);
      function Get_Name (Shape : Shape_T) return String;

      type Quadrilateral_T is new Shape_T with private;
      type Trapezoid_T is new Quadrilateral_T with private;
      type Parallelogram_T is new Trapezoid_T with private;
      type Rectangle_T is new Parallelogram_T with private;
      type Square_T is new Rectangle_T with private;

   private

      type Shape_T is tagged null record;
      type Quadrilateral_T is new Shape_T with null record;
      type Trapezoid_T is new Quadrilateral_T with null record;
      type Parallelogram_T is new Trapezoid_T with null record;
      type Rectangle_T is new Parallelogram_T with null record;
      type Square_T is new Rectangle_T with null record;

      procedure Set_Name (Shape : Shape_T; Name  : String) is null;
      function Get_Name (Shape : Shape_T) return String is ("");
   end Noncompliant;

   package Compliant is
      type Shape_T is tagged private;
      procedure Set_Name (Shape : Shape_T; Name  : String);
      function Get_Name (Shape : Shape_T) return String;

      type Quadrilateral_T is new Shape_T with private;
      type Rectangle_T is new Quadrilateral_T with private;
      type Square_T is new Rectangle_T with private;

   private

      type Shape_T is tagged null record;
      type Quadrilateral_T is new Shape_T with null record;
      type Rectangle_T is new Quadrilateral_T with null record;
      type Square_T is new Rectangle_T with null record;

      procedure Set_Name (Shape : Shape_T; Name  : String) is null;
      function Get_Name (Shape : Shape_T) return String is ("");
   end Compliant;

end Oop03;
