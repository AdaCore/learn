Lesson 4: 10 Bouncing Balls
=====================================================================

.. role:: ada(code)
   :language: ada

Welcome to the AdaCore University!

In this lesson, we are going to look into a program that manipulates ten balls on the screen and makes them bounce on the edges of the screen. Let's get started!


Geometry
---------------------------------------------------------------------

The mathematics that we're going to use here to describe the movement of the balls is different from what we've done before. We're not going to use cosines and sines, but a vector of direction :math:`(Dx, Dy)`, which will represent the direction in the abscissa axis and ordinate axis. At each iteration, each ball is going to move following this :math:`(Dx, Dy)` vector:

.. image:: sdl_app_l4-01.png


Source-code
---------------------------------------------------------------------

The program that is going to implement this behavior is slightly more complicated than the previous one:

.. code:: ada

   with Display;                   use Display;
   with Display.Basic;             use Display.Basic;
   with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

   procedure Main is
      type Ball_Type is record
         Shape  : Shape_Id;
         Dx, Dy : Float;
      end record;

      type Ball_Array is array (Integer range <>) of Ball_Type;

      Seed : Generator;

      Balls : Ball_Array (1 .. 10) :=
      (others =>
         (Shape => New_Circle (0.0, 0.0, 10.0, Blue),
          Dx    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0),
          Dy    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0)));

      procedure Iterate (V : in out Ball_Type) is
      begin
         if Get_X (V.Shape) not in -100.0 .. 100.0 then
            V.Dx := -V.Dx;
         end if;

         if Get_Y (V.Shape) not in -100.0 .. 100.0 then
            V.Dy := -V.Dy;
         end if;

         Set_X (V.Shape, Get_X (V.Shape) + V.Dx);
         Set_Y (V.Shape, Get_Y (V.Shape) + V.Dy);
      end Iterate;

   begin
      loop
         for B of Balls loop
            Iterate (B);
         end loop;

         delay 0.001;
      end loop;
   end Main;

This is mainly for two reasons. First of all, we'll have to manipulate a collection of objects on the screen, so we will need a way to implement this collection. The other reason is that there is some kind of randomization that is going to be required here to have 10 balls moving into 10 different directions.

If we run the application, we can see those 10 balls bouncing on the edges of the screen. As a matter of fact, for a window that is not an exact square, if we look closely, the balls don't quite seem to be bouncing on the left or the right side of the screen. That's because we're considering a system that is a square going from -100 to 100 in each ordinate. If the the window is a square, however, the resulting movement will look much better. On the next sections, we will see how to program this application.


Bouncing balls
---------------------------------------------------------------------

As in the previous example, the movement of the objects is going to be taken care of by a procedure called ``Iterate``. You'll see the same kind of constructions here, in particular, the :ada:`in out` parameter:

.. code:: ada

      procedure Iterate (V : in out Ball_Type) is

However, in this case, the semantics are going to be different, since we're not rotating, but translating the objects. If we get out of the boundaries of the screen, we want to reverse the direction of the movement. That is what is done with the two conditions:

.. code:: ada

         if Get_X (V.Shape) not in -100.0 .. 100.0 then
            V.Dx := -V.Dx;
         end if;

         if Get_Y (V.Shape) not in -100.0 .. 100.0 then
            V.Dy := -V.Dy;
         end if;

As you can see, we're extracting the value of ``x`` axis of the shape, then we're comparing it to the range. There are two new Ada constructions here:

- the :ada:`not in` operator, which checks if a given value is not in the range
   - (as you may imagine, there is a :ada:`in` operator as well, which checks that a value is in the range).
- and a way to denote a range, which is ``number`` (or lower bound), followed by :ada:`..` and then upper bound.

Once we have ``Dx`` and ``Dy``, the movement operation is pretty simple:

.. code:: ada

         Set_X (V.Shape, Get_X (V.Shape) + V.Dx);
         Set_Y (V.Shape, Get_Y (V.Shape) + V.Dy);

We're just retrieving the current X or Y of the shape, and then adding the value of ``Dx`` or ``Dy`` to it.


Randomization
---------------------------------------------------------------------

We're now going to do something more complicated, which is random number generation. As a matter of fact, if you're already accustomed to doing that in other languages, such as C, C++ or Java, it will be very similar. We want to construct a random number and, for doing so, we are going to use a function that returns a value from 0 to 1, and then manipulate this value to create something that looks like our expectation. In Ada, there is a standard library that provides such a function, which is called ``Ada.Numerics.Float_Random``. This is why we're adding a dependency on our program using these :ada:`with` and :ada:`use` clauses:

.. code:: ada

   with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

The next step is to create a seed that will be used to generate those numbers:

.. code:: ada

      Seed : Generator;

The seed can be used to create pseudo-random numbers --- for example, initializing it according to the clock, or player input, or anything. However, in this case, we're keeping it simple, so the seed is just keeping its default value, which will mean that the numbers that are generated by this seed are always going to be the same ones.

We're now getting into the business of creating a random value within a certain range. The first thing we're getting here is a random value between 0.0 and 1.0:

.. code:: ada

            Random (Seed)

But that's not enough: we want something smaller, and we something that can be potentially positive or negative. So let's carry on. To get it smaller, we're multiplying it by a small number (0.05, for example) in order to construct a value between 0 and 0.05. At the same time, we don't want a value that is too small (that is to say too close to zero), so we're adding 0.02 in order to construct a value that is between 0.02 and 0.07:

.. code:: ada

            (Random (Seed) * 0.05 + 0.02)

We now want this number to be either negative or positive. So we're computing a new random number between 0.0 and 1.0, and we're saying that if this number is above 0.5, which is half the cases, then we're evaluating 1.0, otherwise we're evaluating -1.0. This subexpression value is either -1.0 or 1.0:

.. code:: ada

            (if Random (Seed) > 0.5 then 1.0 else -1.0)

Multiplying those two values:

.. code:: ada

            Dx    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0),

gives us the desired effect, that is to say a value that is either between -0.07 and -0.02 or between 0.02 and 0.07. We've so created our random number.


Array type
---------------------------------------------------------------------

Let's now look how to handle collections of objects. Ada provides various ways to handle collections: lists, maps, stacks, etc. But the basic element to handle a collection of objects is an array. That is what we're going to see here.

As you may remember from one of the previous lessons, we've mentioned the fact that Ada is a strongly typed language. One of the effects of that it is not possible to just create an array at the time of variable declaration. An array needs to be typed, so an array type needs to be declared beforehand. That's what we're doing here with this type Ball_Array:

.. code:: ada

      type Ball_Array is array (Integer range <>) of Ball_Type;

One of the things that needs to be specified on this type is the type which is going to be used for the indexing. In Ada, any discrete type can be used for the indexing:

.. code:: ada

      (Integer range <>)

So that includes of course integers (as in the line above), but enumerations as well. For example, it would be possible to index an array on the character type, or Boolean type. There are two main categories of arrays in Ada:

- those that have a size defined at the type declaration time, and
- those that don't, and need a size to be defined at the variable declaration time.

When we say "size", what we actually mean is "boundaries." We will see that later on.

If we write:

.. code:: ada

      range <>

what we mean is that the boundaries of the array are not known at the type declaration time, and they will have to be specified for each instance ---for each object or variable--- individually.

Finally, the last information that needs to be defined on an array type is the type of its content:

.. code:: ada

      type ... of Ball_Type;

One important point to understand ---in particular if you're coming from reference-based languages, such as Java--- is that, in Ada, all components of the array need to have the same size. But here, it's fine: ``Ball_Type`` is a record with three fields, so all objects of ``Ball_Type`` are of the same size, and we can use that for the array elements.

Once the type is declared, we can use it in an array declaration, such as this one:

.. code:: ada

      Balls : Ball_Array (1 .. 10);

Because the type isn't constrained, we need to provide a size for the array at variable declaration time. Again, we're not providing the actual size. Instead, we are providing the boundaries: the inclusive lower bound and upper bound. So this array (``Balls``) is indexed between 1 and 10, so it contains 10 elements. We could also have decided to index it from 0 to 9, or from 100 to 109: this would have given the same size, but different indices. There is no requirement in Ada to index an array from a specific number.


Array aggregate
---------------------------------------------------------------------

The next step is to initialize all the 10 elements of the array. For this, we're going to use a structure that is very close to the one we've used before with the record types, which is an aggregate:

.. code:: ada

      Balls : Ball_Array (1 .. 10) :=
      (others =>
         (Shape => New_Circle (0.0, 0.0, 10.0, Blue),
          Dx    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0),
          Dy    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0)));

With the :ada:`others` reserved word in the aggregate, we're specifying that the initialization expression we're going to provide is going to be the same expression for every single element of the array:

.. code:: ada

      (others => ... )

There are ways to be more specific about how to initialize these things, but we will see that in a further lesson.

In this very case, the expression that is being used to initialize each element is itself an aggregate:

.. code:: ada

          (Shape => New_Circle (0.0, 0.0, 10.0, Blue),
           Dx    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0),
           Dy    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0))

So, if you will, this is a record aggregate nested within an array aggregate.

One important thing to realize here is that what we're saying is that the same expression is going to be used by all the elements, not the same value:

.. code:: ada

           Dx    => (Random (Seed) * 0.05 + 0.02) * (if Random (Seed) > 0.5 then 1.0 else -1.0),

In other words, this expression is going to be evaluated for each single element: ``Random`` is going to be called for each single element --- 40 times overall, since we call ``Random`` four times per element. Therefore, this will create 10 different objects, each of them going into a separate direction.


For loop
---------------------------------------------------------------------

The last step of this program is to call ``Iterate`` for every single ball at each cycle. In order to iterate through all elements of the ``Balls``, we're going to use the :ada:`for ... of` loop:

.. code:: ada

         for B of Balls loop
            Iterate (B);
         end loop;

By saying this, we'll iterate over every element of ``Balls`` and store each successive value into the variable ``B``.
