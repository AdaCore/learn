Appendices
==========
:code-config:`reset_accumulator=True`

Appendix A: Generic Formal Types
--------------------------------

The following tables contain examples of available formal types
for generics:

+-------------------------+--------------------------------------------------------------+-------------------------+
| Formal Type             | Format                                                       | Actual type             |
+=========================+==============================================================+=========================+
| Incomplete type         | :ada:`type T;`                                               | Any type                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete type         | :ada:`type T (<>);`                                          | Any type                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Discrete type           | :ada:`type T is (<>);`                                       | Any integer, modular or |
|                         |                                                              | enumeration type        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Range type              | :ada:`type T is range <>;`                                   | Any signed integer type |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Modular type            | :ada:`type T is mod <>;`                                     | Any modular type        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Floating-point type     | :ada:`type T is digits <>;`                                  | Any floating-point type |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Binary fixed-point type | :ada:`type T is delta <>;`                                   | Any binary fixed-point  |
|                         |                                                              | type                    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Decimal fixed-point     | :ada:`type T is delta <> digits <>;`                         | Any decimal fixed-point |
| type                    |                                                              | type                    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite nonlimited     | :ada:`type T is private;`                                    | Any nonlimited,         |
| private type            |                                                              | definite type           |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite nonlimited   | :ada:`type T (<>) is private;`                               | Any nonlimited type     |
| private type            |                                                              | indefinite or definite  |
|                         |                                                              |                         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Unlimited private type  | :ada:`type T (D : DT) is private;`                           | Any nonlimited type     |
| with discriminant       |                                                              | with discriminant       |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Access type             | :ada:`type A is access T;`                                   | Any access type for     |
|                         |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite derived        | :ada:`type T is new B;`                                      | Any concrete type       |
| type                    |                                                              | derived from base type  |
|                         |                                                              | B                       |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited private type    | :ada:`type T is limited private;`                            | Any definite type,      |
|                         |                                                              | limited or not          |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete tagged       | :ada:`type T is tagged;`                                     | Any concrete, definite, |
| type                    |                                                              | tagged type             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite                | :ada:`type T is tagged private;`                             | Any concrete, definite, |
| tagged private type     |                                                              | tagged type             |
|                         |                                                              |                         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite                | :ada:`type T is tagged limited private;`                     | Any concrete definite   |
| tagged limited private  |                                                              | tagged type, limited or |
| type                    |                                                              | not                     |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract tagged private;`                    | Any nonlimited,         |
| tagged private type     |                                                              | definite tagged type,   |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract tagged limited private;`            | Any definite tagged     |
| tagged limited private  |                                                              | type, limited or not,   |
| type                    |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite derived        | :ada:`type T is new B with private;`                         | Any concrete tagged     |
| tagged type             |                                                              | type derived from base  |
|                         |                                                              | type B                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Definite abstract       | :ada:`type T is abstract new B with private;`                | Any tagged              |
| derived tagged type     |                                                              | type derived from base  |
|                         |                                                              | type B                  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Array type              | :ada:`type A is array (R) of T;`                             | Any array type with     |
|                         |                                                              | range R containing      |
|                         |                                                              | elements of type T      |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Interface type          | :ada:`type T is interface;`                                  | Any interface type T    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited                 | :ada:`type T is limited interface;`                          | Any limited interface   |
| interface type          |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Task interface type     | :ada:`type T is task interface;`                             | Any task interface      |
|                         |                                                              | type T                  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Synchronized interface  | :ada:`type T is synchronized interface;`                     | Any synchronized        |
| type                    |                                                              | interface type T        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Protected interface     | :ada:`type T is protected interface;`                        | Any protected           |
| type                    |                                                              | interface type T        |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Derived interface type  | :ada:`type T is new B and I with private;`                   | Any type T derived from |
|                         |                                                              | base type B and         |
|                         |                                                              | interface I             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Derived type            | :ada:`type T is new B and I1 and I2 with private;`           | Any type T derived from |
| with multiple           |                                                              | base type B and         |
| interfaces              |                                                              | interfaces I1 and I2    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract derived        | :ada:`type T is abstract new B and I with private;`          | Any type T derived from |
| interface type          |                                                              | abstract base type B    |
|                         |                                                              | and interface I         |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Limited derived         | :ada:`type T is limited new B and I with private;`           | Any type T derived from |
| interface type          |                                                              | limited base type B and |
|                         |                                                              | limited interface I     |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract limited        | :ada:`type T is abstract limited new B and I with private;`  | Any type T derived from |
| derived interface type  |                                                              | abstract limited base   |
|                         |                                                              | type B and limited      |
|                         |                                                              | interface I             |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Synchronized interface  | :ada:`type T is synchronized new SI with private;`           | Any type T derived from |
| type                    |                                                              | synchronized interface  |
|                         |                                                              | SI                      |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Abstract synchronized   | :ada:`type T is abstract synchronized new SI with private;`  | Any type T derived from |
| interface type          |                                                              | synchronized interface  |
|                         |                                                              | SI                      |
+-------------------------+--------------------------------------------------------------+-------------------------+

Indefinite version
~~~~~~~~~~~~~~~~~~

Many of the examples above can be used for formal indefinite types:

+-------------------------+--------------------------------------------------------------+-------------------------+
| Formal Type             | Format                                                       | Actual type             |
+=========================+==============================================================+=========================+
| Indefinite limited      | :ada:`type T (<>) is limited private;`                       | Any type, limited or    |
| private type            |                                                              | not, indefinite or      |
|                         |                                                              | definite                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Incomplete indefinite   | :ada:`type T (<>) is tagged;`                                | Any concrete tagged     |
| tagged private type     |                                                              | type,                   |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite              | :ada:`type T (<>) is tagged private;`                        | Any concrete, limited   |
| tagged private type     |                                                              | tagged type,            |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite              | :ada:`type T (<>) is tagged limited private;`                | Any concrete tagged     |
| tagged limited private  |                                                              | type, limited or not,   |
| type                    |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract tagged private;`               | Any nonlimited tagged   |
| tagged private type     |                                                              | type, indefinite or     |
|                         |                                                              | definite, abstract or   |
|                         |                                                              | concrete                |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract tagged limited private;`       | Any tagged type,        |
| tagged limited private  |                                                              | limited or not,         |
| type                    |                                                              | indefinite or definite  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite derived      | :ada:`type T (<>) is new B with private;`                    | Any tagged type derived |
| tagged type             |                                                              | from base type B,       |
|                         |                                                              | indefinite or definite  |
+-------------------------+--------------------------------------------------------------+-------------------------+
| Indefinite abstract     | :ada:`type T (<>) is abstract new B with private;`           | Any tagged type derived |
| derived tagged type     |                                                              | from base type B,       |
|                         |                                                              | indefinite or definite  |
|                         |                                                              | abstract or concrete    |
+-------------------------+--------------------------------------------------------------+-------------------------+

The same examples could also contain discriminants. In this case, :ada:`(<>)`
is replaced by a list of discriminants, e.g.: :ada:`(D: DT)`.

Appendix B: Containers
----------------------

The following table shows all containers available in Ada,
including their versions (standard, bounded, unbounded, indefinite):

+-----------+------------------------------------+-----+---------+-----------+------------+
| Category  | Container                          | Std | Bounded | Unbounded | Indefinite |
+===========+====================================+=====+=========+===========+============+
| Vector    | ``Vectors``                        |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| List      | ``Doubly_Linked_Lists``            |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Hashed_Maps``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Ordered_Maps``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Hashed_Sets``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Ordered_Sets``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Tree      | ``Multiway_Trees``                 |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Generic   | ``Holders``                        |     |         |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized_Queue_Interfaces``  |  Y  |         |           |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized_Queues``            |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Priority_Queues``                |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+

The following table presents the prefixing applied to the container
name that depends on its version. As indicated in the table, the
standard version does not have a prefix associated with it.

+-------------+--------------------------------+
| Version     | Naming prefix                  |
+=============+================================+
| Std         |                                |
+-------------+--------------------------------+
| Bounded     | ``Bounded_``                   |
+-------------+--------------------------------+
| Unbounded   | ``Unbounded_``                 |
+-------------+--------------------------------+
| Indefinite  | ``Indefinite_``                |
+-------------+--------------------------------+
