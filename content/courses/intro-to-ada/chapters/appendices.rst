:next_state: False

Appendices
==========

.. include:: ../../../global.txt

.. _Intro_Ada_Generic_Formal_Types:

Appendix A: Generic Formal Types
--------------------------------

The following tables contain examples of available formal types
for generics:

+--------------------------------------------------------------+-------------------------+
| Formal type                                                  | Actual type             |
+==============================================================+=========================+
| Incomplete type                                              | Any type                |
|                                                              |                         |
| **Format**: :ada:`type T;`                                   |                         |
+--------------------------------------------------------------+-------------------------+
| Discrete type                                                | Any integer, modular or |
|                                                              | enumeration type        |
| **Format**: :ada:`type T is (<>);`                           |                         |
+--------------------------------------------------------------+-------------------------+
| Range type                                                   | Any signed integer type |
|                                                              |                         |
| **Format**: :ada:`type T is range <>;`                       |                         |
+--------------------------------------------------------------+-------------------------+
| Modular type                                                 | Any modular type        |
|                                                              |                         |
| **Format**: :ada:`type T is mod <>;`                         |                         |
+--------------------------------------------------------------+-------------------------+
| Floating-point type                                          | Any floating-point type |
|                                                              |                         |
| **Format**: :ada:`type T is digits <>;`                      |                         |
+--------------------------------------------------------------+-------------------------+
| Binary fixed-point type                                      | Any binary fixed-point  |
|                                                              | type                    |
| **Format**: :ada:`type T is delta <>;`                       |                         |
+--------------------------------------------------------------+-------------------------+
| Decimal fixed-point type                                     | Any decimal fixed-point |
|                                                              | type                    |
| **Format**: :ada:`type T is delta <> digits <>;`             |                         |
+--------------------------------------------------------------+-------------------------+
| Definite nonlimited private type                             | Any nonlimited,         |
|                                                              | definite type           |
| **Format**: :ada:`type T is private;`                        |                         |
+--------------------------------------------------------------+-------------------------+
| Nonlimited Private type with discriminant                    | Any nonlimited type     |
|                                                              | with discriminant       |
| **Format**: :ada:`type T (D : DT) is private;`               |                         |
+--------------------------------------------------------------+-------------------------+
| Access type                                                  | Any access type for     |
|                                                              | type T                  |
| **Format**: :ada:`type A is access T;`                       |                         |
+--------------------------------------------------------------+-------------------------+
| Definite derived type                                        | Any concrete type       |
|                                                              | derived from base type  |
| **Format**: :ada:`type T is new B;`                          | B                       |
+--------------------------------------------------------------+-------------------------+
| Limited private type                                         | Any definite type,      |
|                                                              | limited or not          |
| **Format**: :ada:`type T is limited private;`                |                         |
+--------------------------------------------------------------+-------------------------+
| Incomplete tagged type                                       | Any concrete, definite, |
|                                                              | tagged type             |
| **Format**: :ada:`type T is tagged;`                         |                         |
+--------------------------------------------------------------+-------------------------+
| Definite tagged private type                                 | Any concrete, definite, |
|                                                              | tagged type             |
| **Format**: :ada:`type T is tagged private;`                 |                         |
+--------------------------------------------------------------+-------------------------+
| Definite tagged limited private type                         | Any concrete definite   |
|                                                              | tagged type, limited or |
| **Format**: :ada:`type T is tagged limited private;`         | not                     |
+--------------------------------------------------------------+-------------------------+
| Definite abstract tagged private type                        | Any nonlimited,         |
|                                                              | definite tagged type,   |
| **Format**: :ada:`type T is abstract tagged private;`        | abstract or concrete    |
+--------------------------------------------------------------+-------------------------+
| Definite abstract tagged limited private type                | Any definite tagged     |
|                                                              | type, limited or not,   |
| **Format**:                                                  | abstract or concrete    |
| :ada:`type T is abstract tagged limited private;`            |                         |
+--------------------------------------------------------------+-------------------------+
| Definite derived tagged type                                 | Any concrete tagged     |
|                                                              | type derived from base  |
| **Format**: :ada:`type T is new B with private;`             | type B                  |
+--------------------------------------------------------------+-------------------------+
| Definite abstract derived tagged type                        | Any tagged type derived |
|                                                              | from base type B        |
| **Format**: :ada:`type T is abstract new B with private;`    | abstract or concrete    |
+--------------------------------------------------------------+-------------------------+
| Array type                                                   | Any array type with     |
|                                                              | range R containing      |
| **Format**: :ada:`type A is array (R) of T;`                 | elements of type T      |
+--------------------------------------------------------------+-------------------------+
| Interface type                                               | Any interface type T    |
|                                                              |                         |
| **Format**: :ada:`type T is interface;`                      |                         |
+--------------------------------------------------------------+-------------------------+
| Limited interface type                                       | Any limited interface   |
|                                                              | type T                  |
| **Format**: :ada:`type T is limited interface;`              |                         |
+--------------------------------------------------------------+-------------------------+
| Task interface type                                          | Any task interface      |
|                                                              | type T                  |
| **Format**: :ada:`type T is task interface;`                 |                         |
+--------------------------------------------------------------+-------------------------+
| Synchronized interface type                                  | Any synchronized        |
|                                                              | interface type T        |
| **Format**: :ada:`type T is synchronized interface;`         |                         |
+--------------------------------------------------------------+-------------------------+
| Protected interface type                                     | Any protected           |
|                                                              | interface type T        |
| **Format**: :ada:`type T is protected interface;`            |                         |
+--------------------------------------------------------------+-------------------------+
| Derived interface type                                       | Any type T derived from |
|                                                              | base type B and         |
| **Format**: :ada:`type T is new B and I with private;`       | interface I             |
+--------------------------------------------------------------+-------------------------+
| Derived type with multiple interfaces                        | Any type T derived from |
|                                                              | base type B and         |
| **Format**:                                                  | interfaces I1 and I2    |
| :ada:`type T is new B and I1 and I2 with private;`           |                         |
+--------------------------------------------------------------+-------------------------+
| Abstract derived interface type                              | Any type T derived from |
|                                                              | abstract base type B    |
| **Format**:                                                  | and interface I         |
| :ada:`type T is abstract new B and I with private;`          |                         |
+--------------------------------------------------------------+-------------------------+
| Limited derived interface type                               | Any type T derived from |
|                                                              | limited base type B and |
| **Format**:                                                  | limited interface I     |
| :ada:`type T is limited new B and I with private;`           |                         |
+--------------------------------------------------------------+-------------------------+
| Abstract limited derived interface type                      | Any type T derived from |
|                                                              | abstract limited base   |
| **Format**:                                                  | type B and limited      |
| :ada:`type T is abstract limited new B and I with private;`  | interface I             |
+--------------------------------------------------------------+-------------------------+
| Synchronized interface type                                  | Any type T derived from |
|                                                              | synchronized interface  |
| **Format**:                                                  | SI                      |
| :ada:`type T is synchronized new SI with private;`           |                         |
+--------------------------------------------------------------+-------------------------+
| Abstract synchronized interface type                         | Any type T derived from |
|                                                              | synchronized interface  |
| **Format**:                                                  | SI                      |
| :ada:`type T is abstract synchronized new SI with private;`  |                         |
+--------------------------------------------------------------+-------------------------+

Indefinite version
~~~~~~~~~~~~~~~~~~

Many of the examples above can be used for formal indefinite types:

+--------------------------------------------------------------+-------------------------+
| Formal type                                                  | Actual type             |
+==============================================================+=========================+
| Indefinite incomplete type                                   | Any type                |
|                                                              |                         |
| **Format**: :ada:`type T (<>);`                              |                         |
+--------------------------------------------------------------+-------------------------+
| Indefinite nonlimited private type                           | Any nonlimited type     |
|                                                              | indefinite or definite  |
| **Format**: :ada:`type T (<>) is private;`                   |                         |
+--------------------------------------------------------------+-------------------------+
| Indefinite limited private type                              | Any type, limited or    |
|                                                              | not, indefinite or      |
| **Format**: :ada:`type T (<>) is limited private;`           | definite                |
+--------------------------------------------------------------+-------------------------+
| Incomplete indefinite tagged private type                    | Any concrete tagged     |
|                                                              | type,                   |
| **Format**: :ada:`type T (<>) is tagged;`                    | indefinite or definite  |
+--------------------------------------------------------------+-------------------------+
| Indefinite tagged private type                               | Any concrete,           |
|                                                              | nonlimited tagged type, |
| **Format**: :ada:`type T (<>) is tagged private;`            | indefinite or definite  |
+--------------------------------------------------------------+-------------------------+
| Indefinite tagged limited private type                       | Any concrete tagged     |
|                                                              | type, limited or not,   |
| **Format**: :ada:`type T (<>) is tagged limited private;`    | indefinite or definite  |
+--------------------------------------------------------------+-------------------------+
| Indefinite abstract tagged private type                      | Any nonlimited tagged   |
|                                                              | type, indefinite or     |
| **Format**: :ada:`type T (<>) is abstract tagged private;`   | definite, abstract or   |
|                                                              | concrete                |
+--------------------------------------------------------------+-------------------------+
| Indefinite abstract tagged limited private type              | Any tagged type,        |
|                                                              | limited or not,         |
| **Format**:                                                  | indefinite or definite  |
| :ada:`type T (<>) is abstract tagged limited private;`       | abstract or concrete    |
+--------------------------------------------------------------+-------------------------+
| Indefinite derived tagged type                               | Any tagged type derived |
|                                                              | from base type B,       |
| **Format**: :ada:`type T (<>) is new B with private;`        | indefinite or definite  |
+--------------------------------------------------------------+-------------------------+
| Indefinite abstract derived tagged type                      | Any tagged type derived |
|                                                              | from base type B,       |
| **Format**:                                                  | indefinite or definite  |
| :ada:`type T (<>) is abstract new B with private;`           | abstract or concrete    |
+--------------------------------------------------------------+-------------------------+

The same examples could also contain discriminants. In this case, :ada:`(<>)`
is replaced by a list of discriminants, e.g.: :ada:`(D: DT)`.

.. _Intro_Ada_Containers_Table:

Appendix B: Containers
----------------------

The following table shows all containers available in Ada,
including their versions (standard, bounded, unbounded, indefinite):

+-----------+------------------------------------+-----+---------+-----------+------------+
| Category  | Container                          | Std | Bounded | Unbounded | Indefinite |
+===========+====================================+=====+=========+===========+============+
| Vector    | ``Vectors``                        |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| List      | ``Doubly Linked Lists``            |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Hashed Maps``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Map       | ``Ordered Maps``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Hashed Sets``                    |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Set       | ``Ordered Sets``                   |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Tree      | ``Multiway Trees``                 |  Y  |    Y    |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Generic   | ``Holders``                        |     |         |           |     Y      |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized Queue Interfaces``  |  Y  |         |           |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Synchronized Queues``            |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+
| Queue     | ``Priority Queues``                |     |    Y    |     Y     |            |
+-----------+------------------------------------+-----+---------+-----------+------------+

.. note::
    To get the correct container name, replace the whitespace by ``_`` in the
    names above. (For example, ``Hashed Maps`` becomes :ada:`Hashed_Maps`.)

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
