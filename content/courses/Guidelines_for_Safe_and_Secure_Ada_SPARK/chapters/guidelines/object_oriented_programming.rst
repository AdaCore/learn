
===================================
Object-Oriented Programming (OOP)
===================================

.. include:: ../../../../global.txt

*Goal*
   :Maintainability: :math:`\checkmark`
   :Reliability: :math:`\checkmark`
   :Portability:
   :Performance:
   :Security: :math:`\checkmark`

Description
   Have a plan for selecting the OOP facilities of the language to use.

Rules
   OOP01, OOP02, OOP03, OOP04, OOP05, OOP06, OOP07

There are many issues to consider when planning the use of Object Oriented
features in a high-integrity application. Choices should be made based on the
desired expressive power of the OO features and the required level of
certification or safety case.

For example, the use of inheritance can provide abstraction and separation of
concerns. However, the extensive use of inheritance, particularly in deep
hierarchies, can lead to fragile code bases.

Similarly, when new types of entities are added, dynamic dispatching provides
separation of the code that must change from the code that manipulates those
types and need not be changed to handle new types. However, analysis of dynamic
dispatching must consider every candidate object type and analyze the
associated subprogram for appropriate behavior.

Therefore, the system architect has available a range of possibilities for the
use of OOP constructs, with tool enforcement available for the selections. Note
that full use of OOP, including dynamic dispatching, may not be unreasonable.

The following rules assume use of tagged types, a requirement for full OOP in
Ada.

.. toctree::
   :maxdepth: 1

   No Class-wide Constructs Policy (OOP01) <object_oriented_programming/oop01_no_class-wide_constructs_policy.rst>
   Static Dispatching Only Policy (OOP02) <object_oriented_programming/oop02_static_dispatching_only_policy.rst>
   Limit Inheritance Hierarchy Depth (OOP03) <object_oriented_programming/oop03_limit_inheritance_hierarchy_depth.rst>
   Limit Statically-Dispatched Calls To Primitive Operations (OOP04) <object_oriented_programming/oop04_limit_statically-dispatched_calls_to_primitive_operations.rst>
   Use Explicit Overriding Annotations (OOP05) <object_oriented_programming/oop05_use_explicit_overriding_annotations.rst>
   Use Class-wide Pre/Post Contracts (OOP06) <object_oriented_programming/oop06_use_class-wide_pre-post_contracts.rst>
   Ensure Local Type Consistency (OOP07) <object_oriented_programming/oop07_ensure_local_type_consistency.rst>
