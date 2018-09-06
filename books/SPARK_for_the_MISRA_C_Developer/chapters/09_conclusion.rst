Conclusion
----------

MISRA-C is a very laudable collective effort to significantly reduce the number
of errors in C programs, by defining guidelines focused on avoiding error-prone
programming features of the C programming language rather than on enforcing a
particular programming style. The Rationale provided with each rule is a clear
and unobjectionable justification of the benefit of each rule in that respect.

On the plus side, the C programming language comes without bells and whistles,
which makes it the lingua franca for the majority of embedded platforms of all
sizes. On the minus side, it also comes without belts and suspenders, which
makes it less suitable for developing critical applications. To some degree, it
is possible to put the blame for programming errors on programmers themselves,
as Linus Torvalds did:

   `"Christ, people. Learn C, instead of just stringing random characters
   together until it compiles (with warnings)."`

Programming errors are manufactured by programmers, no doubt about it, and we
should expect a high level of professionalism of those in charge of critical
applications. At the same time, the flip side of the human brain's inventivity
and flexibility is its inadequacy to be 100% correct about thousands of little
details such as those discussed in this book.

Even more convincing is the inability of even experts to sometimes grasp the
full extent of the C programming language rules, as exemplified by Microsoft
expert David LeBlanc (see :ref:`Enforcing Strong Typing for Scalars`) or the
MISRA-C Committee itself (see the :ref:`Preface`).

We have shown that the SPARK programming language and associated formal
verification tools can contribute usefully to the goal of producing error-free
software as pursued in MISRA-C.
