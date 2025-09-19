Basics
--------

.. include:: ../../../global.txt

Ada implements the vast majority of programming concepts that you're accustomed to in C++ and Java: classes, inheritance, templates (generics), etc. Its syntax might seem peculiar, though. It's not derived from the popular C style of notation with its ample use of brackets; rather, it uses a more expository syntax coming from Pascal. In many ways, Ada is a simpler language |mdash| its syntax favors making it easier to conceptualize and read program code, rather than making it faster to write in a cleverly condensed manner. For example, full words like :ada:`begin` and :ada:`end` are used in place of curly braces. Conditions are written using :ada:`if`, :ada:`then`, :ada:`elsif`, :ada:`else`, and :ada:`end if`. Ada's assignment operator does not double as an expression, smoothly eliminating any frustration that could be caused by :cpp:`=` being used where :cpp:`==` should be.

All languages provide one or more ways to express comments.  In Ada, two consecutive hyphens :ada:`--` mark the start of a comment that continues to the end of the line.  This is exactly the same as using :cpp:`//` for comments in C++ and Java.  There is no equivalent of :cpp:`/* ... /*` block comments in Ada; use multiple :ada:`--` lines instead.

Ada compilers are stricter with type and range checking than most C++ and Java programmers are used to. Most beginning Ada programmers encounter a variety of warnings and error messages when coding more creatively, but this helps detect problems and vulnerabilities at compile time |mdash| early on in the development cycle. In addition, dynamic checks (such as array bounds checks) provide verification that could not be done at compile time. Dynamic checks are performed at run time, similar to what is done in Java.

Ada identifiers and reserved words are case insensitive. The identifiers :ada:`VAR`, :ada:`var` and :ada:`VaR` are treated as the same; likewise :ada:`begin`, :ada:`BEGIN`, :ada:`Begin`, etc. Language-specific characters, such as accents, Greek or Russian letters, and Asian alphabets, are acceptable to use. Identifiers may include letters, digits, and underscores, but must always start with a letter. There are 73 reserved keywords in Ada that may not be used as identifiers, and these are:

  =============== ================ ================= ===================
  :ada:`abort`    :ada:`else`      :ada:`null`       :ada:`select`
  :ada:`abs`      :ada:`elsif`     :ada:`of`         :ada:`separate`
  :ada:`abstract` :ada:`end`       :ada:`or`         :ada:`some`
  :ada:`accept`   :ada:`entry`     :ada:`others`     :ada:`subtype`
  :ada:`access`   :ada:`exception` :ada:`out`        :ada:`synchronized`
  :ada:`aliased`  :ada:`exit`      :ada:`overriding` :ada:`tagged`
  :ada:`all`      :ada:`for`       :ada:`package`    :ada:`task`
  :ada:`and`      :ada:`function`  :ada:`pragma`     :ada:`terminate`
  :ada:`array`    :ada:`generic`   :ada:`private`    :ada:`then`
  :ada:`at`       :ada:`goto`      :ada:`procedure`  :ada:`type`
  :ada:`begin`    :ada:`if`        :ada:`protected`  :ada:`until`
  :ada:`body`     :ada:`in`        :ada:`raise`      :ada:`use`
  :ada:`case`     :ada:`interface` :ada:`range`      :ada:`when`
  :ada:`constant` :ada:`is`        :ada:`record`     :ada:`while`
  :ada:`declare`  :ada:`limited`   :ada:`rem`        :ada:`with`
  :ada:`delay`    :ada:`loop`      :ada:`renames`    :ada:`xor`
  :ada:`delta`    :ada:`mod`       :ada:`requeue`
  :ada:`digits`   :ada:`new`       :ada:`return`
  :ada:`do`       :ada:`not`       :ada:`reverse`
  =============== ================ ================= ===================

Ada is designed to be portable. Ada compilers must follow a precisely defined international (ISO) standard language specification with clearly documented areas of vendor freedom where the behavior depends on the implementation. It's possible, then, to write an implementation-independent application in Ada and to make sure it will have the same effect across platforms and compilers.

Ada is truly a general purpose, multiple paradigm language that allows the programmer to employ or avoid features like run-time contract checking, tasking, object oriented programming, and generics. Efficiently programmed Ada is employed in device drivers, interrupt handlers, and other low-level functions. It may be found today in devices with tight limits on processing speed, memory, and power consumption. But the language is also used for programming larger interconnected systems running on workstations, servers, and supercomputers.
