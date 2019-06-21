Basics
--------

Ada implements the vast majority of programming concepts that you're accustomed to in C++ and Java: classes, inheritance, templates (generics), etc. Its syntax might seem peculiar, though. It's not derived from the popular C style of notation with its ample use of brackets; rather, it uses a more expository syntax coming from Pascal. In many ways, Ada is a simpler language---its syntax favors making it easier to conceptualize and read program code, rather than making it faster to write in a cleverly condensed manner. For example, full words like **begin** and **end** are used in place of curly braces. Conditions are written using **if**, **then**, **elsif**, **else**, and **end if**. Ada's assignment operator does not double as an expression, smoothly eliminating any frustration that could be caused by **=** being used where **==** should be.

All languages provide one or more ways to express comments.  In Ada, two consecutive hyphens ``--`` mark the start of a comment that continues to the end of the line.  This is exactly the same as using **//** for comments in C++ and Java.  There is no equivalent of **/\*** ... **\*/** block comments in Ada; use multiple ``--`` lines instead.

Ada compilers are stricter with type and range checking than most C++ and Java programmers are used to. Most beginning Ada programmers encounter a variety of warnings and error messages when coding more creatively, but this helps detect problems and vulnerabilities at compile time---early on in the development cycle. In addition, dynamic checks (such as array bounds checks) provide verification that could not be done at compile time. Dynamic checks are performed at run time, similar to what is done in Java.

Ada identifiers and reserved words are case insensitive. The identifiers *VAR*, *var* and *VaR* are treated as the same; likewise **begin**, **BEGIN**, **Begin**, etc. Language-specific characters, such as accents, Greek or Russian letters, and Asian alphabets, are acceptable to use. Identifiers may include letters, digits, and underscores, but must always start with a letter. There are 73 reserved keywords in Ada that may not be used as identifiers, and these are:

  ======== ========= ========== ============
  abort    else      null       select
  abs      elsif     of         separate
  abstract end       or         some
  accept   entry     others     subtype
  access   exception out        synchronized
  aliased  exit      overriding tagged
  all      for       package    task
  and      function  pragma     terminate
  array    generic   private    then
  at       goto      procedure  type
  begin    if        protected  until
  body     in        raise      use
  case     interface range      when
  constant is        record     while
  declare  limited   rem        with
  delay    loop      renames    xor
  delta    mod       requeue
  digits   new       return
  do       not       reverse
  ======== ========= ========== ============

.. todo::
  *Put the reserved words in bolface*

Ada is designed to be portable. Ada compilers must follow a precisely defined international (ISO) standard language specification with clearly documented areas of vendor freedom where the behavior depends on the implementation. It's possible, then, to write an implementation-independent application in Ada and to make sure it will have the same effect across platforms and compilers.

Ada is truly a general purpose, multiple paradigm language that allows the programmer to employ or avoid features like run-time contract checking, tasking, object oriented programming, and generics. Efficiently programmed Ada is employed in device drivers, interrupt handlers, and other low-level functions. It may be found today in devices with tight limits on processing speed, memory, and power consumption. But the language is also used for programming larger interconnected systems running on workstations, servers, and supercomputers.