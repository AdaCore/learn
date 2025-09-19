Detecting Unreachable Code and Dead Code
----------------------------------------

.. include:: ../../../global.txt

MISRA C defines `unreachable code` as code that cannot be executed, and
it defines `dead code` as code that can be executed
but has no effect on the functional behavior of the program. (These
definitions differ from traditional terminology, which refers to the first
category as "dead code" and the second category as "useless code".)
Regardless of the terminology, however,
both types are actively harmful, as they might confuse
programmers and lead to errors during maintenance.

The "Unused code" section of MISRA C contains seven rules that deal with
detecting both unreachable code and dead code. The two most
important rules are:

* Rule 2.1: `"A project shall not contain unreachable code"`, and

* Rule 2.2: `"There shall not be dead code"`.

Other rules in the same section prohibit unused entities of
various kinds (type declarations, tag declarations, macro declarations, label
declarations, function parameters).

While some simple cases of unreachable code can be detected by static analysis
(typically if a condition in an :c:`if` statement can be determined to be always
true or false), most cases of unreachable code can only be detected by performing
coverage analysis in testing, with the caveat that code reported as not being
executed is not necessarily unreachable (it could simply reflect gaps in the test
suite). Note that `statement coverage`, rather than the more comprehensive
*decision coverage* or *modified condition / decision coverage* (MC/DC) as
defined in the DO-178C standard for airborne software, is sufficient to detect
potential unreachable statements, corresponding to code that is not covered
during the testing campaign.

The presence of dead code is much harder to detect, both statically and
dynamically, as it requires creating a complete dependency graph linking
statements in the code and their effect on visible behavior of the program.

SPARK can detect some cases of both unreachable and dead code through its
precise construction of a dependency graph linking a subprogram's statements
to all its inputs and outputs. This analysis might not be able to
detect complex cases, but it goes well beyond what other analyses do in general.

.. code:: ada prove_flow_button compile_button project=Courses.SPARK_For_The_MISRA_C_Dev.Unreachable_And_Dead_Code.Much_Ado_About_Little

    procedure Much_Ado_About_Little (X, Y, Z : Integer; Success : out Boolean);

    procedure Much_Ado_About_Little (X, Y, Z : Integer; Success : out Boolean) is

       procedure Ok is
       begin
          Success := True;
       end Ok;

       procedure NOk is
       begin
          Success := False;
       end NOk;

    begin
       Success := False;

       for K in Y .. Z loop
          if K < X and not Success then
             Ok;
          end if;
       end loop;

       if X > Y then
          Ok;
       else
          NOk;
       end if;

       if Z > Y then
          NOk;
          return;
       else
          Ok;
          return;
       end if;

       if Success then
          Success := not Success;
       end if;
    end Much_Ado_About_Little;

The only code in the body of :ada:`Much_Ado_About_Little` that affects the result
of the procedure's execution is the :ada:`if Z > Y...` statement, since this
statement sets :ada:`Success` to either True or False regardless of what the
previous statements did.  I.e., the statements preceding this :ada:`if` are
dead code in the MISRA C sense. Since both branches of the :ada:`if Z > Y...`
statement return from the procedure, the subsequent :ada:`if Success...` statement
is unreachable.  GNATprove detects and issues warnings about both
the dead code and the unreachable code.
