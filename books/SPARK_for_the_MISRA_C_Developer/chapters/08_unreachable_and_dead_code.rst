:code-config:`run_button=False;prove_button=True;accumulate_code=False`

Detecting Unreachable Code and Dead Code
----------------------------------------

.. role:: ada(code)
   :language: ada

.. role:: c(code)
   :language: c

MISRA-C calls `unreachable code` the code that cannot be executed, which is
usually called `dead code`. It calls `dead code` the code that can be executed
but has no effect on the behavior of the program. Whatever the terminology,
both types of useless code are actively harmful, as they might confuse
programmers and lead to errors during maintenance by becoming active by
accident.

MISRA-C attempts to detect the presence of both unreachable code and dead code
in a specific section on "Unused code" containing 7 rules. The two most
important ones are MISRA Rule 2.1 stating that `"A project shall not contain
unreachable code"` and MISRA-C Rule 2.2 stating that `"There shall not be dead
code"`. Other rules in this section require that there be no unused entities of
various kinds (type declarations, tag declarations, macro declarations, label
declarations, function parameters).

While some simple cases of unreachable code can be detected by static analysis
(typically if a test can be determined to be always true or false), most cases
of unreachable code can only be detected by performing coverage analysis in
testing. Note that the simpler notion of `statement coverage` is sufficient to
detect potential unreachable code, corresponding to code that is not covered
during the testing campaign.

The presence of dead code is much harder to detect, both statically or
dynamically, as it requires to create a complete dependency graph linking
statements in the code and their effect on visible behavior of the program.

SPARK allows to detect some cases of both unreachable and dead code through its
precise construction of a dependency graph linking statements in a subprogram
to all inputs and outputs of this subprogram. This analysis is performed
separately for every subprogram in order to scale, so it may not be able to
detect more complex cases of unreachable and dead code, although it goes well
beyond what other analyses do in general about unreachable and dead code.

.. code:: ada spark-flow

    procedure Do_Stuff (X, Y, Z : Integer; Success : out Boolean) is

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
    end Do_Stuff;

GNATprove detects that the code prior to the test that ``Z > Y`` has no effect
on any output of procedure Do_Stuff (here simply the output parameter
``Success``), and issues corresponding warnings about dead code. It also
detects that the final if-statement is never reachable, and issues
corresponding warnings about unreachable code.
