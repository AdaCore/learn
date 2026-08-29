"""Names the pipeline's modules have to agree on.

The pipeline is three commands that talk to each other through files on
disk: the extraction step writes an artifact, and the checking step goes
looking for it by name.  Nothing checks that the two names match -- a
mismatch produces no error, only a check that quietly finds nothing to do.
Keeping the names here means the writer and the reader cannot disagree.
"""

# The per-block file the extraction step writes and the checking step reads.
BLOCK_INFO_FILENAME = "block_info.json"

# The record of what was checked for a block, written after the checks run.
BLOCK_CHECKS_FILENAME = "block_checks.json"

# The generated project file, and the configuration pragmas it refers to.
# The two are a pair: the project names the pragma file, so the name used
# when writing the file and the name written into the project have to be the
# same one.
PROJECT_FILENAME = "main.gpr"
PROJECT_PRAGMAS_FILENAME = "main.adc"

# The SPARK variants of the same pair, generated instead of the above when a
# block is proved rather than merely built.
SPARK_PROJECT_FILENAME = "main_spark.gpr"
SPARK_PROJECT_PRAGMAS_FILENAME = "main_spark.adc"


# The ``:class:`` values a course author writes on a code block, which are
# what the checker reads to decide what to do with it.  They arrive as plain
# strings from the RST source, so a misspelling here would not raise -- the
# comparison would simply never match and the check would be skipped in
# silence, on a block that looks checked.  Naming them means a typo is an
# AttributeError at import instead.
CLASS_ADA_NOCHECK = "ada-nocheck"
CLASS_C_NOCHECK = "c-nocheck"

CLASS_ADA_SYNTAX_ONLY = "ada-syntax-only"

CLASS_ADA_COMPILE = "ada-compile"
CLASS_C_COMPILE = "c-compile"

CLASS_ADA_RUN = "ada-run"
CLASS_ADA_NORUN = "ada-norun"
CLASS_ADA_RUN_EXPECT_FAILURE = "ada-run-expect-failure"
CLASS_C_RUN_EXPECT_FAILURE = "c-run-expect-failure"

CLASS_ADA_EXPECT_COMPILE_ERROR = "ada-expect-compile-error"
CLASS_C_EXPECT_COMPILE_ERROR = "c-expect-compile-error"
CLASS_ADA_EXPECT_PROVE_ERROR = "ada-expect-prove-error"

CLASS_ADA_PROVE = "ada-prove"
CLASS_ADA_PROVE_FLOW = "ada-prove-flow"
CLASS_ADA_PROVE_FLOW_REPORT_ALL = "ada-prove-flow-report-all"
CLASS_ADA_PROVE_REPORT_ALL = "ada-prove-report-all"
CLASS_ADA_REPORT_ALL = "ada-report-all"

# The classes that ask for a proof.  Grouped here because the check that
# reads them treats them as one set rather than testing each in turn.
PROVE_CLASSES = [
    CLASS_ADA_PROVE,
    CLASS_ADA_PROVE_FLOW,
    CLASS_ADA_PROVE_FLOW_REPORT_ALL,
    CLASS_ADA_PROVE_REPORT_ALL,
]
