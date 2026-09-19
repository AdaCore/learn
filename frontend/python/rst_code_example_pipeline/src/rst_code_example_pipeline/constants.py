"""Names the pipeline's modules have to agree on.

The pipeline is three commands that talk to each other through files on
disk: the extraction step writes an artifact, and the checking step goes
looking for it by name.  Nothing checks that the two names match -- a
mismatch produces no error, only a check that quietly finds nothing to do.
Keeping the names here means the commands in this package cannot disagree.

Two limits are worth knowing before renaming anything here.

The guarantee stops at the package boundary.
``frontend/sphinx/code_block_info.py`` locates the block info file by its own
copy of the name and treats a miss as "no metadata" rather than an error, so
it has to be changed in step and nothing will say so.  The browser-side
download code writes its own copies of the four project-file names, and of
the project template that refers to them.

And the two project file names are not free even inside the package: the
templates below name the project units ``Main`` and ``Main_Spark``, which
the builder requires to match the file names.  Renaming those two constants
alone produces a project whose unit name does not match its file, which the
builder reports; the unit names have to move with them.
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
# silence, on a block that looks checked.  Naming them turns that typo into
# an AttributeError at the point of use.
#
# What a course author may actually write is fixed elsewhere --
# ``CONTRIBUTING.md`` documents it, and the code-block directive rejects any
# class it does not recognize -- so read this list as the names the checker
# acts on, not as the reference for the RST source.
CLASS_ADA_NOCHECK = "ada-nocheck"
CLASS_C_NOCHECK = "c-nocheck"

CLASS_ADA_SYNTAX_ONLY = "ada-syntax-only"
CLASS_NOSYNTAX_CHECK = "nosyntax-check"

CLASS_ADA_COMPILE = "ada-compile"
CLASS_C_COMPILE = "c-compile"

CLASS_ADA_RUN = "ada-run"
CLASS_ADA_NORUN = "ada-norun"
CLASS_C_RUN = "c-run"
CLASS_C_NORUN = "c-norun"
CLASS_ADA_RUN_EXPECT_FAILURE = "ada-run-expect-failure"
CLASS_C_RUN_EXPECT_FAILURE = "c-run-expect-failure"

CLASS_ADA_EXPECT_COMPILE_ERROR = "ada-expect-compile-error"
CLASS_C_EXPECT_COMPILE_ERROR = "c-expect-compile-error"
CLASS_ADA_EXPECT_PROVE_ERROR = "ada-expect-prove-error"

CLASS_ADA_PROVE = "ada-prove"
CLASS_ADA_PROVE_FLOW = "ada-prove-flow"
CLASS_ADA_PROVE_FLOW_REPORT_ALL = "ada-prove-flow-report-all"
CLASS_ADA_PROVE_REPORT_ALL = "ada-prove-report-all"

# The run classes paired with the language each one names.  A run class is
# honored only for a code block written in that language; one naming the
# other language is reported rather than quietly doing nothing, so that a
# mis-typed class cannot leave a code block unbuilt and still passing.
RUN_CLASS_LANGUAGES = {
    CLASS_ADA_RUN: "ada",
    CLASS_ADA_NORUN: "ada",
    CLASS_ADA_RUN_EXPECT_FAILURE: "ada",
    CLASS_C_RUN: "c",
    CLASS_C_NORUN: "c",
    CLASS_C_RUN_EXPECT_FAILURE: "c",
}

# The classes that ask for a proof.  Grouped here because the check that
# reads them treats them as one set rather than testing each in turn.
PROVE_CLASSES = [
    CLASS_ADA_PROVE,
    CLASS_ADA_PROVE_FLOW,
    CLASS_ADA_PROVE_FLOW_REPORT_ALL,
    CLASS_ADA_PROVE_REPORT_ALL,
]
