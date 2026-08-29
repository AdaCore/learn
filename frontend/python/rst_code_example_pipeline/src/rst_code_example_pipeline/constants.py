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
