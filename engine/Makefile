TEST_DRIVER = python tests/compile_blocks.py

help:
	@echo "make tests  - run the testsuite"
	@echo "make html   - generates the html"

test_ada_intro:
	@echo "===== INTRO TO ADA ====="
	@$(TEST_DRIVER) ../courses/intro-to-ada/intro_to_ada.rst
	@echo ""

test_spark_intro:
	@echo "===== INTRO TO SPARK ====="
	for a in `ls ../courses/intro-to-spark/book/*.rst` ; do\
             $(TEST_DRIVER) $$a ;\
        done
	@echo ""

tests: test_ada_intro test_spark_intro

.PHONY: Makefile help tests
