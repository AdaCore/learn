# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = learnadacorecom
SPHINXCONF    = sphinx
BUILDDIR      = dist
TEST_DRIVER   = python3 tests/compile_blocks.py

mkfile_path  := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir   := $(dir $(mkfile_path))

CONTENT_DIR   = $(mkfile_dir)/../content
HOST_ADDRESS ?= localhost

YARN_BUILD    := yarn run build


help:
	@echo "make tests  - run the testsuite"
	@echo "make test_engine  - generate the engine test html"
	@echo "make html   - generates the html"

test_engine:
	@echo "===== ENGINE TEST ====="
	@$(YARN_BUILD)
	@$(SPHINXBUILD) -M html "tests/" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@echo ""

test_ada_intro:
	@echo "===== INTRO TO ADA ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/intro-to-ada/chapters/*.rst
	@echo ""

test_spark_intro:
	@echo "===== INTRO TO SPARK ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/intro-to-spark/chapters/*.rst
	@echo ""

test_advanced_ada:
	@echo "===== ADVANCED ADA ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/advanced-ada/chapters/*.rst
	@echo ""

test_advanced_spark:
	@echo "===== ADVANCED SPARK ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/advanced-spark/chapters/*.rst
	@echo ""

test_spark_for_the_misra_c_developer:
	@echo "===== SPARK FOR MISRA C DEV ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/SPARK_for_the_MISRA_C_Developer/chapters/*.rst
	@echo ""

test_content: test_ada_intro test_spark_intro test_advanced_ada test_advanced_spark \
	test_spark_for_the_misra_c_developer

publish-staging:
	@echo "Publishing current branch to learn-staging..."
	@if [ ! -d learn-staging-html-pages ] ;\
	then \
		git clone -b gh-pages git@github.com:AdaCore/learn-staging-html-pages.git; \
		cd learn-staging-html-pages; \
	else \
		cd learn-staging-html-pages; \
		git checkout gh-pages && git pull; \
	fi; \
	rm -rf *; \
	git checkout CNAME; \
	cd ../; \
	cp -R $(BUILDDIR)/html/. learn-staging-html-pages/; \
	cd learn-staging-html-pages; \
	git add -A && git commit -m "Regenerate" && git push origin gh-pages; \
	cd ../; \
	rm -rf learn-staging-html-pages

# Development target, rebuilds the site, with it pointing to the local
# code server.
local:
	CODE_SERVER_URL="http://$(HOST_ADDRESS):8000" $(SPHINXBUILD) -M html $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help tests
