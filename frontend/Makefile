# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = learnadacorecom
SPHINXCONF    = sphinx
BUILDDIR      = dist
TEST_DRIVER   = python3 tests/compile_blocks.py -B $(BUILDDIR)/test_output

mkfile_path  := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir   := $(dir $(mkfile_path))

CONTENT_DIR   = $(mkfile_dir)/../content
HOST_ADDRESS ?= localhost
HOST_PORT    ?= 8000

YARN_BUILD    := yarn run build


help:
	@echo "make test_content  - run the testsuite"
	@echo "make test_engine  - generate the engine test html"
	@echo "make html   - generates the html"
	@echo "make latexpdf - generates the PDF document"
	@echo "make pdf_books - generates the PDF document for each course and labs"

test_engine: webpack
	@echo "===== ENGINE TEST ====="
	@$(SPHINXBUILD) -M html $(CONTENT_DIR) \
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

test_ada_for_the_cpp_java_developer:
	@echo "===== ADA FOR THE CPP JAVA DEV ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/Ada_For_The_CPP_Java_Developer/chapters/*.rst
	@echo ""

test_gnat_toolchain_intro:
	@echo "===== GNAT TOOLCHAIN INTRO ====="
	@$(TEST_DRIVER) $(CONTENT_DIR)/courses/GNAT_Toolchain_Intro/chapters/*.rst
	@echo ""

test_content: test_ada_intro test_spark_intro test_advanced_ada test_advanced_spark \
	test_spark_for_the_misra_c_developer \
	test_ada_for_the_cpp_java_developer \
	test_gnat_toolchain_intro


publish-staging:
	@echo "Publishing current branch to learn-staging..."
	@if [ ! -d learn-staging-html-pages ] ;\
	then \
		git clone -b gh-pages git@github.com:AdaCore/learn-staging-html-pages.git; \
	fi && \
	( cd learn-staging-html-pages && \
	  git checkout gh-pages && git pull && \
	  rm -rf * && \
	  git checkout CNAME ) && \
	cp -R $(BUILDDIR)/html/. learn-staging-html-pages/ && \
	cp -R $(BUILDDIR)/pdf_books learn-staging-html-pages/ && \
	( cd learn-staging-html-pages && \
	  git add -A && git commit -m "Regenerate" && git push origin gh-pages ) && \
	rm -rf learn-staging-html-pages

publish:
	@echo "Publishing current branch to learn..."
	@if [ ! -d learn-html-pages ] ;\
        then \
                git clone -b gh-pages git@github.com:AdaCore/learn-html-pages.git; \
        fi && \
	( cd learn-html-pages && \
	  git checkout gh-pages && git pull && \
	  rm -rf * && \
	  git checkout CNAME ) && \
        cp -R $(BUILDDIR)/html/. learn-html-pages/ && \
        ( cd learn-html-pages && \
          git add -A && git commit -m "Regenerate" && git push origin gh-pages ) && \
        rm -rf learn-html-pages

# Development target, rebuilds the site, with it pointing to the local
# code server.
# This should only be called from `yarn run dev`
local:
	CODE_SERVER_URL="http://$(HOST_ADDRESS):$(HOST_PORT)" $(SPHINXBUILD) -M html $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

webpack:
	$(YARN_BUILD)

# Build the site pointing to 'cloudchecker.r53.adacore.com'
site: cleanall webpack pdf_books
	CODE_SERVER_URL="https://cloudchecker.r53.adacore.com" GEN_LEARN_SITE=yes $(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

site-staging: cleanall webpack pdf_books
	$(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

BOOKS = $(wildcard $(CONTENT_DIR)/courses/*/.) $(wildcard $(CONTENT_DIR)/labs/*/.)

pdf_books: $(BOOKS)
$(BOOKS):
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $(shell dirname $@))))
	$(eval SPHINX_PDF := $(shell basename $(shell dirname $@)).pdf)
	$(eval SPHINX_TITLE := $(shell cat $@/conf.txt | grep title | sed -e 's#title=##'))
	$(eval SPHINX_AUTHOR := $(shell cat $@/conf.txt | grep author | sed -e 's#author=##'))
	@mkdir -p $(BUILDDIR)/pdf_books/$(SPHINX_DIR)
	@ export SPHINX_TITLE="$(SPHINX_TITLE)"; \
	  export SPHINX_AUTHOR="$(SPHINX_AUTHOR)"; \
	$(SPHINXBUILD) -M latexpdf $@ \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@ mv $(BUILDDIR)/latex/learnadacorecom.pdf $(BUILDDIR)/pdf_books/$(SPHINX_DIR)/$(SPHINX_PDF)

cleanall:
	@ rm -rf "$(BUILDDIR)"

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help test_content test_engine pdf_books $(BOOKS)
