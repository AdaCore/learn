# You can set these variables from the command line.
SPHINXOPTS         =
SPHINXBUILD        = sphinx-build
SPHINXPROJ         = learnadacorecom
SPHINXCONF         = sphinx
BUILDDIR           = dist
SRC_TEST_DIR       = $(BUILDDIR)/test_output
TEST_LAMBDA       ?= 0
TEST_LOCAL_DRIVER  = PYTHONPATH="$PYTHONPATH:sphinx" python3 tests/compile_blocks.py --keep_files -B $(SRC_TEST_DIR)
TEST_OPTIONS      ?=
TEST_LAMBDA_DRIVER = PYTHONPATH="$PYTHONPATH:sphinx" python3 tests/execute_rst_code_blocks.py --halt-on-failure

ifeq ($(strip $(TEST_LAMBDA)),1)
TEST_DRIVER 	:= $(TEST_LAMBDA_DRIVER)
else
TEST_DRIVER 	:= $(TEST_LOCAL_DRIVER)
endif

MKFILE_PATH  	:= $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR   	:= $(dir $(MKFILE_PATH))

BUILD_MANIFEST 	:= $(MKFILE_DIR)/build-manifest.json

CONTENT_DIR   	= $(MKFILE_DIR)/../content
TEST_CONTENT    = tests/rst
TEST_BUILDDIR   = tests/html

HIDDEN_BOOKS 	?= $(CONTENT_DIR)/hidden.txt

WEBSOCKET_URL_PROD      = wss://backend.learn.adacore.com
export CODE_SERVER_URL := wss://sandbox.backend.learn.adacore.com

COMPLETE_SITE_BOOK = learning-ada


help:
	@echo "make test_content  - run the testsuite"
	@echo "make test_engine  - generate the engine test html"
	@echo "make html   - generates the html"
	@echo "make latexpdf - generates the PDF document"
	@echo "make pdf_books - generates the PDF document for each course and labs"

test_engine: webpack-production
	@echo "===== ENGINE TEST ====="
	@$(SPHINXBUILD) -M html $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@echo ""

test_ada_intro:
	@echo "===== INTRO TO ADA ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 50)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/intro-to-ada/chapters/*.rst
	@echo ""

test_spark_intro:
	@echo "===== INTRO TO SPARK ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/intro-to-spark/chapters/*.rst
	@echo ""

test_intro_to_embedded_sys_prog:
	@echo "===== INTRO TO EMBEDDED SYS PROG ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/intro-to-embedded-sys-prog/chapters/*.rst
	@echo ""

test_advanced_ada:
	@echo "===== ADVANCED ADA ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 50)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(wildcard $(CONTENT_DIR)/courses/advanced-ada/parts/*.rst)
	@echo ""

test_advanced_spark:
	@echo "===== ADVANCED SPARK ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/advanced-spark/chapters/*.rst
	@echo ""

test_whats_new_in_ada_2022:
	@echo "===== WHAT'S NEW IN ADA 2022 ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/whats-new-in-ada-2022/chapters/*.rst
	@echo ""

test_spark_for_the_misra_c_developer:
	@echo "===== SPARK FOR MISRA C DEV ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/SPARK_for_the_MISRA_C_Developer/chapters/*.rst
	@echo ""

test_ada_for_the_cpp_java_developer:
	@echo "===== ADA FOR THE CPP JAVA DEV ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/Ada_For_The_CPP_Java_Developer/chapters/*.rst
	@echo ""

test_ada_for_the_embedded_c_developer:
	@echo "===== ADA FOR THE EMBEDDED C DEV ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/Ada_For_The_Embedded_C_Developer/chapters/*.rst
	@echo ""

test_gnat_toolchain_intro:
	@echo "===== GNAT TOOLCHAIN INTRO ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/courses/GNAT_Toolchain_Intro/chapters/*.rst
	@echo ""

test_training_fundamentals:
	@echo "===== FUNDAMENTALS OF ADA ====="
ifeq ($(strip $(TEST_LAMBDA)),1)
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS))
else
	$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns 80)
endif
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(CONTENT_DIR)/training_examples/fundamentals_of_ada/*.rst
	@echo ""

test_content: test_ada_intro test_spark_intro test_intro_to_embedded_sys_prog \
	test_whats_new_in_ada_2022 \
	test_spark_for_the_misra_c_developer \
	test_ada_for_the_cpp_java_developer \
	test_ada_for_the_embedded_c_developer \
	test_gnat_toolchain_intro
	@cd $(SRC_TEST_DIR) \
		&& zip -9 -r $(COMPLETE_SITE_BOOK)_code.zip . \
			-x "*.log" -x "*/latest*"
	@mkdir -p $(BUILDDIR)/zip
	@mv $(SRC_TEST_DIR)/$(COMPLETE_SITE_BOOK)_code.zip $(BUILDDIR)/zip

test_hidden_content: test_advanced_ada test_advanced_spark \
	test_training_fundamentals
	@echo ""

test_all_content: test_content test_hidden_content
	@echo ""

test_parser:
	@coverage run --source=widget -m unittest discover --start-directory sphinx
	@coverage report --fail-under=90 -m

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
		cp -R $(BUILDDIR)/pdf_books learn-html-pages/ && \
        ( cd learn-html-pages && \
          git add -A && git commit -m "Regenerate" && git push origin gh-pages ) && \
        rm -rf learn-html-pages

# Development target, rebuilds the site, with it pointing to the local
# code server.
# This should only be called from `yarn run dev`
local:
	@SPHINX_CONF_INI="$(SPHINX_CONF_INI)" \
	  SRC_TEST_DIR="$(SRC_TEST_DIR)" \
	  $(SPHINXBUILD) -M html $(CONTENT_DIR) \
	  "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

webpack-production:
	yarn run production

webpack-sandbox:
	yarn run sandbox

sphinx-production:
	SPHINX_CONF_INI="$(SPHINX_CONF_INI)" \
	SRC_TEST_DIR="$(SRC_TEST_DIR)" \
	HIDDEN_BOOKS="$(HIDDEN_BOOKS)" \
	CODE_SERVER_URL="$(WEBSOCKET_URL_PROD)" \
	GEN_LEARN_SITE=yes \
	$(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

sphinx-sandbox:
	SPHINX_CONF_INI="$(SPHINX_CONF_INI)" \
	SRC_TEST_DIR="$(SRC_TEST_DIR)" \
	HIDDEN_BOOKS="$(HIDDEN_BOOKS)" \
	$(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

# Build the site pointing to 'cloudchecker.learn.r53.adacore.com'
site: cleanall test_content webpack-production sphinx-production all_books complete_site_books
	@echo Building $@

site-sandbox: cleanall test_content webpack-sandbox sphinx-sandbox all_books complete_site_books
	@echo Building $@

site-testing: cleantest
	FRONTEND_TESTING=true $(SPHINXBUILD) -M html $(TEST_CONTENT) \
        "$(TEST_BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

BOOKS = $(wildcard $(CONTENT_DIR)/courses/*/.) $(wildcard $(CONTENT_DIR)/labs/*/.)

PDF_BOOKS_DIR := $(BUILDDIR)/pdf_books

%.pdf:
	@echo Building $@
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $@)))
	$(eval SPHINX_PDF := $(shell basename $@ .pdf))
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR)/$(SPHINX_DIR)/$(SPHINX_PDF))
	$(eval SPHINX_CONF_INI := $(shell [ -f ${SPHINX_CONTENT_DIR}/conf.ini ] && echo ${SPHINX_CONTENT_DIR}/conf.ini))
	@mkdir -p $(PDF_BOOKS_DIR)/$(SPHINX_DIR)
	@mkdir -p $(BUILDDIR)/latex
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	GEN_LEARN_SITE=yes $(SPHINXBUILD) -M latexpdf $(SPHINX_CONTENT_DIR) \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@mv $(BUILDDIR)/latex/learnadacorecom.pdf $@

$(PDF_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).pdf:
	@echo Building $@ PDF
	@rm -rf "$(BUILDDIR)/doctrees"
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR))
	$(eval SPHINX_CONF_INI := $(shell [ -f ${SPHINX_CONTENT_DIR}/conf.ini ] && echo ${SPHINX_CONTENT_DIR}/conf.ini))
	@mkdir -p $(PDF_BOOKS_DIR)
	@mkdir -p $(BUILDDIR)/latex
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	GEN_LEARN_SITE=yes $(SPHINXBUILD) -M latexpdf $(SPHINX_CONTENT_DIR) \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@mv $(BUILDDIR)/latex/learnadacorecom.pdf $@

# Generate list: PDF_BOOKS
$(foreach book_dir, $(BOOKS), \
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $(shell dirname ${book_dir})))) \
	$(eval SPHINX_BOOK := $(shell basename $(shell dirname ${book_dir}))) \
	$(eval SPHINX_PDF := $(SPHINX_BOOK).pdf) \
	$(eval HIDDEN_BOOKS_FOUND := $(or $(and $(wildcard $(HIDDEN_BOOKS)),1),0)) \
	$(if $(findstring 1,$(HIDDEN_BOOKS_FOUND)), \
		$(eval HIDDEN_BOOK := $(shell grep -c $(SPHINX_DIR)/$(SPHINX_BOOK) $(HIDDEN_BOOKS))), \
		$(eval HIDDEN_BOOK := 0)) \
	$(if $(findstring 0,$(HIDDEN_BOOK)),$(eval PDF_BOOKS += $(PDF_BOOKS_DIR)/$(SPHINX_DIR)/$(SPHINX_PDF))) \
	)

pdf_books: $(PDF_BOOKS)
	@echo $@ completed.

EPUB_BOOKS_DIR := $(BUILDDIR)/epub_books

%.epub:
	@echo Building $@ EPUB
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $@)))
	$(eval SPHINX_EPUB := $(shell basename $@ .epub))
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR)/$(SPHINX_DIR)/$(SPHINX_EPUB))
	$(eval SPHINX_CONF_INI := $(shell [ -f ${SPHINX_CONTENT_DIR}/conf.ini ] && echo ${SPHINX_CONTENT_DIR}/conf.ini))
	@mkdir -p $(EPUB_BOOKS_DIR)/$(SPHINX_DIR)
	@rm   -rf $(BUILDDIR)/epub
	@mkdir -p $(BUILDDIR)/epub/_static
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	GEN_LEARN_SITE=yes $(SPHINXBUILD) -M epub $(SPHINX_CONTENT_DIR) \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@mv $(BUILDDIR)/epub/learnadacorecom.epub $@

$(EPUB_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).epub:
	@echo Building $@
	@rm -rf "$(BUILDDIR)/doctrees"
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR))
	$(eval SPHINX_CONF_INI := $(shell [ -f ${SPHINX_CONTENT_DIR}/conf.ini ] && echo ${SPHINX_CONTENT_DIR}/conf.ini))
	@mkdir -p $(EPUB_BOOKS_DIR)
	@rm   -rf $(BUILDDIR)/epub
	@mkdir -p $(BUILDDIR)/epub/_static
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	GEN_LEARN_SITE=yes $(SPHINXBUILD) -M epub $(SPHINX_CONTENT_DIR) \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@mv $(BUILDDIR)/epub/learnadacorecom.epub $@

# Generate list: EPUB_BOOKS
$(foreach book_dir, $(BOOKS), \
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $(shell dirname ${book_dir})))) \
	$(eval SPHINX_BOOK := $(shell basename $(shell dirname ${book_dir}))) \
	$(eval SPHINX_EPUB := $(SPHINX_BOOK).epub) \
	$(eval HIDDEN_BOOKS_FOUND := $(or $(and $(wildcard $(HIDDEN_BOOKS)),1),0)) \
	$(if $(findstring 1,$(HIDDEN_BOOKS_FOUND)), \
		$(eval HIDDEN_BOOK := $(shell grep -c $(SPHINX_DIR)/$(SPHINX_BOOK) $(HIDDEN_BOOKS))), \
		$(eval HIDDEN_BOOK := 0)) \
	$(if $(findstring 0,$(HIDDEN_BOOK)),$(eval EPUB_BOOKS += $(EPUB_BOOKS_DIR)/$(SPHINX_DIR)/$(SPHINX_EPUB))) \
	)

epub_books: $(EPUB_BOOKS)
	@echo $@ completed.

complete_site_books: 	$(PDF_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).pdf \
			$(EPUB_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).epub
	@echo $@ completed.

all_books: pdf_books epub_books
	@echo $@ completed.

cleanall:
	@rm -rf "$(BUILDDIR)"
	@rm -f $(BUILD_MANIFEST)

cleantest:
	@rm -rf "$(TEST_BUILDDIR)"


# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	  export SPHINX_LOCAL_BUILD=true; \
	$(SPHINXBUILD) -M $@ $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help test_content test_engine \
	pdf_books epub_books $(BOOKS)
