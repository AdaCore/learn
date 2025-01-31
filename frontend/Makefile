# You can set these variables from the command line.
SPHINXOPTS         =
SPHINXBUILD        = sphinx-build
SPHINXPROJ         = learnadacorecom
SPHINXCONF         = sphinx
BUILDDIR           = dist
SRC_TEST_DIR       = $(BUILDDIR)/test_output
TEST_LAMBDA       ?= 0
TEST_LOCAL_DRIVER  = PYTHONPATH="$PYTHONPATH:sphinx:py_modules" python3 tests/compile_blocks.py --keep_files -B $(SRC_TEST_DIR)
TEST_OPTIONS      ?=
TEST_LAMBDA_DRIVER = PYTHONPATH="$PYTHONPATH:sphinx:py_modules" python3 tests/execute_rst_code_blocks.py --halt-on-failure

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
HIDDEN_CONTENTS	?= $(CONTENT_DIR)/hidden_contents.txt

WEBSOCKET_URL_PROD     ?= wss://backend.learn.adacore.com
export CODE_SERVER_URL := wss://sandbox.backend.learn.adacore.com

COMPLETE_SITE_BOOK = learning-ada



help:                                                                          ## Show this help.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m\033[0m\n"}             \
		/^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-60s\033[0m %s\n", $$1, $$2 }     \
		/^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) }                    \
		' $(MAKEFILE_LIST)


file_found = $(or $(and $(wildcard $(1)),1),0)

rst_hidden_files = \
	$(foreach fr,$(shell cat $(1)),$(shell find $(CONTENT_DIR)/$(fr) -type f))

filter_rst_files = \
	$(eval RST_HIDDEN_FILES := $(call rst_hidden_files,$(1)) ) \
	$(foreach f,$(2),\
		$(if $(findstring $(f),$(RST_HIDDEN_FILES)),,$(f)))

get_filtered_rst_files = \
	$(if $(findstring 0,$(call file_found,$(1))),\
		$(2),\
		$(call filter_rst_files,$(1),$(2)))

get_rst_files = \
	$(eval DIR_LEVELS := $(2)) \
	$(if $(findstring 0,$(DIR_LEVELS)),\
		$(eval ALL_RST_FILES := $(wildcard $(CONTENT_DIR)/$(1)/*.rst)), \
		$(eval ALL_RST_FILES := $(wildcard $(CONTENT_DIR)/$(1)/**/*.rst)) \
	) \
	$(call get_filtered_rst_files,$(HIDDEN_CONTENTS),$(ALL_RST_FILES))

define test_content
	$(eval MAX_SRC_COLUMNS := $(1))
	$(eval DIR_LEVELS := $(2))
	$(eval CONTENT_CHAPTERS := $(3))
	$(if $(findstring 1,$(strip $(TEST_LAMBDA))), \
		$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS)), \
		$(eval TEST_OPTIONS_EXT := $(TEST_OPTIONS) --max-columns $(MAX_SRC_COLUMNS)) \
	)
	$(eval RST_FILES := $(call get_rst_files,$(CONTENT_CHAPTERS),$(DIR_LEVELS)))
	@$(TEST_DRIVER) $(TEST_OPTIONS_EXT) $(RST_FILES)
	@echo ""
endef

##@ Test source-code examples of individual courses
test_content_courses_intro-to-ada:                                             ## Test course: Intro to Ada.
	@echo "===== INTRO TO ADA ====="
	$(eval MAX_SRC_COLUMNS := 50)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/intro-to-ada/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_intro-to-spark:                                           ## Test course: Intro to SPARK.
	@echo "===== INTRO TO SPARK ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/intro-to-spark/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_intro-to-embedded-sys-prog:                               ## Test course: Intro to Embedded System Programming.
	@echo "===== INTRO TO EMBEDDED SYS PROG ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/intro-to-embedded-sys-prog/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_advanced-ada:                                             ## Test course: Advanced Ada.
	@echo "===== ADVANCED ADA ====="
	$(eval MAX_SRC_COLUMNS := 50)
	$(eval DIR_LEVELS := 1)
	$(eval CONTENT_CHAPTERS := courses/advanced-ada/parts)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_advanced-spark:                                           ## Test course: Advanced SPARK.
	@echo "===== ADVANCED SPARK ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/advanced-spark/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_ada-in-practice:                                          ## Test course: Ada In Practice.
	@echo "===== ADA IN PRACTICE ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/ada-in-practice/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_whats-new-in-ada-2022:                                    ## Test course: What's New in Ada 2022.
	@echo "===== WHAT'S NEW IN ADA 2022 ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/whats-new-in-ada-2022/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_SPARK_for_the_MISRA_C_Developer:                          ## Test course: SPARK for the MISRA-C Developer.
	@echo "===== SPARK FOR MISRA C DEV ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/SPARK_for_the_MISRA_C_Developer/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_Ada_For_The_CPP_Java_Developer:                           ## Test course: Ada for the C++/Java Developer.
	@echo "===== ADA FOR THE CPP JAVA DEV ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/Ada_For_The_CPP_Java_Developer/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_Ada_For_The_Embedded_C_Developer:                         ## Test course: Ada for the Embedded C Developer.
	@echo "===== ADA FOR THE EMBEDDED C DEV ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/Ada_For_The_Embedded_C_Developer/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_courses_GNAT_Toolchain_Intro:                                     ## Test course: Intro to the GNAT Toolchain.
	@echo "===== GNAT TOOLCHAIN INTRO ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 0)
	$(eval CONTENT_CHAPTERS := courses/GNAT_Toolchain_Intro/chapters)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

test_content_training_examples:                                                ## Test course: training examples (Fundamentals of Ada).
	@echo "===== TRAINING EXAMPLES: FUNDAMENTALS OF ADA ====="
	$(eval MAX_SRC_COLUMNS := 80)
	$(eval DIR_LEVELS := 1)
	$(eval CONTENT_CHAPTERS := training_examples/fundamentals_of_ada)
	$(call test_content,$(MAX_SRC_COLUMNS),$(DIR_LEVELS),$(CONTENT_CHAPTERS))

##@ Test source-code examples of various courses
test_content_courses:                                                          ## Test all published courses.
test_content_courses:  \
	test_content_courses_intro-to-ada \
	test_content_courses_intro-to-spark \
	test_content_courses_intro-to-embedded-sys-prog \
	test_content_courses_advanced-ada \
	test_content_courses_whats-new-in-ada-2022 \
	test_content_courses_SPARK_for_the_MISRA_C_Developer \
	test_content_courses_Ada_For_The_CPP_Java_Developer \
	test_content_courses_Ada_For_The_Embedded_C_Developer \
	test_content_courses_GNAT_Toolchain_Intro
	@echo ""

test_content: test_content_courses                                             ## Test all published courses and create source-code zip file.
	@cd $(SRC_TEST_DIR) \
		&& zip -9 -r $(COMPLETE_SITE_BOOK)_code.zip . \
			-x "*extracted_projects*.json" \
			-x "*block_checks*.json" \
			-x "*/gnatprove/*" \
			-x "*.log" -x "*/latest*"
	@mkdir -p $(BUILDDIR)/zip
	@mv $(SRC_TEST_DIR)/$(COMPLETE_SITE_BOOK)_code.zip $(BUILDDIR)/zip

test_hidden_content:                                                           ## Test all hidden courses.
test_hidden_content: \
	test_content_courses_advanced-spark
#	DEACTIVATED DUE TO FAILING TESTS:
#		test_content_training_examples
	@echo ""

test_all_content: test_content test_hidden_content                             ## Test all (published and hidden) courses.
	@echo ""

##@ General tests
test_engine: webpack-production                                                ## Test the engine.
	@echo "===== ENGINE TEST ====="
	@$(SPHINXBUILD) -M html $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@echo ""

test_parser:                                                                   ## Test the parser.
	@coverage run --source=widget -m unittest discover --start-directory sphinx
	@coverage report --fail-under=90 -m

##@ Build website
publish:                                                                       ## [DEPRECATED] Publish contents to the learn website.
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
	HIDDEN_CONTENTS="$(HIDDEN_CONTENTS)" \
	CODE_SERVER_URL="$(WEBSOCKET_URL_PROD)" \
	GEN_LEARN_SITE=yes \
	$(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

sphinx-sandbox:
	SPHINX_CONF_INI="$(SPHINX_CONF_INI)" \
	SRC_TEST_DIR="$(SRC_TEST_DIR)" \
	HIDDEN_BOOKS="$(HIDDEN_BOOKS)" \
	HIDDEN_CONTENTS="$(HIDDEN_CONTENTS)" \
	$(SPHINXBUILD) -M html $(CONTENT_DIR) \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

# Build the site pointing to 'cloudchecker.learn.r53.adacore.com'
site:                                                                          ## Build the learn website.
site: cleanall test_content webpack-production sphinx-production all_books complete_site_books
	@echo Building $@

site-sandbox:                                                                  ## Build the learn-sandbox website.
site-sandbox: cleanall test_content webpack-sandbox sphinx-sandbox all_books complete_site_books
	@echo Building $@

site-testing: cleantest                                                        ## Build a testing version of the learn website.
	FRONTEND_TESTING=true $(SPHINXBUILD) -M html $(TEST_CONTENT) \
        "$(TEST_BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

BOOKS = $(wildcard $(CONTENT_DIR)/courses/*/.) $(wildcard $(CONTENT_DIR)/labs/*/.)

PDF_BOOKS_DIR := $(BUILDDIR)/pdf_books

lowercase_word = $(shell echo $(1) | tr '[:upper:]' '[:lower:]')

define build_book_preps_pdf
	$(eval SPHINX_DIR := $(1))
	@mkdir -p $(PDF_BOOKS_DIR)/$(SPHINX_DIR)
	@mkdir -p $(BUILDDIR)/latex
endef

define build_book_preps_epub
	$(eval SPHINX_DIR := $(1))
	@mkdir -p $(EPUB_BOOKS_DIR)/$(SPHINX_DIR)
	@rm   -rf $(BUILDDIR)/epub
	@mkdir -p $(BUILDDIR)/epub/_static
endef

define build_book_generic
	$(eval TARGET_NAME := $(1))
	$(eval TARGET_TYPE := $(2))
	$(eval TARGET_TYPE_LC := $(call lowercase_word,$(TARGET_TYPE)))
	$(eval SPHINX_CONTENT_DIR := $(3))
	$(eval SPHINX_CONF_INI := $(shell [ -f ${SPHINX_CONTENT_DIR}/conf.ini ] && echo ${SPHINX_CONTENT_DIR}/conf.ini))
	$(if $(findstring PDF,$(TARGET_TYPE)),$(eval SPHINX_TARGET_TYPE := latexpdf))
	$(if $(findstring PDF,$(TARGET_TYPE)),$(eval SPHINX_OUTPUT := latex/learnadacorecom.pdf))
	$(if $(findstring EPUB,$(TARGET_TYPE)),$(eval SPHINX_TARGET_TYPE := epub))
	$(if $(findstring EPUB,$(TARGET_TYPE)),$(eval SPHINX_OUTPUT := epub/learnadacorecom.epub))
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	  export HIDDEN_CONTENTS="$(HIDDEN_CONTENTS)"; \
	GEN_LEARN_SITE=yes $(SPHINXBUILD) -M $(SPHINX_TARGET_TYPE) $(SPHINX_CONTENT_DIR) \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@mv $(BUILDDIR)/$(SPHINX_OUTPUT) $@
endef

define build_book
	$(eval TARGET_NAME := $(1))
	$(eval TARGET_TYPE := $(2))
	$(eval TARGET_TYPE_LC := $(call lowercase_word,$(TARGET_TYPE)))
	$(eval SPHINX_DIR := $(shell basename $(shell dirname $(TARGET_NAME))))
	$(eval SPHINX_BOOK := $(shell basename $@ .$(TARGET_TYPE_LC)))
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR)/$(SPHINX_DIR)/$(SPHINX_BOOK))
	$(call build_book_preps_$(TARGET_TYPE_LC),$(SPHINX_DIR))
	$(call build_book_generic,$(TARGET_NAME),$(TARGET_TYPE),$(SPHINX_CONTENT_DIR))
endef

define build_complete_book_preps_pdf
	@mkdir -p $(PDF_BOOKS_DIR)
	@mkdir -p $(BUILDDIR)/latex
endef

define build_complete_book_preps_epub
	@mkdir -p $(EPUB_BOOKS_DIR)
	@rm   -rf $(BUILDDIR)/epub
	@mkdir -p $(BUILDDIR)/epub/_static
endef

define build_complete_book
	$(eval TARGET_NAME := $(1))
	$(eval TARGET_TYPE := $(2))
	$(eval TARGET_TYPE_LC := $(call lowercase_word,$(TARGET_TYPE)))
	@rm -rf "$(BUILDDIR)/doctrees"
	$(eval SPHINX_CONTENT_DIR := $(CONTENT_DIR))
	$(call build_book_complete_preps_$(TARGET_TYPE_LC))
	$(call build_book_generic,$(TARGET_NAME),$(TARGET_TYPE),$(SPHINX_CONTENT_DIR))
endef

%.pdf:
	@echo Building $@ PDF
	$(call build_book,$@,PDF)

$(PDF_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).pdf:
	@echo Building $@ PDF
	$(call build_complete_book,$@,PDF)

generate_book_list = \
	$(foreach book_dir, $(BOOKS), \
		$(eval TARGET_TYPE := $(1)) \
		$(eval TARGET_TYPE_LC := $(call lowercase_word,$(TARGET_TYPE))) \
		$(eval SPHINX_DIR := $(shell basename $(shell dirname $(shell dirname ${book_dir})))) \
		$(eval SPHINX_BOOK := $(shell basename $(shell dirname ${book_dir}))) \
		$(eval SPHINX_BOOK_FULL := $(SPHINX_BOOK).$(TARGET_TYPE_LC)) \
		$(eval HIDDEN_BOOKS_FOUND := $(or $(and $(wildcard $(HIDDEN_BOOKS)),1),0)) \
		$(if $(findstring 1,$(HIDDEN_BOOKS_FOUND)), \
			$(eval HIDDEN_BOOK := $(shell grep -c $(SPHINX_DIR)/$(SPHINX_BOOK) $(HIDDEN_BOOKS))), \
			$(eval HIDDEN_BOOK := 0)) \
		$(if $(findstring 0,$(HIDDEN_BOOK)),$(eval $(TARGET_TYPE)_BOOKS += $($(TARGET_TYPE)_BOOKS_DIR)/$(SPHINX_DIR)/$(SPHINX_BOOK_FULL))) \
	)

##@ Generate e-books
# Generate list: PDF_BOOKS
$(call generate_book_list,PDF)

pdf_books: $(PDF_BOOKS)                                                        ## Build all PDF books.
	@echo $@ completed.

EPUB_BOOKS_DIR := $(BUILDDIR)/epub_books

%.epub:
	@echo Building $@ EPUB
	$(call build_book,$@,EPUB)

$(EPUB_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).epub:
	@echo Building $@ PDF
	$(call build_complete_book,$@,EPUB)

# Generate list: EPUB_BOOKS
$(call generate_book_list,EPUB)

epub_books: $(EPUB_BOOKS)                                                      ## Build all EPUB books.
	@echo $@ completed.

complete_site_books:                                                           ## Build single e-books (PDF/EPUB) with the complete website.
complete_site_books: 	$(PDF_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).pdf \
			$(EPUB_BOOKS_DIR)/$(COMPLETE_SITE_BOOK).epub
	@echo $@ completed.

all_books: pdf_books epub_books                                                ## Build all e-books (PDF/EPUB formats).
	@echo $@ completed.

##@ Clean-ups
cleanall:                                                                      ## Delete all local website publishing data.
	@rm -rf "$(BUILDDIR)"
	@rm -f $(BUILD_MANIFEST)

cleantest:                                                                     ## Delete all test data.
	@rm -rf "$(TEST_BUILDDIR)"


# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@export SPHINX_CONF_INI="$(SPHINX_CONF_INI)"; \
	  export SRC_TEST_DIR="$(SRC_TEST_DIR)"; \
	  export HIDDEN_BOOKS="$(HIDDEN_BOOKS)"; \
	  export HIDDEN_CONTENTS="$(HIDDEN_CONTENTS)"; \
	  export SPHINX_LOCAL_BUILD=true; \
	$(SPHINXBUILD) -M $@ $(CONTENT_DIR) \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help test_content test_engine \
	pdf_books epub_books $(BOOKS)
