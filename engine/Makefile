# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = learnadacorecom
SPHINXCONF    = sphinx
BUILDDIR      = _build
TEST_DRIVER   = python tests/compile_blocks.py


help:
	@echo "make tests  - run the testsuite"
	@echo "make test_engine  - generate the engine test html"
	@echo "make html   - generates the html"
	@echo "make latexpdf - generates the PDF document"
	@echo "make pdf_books - generates the PDF document for each course and labs"

test_engine:
	@echo "===== ENGINE TEST ====="
	@$(SPHINXBUILD) -M html "tests/" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@echo ""

test_ada_intro:
	@echo "===== INTRO TO ADA ====="
	@$(TEST_DRIVER) ../courses/intro-to-ada/chapters/*.rst
	@echo ""

test_spark_intro:
	@echo "===== INTRO TO SPARK ====="
	@$(TEST_DRIVER) ../courses/intro-to-spark/chapters/*.rst
	@echo ""

test_advanced_ada:
	@echo "===== ADVANCED ADA ====="
	@$(TEST_DRIVER) ../courses/advanced-ada/chapters/*.rst
	@echo ""

test_advanced_spark:
	@echo "===== ADVANCED SPARK ====="
	@$(TEST_DRIVER) ../courses/advanced-spark/chapters/*.rst
	@echo ""

test_spark_for_the_misra_c_developer:
	@echo "===== SPARK FOR MISRA C DEV ====="
	@$(TEST_DRIVER) ../courses/SPARK_for_the_MISRA_C_Developer/chapters/*.rst
	@echo ""

test_ada_for_the_cpp_java_developer:
	@echo "===== ADA FOR THE CPP JAVA DEV ====="
	@$(TEST_DRIVER) ../courses/Ada_For_The_CPP_Java_Developer/chapters/*.rst
	@echo ""

test_gnat_toolchain_intro:
	@echo "===== GNAT TOOLCHAIN INTRO ====="
	@$(TEST_DRIVER) ../courses/GNAT_Toolchain_Intro/chapters/*.rst
	@echo ""

tests: test_ada_intro test_spark_intro test_advanced_ada test_advanced_spark \
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
local: cleanall
	CODE_SERVER_URL="Http://localhost:8000" $(SPHINXBUILD) -M html "../" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	# link the real editors.js to the generated location, to ease work
	rm $(BUILDDIR)/html/_static/js/editors.js
	ln -s ../../../../sphinx/learn_theme/static/js/editors.js $(BUILDDIR)/html/_static/js/

# Build the site pointing to 'cloudchecker.r53.adacore.com'
site: cleanall
	CODE_SERVER_URL="https://cloudchecker.r53.adacore.com" $(SPHINXBUILD) -M html "../" \
        "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

BOOKS = $(wildcard ../courses/*/.) $(wildcard ../labs/*/.)

pdf_books: $(BOOKS)
$(BOOKS):
	$(eval SPHINX_DIR := $(shell dirname $@ | awk -F/ '{ print $$2 }' ))
	$(eval SPHINX_PDF := $(shell basename $(shell dirname $@)).pdf)
	$(eval SPHINX_TITLE := $(shell cat $@/conf.txt | grep title | sed -e 's#title=##'))
	@mkdir -p $(BUILDDIR)/pdf_books/$(SPHINX_DIR)
	@ export SPHINX_TITLE="$(SPHINX_TITLE)"; \
	$(SPHINXBUILD) -M latexpdf $@ \
	   "$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	@ mv $(BUILDDIR)/latex/learnadacorecom.pdf $(BUILDDIR)/pdf_books/$(SPHINX_DIR)/$(SPHINX_PDF)

cleanall:
	@ rm -rf "$(BUILDDIR)"

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "../" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help tests pdf_books $(BOOKS)
