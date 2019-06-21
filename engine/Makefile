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
	@$(TEST_DRIVER) ../courses/intro-to-spark/book/*.rst
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

tests: test_ada_intro test_spark_intro test_advanced_ada test_advanced_spark \
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
	rm -rf "$(BUILDDIR)"
	CODE_SERVER_URL="Http://localhost:8000" $(SPHINXBUILD) -M html "../" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"
	# link the real editors.js to the generated location, to ease work
	rm _build/html/_static/js/editors.js
	ln -s ../../../../sphinx/learn_theme/static/js/editors.js _build/html/_static/js/

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "../" \
	"$(BUILDDIR)" $(SPHINXOPTS) $(O) -v -c "$(SPHINXCONF)"

.PHONY: Makefile help tests
