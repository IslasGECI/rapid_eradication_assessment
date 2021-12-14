.PHONY: tests

check:
	R -e "library(styler)" \
	  -e "resumen <- style_dir('src')" \
	  -e "any(resumen[[2]])" \
	  | grep FALSE

coverage: tests

mutants: tests
	@echo "🙁🏹 No mutation testing on R 👾🎉👾"

tests:
	R -e "devtools::test()"