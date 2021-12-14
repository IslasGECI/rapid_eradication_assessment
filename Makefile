.PHONY: tests

check:
	R -e "library(styler)" \
	  -e "resumen <- style_dir('src')" \
	  -e "any(resumen[[2]])" \
	  | grep FALSE

clean:
	rm --force --recursive RapidEradicationAssessment.Rcheck
	rm --force --recursive tests/testthat/_snaps
	rm --force RapidEradicationAssessment_*.tar.gz
	rm --force NAMESPACE

coverage: tests

mutants: tests
	@echo "🙁🏹 No mutation testing on R 👾🎉👾"

setup:
	R -e "devtools::document()" && \
	R CMD build . && \
	R CMD check RapidEradicationAssessment_0.1.0.tar.gz && \
	R CMD INSTALL RapidEradicationAssessment_0.1.0.tar.gz

tests:
	Rscript -e "devtools::test()"