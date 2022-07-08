init: setup tests

.PHONY: check coverage init mutants setup tests

check:
	R -e "library(styler)" \
	  -e "resumen <- style_dir('R')" \
	  -e "resumen <- rbind(resumen, style_dir('src'))" \
	  -e "resumen <- rbind(resumen, style_dir('tests'))" \
	  -e "any(resumen[[2]])" \
	  | grep FALSE

clean:
	rm --force --recursive RapidEradicationAssessment.Rcheck
	rm --force --recursive tests/testthat/_snaps
	rm --force RapidEradicationAssessment_*.tar.gz
	rm --force NAMESPACE

coverage: setup
	Rscript tests/testthat/coverage.R
	
mutants: tests
	@echo "🙁🏹 No mutation testing on R 👾🎉👾"

run:
	Rscript src/pairwise_simulations.R

setup:
	R -e "devtools::document()" && \
	R CMD build . && \
	R CMD check RapidEradicationAssessment_0.1.0.tar.gz && \
	R CMD INSTALL RapidEradicationAssessment_0.1.0.tar.gz

tests:
	Rscript -e "devtools::test()"
