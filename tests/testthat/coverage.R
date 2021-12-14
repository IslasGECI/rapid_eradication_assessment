library(covr)
library(testthat)
cobertura <- file_coverage(
  c(
    "R/do_nothing.R"
  ),
  c(
    "tests/testthat/test_nothing.R"
  )
)
covr::codecov(covertura = cobertura, token = "728351b0-fb40-4b84-8cbf-d1ad18bb9ddd")
