library(testthat)


describe("Test all is ready", {
  it("return one", {
    expected <- 1
    obtained <- return_one()
    expect_equal(expected, obtained)
  })
})

describe("Get version of the module", {
  it("The version is 0.1.0", {
    expected_version <- c("0.1.0")
    obtained_version <- packageVersion("RapidEradicationAssessment")
    version_are_equal <- expected_version == obtained_version
    expect_true(version_are_equal)
  })
})