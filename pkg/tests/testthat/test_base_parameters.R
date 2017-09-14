library(evonet)
context("Default parameter values")

evoparams <- input_parameters_primary()
test_that("defaul initial population>=100", {
  expect_equal(evoparams$initial_pop , 100)
})
