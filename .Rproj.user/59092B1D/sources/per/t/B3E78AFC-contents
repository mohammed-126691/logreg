# File: tests/testthat/test-compute_likelihood.R

test_that("compute_likelihood works correctly", {
  # Example data
  example_data <- data.frame(
    outcome = c(1, 0, 1, 0),
    age = c(50, 40, 60, 30),
    marker = c(5, 3, 6, 4)
  )

  # Fit the logistic model
  fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))

  # Compute likelihood
  likelihood <- compute_likelihood(fitted_model, example_data, "outcome")

  # Check that likelihood is a positive numeric value
  expect_type(likelihood, "double")
  expect_true(likelihood > 0)
})
