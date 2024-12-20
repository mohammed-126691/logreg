# File: tests/testthat/test-fit_logit_model.R

test_that("fit_logit_model works correctly", {
  # Example data
  example_data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    age = rnorm(100, mean = 50, sd = 10),
    marker = rnorm(100, mean = 5, sd = 2)
  )

  # Fit the logistic model
  fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))

  # Check that the model is of class "glm"
  expect_s3_class(fitted_model, "glm")

  # Check that coefficients are not null
  expect_true(!is.null(coef(fitted_model)))

  # Check that the model includes the expected predictors
  predictors <- names(coef(fitted_model))
  expect_true(all(c("(Intercept)", "age", "marker") %in% predictors))
})
