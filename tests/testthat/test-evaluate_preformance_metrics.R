# File: tests/testthat/test-evaluate_performance_metrics.R

test_that("evaluate_performance_metrics works correctly", {
  # Example data
  example_data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    age = rnorm(100, mean = 50, sd = 10),
    marker = rnorm(100, mean = 5, sd = 2)
  )

  # Fit the logistic model
  fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))

  # Evaluate performance metrics
  metrics <- evaluate_performance_metrics(fitted_model, example_data, "outcome", threshold = 0.5)

  # Check that sensitivity and specificity are within [0, 1]
  expect_true(metrics$sensitivity >= 0 && metrics$sensitivity <= 1)
  expect_true(metrics$specificity >= 0 && metrics$specificity <= 1)

  # Check that AUC is within [0, 1]
  expect_true(metrics$auc >= 0 && metrics$auc <= 1)
})
