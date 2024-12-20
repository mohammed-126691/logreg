# File: tests/testthat/test-calculate_odds.R

test_that("calculate_odds works correctly", {
  # Example probabilities
  predicted_prob <- c(0.1, 0.5, 0.8)

  # Calculate odds and log-odds
  results <- calculate_odds(predicted_prob)

  # Check that the odds are calculated correctly
  expect_equal(results$odds, predicted_prob / (1 - predicted_prob))

  # Check that the log-odds are calculated correctly
  expect_equal(results$log_odds, log(results$odds))
})
