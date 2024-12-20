#' Compute Likelihood Function
#'
#' This function computes the likelihood of a logistic regression model.
#' @param model A fitted logistic regression model object.
#' @param data A data frame containing the predictors and outcome variable.
#' @param outcome The name of the binary outcome variable.
#' @return The computed likelihood value.
#' @examples
#' example_data <- data.frame(
#'   outcome = rbinom(100, 1, 0.5),
#'   age = rnorm(100, mean = 50, sd = 10),
#'   marker = rnorm(100, mean = 5, sd = 2)
#' )
#' fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))
#' compute_likelihood(fitted_model, example_data, "outcome")
#' @export
compute_likelihood <- function(model, data, outcome) {
  predicted_prob <- stats::predict(model, type = "response")
  predicted_prob <- pmax(pmin(predicted_prob, 1 - 1e-15), 1e-15)  # Avoid 0 or 1 probabilities
  actual <- data[[outcome]]
  likelihood <- prod(predicted_prob^actual * (1 - predicted_prob)^(1 - actual))
  return(likelihood)
}
