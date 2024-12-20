#' Fit Logistic Regression Model
#'
#' This function fits a logistic regression model for binary classification.
#' @param data A data frame containing the predictors and outcome variable.
#' @param outcome The name of the binary outcome variable (as a string).
#' @param predictors A character vector of predictor variable names.
#' @return A fitted logistic regression model object.
#' @examples
#' example_data <- data.frame(
#'   outcome = rbinom(100, 1, 0.5),
#'   age = rnorm(100, mean = 50, sd = 10),
#'   marker = rnorm(100, mean = 5, sd = 2)
#' )
#' fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))
#' summary(fitted_model)
#' @export
fit_logit_model <- function(data, outcome, predictors) {
  formula <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = "+")))
  model <- stats::glm(formula, data = data, family = stats::binomial(link = "logit"))
  return(model)
}
