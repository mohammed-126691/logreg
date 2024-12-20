#' Evaluate Performance Metrics
#'
#' This function evaluates a logistic regression model using sensitivity, specificity, and AUC.
#' @param model A fitted logistic regression model object.
#' @param data A data frame containing predictors and the outcome variable.
#' @param outcome The name of the binary outcome variable.
#' @param threshold The probability threshold for classification. Default is 0.5.
#' @return A list containing sensitivity, specificity, and AUC.
#' @examples
#' example_data <- data.frame(
#'   outcome = rbinom(100, 1, 0.5),
#'   age = rnorm(100, mean = 50, sd = 10),
#'   marker = rnorm(100, mean = 5, sd = 2)
#' )
#' fitted_model <- fit_logit_model(example_data, "outcome", c("age", "marker"))
#' evaluate_performance_metrics(fitted_model, example_data, "outcome")
#' @export
evaluate_performance_metrics <- function(model, data, outcome, threshold = 0.5) {
  predicted_prob <- stats::predict(model, type = "response")
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  actual <- data[[outcome]]

  # Sensitivity and specificity calculations
  tp <- sum(predicted_class == 1 & actual == 1)
  tn <- sum(predicted_class == 0 & actual == 0)
  fp <- sum(predicted_class == 1 & actual == 0)
  fn <- sum(predicted_class == 0 & actual == 1)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)

  # ROC and AUC using namespace-qualified calls
  roc_curve <- pROC::roc(actual, predicted_prob)
  auc_value <- pROC::auc(roc_curve)

  return(list(sensitivity = sensitivity, specificity = specificity, auc = auc_value))
}
