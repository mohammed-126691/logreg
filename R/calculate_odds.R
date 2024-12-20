#' Calculate Odds and Log-Odds
#'
#' This function computes odds and log-odds from predicted probabilities.
#' @param predicted_prob A numeric vector of predicted probabilities.
#' @return A list containing odds and log-odds.
#' @examples
#' predicted_prob <- c(0.2, 0.5, 0.8)
#' odds_results <- calculate_odds(predicted_prob)
#' odds_results
#' @export
calculate_odds <- function(predicted_prob) {
  odds <- predicted_prob / (1 - predicted_prob)
  log_odds <- log(odds)
  return(list(odds = odds, log_odds = log_odds))
}
