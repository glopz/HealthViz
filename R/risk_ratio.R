#' Risk Ratio Calculation and Plot
#'
#' Computes and visualizes the risk ratio for binary outcome data.
#'
#' @param data A data frame containing the data.
#' @param exposure_col The column name for exposure (1 for exposed, 0 for unexposed).
#' @param outcome_col The column name for outcome (1 for cases, 0 for non-cases).
#'
#' @return A ggplot2 object showing the risk ratio plot.
#' @export
risk_ratio <- function(data, exposure_col, outcome_col) {
  table <- table(data[[exposure_col]], data[[outcome_col]])
  
  risk_exposed <- table[2, 2] / sum(table[2, ])
  risk_unexposed <- table[1, 2] / sum(table[1, ])
  rr <- risk_exposed / risk_unexposed
  
  cat("Risk Ratio:", rr, "\n")
  
  risk_data <- data.frame(
    Group = c("Exposed", "Unexposed"),
    Risk = c(risk_exposed, risk_unexposed)
  )
  
  ggplot(risk_data, aes(x = Group, y = Risk, fill = Group)) +
    geom_bar(stat = "identity") +
    labs(title = "Risk Ratio Plot", x = "Group", y = "Risk") +
    theme_minimal()
}