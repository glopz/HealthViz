#' Prevalence Rate Calculation and Plot
#'
#' Calculates and visualizes prevalence rates across categories or over time.
#'
#' @param data A data frame containing the data.
#' @param group_col The column name for grouping variable.
#' @param outcome_col The column name for the outcome (1 for positive cases, 0 for negative cases).
#' @return A ggplot2 object showing prevalence rates.
#' @importFrom dplyr %>% group_by_at summarize ungroup
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#' @importFrom ggplot2 aes_string
#' @export
prevalence_rate <- function(data, group_col, outcome_col) {
  prevalence_data <- data %>%
    group_by_at(group_col) %>%
    summarize(prevalence = mean(get(outcome_col), na.rm = TRUE)) %>%
    ungroup()

  ggplot(prevalence_data, aes_string(x = group_col, y = "prevalence")) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = "Prevalence Rate", x = group_col, y = "Prevalence Rate") +
    theme_minimal()
}
