#' Prevalence Rate Plot
#'
#' This function calculates and visualizes prevalence rates across categories or time periods.
#'
#' @param data A data frame containing grouping and outcome columns.
#' @param group_col The name of the column for grouping.
#' @param outcome_col The name of the column for outcomes.
#' @return A ggplot object representing prevalence rates.
#' @name prevalence_rate
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#' @importFrom dplyr group_by_at summarize ungroup
#' @importFrom dplyr %>%
#' @export
utils::globalVariables(c(".data"))

prevalence_rate <- function(data, group_col, outcome_col) {
  summary_data <- data %>%
    dplyr::group_by_at(group_col) %>%
    dplyr::summarize(
      Prevalence = mean(.data[[outcome_col]], na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[group_col]], y = Prevalence)) +
    ggplot2::geom_bar(stat = "identity", fill = "blue") +
    ggplot2::labs(
      x = "Group",
      y = "Prevalence Rate",
      title = "Prevalence Rates by Group"
    ) +
    ggplot2::theme_minimal()
}
