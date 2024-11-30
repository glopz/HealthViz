#' Risk Ratio Plot
#'
#' This function computes and visualizes risk ratios for exposure and outcome groups.
#'
#' @param data A data frame containing exposure and outcome columns.
#' @param exposure_col The name of the column for exposure groups.
#' @param outcome_col The name of the column for outcomes.
#' @return A ggplot object representing risk ratios.
#' @name risk_ratio
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#' @importFrom dplyr mutate group_by_at summarize ungroup
#' @export
utils::globalVariables(c(".data", "Risk"))

risk_ratio <- function(data, group_col, cases_col, total_col) {

  # Calculate the risk for each group
  summary_data <- data %>%
    dplyr::group_by_at(group_col) %>%
    dplyr::summarize(
      Risk = sum(.data[[cases_col]], na.rm = TRUE) / sum(.data[[total_col]], na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Calculate the risk ratio (comparing groups)
  risk_ratio_value <- summary_data$Risk[1] / summary_data$Risk[2]

  # Print Risk Ratio
  print(paste("Risk Ratio between groups: ", risk_ratio_value))

  # Plotting
  ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[group_col]], y = Risk, fill = .data[[group_col]])) +
    ggplot2::geom_bar(stat = "identity", width = 0.7) +
    ggplot2::labs(
      x = "Group",
      y = "Risk",
      title = "Risk Ratios by Group"
    ) +
    ggplot2::theme_minimal()
}
