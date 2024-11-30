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
utils::globalVariables(c(".data", "Group", "Risk"))

risk_ratio <- function(data, exposure_col, outcome_col) {
  summary_data <- data %>%
    dplyr::group_by_at(exposure_col) %>%
    dplyr::summarize(Risk = mean(.data[[outcome_col]], na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Group = .data[[exposure_col]])

  ggplot2::ggplot(summary_data, ggplot2::aes(x = Group, y = Risk)) +
    ggplot2::geom_bar(stat = "identity", fill = "coral") +
    ggplot2::labs(x = "Exposure Group", y = "Risk Ratio", title = "Risk Ratios by Exposure Group") +
    ggplot2::theme_minimal()
}
