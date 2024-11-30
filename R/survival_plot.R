#' Create a Kaplan-Meier survival plot
#'
#' This function generates a Kaplan-Meier survival plot using survival data.
#'
#' @param data A data frame containing survival time and status information.
#' @param time_col The name of the column for survival time.
#' @param event_col The name of the column for survival status (1 = event occurred, 0 = censored).
#' @param group_col The name of the column for group classification.
#' @return A Kaplan-Meier survival plot.
#' @name survival_plot
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @importFrom ggplot2 theme_minimal
#' @export
utils::globalVariables(c("time", "surv"))

survival_plot <- function(data, time_col, event_col, group_col) {
  # Check if the columns exist in the data
  if (!(time_col %in% names(data))) {
    stop(paste("Column", time_col, "not found in the data"))
  }
  if (!(event_col %in% names(data))) {
    stop(paste("Column", event_col, "not found in the data"))
  }
  if (!(group_col %in% names(data))) {
    stop(paste("Column", group_col, "not found in the data"))
  }

  # Create a survival object
  surv_object <- survival::Surv(data[[time_col]], data[[event_col]])

  # Fit the survival model
  fit <- survival::survfit(surv_object ~ data[[group_col]], data = data)

  # Generate the Kaplan-Meier survival plot
  survminer::ggsurvplot(
    fit,
    data = data,
    conf.int = TRUE,          # Show confidence intervals
    pval = TRUE,              # Show p-value
    ggtheme = ggplot2::theme_minimal(),  # Minimal theme for clean look
    title = "Kaplan-Meier Survival Curve",
    xlab = "Time",
    ylab = "Survival Probability",
    legend.title = group_col, # Add legend title
    legend.labs = unique(data[[group_col]]) # Use the unique group labels
  )
}
