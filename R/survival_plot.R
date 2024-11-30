#' Create a survival plot
#'
#' This function generates a Kaplan-Meier survival plot using survival data.
#'
#' @param data A data frame containing survival time and status information.
#' @param time_col The name of the column for survival time.
#' @param status_col The name of the column for survival status (numeric or logical).
#' @param group_col The name of the column for grouping (e.g., treatment group).
#' @return A Kaplan-Meier survival plot.
#' @name survival_plot
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @export
utils::globalVariables(c("time", "surv"))

survival_plot <- function(data, time_col, event_col, group_col) {
  # Ensure that the 'status' column is numeric or logical for the Surv object
  data[[event_col]] <- as.numeric(data[[event_col]])

  # Create the Surv object for Kaplan-Meier analysis
  surv_object <- survival::Surv(data[[time_col]], data[[event_col]])

  # Fit the survival curve using the group column for stratification
  fit <- survival::survfit(surv_object ~ data[[group_col]], data = data)

  # Plot the survival curve using ggplot2
  survminer::ggsurvplot(
    fit,
    data = data,
    conf.int = TRUE,
    pval = TRUE,
    ggtheme = ggplot2::theme_minimal(),
    title = "Kaplan-Meier Survival Curve",
    xlab = "Time",
    ylab = "Survival Probability"
  )
}
