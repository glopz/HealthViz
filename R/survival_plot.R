#' Create a survival plot
#'
#' This function generates a Kaplan-Meier survival plot using survival data.
#'
#' @param data A data frame containing survival time and status information.
#' @param time_col The name of the column for survival time.
#' @param status_col The name of the column for survival status.
#' @return A Kaplan-Meier survival plot.
#' @name survival_plot
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @export
utils::globalVariables(c("time", "surv"))

survival_plot <- function(data, time_col, event_col, group_col) {
  surv_object <- survival::Surv(data[[time_col]], data[[event_col]])
  fit <- survival::survfit(surv_object ~ data[[group_col]], data = data)

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
