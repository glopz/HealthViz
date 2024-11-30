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

survival_plot <- function(data, time_col, status_col) {
  fit <- survfit(Surv(data[[time_col]], data[[status_col]]) ~ 1, data = data)
  ggsurvplot(fit, data = data, xlab = "Time", ylab = "Survival Probability", title = "Kaplan-Meier Survival Plot")
}
