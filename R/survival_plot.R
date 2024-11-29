#' Kaplan-Meier Survival Plot
#'
#' Creates Kaplan-Meier survival curves with optional stratification.
#'
#' @param data A data frame containing the survival data.
#' @param time_col The column name for time-to-event data.
#' @param event_col The column name for event status (1 for event, 0 for censor).
#' @param group_col An optional column name for group stratification.
#'
#' @return A ggplot2 object showing the survival curves.
#' @export
survival_plot <- function(data, time_col, event_col, group_col = NULL) {
  surv_obj <- Surv(data[[time_col]], data[[event_col]])
  
  if (!is.null(group_col)) {
    fit <- survfit(surv_obj ~ data[[group_col]])
    g <- ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE, risk.table = TRUE)
  } else {
    fit <- survfit(surv_obj ~ 1)
    g <- ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE, risk.table = TRUE)
  }
  
  return(g)
}