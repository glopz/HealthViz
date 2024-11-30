#' @name survival_plot
#' @title Kaplan-Meier Survival Plot
#' @description Creates Kaplan-Meier survival curves with optional stratification.
#'
#' @param data A data frame containing the survival data.
#' @param time_col The column name for time-to-event data.
#' @param event_col The column name for event status (1 for event, 0 for censor).
#' @param group_col An optional column name for group stratification.
#' @return A ggplot2 object showing the survival curves.
#' @importFrom survival Surv survfit
#' @importFrom stats time
#' @importFrom ggplot2 ggplot aes geom_step labs theme_minimal
#' @export
utils::globalVariables(c("surv", "time"))
survival_plot <- function(data, time_col, event_col, group_col = NULL) {
  surv_obj <- Surv(data[[time_col]], data[[event_col]])

  if (!is.null(group_col)) {
    fit <- survfit(surv_obj ~ data[[group_col]])
    ggplot(data = as.data.frame(fit$surv), aes(x = time, y = surv)) +
      geom_step() +
      labs(
        title = "Kaplan-Meier Survival Curve",
        x = "Time",
        y = "Survival Probability"
      ) +
      theme_minimal()
  } else {
    fit <- survfit(surv_obj ~ 1)
    ggplot(data = as.data.frame(fit$surv), aes(x = time, y = surv)) +
      geom_step() +
      labs(
        title = "Kaplan-Meier Survival Curve (Unstratified)",
        x = "Time",
        y = "Survival Probability"
      ) +
      theme_minimal()
  }
}
