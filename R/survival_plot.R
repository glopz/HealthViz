#' Create a survival plot
#'
#' This function generates a Kaplan-Meier survival plot using survival data.
#'
#' @param data A data frame containing survival time and status information.
#' @param time_col The name of the column for survival time.
#' @param status_col The name of the column for survival status.
#' @param group_col The name of the column for grouping.
#' @return A Kaplan-Meier survival plot.
#' @importFrom survival Surv survfit
#' @importFrom survminer ggsurvplot
#' @importFrom ggplot2 theme_minimal
#' @export
survival_plot <- function(data, time_col, status_col, group_col) {

  # Check if the necessary columns exist in the data
  if (!all(c(time_col, status_col, group_col) %in% names(data))) {
    stop("The provided column names are not present in the data.")
  }

  # Convert status to a logical/numeric format if it's not
  if (!is.numeric(data[[status_col]]) && !is.logical(data[[status_col]])) {
    data[[status_col]] <- ifelse(data[[status_col]] == "alive", 1, 0)
  }

  # Create a survival object using the Surv function
  surv_object <- survival::Surv(time = data[[time_col]], event = data[[status_col]])

  # Fit the survival model based on the grouping factor
  fit <- survival::survfit(surv_object ~ data[[group_col]], data = data)

  # Plot the Kaplan-Meier survival curve
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
