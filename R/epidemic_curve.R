#' Generate an Epidemic Curve
#'
#' Creates an epidemic curve for tracking case counts over time.
#'
#' @param data A data frame containing the outbreak data.
#' @param date_col The column name for dates.
#' @param case_col The column name for case counts.
#' @param bin_width The width of the bins for the date axis (default: 7 days).
#'
#' @return A ggplot2 object showing the epidemic curve.
#' @export
epidemic_curve <- function(data, date_col, case_col, bin_width = 7) {
  ggplot(data, aes_string(x = date_col, y = case_col)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(title = "Epidemic Curve", x = "Date", y = "Number of Cases") +
    theme_minimal()
}