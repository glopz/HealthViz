#' Plot an epidemic curve with both bars and a trend line
#'
#' @param data A data frame containing the date and case count columns.
#' @param date_col The name of the column with date information.
#' @param count_col The name of the column with case counts.
#' @return A ggplot object representing the epidemic curve with a line.
#' @importFrom ggplot2 ggplot aes geom_bar geom_line scale_x_date labs theme_minimal
#' @importFrom rlang .data
#' @export
epidemic_curve <- function(data, date_col, count_col) {
  # Ensure columns are properly handled
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[date_col]], y = .data[[count_col]])) +
    # Bar plot to show the counts for each date
    ggplot2::geom_bar(stat = "identity", fill = "skyblue", alpha = 0.6) +
    # Line plot to show the trend of cases over time
    ggplot2::geom_line(color = "red", size = 1) +
    # Customize the x-axis for date formatting
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    # Add labels and titles
    ggplot2::labs(
      x = "Date",
      y = "Cases",
      title = "Epidemic Curve with Trend Line"
    ) +
    ggplot2::theme_minimal()
}
