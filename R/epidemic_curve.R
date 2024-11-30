#' Plot an epidemic curve
#'
#' @param data A data frame containing the date and case count columns.
#' @param date_col The name of the column with date information.
#' @param count_col The name of the column with case counts.
#' @return A ggplot object representing the epidemic curve.
#' @importFrom ggplot2 ggplot aes_string geom_bar scale_x_date labs theme_minimal
#' @export
epidemic_curve <- function(data, date_col, count_col) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = date_col, y = count_col)) +
    ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(
      x = "Date",
      y = "Cases",
      title = "Epidemic Curve"
    ) +
    ggplot2::theme_minimal()
}

