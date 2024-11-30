survival_plot <- function(data, time_col, status_col, group_col) {

  # Ensure that status_col is numeric (0 or 1 for alive/dead)
  if (!is.numeric(data[[status_col]])) {
    stop("The status column must be numeric (0 or 1).")
  }

  # Create the survival object
  surv_object <- tryCatch({
    message("Creating survival object...")
    survival::Surv(time = data[[time_col]], event = data[[status_col]])
  }, error = function(e) {
    stop("Error creating Surv object: ", e$message)
  })

  # Print to check if surv_object is created
  message("Surv object created successfully: ", class(surv_object))

  # Fit the survival model based on the grouping factor
  fit <- tryCatch({
    message("Fitting the survival model...")
    survival::survfit(surv_object ~ data[[group_col]], data = data)
  }, error = function(e) {
    stop("Error fitting survival model: ", e$message)
  })

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
