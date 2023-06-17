#' Plot Actual vs. Predicted Values
#'
#' This function creates a scatterplot of actual vs. predicted values for a given regression model
#' @import ggplot2
#' @param y_actual A numeric vector of the actual response variable values
#' @param y_pred A numeric vector of the predicted response variable values
#' @param model_name A character string representing the name of the regression model used for predictions
#' @return A ggplot object displaying the scatterplot of actual vs. predicted values, or an error message if an exception occurs
#' @export
plot_y_actual_vs_predicted <- function(y_actual, y_pred, model_name) {
  # Input validation using stopifnot
  stopifnot(
    is.double(y_actual) || is.numeric(y_actual),
    is.double(y_pred) || is.numeric(y_pred),
    is.character(model_name),
    length(y_actual) == length(y_pred)
  )

  # Check if ggplot2 is installed, if not, install it
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }

  # Load ggplot2
  library(ggplot2)

  # Create a data frame with the actual and predicted values
  plot_data <- data.frame(
    Actual = y_actual,
    Predicted = y_pred
  )
  colnames(plot_data) <- c("Actual", "Predicted")

  # Create the scatterplot using ggplot2
  plot <- ggplot(data = plot_data, aes(Actual, Predicted)) + geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title = paste("Actual vs. Predicted Values for", model_name),
      x = "Actual Values",
      y = "Predicted Values"
    ) +
    theme_minimal()

  return(plot)
}
