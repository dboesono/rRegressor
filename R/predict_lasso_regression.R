#' Predict using Lasso Regression
#'
#' This function predicts response variable values using a fitted Lasso regression model.
#'
#' @param fit A list containing the fitted coefficients and intercept from the fit_lasso_regression function
#' @param new_data A data frame containing the predictor variables for which predictions are to be made
#' @param x_vars A character vector of column names corresponding to the predictor variables
#' @return A numeric vector of predicted response variable values
#' @examples
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' p <- 5
#' data <- data.frame(matrix(rnorm(n * p), n, p))
#' colnames(data) <- paste0("x", 1:p)
#' data$y <- data$x1 + data$x2 + rnorm(n)
#'
#' # Fit the Lasso regression using the function
#' fit <- fit_lasso_regression(data, x_vars=c("x1", "x2", "x3", "x4", "x5"), y_var="y", lambda=0.1)
#'
#' # Make predictions using the fitted model
#' predictions <- predict_lasso_regression(fit, new_data=data, x_vars=c("x1", "x2", "x3", "x4", "x5"))
#' @export
predict_lasso_regression <- function(fit, new_data, x_vars) {
  # Check if the input new_data is a data frame
  if(!is.data.frame(new_data)){
    stop("new_data must be a data frame")
  }

  # check that x_vars is character strings
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }

  # Extract the predictor variables
  X <- new_data[, x_vars]
  X <- scale(X)

  # Compute the predicted response variable values
  y_pred <- fit$intercept + X %*% fit$coefficients

  # Return the predictions
  return(y_pred)
}
