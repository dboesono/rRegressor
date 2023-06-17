#' Predict Using Ridge Regression Model
#'
#' Predicts the response variable using a Ridge regression model
#'
#' @param data dataframe containing the feature variables
#' @param ridge_fit a list containing the model object and fitted values
#' @param x_vars character vector of variable names to be used as predictors
#'
#' @return a numeric vector of predicted values
#'
#' @examples
#' data(mtcars)
#' ridge_fit <- fit_ridge_regression(data = mtcars, x_vars = c("wt"), y_var = "mpg", lambda = 0.1)
#' ridge_pred <- predict_ridge_regression(data = mtcars, ridge_fit, x_vars = c("wt"))
#' ridge_pred # display the predicted values
#'
#' @export
predict_ridge_regression <- function(data, ridge_fit, x_vars) {
  # Check if the input data is a data frame
  if(!is.data.frame(data)){
    stop("Input data must be a data frame")
  }

  # check that x_vars is character strings
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }

  # extract the beta coefficients and add intercept term
  beta <- ridge_fit$model$coefficients
  x <- cbind(1, data[, x_vars, drop = FALSE])

  # convert x to matrix format
  x <- as.matrix(x)

  # predict using the fitted model
  fitted_values <- x %*% beta

  # return the predicted values
  return(fitted_values)
}
