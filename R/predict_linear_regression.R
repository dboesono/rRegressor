#' Predict Using Linear Regression Model
#'
#' Predicts the response variable using a linear regression model
#'
#' @param data dataframe containing the feature variables
#' @param lm_fit a list containing the model object and fitted values
#' @param x_vars character vector of variable names to be used as predictors
#' @param intercept logical value indicating whether to include an intercept term in the model
#'
#' @return a numeric vector of predicted values
#'
#' @examples
#' data(mtcars)
#' lm_fit <- fit_linear_regression(data = mtcars, x_vars = c("wt"), y_var = "mpg")
#' lm_pred <- predict_linear_regression(data = mtcars, lm_fit, x_vars = c("wt"))
#' lm_pred # display the predicted values
#'
#' @export
predict_linear_regression <- function(data, lm_fit, x_vars, intercept = TRUE) {
  # Check if the input data is a data frame
  if(!is.data.frame(data)){
    stop("Input data must be a data frame")
  }

  # check that x_vars is character strings
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }

  # extract the beta coefficients and add intercept term if necessary
  beta <- lm_fit$model$coefficients
  if (intercept) {
    x <- cbind(1, data[, x_vars, drop = FALSE])
  } else {
    x <- data[, x_vars, drop = FALSE]
  }

  # convert x to matrix format
  x <- as.matrix(x)

  # predict using the fitted model
  fitted_values <- x %*% beta

  # return the predicted values
  return(fitted_values)
}
