#' Fit Ridge Regression Model
#'
#' Fits a Ridge regression model to the given data
#'
#' @param data dataframe containing the feature and label variables
#' @param x_vars character vector of variable names to be used as predictors
#' @param y_var character string of the variable name to be used as the response variable
#' @param lambda the value of the regularization parameter
#' @param intercept logical value indicating whether to include an intercept term in the model
#'
#' @return a list containing the model object and fitted values
#'
#' @examples
#' data(mtcars)
#' ridge_fit <- fit_ridge_regression(data = mtcars, x_vars = c("wt"), y_var = "mpg", lambda = 0.1)
#' ridge_fit$model # display the model object
#' ridge_fit$fitted_values # display the fitted values
#'
#' @export
fit_ridge_regression <- function(data, x_vars, y_var, lambda, intercept = TRUE) {
  # Check if the input data is a data frame
  if(!is.data.frame(data)){
    stop("Input data must be a data frame")
  }

  # check that x_vars and y_var are character strings
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }

  if (!is.character(y_var)) {
    stop("y_var must be a character string.")
  }

  # extract the x and y variables from the data argument
  if(!all(x_vars %in% colnames(data))){
    stop("x_vars not found in data")
  }

  if(!all(y_var %in% colnames(data))){
    stop("y_var not found in data")
  }

  x <- as.matrix(data[, x_vars, drop = FALSE])
  y <- as.matrix(data[, y_var, drop = FALSE])

  # add intercept term if specified
  if (intercept) {
    x <- cbind(1, x)
  }

  # calculate beta coefficients using the Ridge regression formula
  beta <- solve(t(x) %*% x + lambda * diag(ncol(x))) %*% t(x) %*% y

  # calculate fitted values
  fitted_values <- x %*% beta

  # create a list containing the model object and fitted values
  ridge_fit <- list(model = list(coefficients = beta, residuals = y - fitted_values), fitted_values = fitted_values)

  # return the list
  return(ridge_fit)
}
