#' Fit a Polynomial Regression
#'
#' This function fits a Polynomial Regression to a dataset.
#'
#' @param data A data frame containing the predictor and response variables
#' @param x_vars A character vector of column names corresponding to the predictor variables
#' @param y_var A character string of the column name corresponding to the response variable
#' @param degree The degree of the polynomial (default = 2)
#' @return A list containing the fitted coefficients and the design matrix
#' @export
#' @examples
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' p <- 10
#' data <- data.frame(matrix(runif(n * p, -1, 1), n, p))
#' colnames(data) <- paste0("x", 1:p)
#' data$y <- data$x1^2 + data$x2^2 + rnorm(n, sd = 0.1)
#'
#' # Fit the Polynomial Regression using the function
#' fit <- fit_polynomial_regression(data, x_vars=paste0("x", 1:p), y_var="y", degree=2)
#'
#' # Print the fitted coefficients
#' print(fit$coefficients)
fit_polynomial_regression <- function(data, x_vars, y_var, degree=2) {
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

  # Check that the degree argument is numeric
  if (!is.numeric(degree)) {
    stop("degree argument must be numeric")
  }

  # Extract the predictor variables and the response variable
  if(!all(x_vars %in% colnames(data))){
    stop("x_vars not found in data")
  }

  if(!all(y_var %in% colnames(data))){
    stop("y_var not found in data")
  }

  X <- data[, x_vars]
  y <- data[, y_var]

  # Create the design matrix with polynomial terms and interaction terms
  poly_formula <- as.formula(paste(" ~ (", paste(x_vars, collapse = " + "), ")^", degree))
  X_design <- model.matrix(poly_formula, data=X)

  # Fit the Polynomial Regression using the Normal Equation
  coefficients <- solve(t(X_design) %*% X_design) %*% t(X_design) %*% y

  # Return the fitted coefficients and the design matrix
  return(list(coefficients=coefficients, design_matrix=X_design))
}
