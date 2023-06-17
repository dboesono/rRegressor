#' Predict using the Polynomial Regression Model
#'
#' This function predicts the response variable using the fitted Polynomial Regression model.
#'
#' @param model A list containing the fitted coefficients and the design matrix of the Polynomial Regression model
#' @param new_data A data frame containing the new predictor variable values
#' @param x_vars A character vector of column names corresponding to the predictor variables
#' @param degree The degree of the polynomial (default = 2)
#' @return A vector of predicted response variable values
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
#' # Generate new data
#' new_data <- data.frame(matrix(runif(10 * p, -1, 1), 10, p))
#' colnames(new_data) <- paste0("x", 1:p)
#'
#' # Predict the response variable using the fitted model
#' predictions <- predict_polynomial_regression(fit, new_data, x_vars=paste0("x", 1:p), degree=2)
#' print(predictions)
predict_polynomial_regression <- function(model, new_data, x_vars, degree=2) {
  # Check if the input new_data is a data frame
  if(!is.data.frame(new_data)){
    stop("new_data must be a data frame")
  }

  # check that x_vars is character strings
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }

  # Check that the degree argument is numeric
  if (!is.numeric(degree)) {
    stop("degree argument must be numeric")
  }

  # Extract the predictor variables from the new_data
  X <- new_data[, x_vars]

  # Create the design matrix with polynomial terms and interaction terms for new_data
  poly_formula <- as.formula(paste(" ~ (", paste(x_vars, collapse = " + "), ")^", degree))
  X_design <- model.matrix(poly_formula, data=X)

  # Make predictions using the fitted coefficients
  predictions <- X_design %*% model$coefficients

  return(predictions)
}
