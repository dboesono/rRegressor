#' Fit a Lasso Regression
#'
#' This function fits a Lasso regression to a dataset using coordinate descent.
#'
#' @param data A data frame containing the predictor and response variables
#' @param x_vars A character vector of column names corresponding to the predictor variables
#' @param y_var A character string of the column name corresponding to the response variable
#' @param lambda The regularization parameter
#' @param alpha The mixing parameter between L1 and L2 penalties (default = 1)
#' @param max_iter The maximum number of iterations for the coordinate descent algorithm (default = 1000)
#' @param tol The convergence threshold for the coordinate descent algorithm (default = 1e-4)
#' @return A list containing the fitted coefficients and the intercept
#' @export
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
#' # Print the coefficients
#' print(fit$coefficients)
#' @export
fit_lasso_regression <- function(data, x_vars, y_var, lambda, alpha=1, max_iter=1000, tol=1e-4) {
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

  #check lambda and alpha
  if (lambda < 0) {
    stop("lambda must be non-negative")
  }

  if (alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
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

  # Standardize the predictor variables
  X <- scale(X)
  # y <- scale(y)

  # Compute the correlation matrix
  R <- cor(X)

  # Set the initial coefficients to zero
  beta <- rep(0, ncol(X))

  # Set the initial intercept to the mean of y
  beta0 <- mean(y)

  # Define the penalty factor
  penalty <- lambda * (1 - alpha) / 2

  # Define the update rule for the coefficients
  update_beta <- function(j) {
    x_j <- X[, j]
    r_j <- y - beta0 - X %*% beta + x_j * beta[j]
    z_j <- sum(x_j^2)
    if (z_j == 0) {
      beta_j_new <- 0
    } else {
      beta_j_new <- ifelse(sum(x_j * r_j) < -penalty / 2, (sum(x_j * r_j) + penalty / 2) / z_j,
                           ifelse(sum(x_j * r_j) > penalty / 2, (sum(x_j * r_j) - penalty / 2) / z_j, 0))
    }
    return(beta_j_new)
  }

  # Define the update rule for the intercept
  update_beta0 <- function() {
    beta0_new <- mean(y - X %*% beta)
    return(beta0_new)
  }

  # Coordinate descent algorithm
  for (iter in 1:max_iter) {
    # Save the current coefficients and intercept
    beta_old <- beta
    beta0_old <- beta0

    # Update the coefficients and intercept
    for (j in 1:ncol(X)) {
      beta[j] <- update_beta(j)
    }
    beta0 <- update_beta0()

    # Check convergence
    if (max(abs(beta - beta_old)) < tol && abs(beta0 - beta0_old) < tol) {
      break
    }
  }

  # Return the fitted coefficients and intercept
  return(list(coefficients=beta, intercept=beta0))
}
