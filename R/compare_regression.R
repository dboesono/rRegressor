#' Compare Regression Models
#'
#' This function compares different regression models by calculating their mean squared error (MSE)
#'
#' @param data A data frame containing the data to fit the models
#' @param x_vars A character vector of predictor variable names
#' @param y_var A character string of the response variable name
#' @param new_data A data frame containing the data for making predictions
#' @param lambda A numeric value for the ridge and lasso regularization parameter (default 1)
#' @param alpha A numeric value for the lasso mixing parameter (default 1)
#' @param max_iter A numeric value for the maximum number of iterations for lasso (default 1000)
#' @param tol A numeric value for the tolerance for lasso convergence (default 1e-4)
#' @param degree A numeric value for the polynomial regression degree (default 2)
#' @param k A numeric value for the number of neighbors for kNN regression (default 5)
#' @return A data frame showing the models and their corresponding MSE values, sorted by smallest MSE
#' @export
compare_regression_models <- function(data, x_vars, y_var, new_data, lambda = 1, alpha = 1, max_iter = 1000, tol = 1e-4, degree = 2, k = 5) {

  # Fit the models
  lm_fit <- fit_linear_regression(data, x_vars, y_var)
  ridge_fit <- fit_ridge_regression(data, x_vars, y_var, lambda)
  lasso_fit <- fit_lasso_regression(data, x_vars, y_var, lambda, alpha, max_iter, tol)
  poly_fit <- fit_polynomial_regression(data, x_vars, y_var, degree)

  # Extract predictor variables from new_data
  new_data_x <- new_data[, x_vars]

  # Make predictions
  lm_pred <- predict_linear_regression(new_data_x, lm_fit, x_vars)
  ridge_pred <- predict_ridge_regression(new_data_x, ridge_fit, x_vars)
  lasso_pred <- predict_lasso_regression(lasso_fit, new_data_x, x_vars)
  poly_pred <- predict_polynomial_regression(poly_fit, new_data_x, x_vars, degree)
  knn_pred <- predict_knn_regression(data, x_vars, y_var, new_data_x, k)

  # Calculate MSE
  mse_lm <- calc_mse(new_data[[y_var]], lm_pred)
  mse_ridge <- calc_mse(new_data[[y_var]], ridge_pred)
  mse_lasso <- calc_mse(new_data[[y_var]], lasso_pred)
  mse_poly <- calc_mse(new_data[[y_var]], poly_pred)
  mse_knn <- calc_mse(new_data[[y_var]], knn_pred)

  # Create a data frame with the results
  results <- data.frame(
    Model = c("Linear Regression", "Ridge Regression", "Lasso Regression", "Polynomial Regression", "kNN Regression"),
    MSE = c(mse_lm, mse_ridge, mse_lasso, mse_poly, mse_knn)
  )

  # Sort the data frame by smallest MSE
  results <- results[order(results$MSE), ]

  return(results)
}

