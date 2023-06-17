# First, load the necessary libraries and data
library(testthat)
data("boston_df")

# Test the predict_polynomial_regression function with Boston dataset

# Create a test dataset
test_data <- data.frame(matrix(runif(10 * length(names(boston_df))), ncol = length(names(boston_df))))
colnames(test_data) <- names(boston_df)

# Test that the function returns a vector of the correct length
test_that("predict_polynomial_regression returns a vector of the correct length", {
  fit <- fit_polynomial_regression(boston_df, x_vars=names(boston_df)[1:5], y_var="medv", degree=2)
  preds <- predict_polynomial_regression(fit, test_data, x_vars=names(boston_df)[1:5], degree=2)
  expect_equal(length(preds), nrow(test_data))
})

# Test that the function returns numeric values
test_that("predict_polynomial_regression returns numeric values", {
  fit <- fit_polynomial_regression(boston_df, x_vars=names(boston_df)[1:5], y_var="medv", degree=2)
  preds <- predict_polynomial_regression(fit, test_data, x_vars=names(boston_df)[1:5], degree=2)
  expect_type(preds, "double")
})

# Test that the function throws an error when the model argument is not a list
test_that("predict_polynomial_regression throws an error when model argument is not a list", {
  expect_error(predict_polynomial_regression(1, test_data, x_vars=names(boston_df)[1:5], degree=2))
})

# Test that the function throws an error when the new_data argument is not a data frame
test_that("predict_polynomial_regression throws an error when new_data argument is not a data frame", {
  fit <- fit_polynomial_regression(boston_df, x_vars=names(boston_df)[1:10], y_var="medv", degree=2)
  expect_error(predict_polynomial_regression(fit, 1, x_vars=names(boston_df)[1:10], degree=2))
})

# Test that the function throws an error when x_vars argument is not a character vector
test_that("predict_polynomial_regression throws an error when x_vars argument is not a character vector", {
  fit <- fit_polynomial_regression(boston_df, x_vars=names(boston_df)[1:10], y_var="medv", degree=2)
  expect_error(predict_polynomial_regression(fit, test_data, x_vars=1, degree=2))
})

# Test that the function throws an error when degree argument is not numeric
test_that("predict_polynomial_regression throws an error when degree argument is not numeric", {
  fit <- fit_polynomial_regression(boston_df, x_vars=names(boston_df)[1:11], y_var="medv", degree=2)
  expect_error(predict_polynomial_regression(fit, test_data, x_vars=names(boston_df)[1:11], degree="2"))
})
