library(testthat)

# Load the Boston dataset
data("boston_df")

# Write test cases for predict_lasso_regression function
test_that("predict_lasso_regression function returns expected output", {
  # Test case 1: Test with standardized predictors
  fit <- fit_lasso_regression(boston_df, x_vars = colnames(boston_df)[-14], y_var = "medv", lambda = 0.1)
  new_data <- boston_df[1:5, -14]
  pred <- predict_lasso_regression(fit, new_data, x_vars = colnames(boston_df)[-14])
  expect_equal(length(pred), nrow(new_data))
  expect_type(pred, "double")

  # Test case 2: Test with different lambda value
  fit <- fit_lasso_regression(boston_df, x_vars = colnames(boston_df)[-14], y_var = "medv", lambda = 0.01)
  new_data <- boston_df[1:5, -14]
  pred <- predict_lasso_regression(fit, new_data, x_vars = colnames(boston_df)[-14])
  expect_equal(length(pred), nrow(new_data))
  expect_type(pred, "double")

  # Test case 3: Test with a different response variable
  fit <- fit_lasso_regression(boston_df, x_vars = colnames(boston_df)[-14], y_var = "nox", lambda = 0.1)
  new_data <- boston_df[1:5, -14]
  pred <- predict_lasso_regression(fit, new_data, x_vars = colnames(boston_df)[-14])
  expect_equal(length(pred), nrow(new_data))
  expect_type(pred, "double")

  # Test case 4: Test with a different set of predictor variables
  fit <- fit_lasso_regression(boston_df, x_vars = colnames(boston_df)[1:5], y_var = "medv", lambda = 0.1)
  new_data <- boston_df[1:5, 1:5]
  pred <- predict_lasso_regression(fit, new_data, x_vars = colnames(boston_df)[1:5])
  expect_equal(length(pred), nrow(new_data))
  expect_type(pred, "double")
})
