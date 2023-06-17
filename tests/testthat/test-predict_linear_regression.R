library(testthat)

# Load the fit and predict functions from the appropriate file path
# source("~/rRegressor/R/fit_linear_regression.R")
# source("~/rRegressor/R/predict_linear_regression.R")

# Define a test environment
test_that("predict_linear_regression works correctly", {

  # Load the Boston dataset
  data("boston_df")

  #Test 1 with 5 predictor variables
  # Fit a model using the fit_linear_regression function
  fit <- fit_linear_regression(data = boston_df, x_vars = c("crim", "zn", "indus", "chas", "nox"), y_var = "medv")

  # Test that the predict_linear_regression function returns the expected results
  predicted <- predict_linear_regression(data = boston_df, lm_fit = fit, x_vars = c("crim", "zn", "indus", "chas", "nox"))
  expect_equal(length(predicted), nrow(boston_df))
  expect_type(predicted, "double")

  #Test 2 with 3 predictor variables
  # Fit a model using the fit_linear_regression function
  fit <- fit_linear_regression(data = boston_df, x_vars = c("rm", "age", "dis"), y_var = "medv")

  # Test that the predict_linear_regression function returns the expected results
  predicted <- predict_linear_regression(data = boston_df, lm_fit = fit, x_vars = c("rm", "age", "dis"))
  expect_equal(length(predicted), nrow(boston_df))
  expect_type(predicted, "double")

  #Test 3 with 1 predictor variables
  # Fit a model using the fit_linear_regression function
  fit <- fit_linear_regression(data = boston_df, x_vars = c("rm"), y_var = "medv")

  # Test that the predict_linear_regression function returns the expected results
  predicted <- predict_linear_regression(data = boston_df, lm_fit = fit, x_vars = c("rm"))
  expect_equal(length(predicted), nrow(boston_df))
  expect_type(predicted, "double")
})

