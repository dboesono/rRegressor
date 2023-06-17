library(testthat)

# Load Boston Housing dataset
data("boston_df")

# Load the fit and predict functions from the appropriate file path
# source("~/rRegressor/R/fit_ridge_regression.R")
# source("~/rRegressor/R/predict_ridge_regression.R")

# Write test cases for the predict_ridge_regression function
test_that("predict_ridge_regression returns expected output", {
  # Fit Ridge regression model to Boston Housing dataset with 2 predictor variables
  ridge_fit <- fit_ridge_regression(data = boston_df, x_vars = c("age", "tax"), y_var = "medv", lambda = 0.5)

  # Test case 1: Check the number of predicted values with 2 predictor variables
  pred <- predict_ridge_regression(data = boston_df, ridge_fit = ridge_fit, x_vars = c("age", "tax"))
  expect_length(pred, nrow(boston_df))

  # Test case 2: Check the predicted values data type
  expect_type(pred, "double")

  # Fit Ridge regression model to Boston Housing dataset with 5 predictor variables
  ridge_fit_5 <- fit_ridge_regression(data = boston_df, x_vars = c("crim", "zn", "indus", "chas", "nox"), y_var = "medv", lambda = 0.5)

  # Test case 3: Check the number of predicted values with 5 predictor variables
  pred_5 <- predict_ridge_regression(data = boston_df, ridge_fit = ridge_fit_5, x_vars = c("crim", "zn", "indus", "chas", "nox"))
  expect_length(pred_5, nrow(boston_df))

  # Test case 4: Check the predicted values data type
  expect_type(pred_5, "double")

  # Fit Ridge regression model to Boston Housing dataset with 10 predictor variables
  ridge_fit_10 <- fit_ridge_regression(data = boston_df, x_vars = c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax"), y_var = "medv", lambda = 0.5)

  # Test case 5: Check the number of predicted values with 10 predictor variables
  pred_10 <- predict_ridge_regression(data = boston_df, ridge_fit = ridge_fit_10, x_vars = c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax"))
  expect_length(pred_10, nrow(boston_df))

  # Test case 6: Check the predicted values data type
  expect_type(pred_10, "double")

})


