# load necessary packages and data
library(testthat)
data("boston_df")

# start a new test file
test_that("Testing fit_ridge_regression function", {

  # test case 1: checking if the function returns a list object
  fit <- fit_ridge_regression(data = boston_df, x_vars = c("age", "rm", "lstat"), y_var = "medv", lambda = 0.1)
  expect_type(fit, "list")

  # test case 2: checking if the function returns the correct number of fitted values
  expect_equal(length(fit$fitted_values), nrow(boston_df))

  # test case 3: checking if the coefficients are all numeric
  expect_true(all(sapply(fit$model$coefficients, is.numeric)))

  # test case 4: checking if the intercept term is included in the model
  fit_with_intercept <- fit_ridge_regression(data = boston_df, x_vars = c("age", "rm", "lstat"), y_var = "medv", lambda = 0.1, intercept = TRUE)
  expect_true(all(fit_with_intercept$model$coefficients != 0))

  # test case 5: checking if the function returns an error if an invalid lambda value is specified
  expect_error(fit_ridge_regression(data = boston_df, x_vars = c("age", "rm", "lstat"), y_var = "medv", lambda = "invalid"))

})
