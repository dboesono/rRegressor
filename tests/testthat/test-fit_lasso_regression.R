library(testthat)

# Load the Boston dataset
data("boston_df")

# Test case 1: Test if function returns a list
test_that("fit_lasso_regression returns a list", {
  fit <- fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1)
  expect_type(fit, "list")
})

# Test case 2: Test if function returns coefficients and intercept
test_that("fit_lasso_regression returns coefficients and intercept", {
  fit <- fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1)
  expect_named(fit, c("coefficients", "intercept"))
})

# Test case 3: Test if the number of coefficients returned is correct
test_that("fit_lasso_regression returns the correct number of coefficients", {
  fit <- fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1)
  expect_equal(length(fit$coefficients), 13)
})

# Test case 4: Test if the intercept is a double value
test_that("fit_lasso_regression intercept is a double value", {
  fit <- fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1)
  expect_type(fit$intercept, "double")
})

# Test case 5: Test if the function throws an error if x_vars or y_var are not valid column names
test_that("fit_lasso_regression throws an error for invalid column names", {
  expect_error(fit_lasso_regression(boston_df, x_vars=c("not_a_column", "also_not_a_column"), y_var="medv", lambda=0.1))
  expect_error(fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="not_a_column", lambda=0.1))
})

# Test case 6: Test if the function throws an error if lambda is negative
test_that("fit_lasso_regression throws an error for negative lambda", {
  expect_error(fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=-0.5))
})

# Test case 7: Test if the function throws an error if alpha is not between 0 and 1
test_that("fit_lasso_regression throws an error for alpha not between 0 and 1", {
  expect_error(fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1, alpha=-1))
  expect_error(fit_lasso_regression(boston_df, x_vars=colnames(boston_df)[-14], y_var="medv", lambda=0.1, alpha=2))
})
