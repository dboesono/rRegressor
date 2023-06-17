# Load necessary packages
library(testthat)

# Load the data
data("boston_df")

# Write the tests for the fit_polynomial_regression function
test_that("fit_polynomial_regression function works correctly on Boston dataset", {

  # Fit a polynomial regression model of degree 2 to the Boston dataset
  fit <- fit_polynomial_regression(boston_df, x_vars=c("age", "tax", "nox"), y_var="medv", degree=2)

  # Check that the coefficients are of the expected length
  expect_equal(length(fit$coefficients), 7)

  # Check that the design matrix has the expected number of rows and columns
  expect_equal(dim(fit$design_matrix)[1], nrow(boston_df))
  expect_equal(dim(fit$design_matrix)[2], 7)

  # Check that the coefficients are not all zero
  expect_true(any(fit$coefficients != 0))

  # Check that the first column of the design matrix is all ones
  expect_true(all(fit$design_matrix[,1] == 1))

})

#More tests
# Test 1: Check if the function returns a list
test_that("fit_polynomial_regression returns a list", {
  fit <- fit_polynomial_regression(boston_df, x_vars=c("rm", "lstat"), y_var="medv", degree=2)
  expect_type(fit, "list")
})

# Test 2: Check if the function returns a list with two elements
test_that("fit_polynomial_regression returns a list with two elements", {
  fit <- fit_polynomial_regression(boston_df, x_vars=c("rm", "lstat"), y_var="medv", degree=2)
  expect_length(fit, 2)
})

# Test 3: Check if the coefficients have the correct dimensions
test_that("fit_polynomial_regression returns coefficients with the correct dimensions", {
  fit <- fit_polynomial_regression(boston_df, x_vars=c("rm", "lstat"), y_var="medv", degree=2)
  expect_equal(dim(fit$coefficients), c(4, 1))
})

# Test 4: Check if the design matrix has the correct dimensions
test_that("fit_polynomial_regression returns the design matrix with the correct dimensions", {
  fit <- fit_polynomial_regression(boston_df, x_vars=c("rm", "lstat"), y_var="medv", degree=2)
  expect_equal(dim(fit$design_matrix), c(nrow(boston_df), 4))
})

# Test 5: Check if the function throws an error when the x_vars argument is not a character vector
test_that("fit_polynomial_regression throws an error when x_vars is not a character vector", {
  expect_error(fit_polynomial_regression(boston_df, x_vars=1:2, y_var="medv", degree=2))
})

# Test 6: Check if the function throws an error when the y_var argument is not a character string
test_that("fit_polynomial_regression throws an error when y_var is not a character string", {
  expect_error(fit_polynomial_regression(boston_df, x_vars=c("rm", "lstat"), y_var=1, degree=2))
})
