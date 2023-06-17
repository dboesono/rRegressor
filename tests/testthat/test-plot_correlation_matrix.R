library(testthat)
data("boston_df")

# Test that the function returns the correct class
test_that("Function returns corrplot object", {

  # Check if the result is a corrplot object (class: "list")
  expect_type(plot_correlation_matrix(boston_df), "list")
})

# Test that the function handles incorrect input
test_that("Function handles incorrect input", {
  # Check for error when data is not a data frame
  expect_error(plot_correlation_matrix(42))

  # Check for error when size_coef is not a double
  expect_error(plot_correlation_matrix(boston_df, size_coef = "0.5"))
})
