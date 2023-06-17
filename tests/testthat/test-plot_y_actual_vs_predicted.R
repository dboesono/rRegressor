# Load required packages
library(testthat)
library(ggplot2)

# Start the test file
test_that("plot_y_actual_vs_predicted function tests", {

  # Define inputs for tests
  set.seed(123)
  y_actual <- rnorm(50)
  y_pred <- rnorm(50)
  model_name <- "Test Model"

  # Test input validation
  expect_error(plot_y_actual_vs_predicted(y_actual, y_pred[1:49], model_name))
  expect_error(plot_y_actual_vs_predicted(y_actual[1:49], y_pred, model_name))
  expect_error(plot_y_actual_vs_predicted(y_actual, y_pred, model_name = 1))
  expect_error(plot_y_actual_vs_predicted(y_actual, y_pred, model_name, extra_arg = "test"))

  # Test the output
  plot <- plot_y_actual_vs_predicted(y_actual, y_pred, model_name)
  expect_true(is.ggplot(plot))
  expect_equal(plot$labels$x, "Actual Values")
  expect_equal(plot$labels$y, "Predicted Values")

})
