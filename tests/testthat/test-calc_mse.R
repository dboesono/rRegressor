library(testthat)

data("boston_df")

train_data <- boston_df[1:400, ]
test_data <- boston_df[401:506, ]
x_vars <- colnames(boston_df)[1:13]
y_var <- "medv"

# Test if the function returns correct data type for the Boston dataset
test_that("calc mse returns expected output", {
  preds <- predict_knn_regression(train_data, x_vars, y_var, test_data, k = 5)
  mse <- calc_mse(test_data[, y_var], preds)
  expect_type(mse, "double")
  expect_true(mse >= 0)
})

# Test case for non-numeric inputs
test_that("calc_mse raises an error for non-numeric inputs", {
  y_pred <- "invalid input"
  expect_error(calc_mse(test_data[, y_var], y_pred), "Input y_pred must be a numeric vector")
})

# Test case for inputs of different lengths
test_that("calc_mse raises an error for inputs of different lengths", {
  y_pred <- predict_knn_regression(train_data, x_vars, y_var, test_data[1:5, ], k = 5)
  expect_error(calc_mse(test_data[, y_var], y_pred), "Input y and y_pred must have the same length")
})
