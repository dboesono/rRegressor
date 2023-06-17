library(testthat)

data("boston_df")

# Test if the function returns a numeric vector
test_that("predict_knn_regression returns a numeric vector", {
  preds <- predict_knn_regression(boston_df, x_vars = colnames(boston_df)[1:13], y_var = "medv", boston_df[1:5, ], k = 3)
  expect_type(preds, "double")
})

# Test if the function returns correct number of predictions
test_that("predict_knn_regression returns correct number of predictions", {
  preds <- predict_knn_regression(boston_df, x_vars = colnames(boston_df)[1:13], y_var = "medv", boston_df[1:5, ], k = 3)
  expect_equal(length(preds), 5)
})

# Test if the function returns correct predictions for the Boston dataset
test_that("predict_knn_regression returns correct predictions for the boston_df dataset", {
  train_data <- boston_df[1:400, ]
  test_data <- boston_df[401:506, ]
  x_vars <- colnames(boston_df)[1:13]
  y_var <- "medv"
  preds <- predict_knn_regression(train_data, x_vars, y_var, test_data, k = 5)
  expect_equal(length(preds), 106)
  expect_equal(round(mean(abs(preds - test_data[, y_var]))), 5)
})

# Test if the function handles invalid input data types
test_that("predict_knn_regression handles invalid input data types", {
  expect_error(predict_knn_regression(boston_df, x_vars = 1:13, y_var = "medv", boston_df[1:5, ], k = 3))
  expect_error(predict_knn_regression(boston_df, x_vars = colnames(boston_df)[1:13], y_var = 1, boston_df[1:5, ], k = 3))
  expect_error(predict_knn_regression(boston_df, x_vars = colnames(boston_df)[1:13], y_var = "medv", boston_df[1:5, ], k = -1))
})

# Test if the function handles missing predictor or response variables in the data
test_that("predict_knn_regression handles missing predictor or response variables in the data", {
  expect_error(predict_knn_regression(boston_df[, -1], x_vars = colnames(boston_df)[1:12], y_var = "medv", boston_df[1:5, ], k = 3))
  expect_error(predict_knn_regression(boston_df, x_vars = colnames(boston_df)[1:12], y_var = "unknown_var", boston_df[1:5, ], k = 3))
})

# Test if the function handles missing values in the data
test_that("predict_knn_regression handles missing values in the data", {
  Boston_missing <- boston_df
  Boston_missing[sample(1:dim(boston_df)[1], 10), sample(1:dim(boston_df)[2], 1)] <- NA
  expect_error(predict_knn_regression(Boston_missing, x_vars = colnames(boston_df)[1:13], y_var = "medv", Boston_missing[1:5, ], k = 3))
})
