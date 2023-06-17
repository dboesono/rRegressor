library(testthat)
library(rRegressor)
data("boston_df")

# Split the Boston dataset into training and testing sets
set.seed(42)
index <- sample(1:nrow(boston_df), round(0.8 * nrow(boston_df)))
train_data <- boston_df[index, ]
test_data <- boston_df[-index, ]

test_that("compare_regression_models returns a sorted data frame with correct number of rows and columns", {
  result <- compare_regression_models(train_data, c("lstat", "rm", "ptratio"), "medv", test_data)
  expect_type(result, "list")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("Model", "MSE"))
  expect_equal(result$MSE, sort(result$MSE))
})

test_that("compare_regression_models returns a sorted data frame with smaller MSE for higher-ranked models", {
  result <- compare_regression_models(train_data, c("lstat", "rm", "ptratio"), "medv", test_data)

  for (i in 1:(nrow(result) - 1)) {
    expect_lte(result$MSE[i], result$MSE[i + 1])
  }
})
