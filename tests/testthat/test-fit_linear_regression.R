# load library
library(testthat)

# load Boston dataset
data("boston_df")

# test the fit_linear_regression function
test_that("fit_linear_regression returns expected results on Boston dataset", {
  # test case 1: fit with intercept term
  lm_fit1 <- fit_linear_regression(data = boston_df, x_vars = c("rm", "age"), y_var = "medv", intercept = TRUE)
  expect_equal(length(lm_fit1), 2)
  expect_length(lm_fit1$model, 2)
  expect_length(lm_fit1$model$coefficients, 3)
  expect_length(lm_fit1$model$residuals, nrow(boston_df))
  expect_length(lm_fit1$fitted_values, nrow(boston_df))

  # test case 2: fit without intercept term
  lm_fit2 <- fit_linear_regression(data = boston_df, x_vars = c("rm", "age"), y_var = "medv", intercept = FALSE)
  expect_equal(length(lm_fit2), 2)
  expect_length(lm_fit2$model, 2)
  expect_length(lm_fit2$model$coefficients, 2)
  expect_length(lm_fit2$model$residuals, nrow(boston_df))
  expect_length(lm_fit2$fitted_values, nrow(boston_df))

  # test case 3: fit with one predictor variable
  lm_fit3 <- fit_linear_regression(data = boston_df, x_vars = c("rm"), y_var = "medv", intercept = TRUE)
  expect_equal(length(lm_fit3), 2)
  expect_length(lm_fit3$model, 2)
  expect_length(lm_fit3$model$coefficients, 2)
  expect_length(lm_fit3$model$residuals, nrow(boston_df))
  expect_length(lm_fit3$fitted_values, nrow(boston_df))

  # test case 4: fit with 5 predictor variables and intercept term
  lm_fit4 <- fit_linear_regression(data = boston_df, x_vars = c("rm", "age", "dis", "nox", "tax"), y_var = "medv", intercept = TRUE)
  expect_equal(length(lm_fit4), 2)
  expect_length(lm_fit4$model, 2)
  expect_length(lm_fit4$model$coefficients, 6)
  expect_length(lm_fit4$model$residuals, nrow(boston_df))
  expect_length(lm_fit4$fitted_values, nrow(boston_df))
})
