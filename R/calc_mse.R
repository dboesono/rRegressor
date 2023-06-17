#' Calculate Mean Squared Error
#'
#' Calculates the mean squared error of a linear regression model
#'
#' @param y a numeric vector of actual response values
#' @param y_pred a numeric vector of predicted response values
#'
#' @return a numeric value of the mean squared error
#'
#' @examples
#' y <- c(2, 3, 5, 7)
#' y_pred <- c(1.5, 3.5, 4.5, 6.5)
#' mse <- calc_mse(y, y_pred)
#' mse # display the mean squared error
#'
#' @export
calc_mse <- function(y, y_pred) {
  # Check if input y is a numeric vector
  if(!is.numeric(y)) {
    stop("Input y must be a numeric vector")
  }

  # Check if input y_pred is a numeric vector
  if(!is.numeric(y_pred)) {
    stop("Input y_pred must be a numeric vector")
  }

  # Check if input y and y_pred have the same length
  if(length(y) != length(y_pred)) {
    stop("Input y and y_pred must have the same length")
  }

  mse <- mean((y - y_pred)^2)
  return(mse)
}
