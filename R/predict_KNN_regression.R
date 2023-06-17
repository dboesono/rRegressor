#' Predict using the KNN Regression Model
#'
#' This function predicts the response variable using the KNN Regression model.
#'
#' @param data A data frame containing the predictor and response variables
#' @param x_vars A character vector of column names corresponding to the predictor variables
#' @param y_var A character string of the column name corresponding to the response variable
#' @param new_data A data frame containing the new predictor variable values
#' @param k The number of nearest neighbors to consider (default = 5)
#' @return A vector of predicted response variable values
#' @export
#' @examples
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' p <- 2
#' data <- data.frame(matrix(runif(n * p, -1, 1), n, p))
#' colnames(data) <- paste0("x", 1:p)
#' data$y <- data$x1 + data$x2 + rnorm(n, sd = 0.1)
#'
#' # Generate new data
#' new_data <- data.frame(matrix(runif(10 * p, -1, 1), 10, p))
#' colnames(new_data) <- paste0("x", 1:p)
#'
#' # Predict the response variable using the KNN model
#' predictions <- predict_knn_regression(data, x_vars=paste0("x", 1:p), y_var="y", new_data, k=5)
#' print(predictions)
predict_knn_regression <- function(data, x_vars, y_var, new_data, k=5) {
  # Check if the input data is a data frame
  if(!is.data.frame(data)){
    stop("Input data must be a data frame")
  }

  # Check if the input x_vars is a character vector
  if(!is.character(x_vars)){
    stop("x_vars must be a character vector")
  }

  # Check if the input y_var is a character string
  if(!is.character(y_var)){
    stop("y_var must be a character string")
  }

  # Check if the input new_data is a data frame
  if(!is.data.frame(new_data)){
    stop("new_data must be a data frame")
  }

  # Check if the input k is a positive integer
  if(is.character(k) || k <= 0){
    stop("k must be a positive integer")
  }

  # Check if the predictor variables and response variable are present in the data
  if(!all(x_vars %in% colnames(data)) || !(y_var %in% colnames(data))){
    stop("x_vars and y_var must correspond to column names present in the data")
  }

  # Extract the predictor variables and the response variable
  X_train <- data[, x_vars]
  y_train <- data[, y_var]
  X_new <- new_data[, x_vars]

  # Check if there are missing values in the input data
  if(any(is.na(X_train)) || any(is.na(y_train)) || any(is.na(X_new))){
    stop("Input data contains missing values, please remove them first")
  }

  # Compute the Euclidean distance between two points
  euclidean_distance <- function(a, b) {
    return(sqrt(sum((a - b)^2)))
  }

  # Get the k nearest neighbors
  get_k_nearest_neighbors <- function(x) {
    distances <- apply(X_train, 1, function(row) euclidean_distance(row, x))
    nearest_indices <- order(distances, decreasing = FALSE)[1:k]
    return(nearest_indices)
  }

  # Compute the KNN regression prediction for a single data point
  knn_regression <- function(x) {
    nearest_indices <- get_k_nearest_neighbors(x)
    return(mean(y_train[nearest_indices]))
  }

  # Make predictions for each new data point
  predictions <- apply(X_new, 1, knn_regression)

  return(predictions)
}
