#' Plot Correlation Matrix
#'
#' This function creates a correlation matrix plot of all variables in a given dataset using the corrplot package
#' @import corrplot
#' @export
#' @param data A data frame containing numeric variables
#' @param title A character string representing the title of the plot (optional)
#' @param size_coercoef A numeric representing the size for the correlation coefficients
#' @return A corrplot object displaying the correlation matrix plot, or an error message if an exception occurs
plot_correlation_matrix <- function(data, title = "Correlation Matrix", size_coercoef = 0.5) {
  # Input validation using stopifnot
  stopifnot(
    is.data.frame(data),
    is.character(title),
    is.double(size_coercoef) || is.numeric(size_coercoef)
  )

  # Check if corrplot is installed, if not, install it
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    install.packages("corrplot")
  }

  # Load corrplot
  library(corrplot)

  # Compute pairwise correlations
  cor_matrix <- cor(data)

  # Create the correlation matrix plot using corrplot
  plot <- corrplot(cor_matrix, method = "color", title = title, type = "lower",
                   tl.col = "black",
                   mar = c(0,0,2,0),
                   tl.srt = 45,
                   addCoef.col ='black', number.cex = size_coercoef)

  return(plot)
}
