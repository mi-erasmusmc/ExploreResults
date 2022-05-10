#' @export
scale_data <- function(data, colMean, colSD) {
  data <- sapply(1:ncol(data), function(c) {(data[,c] - colMean[c]) / colSD[c]})
}

#' @export
round_dt <- function(x, digits) {
  # round all numeric variables
  # x: data table
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[,colnames(x)[numeric_columns]] <- round(x[,..numeric_columns], digits)
  x
}

#' @export
create_bootstrap_samples <- function(file, num_samples = 10, sample_size = 10000) {
  set.seed(2021)

  bootstrap_samples <- list()

  for (s in 1:num_samples) {
    bootstrap <- sample(1:nrow(file), sample_size, replace = TRUE)
    bootstrap_samples[[s]] <- bootstrap
  }
  return(bootstrap_samples)
}
