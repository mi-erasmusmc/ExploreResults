
# TODO: add this to EXPLORE?
runCheckData <- function(data, d) {

  # Check if there are NA's
  if(sum(is.na(data)) > 0) {
    ParallelLogger::logInfo(paste0("Dataset ", d, " had ", sum(is.na(data)), " missing values that are imputed with mean"))

    data[] <- lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  }

  # Remove columns with no variation in feature
  if (min(sapply(data, function(c) length(unique(c)))) == 1) {
    ParallelLogger::logInfo(paste0("Dataset ", d, " had ", sum((sapply(data, function(c) length(unique(c))) == 1)), " features with no variation (1 unique value) that are removed"))

    data <- data[,!(sapply(data, function(c) length(unique(c))) == 1)]
  }

  # Remove columns with low # count (< 25)
  if (min((sapply(data, function(c) sum(c!=0)) < 25) == 1)) {
    ParallelLogger::logInfo(paste0("Dataset ", d, " had ", sum(min((sapply(data, function(c) sum(c!=0)) < 25) == 1)), " features with non-zero count less than 25 that are removed"))

    data <- data[,!((sapply(data, function(c) sum(c!=0)) < 25) == 1)]
  }

  return(data)

}


summarizeData <- function(summary_data, data) {
  # Outcome rate
  rate <- sum(data$class) * 100.0 / nrow(data)

  # Number of features
  num_features <- ncol(data) - 1

  # Number of different values per feature
  max_val_features <- max(sapply(data, function(c) length(unique(c))))
  min_val_features <- min(sapply(data, function(c) length(unique(c))))

  # Max range across features
  min_val <- min(data)
  max_val <- max(data)

  summary_data <- rbind(summary_data, c(list("Outcome_rate" = rate,
                                             "Number_of_features" = num_features),
                                              "Max_different_values" = max_val_features,
                                              "Min_different_values" = min_val_features,
                                              "Min_val" = min_val,
                                              "Max_val" =  max_val))
  return(summary_data)
}


getPlpData <- function() {





}
