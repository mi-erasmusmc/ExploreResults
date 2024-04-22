
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

summarizePLP <- function(plp, summary_tasks, data) {

  real_train <- data$Train$labels$outcomeCount
  real_test <- data$Test$labels$outcomeCount

  # Number of outcomes
  outcomes_train <- sum(real_train)
  outcomes_test <- sum(real_test)

  # Number of observations
  observations_train <- length(real_train)
  observations_test <- length(real_test)

  # Outcome rate
  rate_train <- outcomes_train * 100.0 / observations_train
  rate_test <- outcomes_train * 100.0 / observations_train

  # Number of features (occuring in training sample)
  num_features <- length(unique(as.data.frame(data$Train$covariateData$covariates)$covariateId))

  summary_tasks <- rbind(summary_tasks, c(list("Data" = plp,
                                             "Outcomes_train" = outcomes_train,
                                             "Observations_train" = observations_train,
                                             "Outcome_rate_train" = rate_train,
                                             "Outcomes_test" = outcomes_test,
                                             "Observations_test" = observations_test,
                                             "Outcome_rate_test" = rate_test,
                                             "Number_of_features" = num_features)))
  return(summary_tasks)
}


