
oversample <- function(data, summary_data, sampling_strategy, sample_size, d) {

  if (!is.null(sampling_strategy)) { # oversampling if necessary

    # get size of data
    if (sample_size == "same") {
      sample_size <- nrow(data)
    } else if (sample_size == "75%") {
      sample_size <- ceiling(nrow(data)*0.75)
    }

    required_numOutcomes <- ceiling(sampling_strategy * sample_size)

    if (sum(data$class) < required_numOutcomes) {
      ParallelLogger::logInfo(print(paste0('# outcomes before resampling: ', sum(data$class), ' and % outcomes: ', sum(data$class) * 100.0 / length(data$class), ' oversampling adds: ', required_numOutcomes - sum(data$class), ' outcomes and removes ', length(data$class) - sample_size + (required_numOutcomes - sum(data$class)), ' non-outcomes')))

      outcomes <- which(data$class == 1)
      nonOutcomes <- which(data$class == 0)

      removeNonOutcomes <- sample(nonOutcomes, sample_size - required_numOutcomes, replace = FALSE) # Undersample the non-outcome class to make the data more balanced
      oversampleOutcomes <- sample(outcomes, required_numOutcomes, replace = TRUE) # Oversample the outcome class by adding in each outcome multiple times

      new_data <- data[c(removeNonOutcomes, oversampleOutcomes),]

      ParallelLogger::logInfo(print(paste0('# outcomes after resampling: ', sum(new_data$class), ' and % outcomes: ',sum(new_data$class) * 100.0 / length(new_data$class))))

      summary_data[summary_data$Data == d, c("Outcomes_resampling", "Outcome_rate_resampling")] <- c(sum(new_data$class), sum(new_data$class) * 100.0 / length(new_data$class))

      return(new_data)
    }
  }

  # no oversampling needed or turned off
  return(data)


}
# d <- "cover.arff"
# sampling_strategy <- 0.1 # 0.5 = balanced
# sample_size <- 1000
#
# for (d in data_name_list) { # d <- data_name_list[1]
#
#   data <- farff::readARFF(file.path(".", "data", d))
#
#   new_data <- oversample(data, sampling_strategy = 0.1, sample_size = 1000)
#
#   d <- gsub(pattern = ".arff", replacement = "", d, fixed = TRUE)
#   farff::writeARFF(new_data, file.path(".", "data", d, "_balanced.arff"))
# }
