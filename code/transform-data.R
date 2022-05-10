
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




getPlpData <- function() {





}


#
# select_settings <- "full"
#
# ### Load study settings
# study_settings <- read.csv("~/Documents/Git/BenchmarkExplore/inst/Settings/study_settings.csv")
# data_info <- read.csv("~/Documents/Git/BenchmarkExplore/inst/Settings/data_info.csv")
#
# data_path <- paste0(getwd(), "/data/")
# output_path <- paste0(getwd(), "/output/benchmark/")
#
# # Data to include
# data_name <- unlist(study_settings[study_settings$settings == "dataset", select_settings])
#
# if (data_name == "all") {
#   data_name_list <- stringr::str_replace(list.files(path = "data/"), ".arff", "")
# } else {
#   data_name_list <- stringr::str_split(data_name, pattern = ",")[[1]]
# }
#
# for (d in data_name_list) { # d <- data_name_list[1]
#   ParallelLogger::logInfo(print(paste0("Training and testing models for ", d, " data")))
#
#   ### Load data and pre-processing
#   data <- farff::readARFF(paste0(data_path, d, ".arff"))
#   colnames(data) <- tolower(colnames(data))
#   data$class <- data$class == data_info$positive_class[data_info$data == d]
#   data <- as.data.frame(lapply(data, as.numeric))
#
#   # Check if there are NA's
#   if(sum(is.na(data)) > 0) {
#     ParallelLogger::logInfo(paste0("Dataset ", d, " had ", sum(is.na(data)), " missing values that are imputed with mean"))
#
#     data[] <- lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
#   }
#
#   # Remove columns with no variation in feature
#   if (min(sapply(data, function(c) length(unique(c)))) == 1) {
#     ParallelLogger::logInfo(paste0("Dataset ", d, " had ", sum((sapply(data, function(c) length(unique(c))) == 1)), " features with no variation (1 unique value) that are removed"))
#
#     data <- data[,!(sapply(data, function(c) length(unique(c))) == 1)]
#   }
#
#   farff::writeARFF(data, paste0(output_path, "transformed_data/", d, ".arff"))
# }
#
#
# gc()
#
