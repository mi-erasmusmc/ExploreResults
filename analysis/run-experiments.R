library(data.table)
library(Explore)

# setwd("~/Documents/ExploreResults/")
print(getwd())

explore_options <- expand.grid(StartRulelength = c(1),
                               EndRulelength = c(2),
                               Parallel = c("yes"),
                               Sorted = c(FALSE),
                               Constraint_Specificity = "",
                               Constraint_Accuracy = 0.7,
                               # Constraint_Specificity = seq(0.05,0.95,0.1),
                               # Constraint_Accuracy = c(""), # "custom"
                               Maximize = c("SENSITIVITY", "BALANCEDACCURACY"),
                               stringsAsFactors = FALSE) # TODO: check what happens if no solution
# explore_options <- data.frame()

output_path <- paste0(getwd(), "/shiny/output/timings_", Sys.Date())

if (!dir.exists(output_path)) {
  dir.create(output_path)
}

# Data to include
# data_name_list <- list.files(path = file.path(getwd(), "data", "IPCI", "new"))
# data_name_list <- data_name_list[!(data_name_list %in% c("all", "AsthmaStepUp_v1", "COVER_v1", "OutpatientMortality_v1"))]
# data_name_list <- c("iris.arff", "vote.arff", paste0("IPCI/new/", data_name_list))
# data_name_list <- c("IPCI/samples/cover_univariate_var_15obs_1e+06.arff")
data_name_list <- c("iris.arff", "vote.arff")


# Methods to test
methods_list <- c("lasso", "explore")
# methods_list <- c("lasso", "randomforest", "ripper", "explore")

source("code/transform-data.R")
source("code/oversampling-data.R")
source("code/methods.R")
source("code/experiments.R")
source("code/helper.R")

# parallel::mcaffinity(affinity = 1:30)
output <- runExperiments(output_path, data_name_list, methods_list, explore_options, train_fraction = 0.9, num_iterations = 1)

