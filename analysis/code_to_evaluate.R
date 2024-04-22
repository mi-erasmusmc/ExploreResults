
library(data.table)
library(dplyr)

# Paths
root <- getwd()

evaluate_main <- TRUE
evaluate_rr <- FALSE
shiny <- FALSE

# outputFolder <- file.path(root, "shiny", "output", Sys.Date())
# covariates <- "Phenotypes" # "Default", "AgeSex", "Full", "Phenotypes", "Reduced"

name <- "2023-04-05"
covariates <- "Phenotypes"

# OLD:
# name <- "final_linux"
# covariates <- c("Reduced", "Full")

outputFolder <- file.path(root, "shiny", "output", name)
saveFolder <- file.path(outputFolder, paste0("export_", name))

if (!dir.exists(saveFolder)) {
  dir.create(saveFolder, recursive = TRUE)
  file.copy(file.path(outputFolder, "explore_options.csv"), file.path(saveFolder, "explore_options.csv"))
}

### Evaluate - Main analysis ###
if (evaluate_main) {
  source(file.path(root, "code/transform-data.R"))
  source(file.path(root, "code/helper.R"))

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Data_|export_", outputList)] # Remove data / variable selection results

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))

  methods_output <- data.frame()
  for (cov in covariates) { # cov = covariates[1]

    summary_tasks <- data.frame()
    for (plp in datasets) { # plp = datasets[2]

      # Select results for current dataset and covariates
      outputList_d <- outputList[grepl(plp, outputList)]
      outputList_d <- outputList_d[grepl(cov, outputList_d)]

      # Load data
      # OLD:
      # path <- file.path(outputFolder, paste0("Data_", plp, "_Reduced"), "TrainTestData")
      path <- file.path(outputFolder, paste0("Data_", plp, "_", cov), "TrainTestData")
      data <- loadTrainTestData(path)

      # Summarize data
      summary_tasks <- summarizePLP(plp, summary_tasks, data)

      # Covariate data
      result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, paste0("LASSO_", plp, "_1_", cov), "plpResult"))

      cov_data <- result$covariateSummary
      write.csv(cov_data, file.path(saveFolder, paste0("covariates_", plp, "_", cov, ".csv")), row.names = FALSE)

      features <- result$model$model$coefficients$covariateIds
      features <- features[features!='(Intercept)']
      features_names <- result$covariateSummary[c("covariateId", "covariateName")]

      # Save objects
      models <- setNames(data.table(matrix(0, nrow = 0, ncol = length(features)+4)), c("method", "iteration", "option", features, "model size")) # NO INTERCEPT

      for (o in outputList_d) { # o = outputList_d[3]
        print(o)

        method <- unlist(strsplit(o, split = "_"))[1]
        i <- unlist(strsplit(o, split = "_"))[3]

        result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, o, "plpResult"))

        if (is.null(result$prediction)) {
          warning('No predictions!')

        } else {
          # Save computation time method
          time <- result$model$trainDetails$trainingTime

          # Save model
          if (method == "LASSO" || method == "IHT") {
            varImp <- result$model$model$coefficients
            vars <- varImp$betas[varImp$covariateIds!='(Intercept)']
            names(vars) <- as.double(varImp$covariateIds[varImp$covariateIds!='(Intercept)'])
            size <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))
          } else if (method == "RandomForest" || method == "DecisionTree") {
            varImp <- result$model$covariateImportance
            vars <- varImp$covariateValue
            names(vars) <- varImp$covariateId
            size <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))
          } else if (method == "EXPLORE" || method == "RIPPER" || method == "GOSDT") {
            varImp <- result$model$covariateImportance
            vars <- varImp$covariateValue
            names(vars) <- varImp$covariateId
            size <- sum(varImp$covariateValue)
          } else if (method == "XGBoost") {
            varImp <- result$model$covariateImportance
            varImp <- varImp %>% group_by(covariateId) %>% summarise(included=max(included)) # Variables can occur twice in computed var importance
            vars <- varImp$included
            names(vars) <- varImp$covariateId
            size <- sum(varImp$included)
          } else {
            stop('Model not included')
          }

          vars <- vars[vars != 0]
          vars <- sapply(features, function(f) ifelse(f %in% names(vars), 1, 0))

          model <- c(method=method, iteration=1, option=i, as.list(vars), `model size`=size)
          models <- rbind(models, model)

          # Evaluate predictions
          real_train <- result$prediction$outcomeCount[result$prediction$evaluationType == "Train"]
          predictions_train <- result$prediction$value[result$prediction$evaluationType == "Train"]

          real_test <- result$prediction$outcomeCount[result$prediction$evaluationType == "Test"]
          predictions_test <- result$prediction$value[result$prediction$evaluationType == "Test"]

          if (method == "EXPLORE") {
            model_description <- result$model$model$fit

            # OLD:
            # eval_train_prob <- evaluateExplore(modelsCurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Train)
            eval_train_prob <- evaluateExplore(modelsCurve = result$model$model$modelsCurve, plpModel=result$model, data=data$Train)
            eval_train_class <- evaluateModel(predictions_train, real_train, model=model_description)

            # OLD:
            # eval_test_prob <-  evaluateExplore(modelsCurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Test)
            eval_test_prob <-  evaluateExplore(modelsCurve = result$model$model$modelsCurve, plpModel=result$model, data=data$Test)
            eval_test_class <- evaluateModel(predictions_test, real_test, model=model_description)

            # Add feature names
            covs <- unlist(stringr::str_extract_all(model_description, pattern = "[:digit:]{3,}+"))
            names(covs) <- sapply(covs, function(c) features_names$covariateName[features_names$covariateId == c])
            model_description <- stringr::str_replace_all(model_description, setNames(names(covs), covs))

          } else {
            model_description <- paste0(size, " covariates")
            eval_train_prob <- evaluateModel(predictions_train, real_train, model=model_description, class=F)
            eval_train_class <- evaluateModel(prob_to_class(predictions_train, real_train), model=model_description, real_train)

            eval_test_prob <- evaluateModel(predictions_test, real_test, model=model_description, class=F)
            eval_test_class <- evaluateModel(prob_to_class(predictions_test, real_test), model=model_description, real_test)
          }

          eval <- append(append(append(eval_test_class, eval_test_prob), eval_train_class), eval_train_prob)
          names(eval) <- c(paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_test_prob), "_Test_Prob"), paste0(names(eval_train_class), "_Train_Class"), paste0(names(eval_train_prob), "_Train_Prob"))
          methods_output <- rbind(methods_output, c(append(list(Time = time, Data = plp, Covariates=as.character(cov), Method = method, Iteration = 1, Option = i, Model = model_description), eval)))
        }
      }

      write.csv(models, file.path(saveFolder, paste0("models_", plp, "_", cov, ".csv")), row.names = FALSE)
    }
    write.csv(summary_tasks, file.path(saveFolder, paste0("summary_tasks_", cov, ".csv")), row.names = FALSE)

    write.csv(methods_output, file.path(saveFolder, paste0("output_methods.csv")), row.names = FALSE)
  }
}

### Evaluate - Additional analysis ###
if (evaluate_rr) {

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Data_|export_", outputList)] # Remove data / variable selection results

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))

  rashomon_ratio <- data.table(data = character(), threshold =  numeric(), rashomon_ratio = numeric(), total_candidates = integer(), restricted_candidates = integer(), time = numeric())

  option <- 3 # TODO: Maybe loop over it?

  for (cov in covariates) { # cov = covariates[1]
    for (plp in datasets) { # plp = datasets[1]
      ParallelLogger::logInfo(print(paste0("Computing Rashomon ratio for ", plp)))

      # Select results for current dataset
      outputList_d <- outputList[grepl(plp, outputList)]

      # Read in results file
      results <- paste(readLines(file.path(outputFolder, paste0("EXPLORE_", plp, "_", option, "_", cov), "Explore", "train_data.result")), collapse="\n")

      total_candidates <- stringr::str_extract_all(results, "Total Count Candidates:.*?\u000A")[[1]]
      total_candidates <- as.data.frame(stringr::str_remove_all(total_candidates, "Total Count Candidates:"))
      total_candidates <- stringr::str_replace_all(total_candidates, "\\n", "")

      results <- paste(readLines(file.path(outputFolder, paste0("EXPLORE_", plp, "_", option, "_", cov, "_RR"), "Explore", "train_data.result")), collapse="\n")

      restricted_candidates <- stringr::str_extract_all(results, "Total Count Candidates:.*?\u000A")[[1]]
      restricted_candidates <- as.data.frame(stringr::str_remove_all(restricted_candidates, "Total Count Candidates:"))
      restricted_candidates <- stringr::str_replace_all(restricted_candidates, "\\n", "")

      rr <- as.numeric(restricted_candidates) / as.numeric(total_candidates)

      rashomon_ratio <- rbind(rashomon_ratio, c(list(data = plp,
                                                     threshold = performance_threshold,
                                                     rashomon_ratio = rr,
                                                     total_candidates = total_candidates,
                                                     restricted_candidates = restricted_candidates)))
    }
  }

  write.csv(rashomon_ratio, paste0(saveFolder, "rashomon_ratio.csv"), row.names = FALSE)
}

### Launch shiny ###
if (shiny) {
  shiny::runApp('shiny')
}
