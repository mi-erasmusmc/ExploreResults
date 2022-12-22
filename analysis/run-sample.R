
### 1. GENERAL SETTINGS
library(PatientLevelPrediction)
library(Cyclops)
library(farff)
library(factoextra) # pca

source("~/Documents/ExploreResults/code/helper.R")
ParallelLogger::addDefaultFileLogger('log-preparation-data.txt')

root <- "/home/amarkus/Documents"
path <- file.path(root, "Benchmark problems", "19dec2022", "output", "IPCI")
save_path <- file.path(path, "samples/")

# input: high to low
n_var <- c(200, 100, 50, 20)
max_obs <- c(100000)
methods <- c("univariate") #
# "lasso-cyclops", "pca"
# methods <- c("univariate", "lasso-glm", "lasso-cyclops", "pca")
benchmark_performance <- TRUE
data_list <- c("hospitalreadmission", "eolconversation", "heartfailurestroke")

for (data in data_list) { # data = data_list[1]
  ### 2. SELECT PREDICTION PROBLEM
  if (data == "hospitalreadmission") {
    outputFolder <- file.path(path, "HospitalReadmission")
    plpData <- loadPlpData(file.path(outputFolder, "targetId_1033_L1"))
    population <- readRDS(file.path(outputFolder, "Analysis_1", "plpResult", "runPlp.rds"))$prediction
    file_name <- "hospitalreadmission"
  } else if (data == "eolconversation") {
    outputFolder <- file.path(path, "EoLConversation")
    plpData <- loadPlpData(file.path(outputFolder, "targetId_1036_L1"))
    population <- readRDS(file.path(outputFolder, "Analysis_1", "plpResult", "runPlp.rds"))$prediction
    file_name <- "eolconversation"
  } else if (data == "heartfailurestroke") {
    outputFolder <- file.path(path, "HeartfailureStroke")
    plpData <- loadPlpData(file.path(outputFolder, "targetId_1060_L1"))
    population <- readRDS(file.path(outputFolder, "Analysis_1", "plpResult", "runPlp.rds"))$prediction
    file_name <- "heartfailurestroke"
  }
  population$evaluationType <- NULL
  all_results <- data.frame()

  for (obs in max_obs) { # obs = max_obs[1]
    set.seed(3110)
    population <- population[sample(1:nrow(population), min(obs, nrow(population))),]

    if(colnames(population)[ncol(population)]!='indexes'){
      warning('indexes column not present as last column - setting all index to 1')
      population$indexes <- rep(1, nrow(population))
    }

    ### 3. TRANSFORM DATA
    y = population$outcomeCount

    sparse_data <- toSparseM(plpData, population, map = NULL)
    data <- sparse_data$data
    x <- as.matrix(data) # data[population$rowId,]
    colnames(x) <- 1:ncol(x) # number columns in order

    # labels <- merge(sparse_data$map, sparse_data$covariateRef, by.x = "oldCovariateId", by.y = "covariateId")
    # labels_map <- labels$newCovariateId
    labels <- sparse_data$covariateRef
    labels_map <- labels$columnId
    # names(labels_map) <- paste0(labels$covariateName, " (", labels$oldCovariateId, ")")
    names(labels_map) <- paste0(labels$covariateName, " (", labels$covariateId, ")")
    colnames(x) <- sapply(colnames(x), function(c) {names(which(labels_map == as.integer(c)))}) # name columns

    ### 4. PRE-VARIABLE SELECTION
    # data <- farff::readARFF(paste0(root, "/ExploreResults/data/IPCI/samples/cover_univariate_var_29obs_1e+05.arff"))
    # data <- farff::readARFF(paste0(root, "/Git/ExploreResults/data/cover.arff"))
    # y <- data[,"class"]
    # x <- data[, !(colnames(data) %in% c("class"))]

    for (method in methods) { # method = methods[1]
      if (method == "univariate") {
        # Univariate variable selection
        correlation <- sapply(1:ncol(x), function(c) cor(x[,c], y, method = "pearson"))
        features <- x[,order(abs(correlation), decreasing = TRUE)]

      } else if (method == "lasso-glm") {
        features <- lasso_glm(x, y, return = "features")

      } else if (method == "lasso-cyclops") {
        features <- lasso_cyclops(x, y, return = "features")

      } else if (method == "information gain") {
        # e.g. split in decision tree

      } else if (method == "pca") {
        pca <- prcomp(x, scale = FALSE)
        features <- predict(pca, newdata = x)

        # fviz_eig(pca)
        # other packages: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
      }

      for (var in n_var) { # var = n_var[1]
        count <- min(var, ncol(features))
        features <- features[,1:count]

        ### 5. SAVE EXPORTED DATA
        dataset <- data.frame(as.matrix(features), y)
        colnames(dataset) <- c(colnames(features), 'class')

        farff::writeARFF(dataset, paste0(save_path, file_name, "_", method, "_var_", count, "obs_", obs, ".arff"), overwrite = TRUE)

        ### 6. TEST PERFORMANCE (CLASS OR PROBABILITY)
        if (benchmark_performance) {
          model_lasso <- lasso_glm(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, return = "model")

          predict_glm <- lasso_glm(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, model_lasso, return = "predict_prob")
          eval <- evaluateModel(predict_glm, dataset$class)
          all_results <- rbind(all_results, c(Name = paste0("AUC GLM probability -", method, " - # var ", count, " # obs ", obs), eval))
          ParallelLogger::logInfo(paste0("AUC GLM probability -", method, " - # var ", count, " # obs ", obs, ": ", eval$Perf_AUC))

          predict_glm <- lasso_glm(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, model_lasso, return = "predict_class", optimise_class = "default lasso")
          eval <- evaluateModel(predict_glm, dataset$class)
          all_results <- rbind(all_results, c(Name = paste0("AUC GLM class default -", method, " - # var ", count, " # obs ", obs), eval))
          ParallelLogger::logInfo(paste0("AUC GLM class default -", method, " - # var ", count, " # obs ", obs, ": ", eval$Perf_AUC))

          predict_glm <- lasso_glm(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, model_lasso, return = "predict_class", optimise_class = "f1_score")
          eval <- evaluateModel(predict_glm, dataset$class)
          all_results <- rbind(all_results, c(Name = paste0("AUC GLM class f1_score -", method, " - # var ", count, " # obs ", obs), eval))
          ParallelLogger::logInfo(paste0("AUC GLM class f1_score -", method, " - # var ", count, " # obs ", obs, ": ", eval$Perf_AUC))

          predict_glm <- lasso_glm(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, model_lasso, return = "predict_class", optimise_class = "ROC01")
          eval <- evaluateModel(predict_glm, dataset$class)
          all_results <- rbind(all_results, c(Name = paste0("AUC GLM class ROC01 -", method, " - # var ", count, " # obs ", obs), eval))
          ParallelLogger::logInfo(paste0("AUC GLM class ROC01 -", method, " - # var ", count, " # obs ", obs, ": ", eval$Perf_AUC))

          # predict_cyclops <- lasso_cyclops(dataset[, !(colnames(dataset) %in% c("class"))], dataset$class, return = "predict_prob")
          # Error in coef.cyclopsFit(model_lasso) :
          #   Cyclops estimation is null; suspect that estimation did not converge.
          # eval <- evaluateModel(predict_cyclops, dataset$class)
          # all_results <- rbind(all_results, c(Name = paste0("AUC Cyclops probability -", method, " - # var ", count, " # obs ", obs), eval))
          # ParallelLogger::logInfo(paste0("AUC Cyclops -", method, " - # var ", count, " # obs ", obs, ": ", eval$Perf_AUC))
        }
      }
    }
  }
  write.csv(all_results, paste0(save_path, file_name, "_check_results.csv"))
}

# PatientLevelPrediction::viewMultiplePlp("~/Documents/ExploreResults/data/IPCI/COVER_v1")



