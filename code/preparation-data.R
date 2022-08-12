
### 1. GENERAL SETTINGS
library(PatientLevelPrediction)
library(farff)
library(factoextra) # pca

root <- "/home/amarkus/Documents"

path <- file.path(root, "ExploreResults", "data", "IPCI")

n_var <- c(20, 50, 100, 200)
max_obs <- c(1000000000, 100000)
methods <- c("univariate", "lasso")

### 2. SELECT PREDICTION PROBLEM
OutpatientMortality <- TRUE
AsthmaStepUp <- FALSE
COVER <- FALSE

if (OutpatientMortality) {
  outputFolder <- file.path(path, "OutpatientMortality_v1")
  plpData <- loadPlpData(file.path(outputFolder, "PlpData_L1_T500300"))
  population = readRDS(file.path(outputFolder, "StudyPop_L1_T500300_O9999_P1.rds"))
  file_name <- "outpatientmortality"
} else if (AsthmaStepUp) {
  outputFolder <- file.path(path, "AsthmaStepUp_v1")
  plpData <- loadPlpData(file.path(outputFolder, "PlpData_L1_T647"))
  population = readRDS(file.path(outputFolder, "StudyPop_L1_T647_O648_P1.rds"))
  file_name <- "asthmastepup"
} else if (COVER) {
  outputFolder <- file.path(path, "COVER_v1")
  plpData <- loadPlpData(file.path(outputFolder, "PlpData_L1_T1001"))
  population = readRDS(file.path(outputFolder, "StudyPop_L1_T1001_O4001_P1.rds"))
  file_name <- "cover"
}

for (method in methods) { # method = methods[1]
  for (var in n_var) { # var = n_var[1]
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
      x = data[population$rowId,]
      # x <- as.matrix(x)
      colnames(x) <- 1:ncol(x) # number columns in order

      labels <- merge(sparse_data$map, sparse_data$covariateRef, by.x = "oldCovariateId", by.y = "covariateId")
      labels_map <- labels$newCovariateId
      names(labels_map) <- paste0(labels$covariateName, " (", labels$oldCovariateId, ")")
      colnames(x) <- sapply(colnames(x), function(c) {names(which(labels_map == as.integer(c)))}) # name columns

      ### 4. PRE-VARIABLE SELECTION

      # standardize variables??

      # temp to create these methods
      # folder <- "/Users/aniekmarkus/Documents/Git/ExploreResults/data"
      # data <- farff::readARFF(paste0(folder, "/", file_name, ".arff"))
      # y <- data[,"class"]
      # x <- data[, !(colnames(data) %in% c("class"))]

      if (method == "univariate") {
        # Univariate variable selection
        correlation <- sapply(1:ncol(x), function(c) cor(x[,c], y, method = "pearson"))
        features <- x[,order(abs(correlation), decreasing = TRUE) <= var]

      } else if (method == "lasso") {
        # Lasso logistic regression

        lambdas <- 10^seq(2, -3, by = -.1)
        model_lasso <- glmnet::cv.glmnet(x=data.matrix(x), y = y, alpha = 1, lambda = lambdas, standardize = FALSE, nfolds = 5, family = "binomial")

        coef <- as.matrix(coef(model_lasso, s = "lambda.min"))
        coef_ordered <- coef[order(abs(coef), decreasing = TRUE)]
        names_ordered <- rownames(coef)[order(abs(coef), decreasing = TRUE)]
        names(coef_ordered) <- names_ordered

        coef_ordered <- coef_ordered[names(coef_ordered) != "(Intercept)"]
        n_sel <- sum(coef_ordered != 0)

        features <- x[,names(coef_ordered[1:min(var, n_sel)])]

      } else if (method == "information gain") {

        # e.g. split in decision tree

      } else if (method == "pca") {

        n_pc <- 10

        pca <- prcomp(x, scale = FALSE)
        features <- predict(pca, newdata = x)
        features <- features[,1:n_pc]

        # fviz_eig(pca)

        # other packages: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

      }

      ### 5. SAVE EXPORTED DATA
      dataset <- data.frame(as.matrix(features), y)
      colnames(dataset) <- c(colnames(features), 'class')

      farff::writeARFF(dataset, paste0(path, "/", file_name, "_", method, "_var_", var, "obs_", obs, ".arff"))

    }
  }
}

# PatientLevelPrediction::viewMultiplePlp("~/Documents/ExploreResults/data/IPCI/COVER_v1")
