library(PatientLevelPrediction)
library(Explore)
library(data.table)
library(job)
# reticulate::use_virtualenv("r-reticulate")
# options(andromedatempdir = "J:/amarkus/Documents/R/Temp")

plp <- "Test"
nVar <- 20

selection <- TRUE
train <- TRUE
evaluate <- TRUE
shiny <- TRUE

# Paths
root <- getwd()
outputFolder <- file.path(root, "shiny", "output", Sys.Date())

if (!dir.exists(outputFolder)) {
  dir.create(outputFolder)
}

### Prediction tasks ###

if (selection || train) {


  if (plp == "Test") {
    # Get connection details
    connectionDetails <- Eunomia::getEunomiaConnectionDetails()

    # Create cohorts
    Eunomia::createCohorts(connectionDetails)

    # Select cohorts
    cohortId <- 4
    cohortNames <- 'target'
    outcomeId <- 3
    outcomeNames <- 'outcome'

    # Select population
    populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
      requireTimeAtRisk = F,
      riskWindowStart = 1,
      riskWindowEnd = 365)

    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      cohortDatabaseSchema = "main",
      cohortTable = "cohort",
      targetId = cohortId,
      outcomeIds = outcomeId,
      outcomeDatabaseSchema = "main",
      outcomeTable =  "cohort",
      cdmDatabaseName = 'eunomia'
    )

  } else {
    # Get connection details
    # TODO: sys.setenv / sys.getenv
    # Details for connecting to the server:
    dbms <- 'postgresql'
    user <- 'amarkus'
    pw <- 'amarkus'
    server <- 'Res-Srv-Lin-02/CDM-M-20220414'
    port <- 5432

    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                    server = server,
                                                                    user = user,
                                                                    password = pw,
                                                                    port = port)
    cohortTable <- 'cohorts_v1'

    if (plp == "HospitalReadmission") {
      # Select cohorts
      cohortId <- 1033
      cohortNames <- 'inpatient visit'
      outcomeId <- 1034
      outcomeNames <- 'hospital readmission'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 28,
                                                                                  riskWindowStart = 2,
                                                                                  startAnchor = "cohort end",
                                                                                  riskWindowEnd = 30,
                                                                                  endAnchor = "cohort end",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    } else if (plp == "EoLConversation") {
      # Select cohorts
      cohortIds <- 1036
      cohortNames <- 'old patient with GP visit'
      outcomeIds <- 1038
      outcomeNames <- 'conversation'

      # Select population
      populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                     washoutPeriod = 365,
                                                                                     minTimeAtRisk = 179,
                                                                                     riskWindowStart = 1,
                                                                                     startAnchor = "cohort start",
                                                                                     riskWindowEnd = 180,
                                                                                     endAnchor = "cohort start",
                                                                                     removeSubjectsWithPriorOutcome = FALSE)
    } else if (plp == "HeartfailureStroke") {
      # Select cohorts
      cohortIds <- 1060
      cohortNames <- 'T2DM patients'
      outcomeIds <- 1058
      outcomeNames <- 'heart failure or stroke'

      # Select population
      populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                     washoutPeriod = 365,
                                                                                     minTimeAtRisk = 1824,
                                                                                     riskWindowStart = 1,
                                                                                     startAnchor = "cohort start",
                                                                                     riskWindowEnd = 1825,
                                                                                     endAnchor = "cohort start",
                                                                                     removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "AsthmaExacerbation") {
      # Select cohorts
      cohortIds <- 1078
      cohortNames <- 'asthma patients'
      outcomeIds <- 1054
      outcomeNames <- 'asthma exacerbation'

      # Select population
      populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                     washoutPeriod = 365,
                                                                                     minTimeAtRisk = 729,
                                                                                     riskWindowStart = 1,
                                                                                     startAnchor = "cohort start",
                                                                                     riskWindowEnd = 730,
                                                                                     endAnchor = "cohort start",
                                                                                     removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "COPDMortality") {

      # Select cohorts
      cohortIds <- 1035
      cohortNames <- 'COPD patients'
      outcomeIds <- 1037
      outcomeNames <- 'all-cause mortality'

      # Select population
      populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                     washoutPeriod = 365,
                                                                                     minTimeAtRisk = 729,
                                                                                     riskWindowStart = 1,
                                                                                     startAnchor = "cohort start",
                                                                                     riskWindowEnd = 730,
                                                                                     endAnchor = "cohort start",
                                                                                     removeSubjectsWithPrior = FALSE)

    }

    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                     cdmDatabaseSchema = "cdm",
                                                                     cdmDatabaseId = "CDM-M-20220414",
                                                                     cohortDatabaseSchema = "amarkustseinen",
                                                                     cohortTable = cohortTable,
                                                                     targetId = cohortId,
                                                                     outcomeIds = outcomeId,
                                                                     outcomeDatabaseSchema = "amarkustseinen",
                                                                     outcomeTable = cohortTable,
                                                                     cdmDatabaseName = "IPCI")
  }

  # Possible covariates
  covariateSettingList_default <- FeatureExtraction::createDefaultCovariateSettings()
  covariateSettingList_agesex <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T)
  covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T,
                                                                     useConditionGroupEraAnyTimePrior = T, useConditionGroupEraLongTerm = T,
                                                                     useConditionGroupEraShortTerm = T, useDrugGroupEraLongTerm = T,
                                                                     useDrugGroupEraShortTerm = T, useDrugGroupEraOverlapping = T)

  # Selected covariates
  plpData <- PatientLevelPrediction::getPlpData(
    databaseDetails = databaseDetails,
    restrictPlpDataSettings = createRestrictPlpDataSettings(),
    covariateSettings = covariateSettingList
  )
}

### Pre-process data ###
if (selection) {
  modelSettings <- PatientLevelPrediction::setUnivariateSelection(modelSettings=setLassoLogisticRegression(),
                                                                  nVariables = nVar,
                                                                  saveDirectory = outputFolder)

  plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                               outcomeId = outcomeId,
                                               modelSettings = modelSettings,
                                               analysisId = paste0("Univariate", nVar, "_", plp),
                                               analysisName = paste0("Univariate selection with top ", nVar, " correlated variables"),
                                               populationSettings = populationSettings,
                                               splitSettings = createDefaultSplitSetting(),
                                               sampleSettings = createSampleSettings(),
                                               featureEngineeringSettings = createFeatureEngineeringSettings(),
                                               preprocessSettings = createPreprocessSettings(minFraction=0.001,
                                                                                             normalize = T,
                                                                                             removeRedundancy = T),
                                               logSettings = createLogSettings(),
                                               executeSettings = createExecuteSettings(runSplitData = T,
                                                                                       runSampleData = T,
                                                                                       runfeatureEngineering = T,
                                                                                       runPreprocessData = T,
                                                                                       runModelDevelopment = T,
                                                                                       runCovariateSummary = F),
                                               saveDirectory = outputFolder,
                                               saveData = T,
                                               loadData = NULL)
}

### Train models ###
if (train) {
  list_models <- list()

  # EXPLORE
  explore_options <- expand.grid(StartRulelength = c(1),
                                 EndRulelength = c(2, 3), # Add: 4
                                 Parallel = c("yes"), # Add: no
                                 Constraint_Accuracy = "",
                                 Maximize = c("BALANCEDACCURACY"), # BALANCEDACCURACY
                                 stringsAsFactors = FALSE) # TODO: check what happens if no solution
  explore_options$Option <- 1:nrow(explore_options)
  write.csv(explore_options, file.path(outputFolder, "explore_options.csv"), row.names = FALSE)
  # TODO: use explore_options as input, output correct option # in output (eg via i)?

  for (option in explore_options$Option) {
    analysisId <- paste0("EXPLORE_", plp, "_", option)

    modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                        startRulelength = explore_options$StartRulelength[option],
                                                        endRulelength = explore_options$EndRulelength[option],
                                                        maximize = explore_options$Maximize[option],
                                                        accuracy = explore_options$Constraint_Accuracy[option],
                                                        parallel = explore_options$Parallel[option],
                                                        aucCurve = TRUE,
                                                        saveDirectory = file.path(outputFolder, analysisId))

    list_models[[paste0("EXPLORE", option)]] <- list(analysisId=analysisId,
                                                     analysisName=paste0("EXPLORE decision rule"),
                                                     modelSettings=modelSettings)
  }

  # Possible models
  # LASSO
  list_models[["LASSO"]] <- list(analysisId=paste0("LASSO_", plp, "_1"),
                                 analysisName=paste0("LASSO logistic regression"),
                                 modelSettings=PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01))

  # RIPPER
  # analysisId <- paste0("RIPPER_", plp, "_1")
  # analysisName <- paste0("RIPPER")
  # modelSettings <- PatientLevelPrediction::setRIPPER()
  # models[["RIPPER"]] <- list(analysisId, analysisName, modelSettings)

  # Gradient boosting machine
  list_models[["XGBoost"]] <- list(analysisId=paste0("XGBoost_", plp, "_1"),
                                   analysisName=paste0("Gradient boosting machine"),
                                   modelSettings=PatientLevelPrediction::setGradientBoostingMachine())

  # Random forest
  # PatientLevelPrediction::configurePython()
  list_models[["RandomForest"]] <- list(analysisId=paste0("RandomForest_", plp, "_1"),
                                        analysisName=paste0("Random forest"),
                                        modelSettings=PatientLevelPrediction::setRandomForest(ntrees = list(100), maxDepth = list(3)))

  PatientLevelPrediction::setPythonEnvironment(envname='r-reticulate', envtype = 'conda')

  for (m in names(list_models)) { # m = "LASSO"

    model <- list_models[[m]]

    #job::job({
    plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                                 outcomeId = outcomeId,
                                                 modelSettings = model$modelSettings,
                                                 analysisId = model$analysisId,
                                                 analysisName = model$analysisName,
                                                 populationSettings = populationSettings,
                                                 splitSettings = createDefaultSplitSetting(),
                                                 sampleSettings = createSampleSettings(),
                                                 featureEngineeringSettings = createFeatureEngineeringSettings(),
                                                 preprocessSettings = createPreprocessSettings(minFraction=0.001,
                                                                                               normalize = T,
                                                                                               removeRedundancy = F),
                                                 logSettings = createLogSettings(),
                                                 executeSettings = createExecuteSettings(runSplitData = F,
                                                                                         runSampleData = F,
                                                                                         runfeatureEngineering = F,
                                                                                         runPreprocessData = F,
                                                                                         runModelDevelopment = T,
                                                                                         runCovariateSummary = T),
                                                 saveDirectory = outputFolder,
                                                 saveData = F,
                                                 loadData = file.path(outputFolder, paste0("Univariate", nVar, "_", plp), "TrainTestData"))
    # TODO: warn when no model?
    #})
  }
}

### Evaluate ###
if (evaluate) {
  source("~/Documents/Git/ExploreResultsPLP/code/helper.R")
  source("~/Documents/Git/ExploreResultsPLP/code/transform-data.R")

  outputList <- list.dirs(path = paste0(outputFolder, "/"), full.names = F, recursive = F)
  outputList <- outputList[!grepl("Univariate", outputList)] # Remove variable selection results

  summary_data <- data.frame()
  methods_output <- data.frame()

  datasets <- unique(sapply(outputList, function(l) unlist(strsplit(l, split = "_"))[[2]]))

  for (plp in datasets) { # plp = datasets[1]
    # Select results for current dataset
    outputList_d <- outputList[grepl(plp, outputList)]

    # Load data
    path <- file.path(outputFolder, paste0("Univariate", nVar, "_", plp), "TrainTestData")
    data <- loadTrainTestData(path)

    # Summarize data
    result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, outputList_d[1], "plpResult"))
    summary_data <- summarizeDataPLP(summary_data, result, plp)

    # Save objects
    features <- result$model$covariateImportance$covariateId
    models <- setNames(data.table(matrix(0, nrow = 0, ncol = length(features)+4)), c("method", "iteration", "option", features, "model size")) # NO INTERCEPT

    for (o in outputList_d) { #  o = outputList_d[1]
      print(o)

      method <- unlist(strsplit(o, split = "_"))[1]
      i <- unlist(strsplit(o, split = "_"))[3]

      result <- PatientLevelPrediction::loadPlpResult(file.path(outputFolder, o, "plpResult"))
      # TODO: add check when no model fitted (skip the remaining code)

      # Save computation time method
      time <- result$model$trainDetails$trainingTime

      # Save model
      if (method == "LASSO") {
        varImp <- result$model$model$coefficients # result$model$covariateImportance
        vars <- varImp$betas[varImp$covariateIds!='(Intercept)']
        names(vars) <- as.double(varImp$covariateIds[varImp$covariateIds!='(Intercept)'])
        size <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))
      } else if (method == "RandomForest" || method == "XGBoost") {
        varImp <- result$model$covariateImportance # result$model$model$coefficients
        vars <- varImp$covariateValue
        names(vars) <- varImp$covariateId
        size <- sum(sapply(vars, function(v) ifelse(v != 0, 1, 0)))
      } else if (method == "EXPLORE") {
        varImp <- result$model$covariateImportance
        vars <- varImp$covariateValue
        names(vars) <- varImp$covariateId
        size <- sum(varImp$covariateValue)
      } else {
        stop('Model not included')
        # TODO: check for all algorithms of interest
        # count_vars <- randomForest::varUsed(model_randomforest, by.tree = FALSE, count = TRUE)
        # names(count_vars) <- colnames(train)[1:(ncol(train)-1)]
        # vars <- lapply(colnames(models)[3:(ncol(models)-1)], function(c) ifelse(c %in% names(count_vars), count_vars[c], NA))
        # model <- c(list(method, i), as.list(vars), sum(sapply(vars, function(v) ifelse(v > 0, 1, 0)), na.rm = TRUE))
      }

      # TODO: remove variables excluded by unvariate analysis
      model <- c(method=method, iteration=1, option=i, as.list(vars), `model size`=size)
      models <- rbind(models, model)

      # Evaluate predictions
      # TODO: check/add CV?
      real_train <- result$prediction$outcomeCount[result$prediction$evaluationType == "Train"]
      predictions_train <- result$prediction$value[result$prediction$evaluationType == "Train"]

      real_test <- result$prediction$outcomeCount[result$prediction$evaluationType == "Test"]
      predictions_test <- result$prediction$value[result$prediction$evaluationType == "Test"]

      if (method != "EXPLORE") { # OR ALSO FOR EXPLORE??
        model_description <- paste0(size, " covariates")
        eval_train_prob <- evaluateModel(predictions_train, real_train, model=model_description, class=F)
        eval_train_class <- evaluateModel(prob_to_class(predictions_train, real_train), model=model_description, real_train)

        eval_test_prob <- evaluateModel(predictions_test, real_test, model=model_description, class=F)
        eval_test_class <- evaluateModel(prob_to_class(predictions_test, real_test), model=model_description, real_test)
      } else {
        model_description <- result$model$model$fit
        eval_train_prob <- exploreCurve(models_AUCcurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Train)
        eval_train_class <- evaluateModel(predictions_train, real_train, model=model_description)

        eval_test_prob <-  exploreCurve(models_AUCcurve = result$model$model$models_AUCcurve, plpModel=result$model, data=data$Test)
        eval_test_class <- evaluateModel(predictions_test, real_test, model=model_description)
      }

      eval <- append(append(append(eval_test_class, eval_test_prob), eval_train_class), eval_train_prob)
      names(eval) <- c(paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_test_prob), "_Test_Prob"), paste0(names(eval_train_class), "_Train_Class"), paste0(names(eval_train_prob), "_Train_Prob"))
      methods_output <- rbind(methods_output, c(append(list(Time = time, Data = plp, Method = method, Iteration = 1, Option = i, Model = model_description), eval)))

      # TODO: create accuracy bound from lasso performance
      # if (method == "lasso" && i == 1) {
      #   bound <- result[[5]][[o]]
      # }
    }

    write.csv(models, file.path(outputFolder, paste0("models_", plp, ".csv")), row.names = FALSE)
  }

  write.csv(summary_data, file.path(outputFolder, paste0("summary_data.csv")), row.names = FALSE)
  write.csv(methods_output, file.path(outputFolder, paste0("output_methods.csv")), row.names = FALSE)
}

### Launch shiny ###
if (shiny) {
  shiny::runApp('shiny')
}


