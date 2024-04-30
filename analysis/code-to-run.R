# remotes::install_github("mi-erasmusmc/patientlevelprediction@explore")
library(PatientLevelPrediction)

# remotes::install_github("mi-erasmusmc/explore@main")
library(Explore)

library(Eunomia)
library(DatabaseConnector)
library(RWeka)
library(CohortGenerator)
library(data.table)
library(dplyr)
library(caret)
library(pracma)

# reticulate::use_condaenv("todo")

plp <- "Test"
plptasks <- c("HospitalReadmission", "COPDMortality", "EoLConversation", "HeartfailureStroke", "AsthmaExacerbation")
covariates <- "Phenotypes" # "Default", "AgeSex", "Full", "Phenotypes"

cohorts <- FALSE
selection <- TRUE
train <- TRUE

# Paths
root <- getwd()
# outputFolder <- file.path(root, "shiny", "output", Sys.Date())
outputFolder <- file.path(root, "shiny", "output", "2023-04-05")

if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}

for (plp in plptasks) {

  ### Specification prediction tasks ###
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
    # Details for connecting to the server:
    dbms <- 'todo'
    user <- 'todo'
    pw <- 'todo'
    server <- 'todo'
    port <- 'todo'

    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                    server = server,
                                                                    user = user,
                                                                    password = pw,
                                                                    port = port)
    cohortTable <- 'cohorts_explore'
    baseUrl <- 'todo'

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
      cohortId <- 1036
      cohortNames <- 'old patient with GP visit'
      outcomeId <- 1038
      outcomeNames <- 'conversation'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 179,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 180,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)
    } else if (plp == "HeartfailureStroke") {
      # Select cohorts
      cohortId <- 1060
      cohortNames <- 'T2DM patients'
      outcomeId <- 1058
      outcomeNames <- 'heart failure or stroke'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 1824,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 1825,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "AsthmaExacerbation") {
      # Select cohorts
      cohortId <- 1078
      cohortNames <- 'asthma patients'
      outcomeId <- 1054
      outcomeNames <- 'asthma exacerbation'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 729,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 730,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPriorOutcome = FALSE)

    } else if (plp == "COPDMortality") {

      # Select cohorts
      cohortId <- 1035
      cohortNames <- 'COPD patients'
      outcomeId <- 1037
      outcomeNames <- 'all-cause mortality'

      # Select population
      populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 365,
                                                                                  minTimeAtRisk = 729,
                                                                                  riskWindowStart = 1,
                                                                                  startAnchor = "cohort start",
                                                                                  riskWindowEnd = 730,
                                                                                  endAnchor = "cohort start",
                                                                                  removeSubjectsWithPrior = FALSE)

    }

    cdmDatabaseSchema = 'cdm'
    cohortDatabaseSchema = 'todo'

    databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                                     cdmDatabaseId = "CDM-O-20230321",
                                                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                                                     cohortTable = cohortTable,
                                                                     targetId = cohortId,
                                                                     outcomeIds = outcomeId,
                                                                     outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                     outcomeTable = cohortTable,
                                                                     cdmDatabaseName = "IPCI")

  }

  ### Generate cohorts ###
  if (cohorts) {
    cohorts_study = TRUE
    cohorts_phenotypes = TRUE

    # Create cohort tables
    cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

    CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTableNames = cohortTableNames,
                                        incremental = TRUE)

    # List of cohorts to create
    cohortsToCreate <- data.frame()
    if (cohorts_study) {
      cohortsToCreate_study <- data.frame(rbind(c("Hospital Readmission target; inpatient visit discharge as end date",1033),
                                                c("Hospital Readmission outcome; inpatient visit",1034),
                                                c("EoL conversations target; latest outpatient visit 2016-2021",1036),
                                                c("EoL conversations outcome; first eol conversation",1038),
                                                c("Mortality in COPD target; first COPD diagnosis",1035),
                                                c("Mortality in COPD outcome; death",1037),
                                                c("Asthma exacerbation target;", 1078),
                                                c("Asthma exacerbation outcome;", 1054),
                                                c("Heart failure Stroke target; type 2 diabetes mellitus with no prior stroke or heart failure",1060),
                                                c("Heart failure Stroke outcome; first heart failure or stroke",1058)))
      colnames(cohortsToCreate_study)<-c("cohortName","atlasId")
      cohortsToCreate_study$atlasId<-as.numeric(cohortsToCreate_study$atlasId)

      write.csv(cohortsToCreate_study, file.path(root, "cohortsToCreate_study.csv"), row.names = FALSE)
      cohortsToCreate <- rbind(cohortsToCreate, cohortsToCreate_study)
    }

    if (cohorts_phenotypes) {
      covariateIds <- c(1200:1208, 1210:1263) # SKIP cohort 1209 - cohort generation not finishing

      cohortsToCreate_phenotypes <- ROhdsiWebApi::exportCohortDefinitionSet(
        baseUrl = baseUrl,
        cohortIds = unique(covariateIds)
      )
      cohortsToCreate_phenotypes$cohortName <- gsub("[PLPCov] ", "", cohortsToCreate_phenotypes$cohortName, fixed=TRUE)
      cohortsToCreate_phenotypes$cohortName <- gsub(" - v1", "", cohortsToCreate_phenotypes$cohortName, fixed=TRUE)

      cohortsToCreate_phenotypes <- cohortsToCreate_phenotypes[,c("cohortName","atlasId")]
      write.csv(cohortsToCreate_phenotypes, file.path(root, "cohortsToCreate_phenotypes.csv"), row.names = FALSE)

      cohortsToCreate <- rbind(cohortsToCreate, cohortsToCreate_phenotypes)
    }

    connection <- connect(connectionDetails)

    # Generate target / outcome cohorts first time running study
    for (i in 1:nrow(cohortsToCreate)) {
      writeLines(paste("Copying cohort:", cohortsToCreate$cohortName[i]))
      ROhdsiWebApi::insertCohortDefinitionInPackage(cohortId = cohortsToCreate$atlasId[i],
                                                    name = cohortsToCreate$cohortName[i],
                                                    jsonFolder = file.path(save_path, "JSON"),
                                                    sqlFolder = file.path(save_path, "SQL"),
                                                    baseUrl = baseurl,
                                                    generateStats = F)

      writeLines(paste("Creating cohort:", cohortsToCreate$cohortName[i]))
      sql <- SqlRender::readSql(paste0("cohorts/SQL/", cohortsToCreate$cohortName[i], ".sql"))
      sql <- SqlRender::render(sql,
                               vocabulary_database_schema = cdmDatabaseSchema,
                               cdm_database_schema = cdmDatabaseSchema,
                               target_database_schema = cohortDatabaseSchema,
                               target_cohort_table = cohortTable,
                               target_cohort_id = cohortsToCreate$atlasId[i])
      sql <- SqlRender::translate(sql,
                                  targetDialect = attr(connection, "dbms"))
      DatabaseConnector::executeSql(connection, sql)
    }

    # All cohorts generated
    cohorts = FALSE

  }

  ### Pre-process data ###
  if (selection) {

    # Possible covariates
    if (covariates == "Default") { # Not for EXPLORE without pre-variable selection
      covariateSettingList <- FeatureExtraction::createDefaultCovariateSettings()
    } else if (covariates == "AgeSex") {
      covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T)
    } else if (covariates == "Full") { # Not for EXPLORE without pre-variable selection
      covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T,
                                                                         useConditionGroupEraAnyTimePrior = T, useConditionGroupEraLongTerm = T,
                                                                         useConditionGroupEraShortTerm = T, useDrugGroupEraLongTerm = T,
                                                                         useDrugGroupEraShortTerm = T, useDrugGroupEraOverlapping = T)
    } else if (covariates == "Phenotypes") {
      cohortsToCreate_phenotypes <- read.csv(file.path(root, "cohortsToCreate_phenotypes.csv")) # TODO: always available?

      covariateSettingList <- # Phenotypes from Henrik.
        list(
          FeatureExtraction::createCovariateSettings(
            useDemographicsGender = T,
            useDemographicsAge = T) # useDemographicsAgeGroup = T
        )
      for (i in 1:nrow(cohortsToCreate_phenotypes)){
        covariateSettingList <- append(covariateSettingList,
                                       list(PatientLevelPrediction::createCohortCovariateSettings(
                                         cohortName = cohortsToCreate_phenotypes$cohortName[i],
                                         settingId = 1,
                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                         cohortTable = cohortTable,
                                         cohortId = cohortsToCreate_phenotypes$atlasId[i],
                                         startDay = -365,
                                         endDay = 0,
                                         analysisId = cohortsToCreate_phenotypes$atlasId[i])))
      }
    }

    # Selected covariates
    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = databaseDetails,
      restrictPlpDataSettings = createRestrictPlpDataSettings(),
      covariateSettings = covariateSettingList
    )

    # Get dataset (no selection)
    plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                                 outcomeId = outcomeId,
                                                 modelSettings = PatientLevelPrediction::setLassoLogisticRegression(variance=0.01),
                                                 analysisId = paste0("Data_", plp, "_", covariates),
                                                 analysisName = paste0("No selection"),
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
                                                                                         runModelDevelopment = F,
                                                                                         runCovariateSummary = F),
                                                 saveDirectory = outputFolder,
                                                 saveData = T)

    # Get reduced dataset (apply pre-variable selection)
    # modelSettings <- PatientLevelPrediction::setUnivariateSelection(modelSettings=setLassoLogisticRegression(),
    #                                                                 nVariables = nVar,
    #                                                                 saveDirectory = outputFolder)
    #
    # plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
    #                                              outcomeId = outcomeId,
    #                                              modelSettings = modelSettings,
    #                                              analysisId = paste0("Data_Univariate", nVar, "_", plp),
    #                                              analysisName = paste0("Univariate selection with top ", nVar, " correlated variables"),
    #                                              populationSettings = populationSettings,
    #                                              splitSettings = createDefaultSplitSetting(),
    #                                              sampleSettings = createSampleSettings(),
    #                                              featureEngineeringSettings = createFeatureEngineeringSettings(),
    #                                              preprocessSettings = createPreprocessSettings(minFraction=0.001,
    #                                                                                            normalize = T,
    #                                                                                            removeRedundancy = T),
    #                                              logSettings = createLogSettings(),
    #                                              executeSettings = createExecuteSettings(runSplitData = T,
    #                                                                                      runSampleData = T,
    #                                                                                      runfeatureEngineering = T,
    #                                                                                      runPreprocessData = T,
    #                                                                                      runModelDevelopment = T,
    #                                                                                      runCovariateSummary = F),
    #                                              saveDirectory = outputFolder,
    #                                              saveData = T,
    #                                              loadData = NULL)
  }


  ### Train models ###
  if (train) {

    # Possible models
    list_models <- list()

    # LASSO
    list_models[["LASSO"]] <- list(analysisId=paste0("LASSO_", plp, "_1"),
                                   analysisName=paste0("LASSO logistic regression"),
                                   modelSettings=PatientLevelPrediction::setLassoLogisticRegression(variance = 0.01))

    # Gradient boosting machine
    list_models[["XGBoost"]] <- list(analysisId=paste0("XGBoost_", plp, "_1"),
                                     analysisName=paste0("Gradient boosting machine"),
                                     modelSettings=PatientLevelPrediction::setGradientBoostingMachine(ntrees=c(100,300), maxDepth=c(4,8)))

    # Random forest
    list_models[["RandomForest"]] <- list(analysisId=paste0("RandomForest_", plp, "_1"),
                                          analysisName=paste0("Random forest"),
                                          modelSettings=PatientLevelPrediction::setRandomForest(ntrees=list(100,300), maxDepth=list(4,8)))

    # RIPPER
    list_models[["RIPPER"]] <- list(analysisId=paste0("RIPPER_", plp, "_1"),
                                    analysisName=paste0("RIPPER"),
                                    modelSettings=PatientLevelPrediction::setRIPPER(variableSelection = NULL,
                                                                                    saveDirectory = file.path(outputFolder, paste0("RIPPER_", plp, "_1"))))
    # Decision tree
    list_models[["DecisionTree2"]] <- list(analysisId=paste0("DecisionTree_", plp, "_2"),
                                           analysisName=paste0("DecisionTree_"),
                                           modelSettings=PatientLevelPrediction::setDecisionTree(maxDepth = list(2))) # , maxFeatures = list(nVar)
    list_models[["DecisionTree3"]] <- list(analysisId=paste0("DecisionTree_", plp, "_3"),
                                           analysisName=paste0("DecisionTree_"),
                                           modelSettings=PatientLevelPrediction::setDecisionTree(maxDepth = list(3))) # , maxFeatures = list(nVar)

    # Iterative hard thresholding
    list_models[["IHT10"]] <- list(analysisId=paste0("IHT_", plp, "_10"),
                                   analysisName=paste0("IHT"),
                                   modelSettings=PatientLevelPrediction::setIterativeHardThresholding(K=10, fitBestSubset = TRUE))
    list_models[["IHT5"]] <- list(analysisId=paste0("IHT_", plp, "_5"),
                                  analysisName=paste0("IHT"),
                                  modelSettings=PatientLevelPrediction::setIterativeHardThresholding(K=5, fitBestSubset = TRUE))

    # EXPLORE
    explore_options <- expand.grid(StartRulelength = c(1),
                                   EndRulelength = c(3, 4, 5),
                                   Parallel = c(TRUE),
                                   Sorted = c("none"),
                                   Maximize = c("BALANCEDACCURACY"),
                                   stringsAsFactors = FALSE) # TODO: check what happens if no solution
    explore_options$Option <- 3:5
    write.csv(explore_options, file.path(outputFolder, "explore_options.csv"), row.names = FALSE)

    for (option in explore_options$Option) {
      analysisId <- paste0("EXPLORE_", plp, "_", option)

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = explore_options$StartRulelength[explore_options$Option == option],
                                                          endRulelength = explore_options$EndRulelength[explore_options$Option == option],
                                                          maximize = explore_options$Maximize[explore_options$Option == option],
                                                          parallel = explore_options$Parallel[explore_options$Option == option],
                                                          modelsCurve = TRUE,
                                                          sort_by = explore_options$Sorted[explore_options$Option == option],
                                                          saveDirectory = file.path(outputFolder, paste0(analysisId, "_", covariates)))

      list_models[[paste0("EXPLORE", option)]] <- list(analysisId=analysisId,
                                                       analysisName=paste0("EXPLORE decision rule"),
                                                       modelSettings=modelSettings)

    }

    # GOSDT
    list_models[["GOSDT"]] <- list(analysisId=paste0("GOSDT_", plp, "_1"),
                                   analysisName=paste0("GOSDT"),
                                   modelSettings=PatientLevelPrediction::setGOSDT())

    # Rashomon Ratio
    for (option in explore_options$Option) {
      analysisId <- paste0("EXPLORE_", plp, "_", option, "_RR")

      modelSettings <- PatientLevelPrediction::setExplore(variableSelection = NULL,
                                                          startRulelength = explore_options$StartRulelength[explore_options$Option == option],
                                                          endRulelength = explore_options$EndRulelength[explore_options$Option == option],
                                                          maximize = explore_options$Maximize[explore_options$Option == option],
                                                          balancedAccuracy = 0.7, # TODO: vary based on plp / option
                                                          parallel = explore_options$Parallel[explore_options$Option == option],
                                                          modelsCurve = FALSE,
                                                          sort_by = explore_options$Sorted[explore_options$Option == option],
                                                          saveDirectory = file.path(outputFolder, analysisId))

      list_models[[paste0("EXPLORE", option, "_RR")]] <- list(analysisId=analysisId,
                                                              analysisName=paste0("EXPLORE decision rule"),
                                                              modelSettings=modelSettings)
    }

    # Run for all models
    for (m in names(list_models)) {

      model <- list_models[[m]]

      plpResults <- PatientLevelPrediction::runPlp(plpData = plpData,
                                                   outcomeId = outcomeId,
                                                   modelSettings = model$modelSettings,
                                                   analysisId = paste0(model$analysisId, "_", covariates), # Add name of selected covariates
                                                   analysisName = model$analysisName,
                                                   populationSettings = populationSettings,
                                                   splitSettings = createDefaultSplitSetting(),
                                                   sampleSettings = createSampleSettings(),
                                                   featureEngineeringSettings = createFeatureEngineeringSettings(),
                                                   preprocessSettings = createPreprocessSettings(minFraction=0.001,
                                                                                                 normalize = T,
                                                                                                 removeRedundancy = F),
                                                   logSettings = createLogSettings(),
                                                   executeSettings = createExecuteSettings(runSplitData = F, # T
                                                                                           runSampleData = F,
                                                                                           runfeatureEngineering = F,
                                                                                           runPreprocessData = F, # T
                                                                                           runModelDevelopment = T,
                                                                                           runCovariateSummary = T),
                                                   saveDirectory = outputFolder,
                                                   saveData = F,
                                                   loadData = file.path(outputFolder, paste0("Data_", plp, "_", covariates), "TrainTestData"))
    }
  }
}


