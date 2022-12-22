
library(PatientLevelPrediction)

### 1. BASIC SETTINGS

# reticulate::use_virtualenv("r-reticulate")
options(andromedatempdir = "J:/amarkus/Documents/R/Temp")

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

root <- "/home/amarkus/Documents/Benchmark problems/19dec2022"
path <- file.path(root, "output", "IPCI")

### 2. SELECT PREDICTION PROBLEM
# TODO: add "AsthmaExacerbation" (generate target)
# TODO: fix running in jobs

for (plp in c("HospitalReadmission", "EoLConversation", "HeartfailureStroke")) { # plp = "HospitalReadmission"
  outputFolder <- file.path(path, plp)
  cohortTable <- 'cohorts_v1'

  if (plp == "HospitalReadmission") {
    # Select cohorts
    cohortIds <- 1033
    cohortNames <- 'inpatient visit'
    outcomeIds <- 1034
    outcomeNames <- 'hospital readmission'

    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(firstExposureOnly = FALSE,
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

  } else if (plp == "AsthmaExacerbation") {
    # Select cohorts
    cohortIds <- "TODO"
    cohortNames <- 'asthma patient'
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
  }

  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                   cdmDatabaseSchema = "cdm",
                                                                   cdmDatabaseId = "CDM-M-20220414",
                                                                   cohortDatabaseSchema = "amarkustseinen",
                                                                   cohortTable = cohortTable,
                                                                   targetId = cohortIds,
                                                                   outcomeIds = outcomeIds ,
                                                                   outcomeDatabaseSchema = "amarkustseinen",
                                                                   outcomeTable = cohortTable,
                                                                   cdmDatabaseName = "IPCI")

  # Possible models
  model_lasso <- setLassoLogisticRegression(variance = 0.01)
  # model_lasso <- setLassoLogisticRegression(variance = 1, lowerLimit = 0.001, upperLimit = 5)
  # model_randomforest <- setRandomForest()
  # model_explore_3_100 <- setEXPLORE(output_path = paste0(outputFolder, "/explore_results_3_100/"), variableNumber = 100, start_rule_length = 3, end_rule_length = 3)

  # Possible covariates
  covariateSettingList_default <- FeatureExtraction::createDefaultCovariateSettings()
  covariateSettingList_agesex <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T)
  covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T,
                                                                     useConditionGroupEraAnyTimePrior = T, useConditionGroupEraLongTerm = T,
                                                                     useConditionGroupEraShortTerm = T, useDrugGroupEraLongTerm = T,
                                                                     useDrugGroupEraShortTerm = T, useDrugGroupEraOverlapping = T,
                                                                     useCharlsonIndex = T, useDcsi = T,
                                                                     useChads2 = T, useChads2Vasc = T)

  # Selected covariates
  covariateSettingList <- covariateSettingList

  # Create model design
  run_lasso <- createModelDesign(
    targetId = cohortIds,
    outcomeId = outcomeIds,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
    populationSettings = populationSettingList,
    covariateSettings = covariateSettingList,
    featureEngineeringSettings = createFeatureEngineeringSettings(),
    sampleSettings = createSampleSettings(),
    preprocessSettings = createPreprocessSettings(minFraction = 0.001, normalize = T, removeRedundancy = F),
    modelSettings = model_lasso,
    splitSettings = createDefaultSplitSetting(
      type = "stratified",
      testFraction = 0.25,
      trainFraction = 0.75,
      splitSeed = 123,
      nfold = 3
    ),
    runCovariateSummary = T
  )

  # check python environment
  # run_rf <- createModelDesign(
  #   targetId = cohortIds,
  #   outcomeId = outcomeIds,
  #   restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = T, washoutPeriod = 365),
  #   populationSettings = populationSettingList,
  #   covariateSettings = covariateSettingList,
  #   featureEngineeringSettings = createFeatureEngineeringSettings(),
  #   sampleSettings = createSampleSettings(),
  #   preprocessSettings = createPreprocessSettings(minFraction = 0.0001, normalize = T, removeRedundancy = F),
  #   modelSettings = model_randomforest,
  #   splitSettings = createDefaultSplitSetting(
  #     type = "stratified",
  #     testFraction = 0.25,
  #     trainFraction = 0.75,
  #     splitSeed = 123,
  #     nfold = 3
  #   ),
  #   runCovariateSummary = T
  # )
  modelList <- list(run_lasso)

  # job::job({
  ### 3. RUN ANALYSIS
  result <- PatientLevelPrediction::runMultiplePlp(
    databaseDetails = databaseDetails,
    modelDesignList = modelList,
    onlyFetchData = F,
    logSettings = createLogSettings(
      verbosity = "DEBUG",
      timeStamp = T,
      logName = "runPlp Log"
    ),
    saveDirectory = outputFolder)
  # })
}

### 4. Check results
# PatientLevelPrediction::viewPlp(runPlp = runPlp)

