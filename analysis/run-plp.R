
library(PatientLevelPrediction)

### HELPER FUNCTIONS
createCohortCovariateSettings <- function(covariateName, covariateId,
                                          cohortDatabaseSchema, cohortTable, cohortId,
                                          startDay=-30, endDay=0, count=F,
                                          ageInteraction = F) {
  covariateSettings <- list(covariateName=covariateName, covariateId=covariateId,
                            cohortDatabaseSchema=cohortDatabaseSchema,
                            cohortTable=cohortTable,
                            cohortId=cohortId,
                            startDay=startDay,
                            endDay=endDay,
                            count=count,
                            ageInteraction=ageInteraction)

  attr(covariateSettings, "fun") <- "getCohortCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' Extracts covariates based on cohorts
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getCohortCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "5",
                                   cohortTable = "#cohort_person",
                                   rowIdField = "row_id",
                                   aggregated,
                                   cohortId,
                                   covariateSettings) {



  # Some SQL to construct the covariate:
  sql <- paste(
    "select a.@row_id_field AS row_id, @covariate_id AS covariate_id,",
    "{@ageInteraction}?{max(YEAR(a.cohort_start_date)-p.year_of_birth)}:{",
    "{@countval}?{count(distinct b.cohort_start_date)}:{max(1)}",
    "} as covariate_value",
    "from @cohort_temp_table a inner join @covariate_cohort_schema.@covariate_cohort_table b",
    " on a.subject_id = b.subject_id and ",
    " b.cohort_start_date <= dateadd(day, @endDay, a.cohort_start_date) and ",
    " b.cohort_end_date >= dateadd(day, @startDay, a.cohort_start_date) ",
    "{@ageInteraction}?{inner join @cdm_database_schema.person p on p.person_id=a.subject_id}",
    "where b.cohort_definition_id = @covariate_cohort_id
    group by a.@row_id_field "
  )

  sql <- SqlRender::render(sql,
                           covariate_cohort_schema = covariateSettings$cohortDatabaseSchema,
                           covariate_cohort_table = covariateSettings$cohortTable,
                           covariate_cohort_id = covariateSettings$cohortId,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           covariate_id = covariateSettings$covariateId,
                           endDay=covariateSettings$endDay,
                           countval = covariateSettings$count,
                           ageInteraction = covariateSettings$ageInteraction,
                           cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  # Construct covariate reference:
  sql <- "select @covariate_id as covariate_id, '@concept_set' as covariate_name,
  456 as analysis_id, -1 as concept_id"
  sql <- SqlRender::render(sql, covariate_id = covariateSettings$covariateId,
                           concept_set=paste(ifelse(covariateSettings$count, 'Number of', ''),
                                             covariateSettings$covariateName,
                                             ifelse(covariateSettings$ageInteraction, ' X Age', ''),
                                             ' days before:', covariateSettings$startDay, 'days after:', covariateSettings$endDay)

  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariateRef:
  covariateRef  <- DatabaseConnector::querySql(connection, sql)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

  analysisRef <- data.frame(analysisId = 456,
                            analysisName = "cohort covariate",
                            domainId = "cohort covariate",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")

  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}

### 1. BASIC SETTINGS

# reticulate::use_virtualenv("r-reticulate")
options(andromedatempdir = "J:/amarkus/Documents/R/Temp")

# Details for connecting to the server:
dbms <- 'postgresql'
user <- 'amarkus'
pw <- 'amarkus'
# server <- 'Res-Srv-Lin-02/CDM-K-20210317'
server <- 'Res-Srv-Lin-02/CDM-M-20220414'
port <- 5432

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

root <- "/home/amarkus/Documents/Experiments_EXPLORE/7 nov 22"
# root <- "J:/amarkus/Documents/Experiments_EXPLORE/4 nov 22"
path <- file.path(root, "output", "IPCI")

### 2. SELECT PREDICTION PROBLEM
for (plp in c("Dementia", "AtrialFibrillation", "AsthmaStepUp", "OutpatientMortality")) { # plp = "Dementia", "AtrialFibrillation"
#   OutpatientMortality <- FALSE
#   AsthmaStepUp <- FALSE
#   COVER <- FALSE
#   Dementia <- TRUE
#   AtrialFibrillation <- FALSE

  if (plp == "OutpatientMortality") {
    # Predicting mortality for outpatient visits
    outputFolder <- file.path(path, "OutpatientMortality_v3")
    cohortIds <- 483
    cohortNames <- 'outpatient visits'
    outcomeIds <- 514
    outcomeNames <- 'mortality'
    cohortTable <- "outpatientdeathcohort"
  }

  if (plp == "AsthmaStepUp") { # Bad performing/ change?
    # Predicting step up to higher treatment for asthma patients starting with saba monotherapy
    outputFolder <- file.path(path, "AsthmaStepUp_v3")
    cohortIds <- 647
    cohortNames <- 'asthma saba monotherapy'
    outcomeIds <- 648
    outcomeNames <- 'asthma higher step'
    cohortTable <- "asthmastepupcohort"
    # cohortTable <- "asthmastepup01cohort"
  }

  # if (plp == "COVER") {
  #   outputFolder <- file.path(path, "COVER_v2")
  #   cohortIds <- 1001
  #   cohortNames <- 'flu or covid patients'
  #   outcomeIds <- 4001
  #   outcomeNames <- 'death' # choose other outcome?
  #   cohortTable <- "covidsimplemodelscohort"
  # }

  if (plp == "Dementia") {
    outputFolder <- file.path(path, "Dementia_v3")
    cohortIds <- 657
    cohortNames <- 'Outpatient visits'
    outcomeIds <- 658
    outcomeNames <- 'Dementia'
    cohortTable <- "dementiacohort"
  }

  if (plp == "AtrialFibrillation") {
    outputFolder <- file.path(path, "AtrialFibrillation_v3")
    cohortIds <- 662
    cohortNames <- 'atrial fibrillation or flutter'
    outcomeIds <- 663
    outcomeNames <- 'death'
    cohortTable <- "atrialfibrillationcohort"
  }

  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(connectionDetails = connectionDetails,
                                                                   cdmDatabaseSchema = "cdm",
                                                                   cdmDatabaseId = "CDM-M-20220414",
                                                                   cohortDatabaseSchema = "amarkus",
                                                                   cohortTable = cohortTable,
                                                                   targetId = cohortIds,
                                                                   outcomeIds = outcomeIds ,
                                                                   outcomeDatabaseSchema = "amarkus",
                                                                   outcomeTable = cohortTable,
                                                                   cdmDatabaseName = "IPCI")
  # _v2 -> FeatureExtraction file DomainConceptGroup.sql -> added: AND min_levels_of_separation <= 3

  # Possible models
  model_lasso <- setLassoLogisticRegression(variance = 0.01)
  # model_lasso <- setLassoLogisticRegression(variance = 1, lowerLimit = 0.001, upperLimit = 5)

  # model_randomforest <- setRandomForest()
  # model_explore_3_100 <- setEXPLORE(output_path = paste0(outputFolder, "/explore_results_3_100/"), variableNumber = 100, start_rule_length = 3, end_rule_length = 3)
  # model_explore_3_300 <- setEXPLORE(output_path = paste0(outputFolder, "/explore_results_3_300/"), variableNumber = 300, start_rule_length = 3, end_rule_length = 3)
  #
  # model_explore_4_100 <- setEXPLORE(output_path = paste0(outputFolder, "/explore_results_4_100/"), variableNumber = 100, start_rule_length = 4, end_rule_length = 4)
  # model_explore_4_300 <- setEXPLORE(output_path = paste0(outputFolder, "/explore_results_4_300/"), variableNumber = 300, start_rule_length = 4, end_rule_length = 4)

  # Possible covariates
  covariateSettingList_default <- FeatureExtraction::createDefaultCovariateSettings()
  covariateSettingList_agesex <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T)

  covariateSettingList <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T,
                                                                     useDemographicsRace = T, useDemographicsEthnicity = T,
                                                                     useConditionGroupEraAnyTimePrior = T, useConditionGroupEraLongTerm = T,
                                                                     useConditionGroupEraShortTerm = T, useDrugGroupEraLongTerm = T,
                                                                     useDrugGroupEraShortTerm = T, useDrugGroupEraOverlapping = T,
                                                                     useCharlsonIndex = T, useDcsi = T,
                                                                     useChads2 = T, useChads2Vasc = T)

  if (plp == "OutpatientMortality") {
    # Select model
    # modelList <- list(model_lasso, model_randomforest)

    # Select covariates
    covariateSettingList <- covariateSettingList

    # Get plpdata
    # plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
    #                                               restrictPlpDataSettings = restrictPlpDataSettings,
    #                                               covariateSettings = covariateSettingList)

    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(minTimeAtRisk = 59, riskWindowEnd = 60, removeSubjectsWithPriorOutcome = TRUE)

  }

  if (plp == "AsthmaStepUp") {
    # Select model
    # modelList <- list(model_lasso, model_randomforest)

    # Select covariates
    covariateSettingList <- covariateSettingList

    # Get plpdata
    # plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
    #                                               restrictPlpDataSettings = restrictPlpDataSettings,
    #                                               covariateSettings = covariateSettingList)
    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 365, removeSubjectsWithPriorOutcome = FALSE)

  }

  if (plp == "COVER") {
    # Select model
    #modelList <- list(model_lasso, model_randomforest)

    # Select covariates
    standardCovariates <- FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = T,
                                                                     useDemographicsGender = T,
                                                                     excludedCovariateConceptIds = 8532)


    pathToCustom <- file.path(root, "COVER_customcovariates.csv")

    cohortVarsToCreate <- utils::read.csv(pathToCustom)
    covSets <- list()
    length(covSets) <- nrow(cohortVarsToCreate)+1
    covSets[[1]] <- standardCovariates

    for(i in 1:nrow(cohortVarsToCreate)){
      covSets[[1+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                      covariateId = cohortVarsToCreate$cohortId[i]*1000+456,
                                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                                      cohortTable = cohortTable,
                                                      cohortId = cohortVarsToCreate$atlasId[i],
                                                      startDay=cohortVarsToCreate$startDay[i],
                                                      endDay=-1,
                                                      count= as.character(cohortVarsToCreate$count[i]),
                                                      ageInteraction = as.character(cohortVarsToCreate$ageInteraction[i]))
    }

    covariateSettingList <- list()
    covariateSettingList[[1]] <- covSets

    # covariateSettingList <- covariateSettingList_default

    # Get plpdata
    # plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
    #                                               restrictPlpDataSettings = restrictPlpDataSettings,
    #                                               covariateSettings = covariateSettingList)
    #
    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(binary = T, riskWindowStart = 0, startAnchor = 'cohort start',
                                                                                   riskWindowEnd = 30, endAnchor = 'cohort start',
                                                                                   firstExposureOnly = F, removeSubjectsWithPriorOutcome = F,
                                                                                   priorOutcomeLookback = 99999, requireTimeAtRisk = F,
                                                                                   minTimeAtRisk = 1, includeAllOutcomes = T)

  }

  if (plp == "Dementia") {
    # Select model
    # modelList <- list(model_lasso, model_randomforest)
    # modelList <- model_lasso

    # Select covariates
    covariateSettingList <- covariateSettingList

    # Get plpdata
    # plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
    #                                               restrictPlpDataSettings = restrictPlpDataSettings,
    #                                               covariateSettings = covariateSettingList)
    #
    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(minTimeAtRisk = 1824, riskWindowEnd = 1825, removeSubjectsWithPriorOutcome = TRUE)
  }

  if (plp == "AtrialFibrillation") {
    # Select model
    # modelList <- list(model_lasso, model_randomforest)

    # Select covariates
    covariateSettingList <- covariateSettingList

    # Get plpdata
    # plpData <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails,
    #                                               restrictPlpDataSettings = restrictPlpDataSettings,
    #                                               covariateSettings = covariateSettingList)
    #
    # Select population
    populationSettingList <- PatientLevelPrediction::createStudyPopulationSettings(minTimeAtRisk = 1094, riskWindowEnd = 1095, removeSubjectsWithPriorOutcome = TRUE)

  }

  ### 3. RUN ANALYSIS
  run_lasso <- createModelDesign(
    targetId = cohortIds,
    outcomeId = outcomeIds,
    restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = T, washoutPeriod = 365),
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
    runCovariateSummary = F
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

  # result <- PatientLevelPrediction::runPlp(plpData = plpData,
  #                                          outcomeId =
  #                                            outcomeIds,
  #                                          modelSettings = modelList,
  #                                          populationSettings = populationSettingList,
  #                                          splitSettings = createDefaultSplitSetting(),
  #                                          sampleSettings = createSampleSettings(),
  #                                          featureEngineeringSettings = createFeatureEngineeringSettings(),
  #                                          preprocessSettings = createPreprocessSettings(minFraction = 0.0001, normalize = T, removeRedundancy = F),
  #                                          logSettings = createLogSettings(),
  #                                          executeSettings = createExecuteSettings(runSplitData = T,
  #                                                                                  runSampleData = T,
  #                                                                                  runfeatureEngineering = T,
  #                                                                                  runPreprocessData = T,
  #                                                                                  runModelDevelopment = T,
  #                                                                                  runCovariateSummary = T ),
  #                                          saveDirectory = outputFolder)

}
### 4. Check results
# PatientLevelPrediction::viewPlp(result)

runPlp$performanceEvaluation$evaluationStatistics

