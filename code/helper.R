#' @export
scale_data <- function(data, colMean, colSD) {
  data <- sapply(1:ncol(data), function(c) {(data[,c] - colMean[c]) / colSD[c]})
}

#' @export
round_dt <- function(x, digits) {
  # round all numeric variables
  # x: data table
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[,colnames(x)[numeric_columns]] <- round(x[,..numeric_columns], digits)
  x
}

#' @export
create_bootstrap_samples <- function(file, num_samples = 10, sample_size = 10000) {
  set.seed(2021)

  bootstrap_samples <- list()

  for (s in 1:num_samples) {
    bootstrap <- sample(1:nrow(file), sample_size, replace = TRUE)
    bootstrap_samples[[s]] <- bootstrap
  }
  return(bootstrap_samples)
}

evaluateModel <- function(predictions, real, model=NULL, class=T) {

  if (length(predictions) != length(real)) {
    warning("Length not same")
  }

  if (is.null(predictions)) {
    if (class) {
      results <- list(Perf_AUC=NA,
                      Perf_AUPRC=NA,
                      Perf_PAUC=NA,
                      Perf_Accuracy=NA,
                      Perf_Sensitivity=NA,
                      Perf_Specificity=NA,
                      Perf_PPV=NA,
                      Perf_NPV=NA,
                      Perf_BalancedAccuracy=NA,
                      Perf_F1score=NA,
                      Curve_TPR=NA,
                      Curve_FPR=NA,
                      Curve_Thresholds=NA,
                      Curve_Models=NA,
                      N_outcomes=NA,
                      N_controls=NA,
                      N_total=NA)
    } else {
      results <- list(Perf_AUC=NA,
                      Perf_AUPRC=NA,
                      Perf_PAUC=NA,
                      Perf_Accuracy=NA,
                      Curve_Sensitivity=NA,
                      Curve_Specificity=NA,
                      Curve_PPV=NA,
                      Curve_NPV=NA,
                      Perf_BalancedAccuracy=NA,
                      Perf_F1score=NA,
                      Curve_TPR=NA,
                      Curve_FPR=NA,
                      Curve_Thresholds=NA,
                      Curve_Models=NA,
                      N_outcomes=NA,
                      N_controls=NA,
                      N_total=NA)
    }
    return(results)
  } else if (length(unique(predictions)) == 1) {
    warning('No variation in predictions!')
  }

  if (class) { # Return for classes
    conf_matrix <- table(factor(predictions, levels = c(0,1)), factor(real, levels = c(0,1))) # binary prediction
    summary_performance <- caret::confusionMatrix(conf_matrix, positive = '1')

    accuracy <- summary_performance$overall['Accuracy']
    sensitivity <- summary_performance$byClass['Sensitivity']
    specificity <- summary_performance$byClass['Specificity']
    PPV <- summary_performance$byClass['Pos Pred Value']
    NPV <- summary_performance$byClass['Neg Pred Value']
    balanced_accuracy <- summary_performance$byClass['Balanced Accuracy']
    F1_score <- summary_performance$byClass['F1']

    curve_TPR <- c(1.0, sensitivity, 0)
    curve_FPR <- c(1.0, 1-specificity, 0)

    results <- list(Perf_AUC=pracma::trapz(curve_FPR[length(curve_FPR):1],curve_TPR[length(curve_TPR):1]), # roc$auc,
                    Perf_AUPRC=tryCatch(PRROC::pr.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)$auc.integral, error=function(err) NA), # to catch exception when only outcomes or non-outcomes
                    Perf_PAUC=pROC::auc(real, predictions, partial.auc = c(1,0.8), partial.auc.focus = "sensitivity", partial.auc.correct=TRUE, levels = c("0", "1"), direction = "<"),
                    Perf_Accuracy=accuracy,
                    Perf_Sensitivity=sensitivity,
                    Perf_Specificity=specificity,
                    Perf_PPV=PPV,
                    Perf_NPV=NPV,
                    Perf_BalancedAccuracy=balanced_accuracy,
                    Perf_F1score=F1_score,
                    Curve_TPR=paste(curve_TPR, collapse = "_"),
                    Curve_FPR=paste(curve_FPR, collapse = "_"),
                    Curve_Thresholds=paste(c(-Inf, 0.5, Inf), collapse = "_"),
                    Curve_Models=ifelse(is.null(model), NA, model),
                    N_outcomes=sum(real == 1),
                    N_controls=sum(real == 0),
                    N_total=length(real))

  } else { # Return for probabilities

    roc <- pROC::roc(real, predictions, levels = c("0", "1"), direction = "<")

    curve_sensitivity <- roc$sensitivities
    curve_specificity <- roc$specificities
    curve_TPR <- curve_sensitivity
    curve_FPR <- 1 - curve_specificity
    curve_thresholds <- roc$thresholds

    # Reduce number of thresholds
    if (length(curve_thresholds) > 20) {
      indexes <- round(seq(from=1,to=length(curve_thresholds),length.out = 20))

      curve_TPR <- curve_TPR[indexes]
      curve_FPR <- curve_FPR[indexes]
      curve_sensitivity <- curve_sensitivity[indexes]
      curve_specificity <- curve_specificity[indexes]
      curve_thresholds <- curve_thresholds[indexes]
    }

    # For each threshold compute other statistics (not automatically included in above functionality)
    curve_Accuracy <- c()
    curve_PPV <- c()
    curve_NPV <- c()
    curve_BalancedAccuracy <- c()
    curve_F1score <- c()

    for (c in 1:length(curve_thresholds)) {

      pred_class_c <- as.numeric(ifelse(predictions >= curve_thresholds[[c]], 1, 0))
      eval <- evaluateModel(pred_class_c, real)

      curve_Accuracy[c] <- eval$Perf_Accuracy
      curve_PPV[c] <- eval$Perf_PPV
      curve_NPV[c] <- eval$Perf_NPV
      curve_BalancedAccuracy[c] <- eval$Perf_BalancedAccuracy
      curve_F1score[c] <- eval$Perf_F1score
    }

    results <- list(Perf_AUC= pracma::trapz(curve_FPR[length(curve_FPR):1],curve_TPR[length(curve_TPR):1]), # roc$auc,
                    Perf_AUPRC=tryCatch(PRROC::pr.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)$auc.integral, error=function(err) NA),# to catch exception when only outcomes or non-outcomes
                    Perf_PAUC=pROC::auc(real, predictions, partial.auc = c(1,0.8), partial.auc.focus = "sensitivity", partial.auc.correct=TRUE, levels = c("0", "1"), direction = "<"),
                    Curve_Accuracy=paste(curve_Accuracy, collapse = "_"),
                    Curve_Sensitivity=paste(curve_sensitivity, collapse = "_"),
                    Curve_Specificity=paste(curve_specificity, collapse = "_"),
                    Curve_PPV=paste(curve_PPV, collapse = "_"),
                    Curve_NPV=paste(curve_NPV, collapse = "_"),
                    Curve_BalancedAccuracy=paste(curve_BalancedAccuracy, collapse = "_"),
                    Curve_F1score=paste(curve_F1score, collapse = "_"),
                    Curve_TPR=paste(curve_TPR, collapse = "_"),
                    Curve_FPR=paste(curve_FPR, collapse = "_"),
                    Curve_Thresholds=paste(curve_thresholds, collapse = "_"),
                    Curve_Models=ifelse(is.null(model), NA, model),
                    N_outcomes=sum(real == 1),
                    N_controls=sum(real == 0),
                    N_total=length(real))
  }

  return(results)
}


prob_to_class <- function(predictions, real, optimise_class = "AUC") {
  values <- seq(0.01,0.99,0.01)

  if (length(unique(predictions)) == 1) {
    warning('No variation in predictions!')
    threshold <- 0.5 # default
  } else {

    suppressWarnings({
      if (optimise_class == "AUC") {
        f1_scores <- sapply(values, function(thres) {
          class <- as.numeric(ifelse(predictions >= thres, 1, 0))
          score <- evaluateModel(class, real)$Perf_AUC
        })
        threshold <- values[which.max(f1_scores)]
      } else if (optimise_class == "f1_score") {
        f1_scores <- sapply(values, function(thres) {
          class <- as.numeric(ifelse(predictions >= thres, 1, 0))
          score <- evaluateModel(class, real)$Perf_F1score
        })
        threshold <- values[which.max(f1_scores)]
      } else if (optimise_class == "ROC01") {
        roc_scores <- sapply(values, function(thres) {
          class <- as.numeric(ifelse(predictions >= thres, 1, 0))

          distance <- (evaluateModel(class, real)$Perf_Specificity-0)^2 +
            (evaluateModel(class, real)$Perf_Sensitivity-1)^2
        }
        )
        threshold <- values[which.min(roc_scores)]
      } else if (optimise_class == "custom") {
        # cost_scores <- sapply(values, function(thres) {
        #   cost = 0
        #
        #   cost+=(TN*-10000)
        #   cost+=(FP*1000)
        #   cost+=(FN*1500)
        #   cost+=(TP*-20000)
        #
        # }
        # )
        # # plot(seq(0.01,0.99,0.01), cost_scores)
        # threshold <- values[which.min(cost_scores)]
      }
    })
  }

  class <- as.numeric(ifelse(predictions >= threshold, 1, 0))

  return(class)

}

# This is the extended verison of Explore::rocCurveExplore()
evaluateExplore <- function(modelsCurve, plpModel, data) {

  if (is.null(modelsCurve)) {
    results <- list(Perf_AUC=NA,
                    Perf_AUPRC=NA,
                    Perf_PAUC=NA,
                    Curve_Accuracy=NA,
                    Curve_Sensitivity=NA,
                    Curve_Specificity=NA,
                    Curve_PPV=NA,
                    Curve_NPV=NA,
                    Curve_BalancedAccuracy=NA,
                    Curve_F1score=NA,
                    Curve_TPR=NA,
                    Curve_FPR=NA,
                    Curve_Thresholds=NA,
                    Curve_Models=NA,
                    N_outcomes=NA,
                    N_controls=NA,
                    N_total=NA)
    return(results)
  }

  # Convert to dense covariates
  covariates <- as.data.frame(data$covariateData$covariates)
  # covariates <- covariates[covariates$covariateId %in% plpModel$model$coefficients,] # COMMENTED OUT AS VARIABLES INCLUDED DIFFER PER MODEL BELOW!
  denseData <- reshape2::dcast(covariates, rowId ~ covariateId, value.var = 'covariateValue', fill = 0)

  cohort <- data$labels
  labels <- cohort$outcomeCount

  exploreData <- merge(cohort[c("rowId", "outcomeCount")], denseData, by = 'rowId', all.x = TRUE)
  exploreData[is.na(exploreData)] <- 0
  exploreData[c("rowId", "outcomeCount")] <- NULL

  # Convert to same feature names as rule strings
  # colnames(exploreData) <- paste0("'", colnames(exploreData), "'")

  # Combine all these results
  curve_TPR <- c(1,0)
  curve_FPR <- c(1,0)
  curve_models <- c(NA, NA)
  # curve_thresholds <- c(-Inf, Inf)

  curve_Accuracy <- c(0,0) # TODO: check edge cases related to outcomes vs non-outcomes!
  curve_sensitivity <- c(0,1)
  curve_specificity <- c(1,0)
  curve_PPV <- c(0,1)
  curve_NPV <- c(1,0)
  curve_BalancedAccuracy <- c(0,0) # TODO: check edge cases related to outcomes vs non-outcomes!
  curve_F1score <- c(0,0) # TODO: check edge cases related to outcomes vs non-outcomes!

  for (c in length(modelsCurve):1) {
    model <- modelsCurve[c]

    # Predict using train and test
    predict <- tryCatch(as.numeric(Explore::predictExplore(model = model, test_data = exploreData)))

    # Compute metrics
    eval_curve <- evaluateModel(predict, labels)

    curve_TPR[c+2] <- eval_curve$Perf_Sensitivity
    curve_FPR[c+2] <- 1- eval_curve$Perf_Specificity
    curve_models[c+2] <- model
    curve_thresholds <- 0.5

    N_outcomes <- eval_curve$N_outcomes
    N_controls <- eval_curve$N_controls
    N_total <- eval_curve$N_total

    curve_Accuracy[c+2] <- eval_curve$Perf_Accuracy
    curve_sensitivity[c+2] <- eval_curve$Perf_Sensitivity
    curve_specificity[c+2] <- eval_curve$Perf_Specificity
    curve_PPV[c+2] <- eval_curve$Perf_PPV
    curve_NPV[c+2] <- eval_curve$Perf_NPV
    curve_BalancedAccuracy[c+2] <- eval_curve$Perf_BalancedAccuracy
    curve_F1score[c+2] <- eval_curve$Perf_F1score
  }

  # Output
  result <- list(Perf_AUC=pracma::trapz(curve_FPR[length(curve_FPR):1],curve_TPR[length(curve_TPR):1]),
                 Perf_AUPRC=NA,
                 Perf_PAUC=NA,
                 Curve_Accuracy=paste(curve_Accuracy, collapse = "_"),
                 Curve_Sensitivity=paste(curve_sensitivity, collapse = "_"),
                 Curve_Specificity=paste(curve_specificity, collapse = "_"),
                 Curve_PPV=paste(curve_PPV, collapse = "_"),
                 Curve_NPV=paste(curve_NPV, collapse = "_"),
                 Curve_BalancedAccuracy=paste(curve_BalancedAccuracy, collapse = "_"),
                 Curve_F1score=paste(curve_F1score, collapse = "_"),
                 Curve_TPR=paste(curve_TPR, collapse = "_"),
                 Curve_FPR=paste(curve_FPR, collapse = "_"),
                 Curve_Thresholds=paste(curve_thresholds, collapse = "_"),
                 Curve_Models=paste(curve_models, collapse = "_"),
                 N_outcomes=paste(N_outcomes, collapse = "_"),
                 N_controls=paste(N_controls, collapse = "_"),
                 N_total=paste(N_total, collapse = "_"))

  return(result)
}

loadTrainTestData <- function(file, readOnly = TRUE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))

  result <- list(Train = list(covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "train-covariates")),
                              labels = readRDS(file.path(file, "train-labels.rds")),
                              folds = readRDS(file.path(file, "train-folds.rds"))),
                 Test = list(covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "test-covariates")),
                             labels = readRDS(file.path(file, "test-labels.rds"))))
  attr(result$Train, "metaData") <- readRDS(file.path(file, "metadata.rds"))

  return(result)
}
