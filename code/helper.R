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
    # FOR CLASSES
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

  roc <- pROC::roc(real, predictions, levels = c("0", "1"), direction = "<")

  curve_TPR <- roc$sensitivities
  curve_FPR <- 1 - roc$specificities
  curve_thresholds <- roc$thresholds

  if (length(curve_thresholds) > 20) {
    indexes <- round(seq(from=1,to=length(curve_thresholds),length.out = 20))

    curve_TPR <- curve_TPR[indexes]
    curve_FPR <- curve_FPR[indexes]
    curve_thresholds <- curve_thresholds[indexes]
  }

  N_outcomes <- length(roc$cases)
  N_controls <- length(roc$controls)
  N_total <- N_outcomes + N_controls

  partial_auc <- pROC::auc(real, predictions, partial.auc = c(1,0.8), partial.auc.focus = "sensitivity", partial.auc.correct=TRUE, levels = c("0", "1"), direction = "<")

  pr <- NA
  try(pr <- PRROC::pr.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)$auc.integral, silent = TRUE)

  # plot(roc)
  # plot(pr)

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

    results <- list(Perf_AUC= roc$auc,
                    Perf_AUPRC=pr,
                    Perf_PAUC=partial_auc,
                    Perf_Accuracy=accuracy,
                    Perf_Sensitivity=sensitivity,
                    Perf_Specificity=specificity,
                    Perf_PPV=PPV,
                    Perf_NPV=NPV,
                    Perf_BalancedAccuracy=balanced_accuracy,
                    Perf_F1score=F1_score,
                    Curve_TPR=paste(curve_TPR, collapse = "_"),
                    Curve_FPR=paste(curve_FPR, collapse = "_"),
                    Curve_Thresholds=paste(curve_thresholds, collapse = "_"),
                    Curve_Models=ifelse(is.null(model), NA, model),
                    N_outcomes=N_outcomes,
                    N_controls=N_controls,
                    N_total=N_total)

  } else { # Return for probabilities

    curve_sensitivity <- c()
    curve_specificity <- c()
    curve_PPV <- c()
    curve_NPV <- c()

    for (c in 1:length(curve_thresholds)) {

      pred_class_c <- as.numeric(ifelse(predictions >= curve_thresholds[[c]], 1, 0))
      eval <- evaluateModel(pred_class_c, real)

      curve_sensitivity[c] <- eval$Perf_Sensitivity
      curve_specificity[c] <- eval$Perf_Specificity
      curve_PPV[c] <- eval$Perf_PPV
      curve_NPV[c] <- eval$Perf_NPV
    }

    results <- list(Perf_AUC= roc$auc,
                    Perf_AUPRC=pr,
                    Perf_PAUC=partial_auc,
                    Perf_Accuracy=NA, # add curve?
                    Curve_Sensitivity=paste(curve_sensitivity, collapse = "_"),
                    Curve_Specificity=paste(curve_specificity, collapse = "_"),
                    Curve_PPV=paste(curve_PPV, collapse = "_"),
                    Curve_NPV=paste(curve_NPV, collapse = "_"),
                    Perf_BalancedAccuracy=NA, # add curve?
                    Perf_F1score=NA, # add curve?
                    Curve_TPR=paste(curve_TPR, collapse = "_"),
                    Curve_FPR=paste(curve_FPR, collapse = "_"),
                    Curve_Thresholds=paste(curve_thresholds, collapse = "_"),
                    Curve_Models=ifelse(is.null(model), NA, model),
                    N_outcomes=N_outcomes,
                    N_controls=N_controls,
                    N_total=N_total)
  }

  return(results)
}



lasso_glm <- function(x, y, model_lasso = NULL, return = "model", optimise_class = "f1_score") {
  # Lasso logistic regression using glmnet
  if (is.null(model_lasso)) {
    model_lasso <- glmnet::cv.glmnet(x=data.matrix(x), y = y, alpha = 1, standardize = TRUE, nfolds = 5, family = "binomial")
  }

  if (return == "model") {
    return(model_lasso)

  } else if (return == "features") {
    coef <- as.matrix(coef(model_lasso, s = "lambda.min")) # get importance
    coef_ordered <- coef[order(abs(coef), decreasing = TRUE)]
    names_ordered <- rownames(coef)[order(abs(coef), decreasing = TRUE)]
    names(coef_ordered) <- names_ordered

    coef_ordered <- coef_ordered[names(coef_ordered) != "(Intercept)"]
    n_sel <- sum(coef_ordered != 0)

    features <- x[,names(coef_ordered[1:n_sel])]

    return(features)

  } else if (return == "predict_prob") {
    predictions <- predict(model_lasso, newx =  data.matrix(x), type="response", s="lambda.min")
    return(predictions)

  } else if (return == "predict_class") {
    predictions <- predict(model_lasso, newx = data.matrix(x), type="response", s="lambda.min")

    if (optimise_class == "default lasso") {
      class <- as.numeric(predict(model_lasso, newx = data.matrix(x), type="class", s="lambda.min"))
    } else {
      class <- prob_to_class(predictions = predictions, real = y, optimise_class = optimise_class)
    }
    return(class)
  }
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
        # plot(seq(0.01,0.99,0.01), f1_scores)
        threshold <- values[which.max(f1_scores)]
      } else if (optimise_class == "f1_score") {
        f1_scores <- sapply(values, function(thres) {
          class <- as.numeric(ifelse(predictions >= thres, 1, 0))
          score <- evaluateModel(class, real)$Perf_F1score
        })
        # plot(seq(0.01,0.99,0.01), f1_scores)
        threshold <- values[which.max(f1_scores)]
      } else if (optimise_class == "ROC01") {
        roc_scores <- sapply(values, function(thres) {
          class <- as.numeric(ifelse(predictions >= thres, 1, 0))

          distance <- (evaluateModel(class, real)$Perf_Specificity-0)^2 +
            (evaluateModel(class, real)$Perf_Sensitivity-1)^2
        }
        )
        # plot(seq(0.01,0.99,0.01), roc_scores)
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


lasso_cyclops <- function(x, y, model_lasso = NULL, return = "features") {
  if (is.null(model_lasso)) {
    # Lasso logistic regression using cyclops (large scale regularized regressions)
    outcomes <- data.frame(rowId = 1:length(y), y = y)

    x <- data.frame(data.matrix(x))
    x$rowId = rownames(x)
    # x <- cbind("rowId" = 1:nrow(x), x)
    covariates = reshape2::melt(x, id.vars = "rowId", variable.name = "covariateId", value.name = "covariateValue")
    covariates$rowId <- as.integer(covariates$rowId)
    covariates$covariateId <- as.integer(covariates$covariateId)

    cyclopsData <- convertToCyclopsData(outcomes = outcomes, covariates = covariates, modelType = "lr",  addIntercept = TRUE)
    model_lasso <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
  }

  if (return == "features") {
    coef <- coef(model_lasso)

    coef_ordered <- coef[order(abs(coef), decreasing = TRUE)]
    names_ordered <- names(coef)[order(abs(coef), decreasing = TRUE)]
    names(coef_ordered) <- names_ordered

    coef_ordered <- coef_ordered[names(coef_ordered) != "(Intercept)"]
    n_sel <- sum(!is.na(coef_ordered))

    features <- x[,as.integer(names(coef_ordered[1:n_sel]))]

    return(features)
  } else if (return == "predict_prob") {
    warning("Predictions cyclops currently only for train data!")

    predictions <- predict(model_lasso) # response scale
    return(predictions)
  }

  # TODO: alternative class = meanLinearPredictor?

}


explore_AUCcurve <- function(train, data_path, test, data_name, output_path_curve = paste0(output_path, "/explore/AUC_curve"), models, i, o, explore_options, bound) {

  explore_options_curve <- explore_options[o,]
  explore_options_curve$Maximize <- "SENSITIVITY"
  explore_options_curve$Constraint_Specificity <- NULL
  explore_options_curve <- cbind(explore_options_curve, Constraint_Specificity = seq(0.05,0.95,0.1), row.names = NULL)

  # Run EXPLORE with explore_options_curve
  if (!dir.exists(output_path_curve)) {
    dir.create(output_path_curve)
  }

  result_curve <- createModel("explore", train, data_path, test, data_name, output_path_curve, models, i, explore_options_curve, bound, FALSE)

  # Combine all these results
  curve_TPR <- c(1,0)
  curve_FPR <- c(1,0)
  curve_models <- c(NA, NA)
  # curve_thresholds <- c(-Inf, Inf)

  curve_sensitivity <- c(0,1)
  curve_specificity <- c(1,0)
  curve_PPV <- c(0,1)
  curve_NPV <- c(1,0)

  for (c in length(result_curve[[1]]):1) {
    eval_test_curve <- evaluateModel(result_curve[[1]][[c]], test$class) # class for explore

    curve_TPR[c+2] <- eval_test_curve$Perf_Sensitivity
    curve_FPR[c+2] <- 1- eval_test_curve$Perf_Specificity
    curve_models[c+2] <- result_curve[[6]][[c]]
    # curve_thresholds[c+2] <- eval_test_curve$Perf_PPV
    curve_thresholds <- 0.5

    N_outcomes <- eval_test_curve$N_outcomes
    N_controls <- eval_test_curve$N_controls
    N_total <- eval_test_curve$N_total

    curve_sensitivity[c+2] <- eval_test_curve$Perf_Sensitivity
    curve_specificity[c+2] <- eval_test_curve$Perf_Specificity
    curve_PPV[c+2] <- eval_test_curve$Perf_PPV
    curve_NPV[c+2] <- eval_test_curve$Perf_NPV
  }

  return(list(curve_TPR, curve_FPR, curve_models, curve_thresholds, N_outcomes, N_controls, N_total,
              curve_sensitivity, curve_specificity, curve_PPV, curve_NPV))
}


exploreCurve <- function(models_AUCcurve, plpModel, data) {

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

  curve_sensitivity <- c(0,1)
  curve_specificity <- c(1,0)
  curve_PPV <- c(0,1)
  curve_NPV <- c(1,0)

  for (c in length(models_AUCcurve):1) {
    model <- models_AUCcurve[c]

    # Predict using train and test
    predict <- tryCatch(as.numeric(Explore::predictExplore(model = model, test_data = exploreData)))

    eval_curve <- evaluateModel(predict, labels)

    curve_TPR[c+2] <- eval_curve$Perf_Sensitivity
    curve_FPR[c+2] <- 1- eval_curve$Perf_Specificity
    curve_models[c+2] <- model
    # curve_thresholds[c+2] <- eval_curve$Perf_PPV
    curve_thresholds <- 0.5

    N_outcomes <- eval_curve$N_outcomes
    N_controls <- eval_curve$N_controls
    N_total <- eval_curve$N_total

    curve_sensitivity[c+2] <- eval_curve$Perf_Sensitivity
    curve_specificity[c+2] <- eval_curve$Perf_Specificity
    curve_PPV[c+2] <- eval_curve$Perf_PPV
    curve_NPV[c+2] <- eval_curve$Perf_NPV
  }

  result <- list(Perf_AUC=NA,
                 Perf_AUPRC=NA,
                 Perf_PAUC=NA,
                 Perf_Accuracy=NA, # add curve?
                 Curve_Sensitivity=paste(curve_sensitivity, collapse = "_"),
                 Curve_Specificity=paste(curve_specificity, collapse = "_"),
                 Curve_PPV=paste(curve_PPV, collapse = "_"),
                 Curve_NPV=paste(curve_NPV, collapse = "_"),
                 Perf_BalancedAccuracy=NA, # add curve?
                 Perf_F1score=NA, # add curve?
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
