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

evaluateModel <- function(predictions, real) {

  if (length(unique(predictions)) == 1) {
    warning('No variation in predictions!')
  }

  roc <- pROC::roc(real, predictions, levels = c("0", "1"), direction = "<")

  curve_TPR <- roc$sensitivities
  curve_FPR <- 1 - roc$specificities
  curve_thresholds <- roc$thresholds

  N_outcomes <- length(roc$cases)
  N_controls <- length(roc$controls)
  N_total <- N_outcomes + N_controls

  partial_auc <- pROC::auc(real, predictions, partial.auc = c(1,0.8), partial.auc.focus = "sensitivity", partial.auc.correct=TRUE, levels = c("0", "1"), direction = "<")

  pr <- NA
  try(pr <- PRROC::pr.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)$auc.integral, silent = TRUE)

  # plot(roc)
  # plot(pr)

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
                  N_outcomes=N_outcomes,
                  N_controls=N_controls,
                  N_total=N_total)
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

prob_to_class <- function(predictions, real, optimise_class = "ROC01") {
  values <- seq(0.01,0.99,0.01)

  if (length(unique(predictions)) == 1) {
    warning('No variation in predictions!')
    threshold <- 0.5 # default
  } else {

    suppressWarnings({

      if (optimise_class == "f1_score") {
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

