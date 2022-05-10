runExperiments <- function(data_name_list, options, output_path, compare_settings = TRUE, compare_methods = FALSE) {

  output1 <- data.frame()
  output2 <- data.frame()

  options$Time <- NA
  options$Model <- NA
  options$Performance_Accuracy <- NA

  for (d in data_name_list) { # d <- data_name_list[1]
    ParallelLogger::logInfo(print(paste0("Checking computation times for ", d, " data")))

    ## DATA
    data <- farff::readARFF(file.path(getwd(), "data", d))
    d <- gsub(pattern = ".arff", replacement = "", d, fixed = TRUE)

    output_d <- options
    output_d$Data <- d

    data <- runCheckData(data, d)
    colnames(data)[1:(ncol(data)-1)] <- paste0("var_", colnames(data)[1:(ncol(data)-1)]) # add var to colnames (numbers don't work)

    if (compare_settings) {
      # Run LASSO
      model_lasso <- glmnet::cv.glmnet(x=data.matrix(data[, -which(names(data) == "class")]), y = data$class, alpha = 1, lambda = 10^seq(2, -3, by = -.1), standardize = FALSE, nfolds = 5, family = "binomial")
      coef <- as.matrix(coef(model_lasso, s = "lambda.min")) # get importance
      coef <- rownames(coef)[order(-abs(coef))] # order from high to low
      coef <- coef[-which(coef == "(Intercept)")] # remove intercept
      data_sorted <- data[,c(coef,"class")] # sort data features by LASSO importance

      pred_lasso <- predict(model_lasso, newx = data.matrix(data[, -which(names(data) == "class")]), type="class", s = "lambda.min")
      eval <- evaluateModel(as.numeric(pred_lasso), data$class)
      output_d$Constraint_Accuracy[output_d$Constraint_Accuracy == "custom"] <- eval$accuracy*0.9

      for (o in 1:nrow(output_d)) { # o <- 1

        if (output_d$Sorted[o]) {
          data_input <- data_sorted
        } else {
          data_input <- data
        }

        output_d_o <- timeModel(data = data_input, options = output_d, output_path = output_path, d = d, o = o)
        output1 <- rbind(output1, output_d_o)

        write.csv(output1, file.path(output_path, paste0("compare_settings_", d, ".csv")), row.names = FALSE)
      }
    }

    if (compare_methods) {
      methods_list <- c("lasso", "randomforest", "ripper", "explore")
      models <- setNames(data.table(matrix(0, nrow = 0, ncol = ncol(data)+3)), c("method", "iteration", "intercept", colnames(data)[1:(ncol(data)-1)], "model size"))

      # Create train/test split
      split_data <- splitTrainTest(data, d, output_path, frac = 0.7)

      bound = NULL;

      for (m in methods_list) {

        ParallelLogger::logInfo(print(paste0("Method ", m, " for ", d, " data")))

        # Create model predictions and save model
        time_start <- Sys.time()
        i <- 1
        result <- createModel(m, split_data[[1]], data_path, split_data[[2]], d, output_path, models, i, bound)
        time_end <- Sys.time()

        models <- rbind(models, result[[2]])

        # Evaluate model predictions and save
        eval <- evaluateModel(result[[1]], split_data[[2]]$class)
        output2 <- rbind(output2, c(list(Time = difftime(time_end, time_start, units = "mins"), Model = NA, Performance_Accuracy = eval[[ "accuracy"]], Performance_AUC = eval[["auc_roc"]], Performance_AUPRC = eval[["auc_pr"]], Data = d, Method = m)))

        # Create accuracy bound from lasso performance
        if (m == "lasso") {
          bound <- result[[3]]
        }
      }

      write.csv(round_dt(models,3), file.path(output_path, paste0("models_", d, ".csv")), row.names = FALSE)
      write.csv(output2, file.path(output_path, paste0("compare_methods_", d, ".csv")), row.names = FALSE)
    }
  }

  # TODO: group by ... and aggregate results over different i

  return(list(output1,output2,models))
}


### Help functions
timeModel <- function(i, data, options, output_path, d, o) {

  output_d_o <- data.frame()

  for (i in 1:3) {
    output_d_o_i <- options[o,]

    time_start <- Sys.time()

    # Option 2: pre-specified settings file with input data
    rule_string <- R.utils::withTimeout(Explore::trainExplore(output_path = file.path(output_path, "explore/"), file_name = paste0(d, "_train_", o, "_", i),
                                                              train_data = data, ClassFeature = "'class'", PositiveClass = 1, StartRulelength = options$StartRulelength[o],
                                                              EndRulelength = options$EndRulelength[o], Accuracy = options$Constraint_Accuracy[o], Parallel = options$Parallel[o]), timeout = 5, onTimeout = "silent")
    time_end <- Sys.time()

    if (!(rule_string == "") && !is.null(rule_string)) { # TODO: differentiate between no model with constraints ("") or maximum time exceeded (NULL)
      # TODO: can model be NA?

      # Save model
      output_d_o_i$Model <- rule_string

      # Compute and save prediction performance
      pred_explore <- Explore::predictExplore(model = rule_string, test_data = data)
      eval <- evaluateModel(pred_explore, data$class)
      output_d_o_i$Performance_Accuracy <- eval[[ "accuracy"]]
      output_d_o_i$Time <- difftime(time_end, time_start, units = "mins")

    } else if (rule_string == "") {
      output_d_o_i$Model <- "model not available"
      output_d_o_i$Performance_Accuracy <- NA
      output_d_o_i$Time <- NA
    } else if (is.null(rule_string)) {
      output_d_o_i$Model <- "time exceeded"
      output_d_o_i$Performance_Accuracy <- NA
      output_d_o_i$Time <- NA
    }

    output_d_o <- rbind(output_d_o, output_d_o_i)
  }

  return(output_d_o)
}

evaluateModel <- function(predictions, real) {

  roc <- PRROC::roc.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)

  # TODO: solve this in a nicer way (fails if predictions all zero)
  pr <- NA
  try(pr <- PRROC::pr.curve(scores.class0 = predictions[real == 1], scores.class1 = predictions[real == 0], curve = TRUE)$auc.integral, silent = TRUE)

  # plot(roc)
  # plot(pr)

  conf_matrix <- table(factor(predictions, levels = c(0,1)), factor(real, levels = c(0,1))) # binary prediction
  summary_performance <- caret::confusionMatrix(conf_matrix, positive = '1')

  accuracy <- summary_performance$overall['Accuracy']
  sensitivity <- caret::sensitivity(conf_matrix)
  specificity <- caret::specificity(conf_matrix)
  PPV <- summary_performance$byClass['Pos Pred Value']
  NPV <- summary_performance$byClass['Neg Pred Value']

  results <- list(auc_roc=roc$auc, auc_pr=pr, accuracy=accuracy, sensitivity=sensitivity, specificity=specificity, PPV=PPV, NPV=NPV)

  return(results)
}

