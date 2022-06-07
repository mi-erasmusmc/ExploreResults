runExperiments <- function(output_path, data_name_list, methods_list, explore_options, train_fraction = 0.7, num_iterations = 1) {

  summary_data <- data.frame()
  methods_output <- data.frame()
  explore_output <- data.frame()

  explore_options$Time <- NA
  explore_options$Model <- NA
  explore_options$Performance_Accuracy <- NA

  for (d in data_name_list) { # d <- data_name_list[1]
    ParallelLogger::logInfo(print(paste0("Checking computation times for ", d, " data")))

    ## LOAD DATA
    data <- farff::readARFF(file.path(getwd(), "data", d))
    d <- gsub(pattern = ".arff", replacement = "", d, fixed = TRUE)

    colnames(data)[1:(ncol(data)-1)] <- paste0("var_", colnames(data)[1:(ncol(data)-1)]) # add var to colnames (numbers don't work)

    # Run checks
    data <- runCheckData(data, d)

    # Summarize data
    summary_data <- summarizeData(summary_data, data)

    # Create train/test split
    split_data <- splitTrainTest(data, d, output_path, frac = train_fraction)

    bound = NULL;
    models <- setNames(data.table(matrix(0, nrow = 0, ncol = ncol(data)+3)), c("method", "iteration", "intercept", colnames(data)[1:(ncol(data)-1)], "model size"))

    for (m in methods_list) { # m <- methods_list[1]

      ParallelLogger::logInfo(print(paste0("Method ", m, " for ", d, " data")))

      for (i in 1:num_iterations) { # i <- 1
        # Create model predictions and record time
        time_start <- Sys.time()

        result <- createModel(m, split_data[[1]], data_path, split_data[[2]], d, output_path, models, i, explore_options, bound)

        time_end <- Sys.time()

        # Save model and evaluate predictions
        models <- rbind(models, result[[2]])

        # TODO: fix automatic explore!
        # eval <- evaluateModel(result[[1]], split_data[[2]]$class)
        # methods_output <- rbind(methods_output, c(list(Time = difftime(time_end, time_start, units = "mins"), Model = NA, Performance_Accuracy = eval[[ "accuracy"]], Performance_AUC = eval[["auc_roc"]], Performance_AUPRC = eval[["auc_pr"]], Data = d, Method = m, Iteration = i, Option = o)))

        for (o in 1:length(result[[1]])) {
          eval <- evaluateModel(result[[1]][[o]], split_data[[2]]$class)
          methods_output <- rbind(methods_output, c(list(Time = difftime(time_end, time_start, units = "mins"), Model = NA, Performance_Accuracy = eval[[ "accuracy"]], Performance_AUC = eval[["auc_roc"]], Performance_AUPRC = eval[["auc_pr"]], Data = d, Method = m, Iteration = i, Option = o)))
        }

        if (m == "lasso" && i == 1) { # Create accuracy bound from lasso performance
          bound <- result[[3]]
        } else if (m %like% "explore") { # Save additional output explore
          explore_output <- rbind(explore_output, result[[3]])
        }
      }

      # Save update each time one method is finished
      write.csv(round_dt(models, 3), file.path(output_path, paste0("models_", d, ".csv")), row.names = FALSE)
      write.csv(methods_output, file.path(output_path, paste0("output_methods_", d, ".csv")), row.names = FALSE)
      write.csv(explore_output, file.path(output_path, paste0("explore_output_", d, ".csv")), row.names = FALSE)
    }
  }

  # TODO: group by ... and aggregate results over different i

  return(list(explore_output, methods_output))
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

