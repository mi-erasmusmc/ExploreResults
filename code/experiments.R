runExperiments <- function(output_path, data_name_list, methods_list, explore_options, train_fraction = 0.7, num_iterations = 1, sampling_strategy = NULL, sample_size = "same") {

  summary_data <- data.frame()
  methods_output <- data.frame()
  explore_output <- data.frame()

  explore_options$Option <- 1:nrow(explore_options)
  write.csv(explore_options, file.path(output_path, "explore_options.csv"), row.names = FALSE)

  for (d in data_name_list) { # d <- data_name_list[1]
    ParallelLogger::logInfo(print(paste0("Checking computation times for ", d, " data")))

    ## LOAD DATA
    data <- farff::readARFF(file.path(getwd(), "data", d))
    varnames <- data.frame(original=colnames(data))

    # Rename variables
    if (grepl(pattern = "IPCI/", d)) {
      # colnames(data)[1:(ncol(data)-1)] <- stringr::str_split(colnames(data)[1:(ncol(data)-1)], '\\([0-9]|[0-9]\\)', simplify = TRUE)[,2]

      colnames(data)[1:(ncol(data)-1)] <- paste0("var_", 1:(ncol(data)-1)) # add var to colnames (numbers don't work)
    }

    d <- gsub(pattern = ".arff", replacement = "", d, fixed = TRUE)
    d <- gsub(pattern = "IPCI/new/", replacement = "", d, fixed = TRUE)
    d <- gsub(pattern = "IPCI/samples/", replacement = "", d, fixed = TRUE)

    varnames$new <- colnames(data)
    write.csv(varnames, file.path(output_path, paste0("varnames_", d, ".csv")), row.names = FALSE)

    # Run checks
    data <- runCheckData(data, d)

    # Summarize data
    # TODO: count outcomes test data?
    summary_data <- summarizeData(summary_data, data, d)

    # Create train/test split
    split_data <- splitTrainTest(data, d, output_path, frac = train_fraction)

    # Correct imbalance in both train data (leave test data the same)
    split_data[[1]] <- oversample(split_data[[1]], summary_data, sampling_strategy, sample_size, d)

    bound = NULL;
    models <- setNames(data.table(matrix(0, nrow = 0, ncol = ncol(data)+3)), c("method", "iteration", "intercept", colnames(data)[1:(ncol(data)-1)], "model size"))

    for (m in methods_list) { # m <- methods_list[1]

      ParallelLogger::logInfo(print(paste0("Method ", m, " for ", d, " data")))

      for (i in 1:num_iterations) { # i <- 1
        # Create model predictions and record time
        time_start <- Sys.time()
        result <- createModel(m, split_data[[1]], data_path, split_data[[2]], d, output_path, models, i, explore_options, bound)
        time_end <- Sys.time()

        for (o in 1:length(result[[1]])) {
          # Save model and evaluate predictions
          models <- rbind(models, result[[2]][[o]])

          eval_test <- evaluateModel(result[[1]][[o]], split_data[[2]]$class) # class for explore, prob for all others
          if (m != "explore") {
            eval_test_prob <- eval_test
            eval_test_class <- evaluateModel(prob_to_class(result[[1]][[o]], split_data[[2]]$class), split_data[[2]]$class)
          } else {
            eval_test_prob <- list(Perf_AUC=NA,
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
                                   N_outcomes=NA,
                                   N_controls=NA,
                                   N_total=NA)
            eval_test_class <- eval_test
          }
          eval_train <- result[[3]][[o]]

          eval <- append(append(eval_test_class, eval_test_prob), eval_train)
          names(eval) <- c(paste0(names(eval_test_class), "_Test_Class"), paste0(names(eval_test_prob), "_Test_Prob"), paste0(names(eval_train), "_Train_Class"))
          methods_output <- rbind(methods_output, c(append(list(Time = difftime(time_end, time_start, units = "mins"), Data = d, Method = m, Iteration = i, Option = o), eval)))

          if (m == "lasso" && i == 1) { # Create accuracy bound from lasso performance
            bound <- result[[4]][[o]]
          } else if (m %like% "explore") { # Save additional output explore
            explore_output <- rbind(explore_output, result[[4]][[o]])
          }

        }
      }

      write.csv(models, file.path(output_path, paste0("models_", d, ".csv")), row.names = FALSE)
    }
  }

  write.csv(summary_data, file.path(output_path, paste0("summary_data.csv")), row.names = FALSE)
  write.csv(explore_output, file.path(output_path, paste0("explore_output.csv")), row.names = FALSE)
  write.csv(methods_output, file.path(output_path, paste0("output_methods.csv")), row.names = FALSE)

  return(list(explore_output, methods_output))
}
