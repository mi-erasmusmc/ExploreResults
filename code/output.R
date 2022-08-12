outputPerformance <- function(output_methods) {

  ## Plots data
  metrics <- c("Performance_AUC", "Performance_AUPRC", "Performance_Accuracy", "Performance_Sensitivity",
               "Performance_Specificity" , "Performance_PPV", "Performance_NPV", "Performance_BalancedAccuracy", "Performance_F1score")

  print("Compare performance on different data/methods per metric")
  for (m in metrics) { # m <- "Performance_AUC"
    p <- ggplot(output_methods, aes(Data, eval(parse(text=m)), variable = Method, group = Method, colour = Method)) +
      geom_line() + # DOES NOT SHOW RESULTS WITH ONE DATASET
      labs(title = paste0("Performance ", strsplit(m, "_")[[1]][2]), x = "Data", y = strsplit(m, "_")[[1]][2]) +
      theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
      # scale_y_log10(labels = scales::comma)
    print(p)
  }

  print("Compare performance on different methods/metrics per dataset")
  for (d in unique(output_methods$Data)) { # d <- "cover_var_50obs_1e+09"
    output_methods_d <- output_methods[output_methods$Data == d,]
    output_methods_d <-reshape2::melt(output_methods_d, id = c("Time", "Model", "Data", "Method", "Iteration", "Option"), value.name = "Value")
    output_methods_d$variable <- sapply(as.character(output_methods_d$variable), function(i) strsplit(i, "_")[[1]][2])

    p <- ggplot(output_methods_d, aes(Method, Value, variable = variable, group = variable, colour = variable)) +
      geom_line() +
      labs(title = paste0("Dataset ", d), x = "Method", y = "Performance") +
      theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
      # scale_y_log10(labels = scales::comma)
    print(p)
  }
}


outputTime <- function(explore_output) {

  # TODO: add something on candidates / upperbound computation time?
  # predicting computation time EXPLORE to set max rule length to try...)

  ## Plots data
  # Over rule lengths
  p <- ggplot(explore_output, aes(EndRulelength, Time, shape = Parallel, color = Data)) +
    geom_point() +
    theme_bw() +
    scale_y_log10(labels = scales::comma) +
    ylab("Time (in minutes)")
  print(p)

  # Over variables (only RWD)
  p <- ggplot(explore_output, aes(Number_of_features, Time, shape = Parallel, color = Data)) +
    geom_point() +
    theme_bw() +
    scale_y_log10(labels = scales::comma) +
    ylab("Time (in minutes)")
  print(p)

  # Over observations (only RWD)
  # p <- ggplot(explore_output, aes(Observations, Time, shape = Parallel, color = Data)) +
  #   geom_point() +
  #   theme_bw() +
  #    scale_y_log10(labels = scales::comma) +
  #   ylab("Time (in minutes)")
  # print(p)

  ## Parallel improvements
  if ("no" %in% explore_output$Parallel) {
    data <- explore_output
    data$Model <- NULL # might be different models for same performance
    data$Parallel <- paste0("Parallelization_", data$Parallel)
    max_value <- max(data$Time)*1.2
    data$Observations <- NULL
    data <- data[data$Sorted == "FALSE",] # select one option

    data <- reshape2::dcast(data,  ... ~ Parallel, value.var = "Time", fun.aggregate = mean)

    # PLOT WITH/WITHOUT PARALLEL
    p <- ggplot(data, aes(Parallelization_no, Parallelization_yes, color=EndRulelength,size = Performance_Accuracy, shape = Data)) +
      geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      xlim(0, max(max_value)) +
      ylim(0, max(max_value))

    print(p)

    # data$Parallel_improvement <- (data$Parallelization_yes - data$Parallelization_no) * 100.0 / data$Parallelization_no
    data$Parallel_improvement <- data$Parallelization_no * 1.0 /data$Parallelization_yes

    # PLOT IMPROVEMENT
    p <- ggplot(data, aes(EndRulelength, Parallel_improvement, color=Data)) +
      geom_abline(intercept = 1, slope = 0) +
      geom_point() +
      theme_bw() +
      ylab("Improvement (times faster)")

    print(p)
  }
}
























