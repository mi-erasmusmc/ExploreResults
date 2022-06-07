runOutput <- function(explore_output, methods_output) {

  # output_files <- list.files(path = output_path)
  # output_files <- output_files[!(output_files %in% c("explore"))]

  ## Combine data
  # methods_output <- data.frame()
  #
  # for (o in output_files) { # o <- output_files[1]
  #   print(o)
  #   import <- read.csv(file.path(path, o))
  #   if (FILENAME IS TYPE methods_output) {
  #   methods_output <- rbind(methods_output, import)
  #
  # }

  ## Clean data
  explore_output$Dataset <- stringr::str_split(explore_output$Data, "_", simplify = TRUE)[,1]
  explore_output$Var <- as.numeric(gsub("obs", "", stringr::str_split(explore_output$Data, "_", simplify = TRUE)[,3])) # var
  explore_output$Obs <- as.numeric(stringr::str_split(explore_output$Data, "_", simplify = TRUE)[,4]) # obs

  ## Plot data

  # Variables
  p <- ggplot(explore_output, aes(Var, Time, color = Dataset)) +
    geom_point() +
    theme_bw() +
    scale_y_log10(labels = scales::comma) +
    ylab("Time (in minutes)")
  ggplotly(p)

  # Observations
  p <- ggplot(explore_output, aes(Obs, Time, color = Dataset)) +
    geom_point() +
    theme_bw() +
    # scale_y_log10(labels = scales::comma) +
    ylab("Time (in minutes)")
  ggplotly(p)

  # Rule lengths
  # TODO: add plot here


  ## Plot visualizing computation time:
  data <- explore_output
  data$Model <- NULL # might be different models for same performance
  data$Parallel <- paste0("Parallelization_", data$Parallel)
  data$Sorted <- paste0("Sorted_", data$Sorted)
  data$Constraint_Accuracy <- paste0("Constraint_Accuracy_", data$Constraint_Accuracy)

  max_value <- max(data$Time)*1.2

  p <- ggplot(data, aes(EndRulelength, Time, color = Data)) +
    geom_point(aes(shape = Parallel)) +
    theme_bw() +
    scale_y_log10(labels = scales::comma) + ylab("Time (in minutes)")

  ggplotly(p)


  ## Plot visualizing effect of sorting features based on importance LASSO logistic regression on computation time:
  data <- explore_output
  data$Model <- NULL # might be different models for same performance
  data <- data[data$Parallel == "yes",]
  data$Sorted <- paste0("Sorted_", data$Sorted)
  max_value <- max(data$Time)*1.2
  data <- reshape2::dcast(data,  ... ~ Sorted, value.var = "Time", fun.aggregate = mean)

  p <- ggplot(data, aes(Sorted_FALSE, Sorted_TRUE, size = Performance_Accuracy, color=EndRulelength, shape = Data)) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    geom_point() +
    theme_bw() +
    xlim(0, max(max_value)) +
    ylim(0, max(max_value))

  ggplotly(p)

  data$Sorted_improvement <- (data$Sorted_TRUE - data$Sorted_FALSE) * 100.0 / data$Sorted_FALSE

  p <- ggplot(data, aes(EndRulelength, Sorted_improvement, size = Performance_Accuracy, color=Data, shape = Data)) +
    geom_abline(intercept = 0, slope = 0) +
    geom_point() +
    theme_bw()

  ggplotly(p)


  ## Plot visualizing effect of adding accuracy constraint on computation time:
  data <- explore_output
  data$Model <- NULL # might be different models for same performance
  data <- data[data$Parallel == "yes",]
  data$Constraint_Accuracy <- paste0("Constraint_Accuracy_", data$Constraint_Accuracy)
  max_value <- max(data$Time)*1.2
  data <- reshape2::dcast(data,  ... ~ Constraint_Accuracy, value.var = "Time", fun.aggregate = mean)

  p <- ggplot(data, aes(Constraint_Accuracy_NA, Constraint_Accuracy_0.8, size = Performance_Accuracy, color=EndRulelength, shape = Data)) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    geom_point() +
    theme_bw() +
    xlim(0, max(max_value)) +
    ylim(0, max(max_value))

  ggplotly(p)

  data$Accuracy_improvement <- (data$Constraint_Accuracy_0.8 - data$Constraint_Accuracy_NA) * 100.0 / data$Constraint_Accuracy_NA

  p <- ggplot(data, aes(EndRulelength, Accuracy_improvement, size = Performance_Accuracy, color=Data, shape = Data)) +
    geom_abline(intercept = 0, slope = 0) +
    geom_point() +
    theme_bw()

  ggplotly(p)

  ## Plot visualizing effect of parallelization on computation time:
  data <- results
  data$Model <- NULL # might be different models for same performance
  data$Parallel <- paste0("Parallelization_", data$Parallel)
  max_value <- max(data$Time)*1.2
  data <- reshape2::dcast(data,  ... ~ Parallel, value.var = "Time", fun.aggregate = mean)

  p <- ggplot(data, aes(Parallelization_no, Parallelization_yes, size = Performance_Accuracy, color=EndRulelength, shape = Data)) +
    geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
    geom_point() +
    theme_bw() +
    xlim(0, max(max_value)) +
    ylim(0, max(max_value))

  ggplotly(p)

  data$Parallel_improvement <- (data$Parallelization_yes - data$Parallelization_no) * 100.0 / data$Parallelization_no

  p <- ggplot(data, aes(EndRulelength, Parallel_improvement, size = Performance_Accuracy, color=Data, shape = Data)) +
    geom_abline(intercept = 0, slope = 0) +
    geom_point() +
    theme_bw()

  ggplotly(p)



}













