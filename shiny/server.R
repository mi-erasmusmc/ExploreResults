# Help functions
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Shiny server function
server <- function(input, output, session) {

  # Functionality for help messages
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }

  observeEvent(input$methodsInfo, {
    showInfoBox("Details", "html/methodsinfo.html")
  })

  # Dynamic input parameters
  output$dynamic_comparisonExplore = renderUI({
    explore_options <- read.csv(file.path(outputFolder, input$resultFolder, "explore_options.csv"))

    one <- selectInput("comparisonA", label = "Comparison Option A", choices = paste0("Option_", explore_options$Option),  selected = "Option_1")
    two <- selectInput("comparisonB", label = "Comparison Option B", choices = paste0("Option_", explore_options$Option),  selected = "Option_2")

    return(tagList(one, two))
  })

  output$dynamic_modelMethods = renderUI({
    one <- selectInput("dataset", label = "Dataset", choices = explore_output_plus()$Data, selected = "Iris")

    return(tagList(one))
  })


  # Data tab
  output$dataTableTitle <- renderText({
    paste0("Information about datasets included in output ", names(which(resultFolders == input$resultFolder)))
  })

  output$dataTable <- renderDataTable({
    summary_data <- read.csv(file.path(outputFolder, input$resultFolder, "summary_data.csv"), row.names = 1)
    table <- t(summary_data)
    table <- round_df(table, 2)

    return(table)
  }, options = list(pageLength = 15, scrollX = TRUE))


  # Explore tab
  output$exploreOptionsTitle <- renderText({
    paste0("Explore options for output ", names(which(resultFolders == input$resultFolder)))
  })

  explore_output_plus <- reactive({
    explore_options <- read.csv(file.path(outputFolder, input$resultFolder, "explore_options.csv"))
    explore_output <- read.csv(file.path(outputFolder, input$resultFolder, "explore_output.csv"))

    explore_output_plus <- merge(explore_options, explore_output, by = "Option")
  })

  output$exploreOptions <- renderDataTable({
    # table <- explore_output_plus()
    table <- read.csv(file.path(outputFolder, input$resultFolder, "explore_options.csv"))

    table <- t(table)
    colnames(table) <- paste0("Option_", table[row.names(table) == "Option",])
    table <- table[!(row.names(table) == "Option"),] # TODO: check iterations?

    return(table)
  }, options = list(pageLength = 15, scrollX = TRUE))


  output$exploreOutputTitle <- renderText({
    paste0("Explore results for output ", names(which(resultFolders == input$resultFolder)))
  })

  output$exploreOutputRuleLength <- renderPlot({
    plot_data <- explore_output_plus()

    ggplot(plot_data, aes(EndRulelength, Time, shape = Parallel, color = Data)) +
      geom_point() +
      theme_bw() +
      scale_y_log10(labels = scales::comma) +
      ylab("Time (in minutes)")
  })

  output$exploreOutputMaximize <- renderPlot({
    plot_data <- explore_output_plus()

    ggplot(plot_data, aes(Maximize, Time, shape = Parallel, color = Data)) +
      geom_point() +
      theme_bw() +
      scale_y_log10(labels = scales::comma) +
      ylab("Time (in minutes)")
  })

  # TODO: add number of features

  output$exploreComparisonTitle <- renderText({
    paste0("Explore comparison results for output ", names(which(resultFolders == input$resultFolder)))
  })

  explore_comparison <- reactive({
    plot_data <- explore_output_plus()

    # select two columns for option A and B
    plot_data <- plot_data[paste0("Option_",plot_data$Option) %in% c(input$comparisonA, input$comparisonB), c("Option", "Time", "Data")]
    plot_data <- reshape2::dcast(plot_data,  Data ~ Option, value.var = "Time", fun.aggregate = mean)
    colnames(plot_data) <- c("Data", "Option A", "Option B")

    return(plot_data)
  })

  output$exploreComparison1 <- renderPlot({
    plot_data <- explore_comparison()

    max_value <- max(c(plot_data$`Option A`, plot_data$`Option B`))*1.2

    # TODO: add performance as size?
    ggplot(plot_data, aes(`Option A`, `Option B`, color=Data, shape = Data)) +
      geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      xlim(0, max(max_value)) +
      ylim(0, max(max_value))
  })

  output$exploreComparison2 <- renderPlot({
    plot_data <- explore_comparison()

    # plot_data$Improvement <- (plot_data$`Option B` - plot_data$`Option A`) * 100.0 / plot_data$`Option A`
    plot_data$Improvement <- plot_data$`Option A` * 1.0 / plot_data$`Option B`

    ggplot(plot_data, aes(Data, Improvement, color=Data)) +
      geom_abline(intercept = 1, slope = 0) +
      geom_point() +
      # theme_bw() +
      theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
      ylab("Improvement (times faster)")
  })

  # TODO: output times

  # Compare methods tab
  output$comparisonTitle <- renderText({
    paste0("Compare results for output ", names(which(resultFolders == input$resultFolder)))
  })

  output_methods_metrics <- reactive({
    plot_data <- read.csv(file.path(outputFolder, input$resultFolder, "output_methods.csv"))
    plot_data <- plot_data[,!(colnames(plot_data) %like% "Curve_|N_")]

    # remove_cols <- (colnames(plot_data) %like% "Sensitivity_|Specificity_|PPV_|NPV_") & (colnames(plot_data) %like% "_Prob")
    # plot_data <- plot_data[,!remove_cols]

    # cols_group <- c("Data", "Method", "Option")
    # cols_other <- colnames(plot_data)[!colnames(plot_data) %in% c(cols_group, "Iteration")]
    cols_group <- c("Data", "Method", "Option", "Iteration")
    cols_other <- colnames(plot_data)[!colnames(plot_data) %in% c(cols_group)]

    plot_data <- data.table(plot_data)
    plot_data <- plot_data[,lapply(.SD, mean), .SDcols = cols_other, by = cols_group]

  })

  output_methods_curve <- reactive({
    plot_data <- read.csv(file.path(outputFolder, input$resultFolder, "output_methods.csv"))

    if (paste0(input$performance, "_", input$evaluate) == "Test_Class") {
      plot_data <- plot_data[plot_data$Data == input$dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Prob", colnames(plot_data))]

    } else if (paste0(input$performance, "_", input$evaluate)  == "Test_Prob") {
      plot_data <- plot_data[plot_data$Data == input$dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Class", colnames(plot_data))]

    } else if (paste0(input$performance, "_", input$evaluate)  == "Train_Class") {
      plot_data <- plot_data[plot_data$Data == input$dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Prob", colnames(plot_data))]

    } else if (paste0(input$performance, "_", input$evaluate)  == "Train_Prob") {
      plot_data <- plot_data[plot_data$Data == input$dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Class", colnames(plot_data))]
    }

    plot_data$group <- paste0(plot_data$Method, "_option", plot_data$Option) # "_iteration", plot_data$Iteration
    colnames(plot_data) <- stringr::str_replace(colnames(plot_data), paste0("_", input$performance, "_", input$evaluate), "")

    all_output <- suppressWarnings(lapply(1:nrow(plot_data), function(row) { # suppressWarnings: Warning: NAs introduced by coercion (happens because NAs converting to numerical)
      values_TPR <- as.numeric(strsplit(plot_data$Curve_TPR[row], split = "_")[[1]])
      values_FPR <- as.numeric(strsplit(plot_data$Curve_FPR[row], split = "_")[[1]])

      if (!is.na(plot_data$Curve_Models[row])) {
        values_models <- strsplit(plot_data$Curve_Models[row], split = "_")[[1]]
      } else {
        values_models <- NA
      }

      if (!is.na(plot_data$Curve_Thresholds[row])) {
        values_threshold <- as.numeric(strsplit(plot_data$Curve_Thresholds[row], split = "_")[[1]])
      } else {
        values_threshold <- NA
      }

      if ("Curve_Sensitivity" %in% colnames(plot_data)) {
        values_sensitivity <- as.numeric(strsplit(plot_data$Curve_Sensitivity[row], split = "_")[[1]])
      } else {
        values_sensitivity <- NA
      }

      if ("Curve_Specificity" %in% colnames(plot_data)) {
        values_specificity <- as.numeric(strsplit(plot_data$Curve_Specificity[row], split = "_")[[1]])
      } else {
        values_specificity <- NA
      }

      if ("Curve_PPV" %in% colnames(plot_data)) {
        values_PPV <- as.numeric(strsplit(plot_data$Curve_PPV[row], split = "_")[[1]])
      } else {
        values_PPV <- NA
      }

      if ("Curve_NPV" %in% colnames(plot_data)) {
        values_NPV <- as.numeric(strsplit(plot_data$Curve_NPV[row], split = "_")[[1]])
      } else {
        values_NPV <- NA
      }

      output <- data.frame(method = plot_data$group[row], iteration = plot_data$Iteration[row], values_TPR, values_FPR, values_models, values_threshold,
                           values_sensitivity, values_specificity, values_PPV, values_NPV,
                           N_outcomes = plot_data$N_outcomes[row], N_controls = plot_data$N_controls[row],
                           N_total = plot_data$N_total[row], row.names = NULL)
      # output <- unique(output)

      return(output)
    }))
    all_output <- rbindlist(all_output)

    return(all_output)
  })

  output$comparisonTable <- renderDataTable({
    table <- t(output_methods_metrics())
    colnames(table) <- paste0(table["Method",], "_option", table["Option",], "_iteration", table["Iteration",])
    table <- table[,table["Data",] == input$dataset]
    table <- table[-c(1:4),]

    return(table)
  }, options = list(pageLength = 25, scrollX = TRUE))

  fig_compare_metric <- function(metric) {
    if (input$evaluate == "Class" | grepl("Perf_AUC_|Perf_AUPRC_|Perf_PAUC_", metric)) {
      plot_data <- output_methods_metrics()
      plot_data$group <- paste0(plot_data$Method, "_option", plot_data$Option) # "_iteration", plot_data$Iteration

      figure = ggplot(plot_data, aes(Data, eval(parse(text=metric)), variable = group, group = group, colour = group)) +
        geom_point() +
        labs(title = paste0("Performance ", strsplit(metric, "_")[[1]][2]), x = "Data", y = strsplit(metric, "_")[[1]][2]) +
        theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
    } else if (grepl("Perf_Sensitivity_|Perf_Specificity_|Perf_PPV_|Perf_NPV", metric)) {
      output <- output_methods_metrics_pt()
      output <- output[grepl(tolower(unlist(strsplit(metric, "_"))[2]), tolower(sub("values_", "", output$variable))), ]

      figure <- ggplot(output, aes(values_threshold, value, group = method, color = method, text = paste("Model: ", values_models,
                                                                                                         "<br>Threshold: ", round(values_threshold,2),
                                                                                                         "<br>Value: ", round(value,2)))) +
        geom_line() +
        geom_point() +
        labs(title = paste0("Performance ", strsplit(metric, "_")[[1]][2]), x = "Data", y = strsplit(metric, "_")[[1]][2]) +
        theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
        #theme_bw()
    } else {
      figure = ggplot() + labs(title= paste0("Output '", strsplit(metric, "_")[[1]][2], "' not available")) + theme(panel.background = element_blank())
    }

    return(ggplotly(figure, tooltip = "text"))
  }

  output$comparisonAUC <- renderPlotly({
    fig_compare_metric(paste0("Perf_AUC_", input$performance, "_", input$evaluate))
  })

  output$comparisonAUPRC <- renderPlotly({
    fig_compare_metric(paste0("Perf_AUPRC_", input$performance, "_", input$evaluate))
  })

  output$comparisonPAUC <- renderPlotly({
    fig_compare_metric(paste0("Perf_PAUC_", input$performance, "_", input$evaluate))
  })

  output$comparisonAccuracy <- renderPlotly({
    fig_compare_metric(paste0("Perf_Accuracy_", input$performance, "_", input$evaluate))
  })

  output$comparisonSensitivity <- renderPlotly({
    fig_compare_metric(paste0("Perf_Sensitivity_", input$performance, "_", input$evaluate))
  })

  output$comparisonSpecificity <- renderPlotly({
    fig_compare_metric(paste0("Perf_Specificity_", input$performance, "_", input$evaluate))
  })

  output$comparisonPPV <- renderPlotly({
    fig_compare_metric(paste0("Perf_PPV_", input$performance, "_", input$evaluate))
  })

  output$comparisonNPV <- renderPlotly({
    fig_compare_metric(paste0("Perf_PPV_", input$performance, "_", input$evaluate))
  })

  output$comparisonBalancedAccuracy <- renderPlotly({
    fig_compare_metric(paste0("Perf_BalancedAccuracy_", input$performance, "_", input$evaluate))
  })

  output$comparisonF1score <- renderPlotly({
    fig_compare_metric(paste0("Perf_F1score_", input$performance, "_", input$evaluate))
  })

  output$aucCurves <- renderPlotly({
    output <- output_methods_curve()

    # info <- explore_output_plus()
    # info$group <- paste0("explore_option", info$Option, "_iteration", info$Iteration)
    # info <- info[info$Data == input$dataset,]

    # temp <- merge(output, info, by.x = "method", by.y = "group")

    output <- output[order(output$values_TPR, decreasing = FALSE),]
    output <- output[output$values_model != "model not available",]
    # result can be non-monotonically increasing for test set

    # temp_res <- data.frame(method="explore_temp", values_TPR=c(1,0.8305, 0.7995, 0.6420, 0.3484,0), values_FPR=c(1,0.2696,0.2466,0.1415,0.0462,0))
    # output <- rbind(output, temp_res, fill=TRUE)
    output$combined_group <- paste0(output$method, "_", output$iteration)
    output$iteration <- as.factor(output$iteration)

    figure <- ggplot(output, aes(values_FPR, values_TPR, group = combined_group, color = method, linetype=iteration, text = paste("Model: ", values_models,
                                                                                                      "<br>TPR: ", round(values_FPR,2),
                                                                                                      "<br>FPR: ", round(values_TPR,2)))) +
      geom_line() +
      geom_point() +
      theme_bw()

    ggplotly(figure, tooltip = "text")
  })


  output_methods_metrics_pt <- reactive({

    output <- output_methods_curve()

    output$values_threshold[output$values_threshold == -Inf] <- 0
    output$values_threshold[output$values_threshold == Inf] <- 1

    output <- output[!(is.na(output$values_model) | output$values_model == "NA"),]

    # output <- output[order(output$values_TPR, decreasing = FALSE),]
    # result can be non-monotonically increasing for test set
    output$N_outcomes <- NULL
    output$N_controls <- NULL
    output$N_total <- NULL
    output$values_TPR <- NULL
    output$values_FPR <- NULL

    output <- melt(output, id.vars = c("method", "iteration", "values_models", "values_threshold"))
    output <- output[order(output$values_threshold, decreasing = FALSE),]
    # result can be non-monotonically increasing for test set, TODO: why for the rest?

  })

  output$ptPlot <- renderPlotly({
    if (input$evaluate == "Prob") {
      output <- output_methods_metrics_pt()

      output$combined_group <- paste0(output$method, "_", output$iteration)
      # output$iteration <- as.factor(output$iteration)

      figure <- ggplot(output, aes(values_threshold, value, group = variable, color = variable, text = paste("Model: ", values_models,
                                                                                                             "<br>Threshold: ", round(values_threshold,2),
                                                                                                             "<br>Value: ", round(value,2)))) +
        facet_wrap(~ combined_group) +
        geom_line() +
        geom_point() +
        theme_bw()

    } else {
      figure <- ggplot() + labs(title= paste0("Output not available")) + theme(panel.background = element_blank())
    }
    ggplotly(figure, tooltip = "text")

  })

  output$modelTable <- renderDataTable({
    # table <- read.csv(file.path(outputFolder, input$resultFolder, paste0("models_", input$dataset, ".csv")), row.names = 1)
    table <- read.csv(file.path(outputFolder, input$resultFolder, paste0("models_", input$dataset, ".csv")))
    row.names(table) <- paste0(table$method, "_iteration", table$iteration)
    table[,c("method", "iteration")] <- NULL

    table <- t(table)

    table <- round_df(table, 2)

    brks <- seq(-1, 1, 1)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}

    # table <- datatable(table) %>% formatStyle(colnames(table), backgroundColor = styleInterval(brks, clrs))

    return(table)
  }, options = list(pageLength = 25, scrollX = TRUE))

  # Below function is adjusted from PLP:
  output$nbTable <- DT::renderDataTable({
    result <- extractNetBenefit()
    result$treatNone <- format(result$treatNone, digits = 2, scientific = F)
    result$treatAll <- format(result$treatAll, digits = 2, scientific = F)
    result$netBenefit <- format(result$netBenefit, digits = 2, scientific = F)
    result
  })

  # Below function is adjusted from PLP:
  output$nbPlot <- renderPlotly({
    result <- extractNetBenefit()
    # result <- result[result$netBenefit >= -0.05,]
    # result <- result[result$treatAll >= -0.05,]
    # ind <- !is.na(result$netBenefit) & is.finite(result$netBenefit) & !is.null(result$netBenefit) & is.finite(result$pt)

    df2 <- tidyr::pivot_longer(
      data = result,
      cols = colnames(result)[!(colnames(result) %in% c('pt', 'method'))],
      names_to = 'variable',
      values_to = 'value'
    )

    figure <- ggplot2::ggplot(
      data = df2,
      ggplot2::aes(
        x = .data$pt,
        y = .data$value
      )
    ) +
      ggplot2::geom_line( # position = position_dodge(width = 0.01),
        ggplot2::aes(
          linetype = .data$variable,
          color = .data$method
        )
      ) + ylim(-0.1, max(result[,c("netBenefit", "treatAll")])*1.2) + xlim(0,0.5)

    ggplotly(figure, tooltip = "text")
  })

  # Below function is adjusted from PLP:
  extractNetBenefit <- reactive({
    output <- output_methods_curve()
    output$method <- paste0(output$method, "_", output$iteration)
    # output$iteration <- NULL

    output <- output[order(output$values_threshold, decreasing = FALSE),]

    output$values_threshold[output$values_threshold == -Inf] <- 0
    output$values_threshold[output$values_threshold == Inf] <- 1

    output <- output[!is.na(output$N_total ),]

    N_o <- unique(output$N_outcome)
    N_c <- unique(output$N_controls)
    N <- N_o + N_c

    netbenefit <- data.frame()

    for (m in unique(output$method)) {
      output_m <- output[output$method == m, ]

      if (length(unique(output_m$values_threshold)) <= 3) { # not enough points for curve (classes)

        pt_m <- 0.2 # output_m$values_threshold
        output_m$TP_m <- output_m$values_TPR*output_m$N_outcomes
        output_m$FP_m <- output_m$values_FPR*output_m$N_controls
        output_m$net_benefit <- output_m$TP_m/N-(output_m$FP_m/N)*(pt_m/(1-pt_m))
        index <- which(output_m$net_benefit == max(output_m$net_benefit))

        # TP_m <- sort(TP_m, decreasing = FALSE)[2] # select the ...
        TP_m <- output_m$TP_m[index]
        # FP_m <- sort(FP_m, decreasing = FALSE)[2] # select the ...
        FP_m <- output_m$FP_m[index]

        print(paste0("method: ", m, " TP_m: ", TP_m, " FP_m: ", FP_m))

        pt <- seq(0, 1, 0.1)
        netbenefit_m <- data.frame(
          method = m,
          pt = pt,
          netBenefit = TP_m/N-(FP_m/N)*(pt/(1-pt)),
          treatAll = N_o/N-N_c/N*(pt/(1-pt)),
          treatNone = 0
        )

      } else { # enough thresholds for curve (probabilities)

        pt <- output_m$values_threshold
        TP <- output_m$values_TPR*output_m$N_outcomes
        FP <- output_m$values_FPR*output_m$N_controls

        netbenefit_m <- data.frame(
          method = output_m$method,
          pt = pt,
          netBenefit = TP/N-(FP/N)*(pt/(1-pt)),
          treatAll = N_o/N-N_c/N*(pt/(1-pt)),
          treatNone = 0
        )
      }

      netbenefit <- rbind(netbenefit, netbenefit_m)
    }

    netbenefit <- unique(netbenefit)

    return(netbenefit)
  })

}

