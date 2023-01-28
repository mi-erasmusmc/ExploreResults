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
  output$dynamic_modelMethods = renderUI({
    one <- selectInput("dataset", label = "Dataset", choices = unique(from_csv_explore_options()$Data), selected = unique(from_csv_explore_options()$Data)[1])

    return(tagList(one))
  })

  # Load files
  from_csv_output_methods <- reactive({
    input_data <- read.csv(file.path(outputFolder, input$resultFolder, "output_methods.csv"))
    if (!("Selection" %in% colnames(input_data))) {
      input_data$Selection <- TRUE
    }
    input_data$Selection_file <- ifelse(input_data$Selection, "", "_Full")

    return(input_data)
  })

  from_csv_explore_options <- reactive({
    input_data <- read.csv(file.path(outputFolder, input$resultFolder, "explore_options.csv"))

    explore_output <- from_csv_output_methods()
    explore_output <- explore_output[explore_output$Method == "EXPLORE", c("Time", "Data", "Iteration", "Option", "Model")]

    explore_output$Time <- sapply( explore_output$Time, function(time) {
      if (grepl("secs", time)) {
        as.numeric(stringr::str_remove(time, " secs")) / 60 # convert to minutes
      } else if (grepl("mins", time)) {
        as.numeric(stringr::str_remove(time, " mins"))
      } else if (grepl("hours", time)) {
        as.numeric(stringr::str_remove(time, " hours")) * 60 # convert to minutes
      }
    })

    explore_options <- merge(input_data, explore_output, by = "Option")

    return(explore_options)
  })


  from_csv_models <- reactive({
    input_data <- read.csv(file.path(outputFolder, input$resultFolder, paste0("models_", input$dataset, input$selection)))

    input_data$selection_file <- gsub(".csv", "", input$selection)

    colnames(input_data) <- gsub("X", "", colnames(input_data))
    row.names(input_data) <- paste0(input_data$method, "_", input_data$option, input_data$selection_file, "_", input_data$iteration)

    input_data[,c("method", "option", "iteration", "selection_file")] <- NULL

    return(input_data)
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

  output$exploreOptions <- renderDataTable({
    table <- from_csv_explore_options()
    table <- t(table)
    colnames(table) <- paste0("Option_", table[row.names(table) == "Option",])
    table <- table[!(row.names(table) == "Option"),] # TODO: check iterations?

    return(table)
  }, options = list(pageLength = 15, scrollX = TRUE))

  output$exploreOutputTitle <- renderText({
    paste0("Explore results for output ", names(which(resultFolders == input$resultFolder)))
  })

  output$exploreOutputRuleLength <- renderPlotly({
    plot_data <- from_csv_explore_options()

    figure <- ggplot(plot_data, aes(EndRulelength, Time, shape = Parallel, color = Data, text = paste("Data: ", Data,
                                                                                                      "<br>Time: ", Time))) +
      geom_point() +
      theme_bw() +
      scale_y_log10(labels = scales::comma) +
      ylab("Time (in minutes)")

    ggplotly(figure, tooltip = "text")
  })

  output$exploreOutputMaximize <- renderPlotly({
    plot_data <- from_csv_explore_options()

    figure <- ggplot(plot_data, aes(Maximize, Time, shape = Parallel, color = Data, text = paste("Data: ", Data,
                                                                                                 "<br>Time: ", Time))) +
      geom_point() +
      theme_bw() +
      scale_y_log10(labels = scales::comma) +
      ylab("Time (in minutes)")

    ggplotly(figure, tooltip = "text")
  })

  # Tables tab
  output$comparisonTitle <- renderText({
    paste0("Compare results for output ", names(which(resultFolders == input$resultFolder)))
  })

  output_methods_metrics <- reactive({
    data <- from_csv_output_methods()
    data <- data[,!(colnames(data) %like% "Curve_|N_|Time")]

    # Aggregate over iterations
    cols_group <- c("Data", "Selection", "Method", "Option", "Model", "Selection_file")
    cols_other <- colnames(data)[!colnames(data) %in% c(cols_group, "Iteration")]

    data <- data.table(data)
    data <- data[,lapply(.SD, mean), .SDcols = cols_other, by = cols_group]

    data$Group <- paste0(data$Method, "_", data$Option, data$Selection_file)
    data$Selection_file <- NULL

    return(data)
  })

  output$comparisonTable <- renderDataTable({
    table <- t(output_methods_metrics())
    colnames(table) <- table["Group",]
    table <- table[,table["Data",] == input$dataset]
    table <- table[-c(1:4, 38),]

    return(table)
  }, options = list(pageLength=50, scrollX = TRUE))

  output$modelTable <- renderDataTable({
    table <- from_csv_models()

    table <- t(table)
    table <- round_df(table, 2)

    # brks <- seq(-1, 1, 1)
    # clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
    # table <- datatable(table) %>% formatStyle(colnames(table), backgroundColor = styleInterval(brks, clrs))

    return(table)
  }, options = list(pageLength = 50, scrollX = TRUE))

  # Figures tab
  data_to_curve <- function(plot_data, performance, dataset, evaluate) {

    if (paste0(performance, "_", evaluate) == "Test_Class") {
      plot_data <- plot_data[plot_data$Data == dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Prob", colnames(plot_data))]

    } else if (paste0(performance, "_", evaluate)  == "Test_Prob") {
      plot_data <- plot_data[plot_data$Data == dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Train", colnames(plot_data)) & !grepl("_Test_Class", colnames(plot_data))]

    } else if (paste0(performance, "_", evaluate)  == "Train_Class") {
      plot_data <- plot_data[plot_data$Data == dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Prob", colnames(plot_data))]

    } else if (paste0(performance, "_", evaluate)  == "Train_Prob") {
      plot_data <- plot_data[plot_data$Data == dataset, !grepl("Perf_", colnames(plot_data)) & !grepl("_Test", colnames(plot_data)) & !grepl("_Train_Class", colnames(plot_data))]
    }

    plot_data$Group <- paste0(plot_data$Method, "_", plot_data$Option, plot_data$Selection_file)

    colnames(plot_data) <- stringr::str_replace(colnames(plot_data), paste0("_", performance, "_", evaluate), "")

    # suppressWarnings: Warning: NAs introduced by coercion (happens because NAs converting to numerical)
    all_output <- suppressWarnings(lapply(1:nrow(plot_data), function(row) {
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

      output <- data.frame(group = plot_data$Group[row], iteration = plot_data$Iteration[row], values_TPR, values_FPR, values_models, values_threshold,
                           values_sensitivity, values_specificity, values_PPV, values_NPV,
                           N_outcomes = plot_data$N_outcomes[row], N_controls = plot_data$N_controls[row],
                           N_total = plot_data$N_total[row], row.names = NULL)

      return(output)
    }))
    all_output <- rbindlist(all_output)

    # Correct bounds thresholds if needed
    all_output$values_threshold[all_output$values_threshold == -Inf] <- 0
    all_output$values_threshold[all_output$values_threshold == Inf] <- 1

    return(all_output)
  }

  output_methods_curve <- reactive({
    plot_data <- from_csv_output_methods()
    all_output <- data_to_curve(plot_data, performance = input$performance, dataset = input$dataset, evaluate = input$evaluate)
    return(all_output)
  })

  fig_compare_metric <- function(metric) {
    if (input$evaluate == "Class" | grepl("Perf_AUC_|Perf_AUPRC_|Perf_PAUC_", metric)) {
      plot_data <- output_methods_metrics()

      figure = ggplot(plot_data, aes(Data, eval(parse(text=metric)), variable = Group, group = Group, colour = Group)) +
        geom_point() +
        labs(title = paste0("Performance ", strsplit(metric, "_")[[1]][2]), x = "Data", y = strsplit(metric, "_")[[1]][2]) +
        theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
    } else if (grepl("Perf_Sensitivity_|Perf_Specificity_|Perf_PPV_|Perf_NPV", metric)) {
      output <- output_methods_metrics_pt()
      output <- output[grepl(tolower(unlist(strsplit(metric, "_"))[2]), tolower(sub("values_", "", output$variable))), ]

      figure <- ggplot(output, aes(values_threshold, value, group = combined_group, color = combined_group, text = paste("Model: ", values_models,
                                                                                                         "<br>Threshold: ", round(values_threshold,2),
                                                                                                         "<br>Value: ", round(value,2)))) +
        geom_line() +
        geom_point() +
        labs(title = paste0("Performance ", strsplit(metric, "_")[[1]][2]), x = "Data", y = strsplit(metric, "_")[[1]][2]) +
        theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
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
    fig_compare_metric(paste0("Perf_NPV_", input$performance, "_", input$evaluate))
  })

  output$comparisonBalancedAccuracy <- renderPlotly({
    fig_compare_metric(paste0("Perf_BalancedAccuracy_", input$performance, "_", input$evaluate))
  })

  output$comparisonF1score <- renderPlotly({
    fig_compare_metric(paste0("Perf_F1score_", input$performance, "_", input$evaluate))
  })

  output$tradeOff <- renderPlotly({
    output <- output_methods_metrics()

    output$Size[output$Method != "EXPLORE"] <- gsub(" covariates", "", output$Model[output$Method != "EXPLORE"]) # all models except EXPLORE
    output$Size[output$Method == "EXPLORE"] <- stringr::str_count(output$Model[output$Method == "EXPLORE"], "AND|OR") + 1 # EXPLORE
    output$Size <- as.numeric(output$Size)

    figure <- ggplot(output, aes(eval(parse(text=paste0("Perf_AUC_", input$performance, "_Prob"))), Size, color = Group, shape=Data, text = paste("Model: ", Group,
                                                                                                                                                  "<br>Data: ", Data))) +
      geom_point() +
      theme_bw() + xlim(0.5, 1) + xlab(paste0("AUC ", input$performance))

    ggplotly(figure, tooltip = "text")
  })

  output$aucCurves <- renderPlotly({
    output <- output_methods_curve()

    output <- output[order(output$values_TPR, decreasing = FALSE),]
    output <- output[output$values_models != "model not available",]
    # result can be non-monotonically increasing for test set

    # temp <- data.frame(group="explore_extra", values_TPR=c(1,0.8305, 0.7995, 0.6420, 0.3484,0), values_FPR=c(1,0.2696,0.2466,0.1415,0.0462,0))
    # output <- rbind(output, temp, fill=TRUE)

    # To visualize different iterations: add output$iteration <- as.factor(output$iteration) + to figure linetype = "iterations"

    figure <- ggplot(output, aes(values_FPR, values_TPR, group = group, color = group, text = paste("Model: ", values_models,
                                                                                                    "<br>TPR: ", round(values_FPR,2),
                                                                                                    "<br>FPR: ", round(values_TPR,2)))) +
      geom_line() +
      geom_point() +
      theme_bw()

    ggplotly(figure, tooltip = "text")
  })

  output_methods_metrics_pt <- reactive({
    output <- output_methods_curve()

    # Need combined group to display models
    output$combined_group <- paste0(output$group, "_", output$iteration)

    output <- output[!(is.na(output$values_models) | output$values_models == "NA"),]
    output[,c("iteration", "N_outcomes", "N_controls", "N_total", "values_TPR", "values_FPR")] <- NULL

    output$unique_id <- row.names(output)

    output <- melt(output, id.vars = c("combined_group", "group", "values_models", "values_threshold", "unique_id"))
    output <- output[order(output$values_threshold, decreasing = FALSE),]
    # result can be non-monotonically increasing for test set

    return(output)
  })

  output$ptPlot <- renderPlotly({
    if (input$evaluate == "Prob") {
      output <- output_methods_metrics_pt()

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

  output$ptPlot_reversed <- renderPlotly({
    if (input$evaluate == "Prob") {
      output <- output_methods_metrics_pt()

      figure <- ggplot(output, aes(variable, value, group = unique_id, color = combined_group, text = paste("Model: ", values_models,
                                                                                                            "<br>Threshold: ", round(values_threshold,2),
                                                                                                            "<br>Value: ", round(value,2)))) +
        geom_line() +
        geom_point() +
        theme_bw()
    } else {
      figure <- ggplot() + labs(title= paste0("Output not available")) + theme(panel.background = element_blank())
    }
    ggplotly(figure, tooltip = "text")

  })

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

    df2 <- tidyr::pivot_longer(
      data = result,
      cols = colnames(result)[!(colnames(result) %in% c('pt', 'combined_group'))],
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
          color = .data$combined_group
        )
      ) + ylim(-0.1, max(result[,c("netBenefit", "treatAll")])*1.2) + xlim(0,0.4)

    ggplotly(figure, tooltip = "text")
  })

  extractNetBenefit <- reactive({
    output <- output_methods_curve()
    output$combined_group <- paste0(output$group, "_", output$iteration)
    output$iteration <- NULL

    output <- output[order(output$values_threshold, decreasing = FALSE),]

    output <- output[!is.na(output$N_total),]

    N_o <- unique(output$N_outcome)
    N_c <- unique(output$N_controls)
    N <- N_o + N_c

    netbenefit <- data.frame()

    for (m in unique(output$combined_group)) {
      output_m <- output[output$combined_group == m, ]

      output_m$TP_m <- output_m$values_TPR*output_m$N_outcomes
      output_m$FP_m <- output_m$values_FPR*output_m$N_controls

      # NB curve for classes
      if (nrow(output_m) == 3) {

        pt <- c(seq(0.05,0.65,0.1), seq(0.75,0.95,0.02))

        # Get TP and FP for this given model (at threshold = 0.5)
        TP_m <- output_m$TP_m[output_m$values_threshold == 0.5]
        FP_m <- output_m$FP_m[output_m$values_threshold == 0.5]

        netbenefit_m <- data.frame(
          combined_group = m,
          pt = pt,
          netBenefit = TP_m/N-(FP_m/N)*(pt/(1-pt)),
          treatAll = N_o/N-N_c/N*(pt/(1-pt)),
          treatNone = 0
        )

      } else if (nrow(output_m) > 3) {
        # NB Curve for probabilities

        if (grepl("EXPLORE", m)) {

          # Add unique ID to models
          output_m$unique_id <- 1:nrow(output_m)

          # Choose models to use for curve based on train set (for test set)
          if (input$performance == "Test") {
            plot_data <- from_csv_output_methods()
            output_select <- data_to_curve(plot_data, performance = "Train", dataset = input$dataset, evaluate = input$evaluate)

            output_select$combined_group <- paste0(output_select$group, "_", output_select$iteration)
            output_select$iteration <- NULL
            output_select <- output_select[order(output_select$values_threshold, decreasing = FALSE),]
            output_select <- output_select[!is.na(output_select$N_total),]

            output_select_m <- output_select[output_select$combined_group == m, ]

            output_select_m$TP_m <- output_select_m$values_TPR*output_select_m$N_outcomes
            output_select_m$FP_m <- output_select_m$values_FPR*output_select_m$N_controls

            # Add unique ID to models
            output_select_m$unique_id <- 1:nrow(output_m)
          } else if (input$performance == "Train") {
            output_select_m <- output_m
          }

          pt <- 1 - c(seq(0.05,0.65,0.1), seq(0.75,0.97,0.02))
          netbenefit_m <- data.frame()

          for (pt_m in pt) { # pt_m <- 0.2

            # Compute net benefit using train set
            output_select_m$net_benefit <- output_select_m$TP_m/N-(output_select_m$FP_m/N)*(pt_m/(1-pt_m))

            # Select first model that achieves highest net benefit
            index <- which(output_select_m$net_benefit == max(output_select_m$net_benefit))[1]
            model_id <- output_select_m$unique_id[index]

            # Compute net benefit using the errors of this model in the test set
            TP_pt_m <- output_m$TP_m[output_m$unique_id  == model_id]
            FP_pt_m <- output_m$FP_m[output_m$unique_id  == model_id]

            netbenefit_pt_m <- list(combined_group = m,
                                    pt = pt_m,
                                    netBenefit = TP_pt_m/N-(FP_pt_m/N)*(pt_m/(1-pt_m)),
                                    treatAll = N_o/N-N_c/N*(pt_m/(1-pt_m)),
                                    treatNone = 0)

            netbenefit_m <- rbind(netbenefit_m, netbenefit_pt_m)
          }

        } else {
          # Traditional computation of NB
          pt <- output_m$values_threshold
          TP <- output_m$TP_m
          FP <- output_m$FP_m

          netbenefit_m <- data.frame(
            combined_group = output_m$combined_group,
            pt = pt,
            netBenefit = TP/N-(FP/N)*(pt/(1-pt)),
            treatAll = N_o/N-N_c/N*(pt/(1-pt)),
            treatNone = 0
          )
        }
      } else {
        warning("Found less than 3 points during computation NB curve!")
      }

      netbenefit <- rbind(netbenefit, netbenefit_m)
    }

    netbenefit <- unique(netbenefit)

    return(netbenefit)
  })

}

