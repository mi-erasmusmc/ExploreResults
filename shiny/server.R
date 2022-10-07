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
    paste0("Information included datasets for output ", names(which(resultFolders == input$resultFolder)))
  })

  output$dataTable <- renderDataTable({
    summary_data <- read.csv(file.path(outputFolder, input$resultFolder, "summary_data.csv"), row.names = 1)
    table <- t(summary_data)
    table <- round_df(table, 2)

    return(table)
  }, options = list(pageLength = 15))


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
    table <- explore_output_plus()
    table <- t(table)
    colnames(table) <- paste0("Option_", table[row.names(table) == "Option",])
    table <- table[!(row.names(table) == "Option"),] # TODO: check iterations?

    return(table)
  }, options = list(pageLength = 15))


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
      theme_bw() +
      ylab("Improvement (times faster)")
  })

  # TODO: output times

  # Compare methods tab
  output$comparisonTitle <- renderText({
    paste0("Compare results for output ", names(which(resultFolders == input$resultFolder)))
  })

  output_methods <- reactive({read.csv(file.path(outputFolder, input$resultFolder, "output_methods.csv"))})

  output$comparisonTable <- renderDataTable({
    table <- t(output_methods())
    colnames(table) <- paste0("Result_", 1:ncol(table))

    return(table)
  }, options = list(pageLength = 25))

  fig_compare_metric <- function(metric) {
    plot_data <- output_methods()

    ggplot(output_methods(), aes(Data, eval(parse(text=metric)), variable = Method, group = Method, colour = Method)) +
      geom_point() + # geom_line() DOES NOT SHOW RESULTS WITH ONE DATASET
      labs(title = paste0("Performance ", strsplit(metric, "_")[[1]][2]), x = "Data", y = strsplit(metric, "_")[[1]][2]) +
      theme(axis.text.x = element_text(angle = 90)) + ylim(0,1)
  }

  output$comparisonAUC <- renderPlot({
    fig_compare_metric(paste0("Perf_AUC_", input$resultSet))
  })

  output$comparisonAUPRC <- renderPlot({
    fig_compare_metric(paste0("Perf_AUPRC_", input$resultSet))
  })

  output$comparisonPAUC <- renderPlot({
    fig_compare_metric(paste0("Perf_PAUC_", input$resultSet))
  })

  output$comparisonAccuracy <- renderPlot({
    fig_compare_metric(paste0("Perf_Accuracy_", input$resultSet))
  })

  output$comparisonSensitivity <- renderPlot({
    fig_compare_metric(paste0("Perf_Sensitivity_", input$resultSet))
  })

  output$comparisonSpecificity <- renderPlot({
    fig_compare_metric(paste0("Perf_Specificity_", input$resultSet))
  })

  output$comparisonPPV <- renderPlot({
    fig_compare_metric(paste0("Perf_PPV_", input$resultSet))
  })

  output$comparisonNPV <- renderPlot({
    fig_compare_metric(paste0("Perf_PPV_", input$resultSet))
  })

  output$comparisonBalancedAccuracy <- renderPlot({
    fig_compare_metric(paste0("Perf_BalancedAccuracy_", input$resultSet))
  })

  output$comparisonF1score <- renderPlot({
    fig_compare_metric(paste0("Perf_F1score_", input$resultSet))
  })

  output$modelTable <- renderDataTable({
    table <- read.csv(file.path(outputFolder, input$resultFolder, paste0("models_", input$dataset, ".csv")))
    table <- t(table)

    return(table)
  }, options = list(pageLength = 25))

}
