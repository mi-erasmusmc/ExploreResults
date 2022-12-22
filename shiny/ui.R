# Help functions
addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button",
    id = infoId,
    "i"
  )
  item$children[[1]]$children <-
    append(item$children[[1]]$children, list(infoTag))
  return(item)
}

# Shiny ui function
ui <- dashboardPage(
  dashboardHeader(title = "EXPLORE Results",
                  tags$li(div(img(src = 'logo.png',
                                  title = "OHDSI PLP", height = "40px", width = "40px"),
                              style = "padding-top:0px; padding-bottom:0px;"),
                          class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",

      # Tabs (some with additional information)
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "data"),
      menuItem("EXPLORE", tabName = "explore"),
      addInfo(menuItem("Compare methods", tabName = "methods"), "methodsInfo"),

      # Input parameters
      # -> "Test_Class", "Test_Prob", "Train_Class"
      selectInput("resultFolder", label = "Result Folder", choices = resultFolders,  selected = resultFolders[1]),
      conditionalPanel(condition = "input.tabs=='explore'",
                       htmlOutput("dynamic_comparisonExplore")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       selectInput("performance", label = "Performance", choices = c("Train", "Test"),  selected = "Train")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       selectInput("evaluate", label = "Evaluate", choices = c("Class", "Prob"),  selected = "Class")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       htmlOutput("dynamic_modelMethods")
      )

    )
  ),
  dashboardBody(

    tags$body(tags$div(id="ppitest", style="width:1in;visible:hidden;padding:0px")),
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                            '),

    tabItems(
      tabItem(
        tabName = "about",
        br(),
        p(
          "Investigating the potential of EXPLORE for patient-level prediction models developed in OHDSI by comparing the performance to frequently used methods for patient-level prediction."
        ),
        HTML("<li>ExploreResults package: <a href=\"https://github.com/AniekMarkus/ExploreResults\">GitHub ExploreResults</a></li>"),
        HTML("<li>Explore package: <a href=\"https://github.com/mi-erasmusmc/Explore\">GitHub Explore</a></li>"),
        # h3("Description"),
        # p("...")
      ),

      tabItem(tabName = "data",
              box(width = 12,
                  textOutput("dataTableTitle"),
                  dataTableOutput("dataTable")
              )
      ),

      tabItem(tabName = "explore",
              tabsetPanel(
                id = "",
                tabPanel(
                  "Summary table",
                  br(),
                  textOutput("exploreOptionsTitle"),
                  br(),
                  dataTableOutput("exploreOptions")
                ),
                tabPanel(
                  "Summary time",
                  br(),
                  textOutput("exploreOutputTitle"),
                  br(),
                  box(
                    plotOutput("exploreOutputRuleLength")
                  ),
                  box(
                    plotOutput("exploreOutputMaximize")
                  )
                ),
                tabPanel(
                  "Comparison time",
                  br(),
                  textOutput("exploreComparisonTitle"),
                  br(),
                  box(
                    plotOutput("exploreComparison1")
                  ),
                  box(
                    plotOutput("exploreComparison2")
                  )
                )
              )
      ),

      tabItem(tabName = "methods",
              tabsetPanel(
                id = "",
                tabPanel(
                  "Summary table",
                  br(),
                  textOutput("comparisonTitle"),
                  dataTableOutput("comparisonTable")
                ),
                tabPanel(
                  "Summary metrics",
                  br(),
                  box(width = 4,
                      plotlyOutput("comparisonAUC"),
                      plotlyOutput("comparisonAUPRC"),
                      plotlyOutput("comparisonPAUC")
                  ),
                  box(width = 4,
                      plotlyOutput("comparisonSensitivity"),
                      plotlyOutput("comparisonSpecificity"),
                      plotlyOutput("comparisonPPV"),
                      plotlyOutput("comparisonNPV")
                  ),
                  box(width = 4,
                      plotlyOutput("comparisonF1score"),
                      plotlyOutput("comparisonBalancedAccuracy"),
                      plotlyOutput("comparisonAccuracy")
                  ),
                ),
                tabPanel(
                  "AUC curves",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("aucCurves")
                ),
                tabPanel(
                  "PTP",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("ptPlot")
                ),
                tabPanel(
                  "Net benefit",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("nbPlot"),
                  dataTableOutput("nbTable")
                ),
                tabPanel(
                  "Summary model",
                  br(),
                  # textOutput("comparisonTitle"),
                  dataTableOutput("modelTable")
                )
              )

      )
    )
  )
)
