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
      menuItem("Comparison tables", tabName = "tables"),
      menuItem("Comparison figures", tabName = "methods"),

      # Input parameters
      selectInput("resultFolder", label = "Result Folder", choices = resultFolders,  selected = resultFolders[1]),
      conditionalPanel(condition = "input.tabs=='tables' || input.tabs=='methods'",
                       htmlOutput("dynamic_modelMethods")
      ),
      conditionalPanel(condition = "input.tabs=='tables'",
                       selectInput("selection", label = "Covariates", choices = list("All" = "_Full.csv", "Selected 50" = ".csv"), selected = "Selected 50")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       selectInput("performance", label = "Performance", choices = c("Train", "Test"),  selected = "Test")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       selectInput("evaluate", label = "Evaluate", choices = c("Class", "Prob"),  selected = "Prob")
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
                  "Settings and results",
                  br(),
                  textOutput("exploreOptionsTitle"),
                  br(),
                  dataTableOutput("exploreOptions")
                ),
                tabPanel(
                  "Times",
                  br(),
                  textOutput("exploreOutputTitle"),
                  br(),
                  box(
                    plotlyOutput("exploreOutputRuleLength")
                  ),
                  box(
                    plotlyOutput("exploreOutputMaximize")
                  )
                )
              )
      ),
      tabItem(tabName = "tables",
              tabsetPanel(
                id = "",
                tabPanel(
                  "Performance",
                  br(),
                  textOutput("comparisonTitle"),
                  dataTableOutput("comparisonTable")
                ),
                tabPanel(
                  "Models",
                  br(),
                  dataTableOutput("modelTable")
                )
              )
      ),
      tabItem(tabName = "methods",
              tabsetPanel(
                id = "",
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
                  "Trade-off",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("tradeOff")
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
                  "PTP reversed",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("ptPlot_reversed")
                ),
                tabPanel(
                  "Net benefit",
                  br(),
                  # textOutput("comparisonTitle"),
                  plotlyOutput("nbPlot"),
                  dataTableOutput("nbTable")
                )
              )

      )
    )
  )
)
