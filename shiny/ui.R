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
  dashboardHeader(title = "Pathways Results",
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
      selectInput("resultFolder", label = "Result Folder", choices = resultFolders,  selected = resultFolders[1]),
      conditionalPanel(condition = "input.tabs=='explore'",
                       htmlOutput("dynamic_comparisonExplore")
      ),
      conditionalPanel(condition = "input.tabs=='methods'",
                       selectInput("resultSet", label = "Performance", choices = c("Test", "Train"),  selected = "Test")
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
          "..."
        ),
        HTML("<li>R study package: <a href=\"https://github.com/mi-erasmusmc/AsthmaCOPDTreatmentPatterns\">GitHub</a></li>"),
        h3("Description"),
        p("...")
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
                  "Summary figures",
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
                  "Comparison",
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
                  "Summary figures",
                  br(),
                  box(width = 4,
                      plotOutput("comparisonAUC"),
                      plotOutput("comparisonAUPRC"),
                      plotOutput("comparisonPAUC")
                  ),
                  box(width = 4,
                      plotOutput("comparisonF1score"),
                      plotOutput("comparisonBalancedAccuracy"),
                      plotOutput("comparisonAccuracy")
                   ),
                  box(width = 4,
                      plotOutput("comparisonSensitivity"),
                      plotOutput("comparisonSpecificity"),
                      plotOutput("comparisonPPV"),
                      plotOutput("comparisonNPV")
                  ),
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
