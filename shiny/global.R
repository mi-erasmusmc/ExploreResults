library(shiny)
library(shinydashboard)
library(shinymanager)
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)
library(plotly)

# Set working directory (shiny folder)
# if (!grepl("shiny$", getwd())) {
#  setwd(paste0(getwd(), "/shiny"))
# }

local <- paste0(getwd())
addResourcePath("workingdirectory", getwd())

outputFolder <- file.path(local, "output")

resultFolders <- list("Name output" = "yyyy-mm-dd")

writeLines("Data Loaded")

