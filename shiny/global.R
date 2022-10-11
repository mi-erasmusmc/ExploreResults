library(shiny)
library(shinydashboard)
library(shinymanager)
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)

# Set working directory (shiny folder)
# if (!grepl("shiny$", getwd())) {
#  setwd(paste0(getwd(), "/shiny"))
# }

local <- paste0(getwd())
addResourcePath("workingdirectory", getwd())

outputFolder <- file.path(local, "output")

# outputfolders <- rev(sapply(list.dirs(file.path(local, "output"), recursive = FALSE, full.names = FALSE),
#                              function(g) sub("timings_", "", g)))

# TODO: change to automatically update?
resultFolders <- list("2022-10-11" = "timings_2022-10-11",
                       "2022-10-07" = "timings_2022-10-07",
                      "2022-08-12" = "timings_2022-08-12")

writeLines("Data Loaded")

