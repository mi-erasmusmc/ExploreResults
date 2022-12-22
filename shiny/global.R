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

# resultFolders <- rev(sapply(list.dirs(file.path(local, "output"), recursive = FALSE, full.names = FALSE),
#                              function(g) sub("timings_", "", g)))

# TODO: change to automatically update?
resultFolders <- list("timings_2022-12-16" = "timings_2022-12-16",
                      "timings_2022-12-16-rerun" = "timings_2022-12-16-rerun",
                      "timings_2022-12-08" = "timings_2022-12-08",
                      "timings_2022-12-07" = "timings_2022-12-07",
                      "timings_2022-11-22" = "timings_2022-11-22")

writeLines("Data Loaded")

# check same sample?
# d <- "iris_train_2_1.arff"
# data1 <- farff::readARFF(file.path(outputFolder, "timings_2022-12-16", "explore", d))
# data2 <- farff::readARFF(file.path(outputFolder, "timings_2022-12-08", "explore", d))
#
# sum(data1!=data2)

