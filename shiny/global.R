# Load necessary libraries
library(shiny)
library(ipeadatar)
library(tidyverse)
library(plotly)
library(shinybusy)
library(fpp3)
library(shinyWidgets)
library(DT)

# Retrieve data
# write_csv(ipeadatar::available_series("br"), "datasets.csv")

datasets <- read_csv("datasets.csv")
#subject <- ipeadatar::available_subjects(language = c("en","br"))
