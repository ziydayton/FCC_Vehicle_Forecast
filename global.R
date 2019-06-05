
rm(list = ls())

library(shiny)
library(data.table)
library(forecast)

source("Func.R", local = TRUE)
source("PlotFunc.R", local = TRUE)

fleet.data <- read.csv("data/Auto_Info.csv",
                       stringsAsFactors = FALSE)
