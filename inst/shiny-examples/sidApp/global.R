# This script add the AVA library path
# This is needed to run the app on the Shiny Server Pro
#if(file.exists('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R'))
#  source('/CHBS/apps/busdev_apps/AVA/RLib/AVA.R', local = T)

#library(ava)
#library(sidtools)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(data.table)
library(dplyr)
library(stringr)
library(R.utils)
library(sidtoolbox)
library(arules)
library(arulesViz)
library(pso)
library(Hmisc)
library(MASS)
library(survival)


options(appName="sidApp")
options(appDesc="Subgroup identification application")
options(deployVersion="Beta")
options(author="Marzie Rasekh")
options(email="marzie.eslami_rasekh@novartis.com")

SIMULATE_CASE <- c("prognostic", "predictive")
MEASURE_FILTER_BY <- c("support", "confidence", "lift")
CORRELATION_METHOD <- c("spearman", "pearson")
CORRELATION_INFERENCE_TEST_TYPES <- c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")
CORRELATION_INFERENCE_TEST_STATS <- c("quad", "max")
PSO_METHODS <- c("auto", "manual", "Parato (to be added)")

values <- reactiveValues(
  TSDT = list(), 
  TSDTparam = list(),
  PSO = data.table(), 
  VT = list(),
  Vparam = list(),
  sid.data = list()
)
