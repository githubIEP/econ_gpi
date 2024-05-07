
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, WDI, readr, openxlsx, readxl, 
               DataCombine, countrycode, ggplot2, patchwork)

devtools::install_github("david-hammond/tidyindexR")

options (scipen = 999)


source("03_scripts/ProjectFunction.R")
source("03_scripts/ProjectVariables.R")



script_dir <- "03_scripts/01_cleaning"

script_files <- list.files(script_dir, pattern = "\\.R$", full.names = TRUE)

for (script_file in script_files) {
  source(script_file)
}

source("03_scripts/02_standardOutputs/Section 3 - EconCostingCharts.R")
source("03_scripts/ProjectExport.R")
