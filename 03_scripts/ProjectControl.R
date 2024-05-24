
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, WDI, readr, openxlsx, readxl, 
               DataCombine, countrycode, ggplot2, patchwork)

devtools::install_github("david-hammond/tidyindexR")

options (scipen = 999)


source("03_scripts/ProjectFunction.R")
source("03_scripts/ProjectVariables.R")
source("03_scripts/01_cleaning/01-admin.R")
source("03_scripts/01_cleaning/02-economic-script.R")
source("03_scripts/01_cleaning/03-Cost_data_frame.R")
source("03_scripts/01_cleaning/04-Cost_Calc.R")
source("03_scripts/02_standardOutputs/Section 3 - EconCostingCharts.R")
source("03_scripts/ProjectExport.R")
