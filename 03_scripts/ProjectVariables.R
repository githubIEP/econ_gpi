#### Project Variables #####

#### General Variables ####

REPORT_YEAR = 2024
LATEST_YEAR = 2023
FIRST_GPI_YEAR = 2008
PANEL_SSD = 15 # Update this number every year by one
PANEL_FREQ = 16 # Update this number every year by one
PANEL_PSE = 10 # Update this number every year by one
SMALL_ARMS_YEAR = 3 # Update this number every year by one
PEACEBUILDING_YEAR = 3 # Update this number every year by one


### Read Files and data ###
GPI_DATA = readRDS("02_data/processed/GPI_2024_EconomicCosting.rds")
UNIT_COST = read_excel("02_data/processed/unit costs for gpi2022.xlsx", sheet = "unit costs r")
WHITE_HOUSE = "https://www.whitehouse.gov/wp-content/uploads/2022/03/hist05z1_fy2023.xlsx" 
WHITE_HOUSE_INTEREST = "https://www.whitehouse.gov/wp-content/uploads/2022/03/hist06z1_fy2023.xlsx"
UNHCR = read_csv("02_data/processed/Refugees and IDP costs unhcr 2020.csv")
UNHCR_2021 = read_excel("02_data/processed/Global Funding Overview 31 December 2021-converted.xlsx", sheet = "data", col_types = c("text", "numeric"))
VIOLENT_ASSAULT = read.csv("02_data/processed/assault_data.csv")
SEXUAL_ASSAULT = read_csv("02_data/processed/sexual_violence.csv")
SMALL_ARMS = read_excel("02_data/processed/Small arms survey data 2020.xlsx", sheet = "clean")
SECU_AGENCY = read.csv("02_data/processed/security agency costs 2018gpi.csv", stringsAsFactors = FALSE)
PEACEBUILDING = read.csv("02_data/processed/peacebuilding 2022.csv")
PEACEBUILDING_FUNDING = read_excel("02_data/processed/Peacebuilding Fund - Projects by Country.xlsx")
PRIVATE_SECURITY = read_csv("02_data/processed/private security numbers updated 2021.csv")
INTERNAL_SECURITY = read_excel("02_data/processed/expenditure on public order and safety IMF.xlsx")
GPI_DASHBOARD = rio::import("02_data/processed/GPI_2024_FINAL.xlsx")
SIPRI_MILEX = read_excel("02_data/processed/SIPRI-Milex-data-1949-2022.xlsx", sheet = "Share of GDP", skip = 5)





IEP_USERPATH = "C:\\Users\\KaranMenon\\OneDrive - Institute for Economics and Peace\\Institute for Economics and Peace"



#### Charts
CHART_UNIT = "cm"

ONEDRIVE = paste0(IEP_USERPATH,"/Global Peace Index/",REPORT_YEAR," GPI")

CHARTBOOK_3 = paste0(ONEDRIVE,"/Layout/Charts/GPI_EconCostingChartbook_",REPORT_YEAR,".xlsx")

CHART_FILES = paste0(ONEDRIVE,"/Layout/Charts")
IMAGE_FILES = paste0(ONEDRIVE,"/Layout/Images")
TABLE_FILES = paste0(ONEDRIVE,"/Layout/Tables")
MAP_FILES = paste0(ONEDRIVE,"/Layout/Maps")

# Chart Sizes
CHARTS <- list(
  small = c(width = 8.45, height = 10),
  medium = c(width = 12, height = 10),
  large = c(width = 17.6, height = 10)
)

# Map Sizes
MAPS <- list(
  small = c(width = 12, height = 8),
  medium = c(width = 14, height = 10),
  large = c(width = 28, height = 14)
)

# Chart Fonts
HEAVY_FONT = "Helvetica LT Pro" 
LIGHT_FONT = "Helvetica LT Pro Light" 


