##### ----- EXPORT CHARTS, MAPS AND TABLES
#' Note that for the export to work, the standardoutputs and analysis scripts have to 
#' be run first, in order to create the chart information, plot, and df objects

### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)



### --- Section 3 --------------------------------------------------------------
#' Section 3 Econ Costing Charts

# Create Workbook
wbCHARTS_SECTION3 <- createWorkbook()

# List
SECTION3_EXPORT <- c(
  "CHART_CompPie","TABLE_ImpactChange", "CHART_Trend_YOYTrend",
  "TABLE_ImpactChangeTrend","CHART_DomainTrend","CHART_ArmedViolence","CHART_InterpersonalViolence",
  "CHART_ViolenceContainment","CHART_PerCap", "TABLE_combined",
  "CHART_EconImpactChange", "TABLE_TenCountries", "CHART_Composition", "TABLE_Appendix")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("3", wbCHARTS_SECTION3, CHARTBOOK_3, SECTION3_EXPORT)


