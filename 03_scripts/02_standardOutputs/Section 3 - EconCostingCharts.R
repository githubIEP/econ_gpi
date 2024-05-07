##### ----- GPI ECON COSTING STANDARD CHARTS AND TABLES
#' This script is for producing all the standard GPI Econ Costing Charts and Tables
##### -----


### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)
library(iepg)
library(dplyr)
library(scales)
library(cowplot)
library(grid)
library(iepsqlite)
library(patchwork)
library(kableExtra)
library(rvest)
library(ggrepel)

### --- List of Standard Charts and Tables

# Composition of the global economic impact of violence, 2023
CHART_CompPie = c(title = "Composition of the global economic impact of violence, 2023",
                  sheet = "CompPie", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")

# Change in the global economic impact of violence, billions of PPP 2022 US dollars, 2022–2023

TABLE_ImpactChange = c(title = "Change in the global economic impact of violence, billions of PPP 2023 US dollars, 2022–2023",
                       sheet = "ImpactChange", source = "IEP Calculations", xtext = "", ytext = "",
                       type = "Table", position = "Normal")


# Trend in the global economic impact of violence, 2008–2023

CHART_Trend_YOYTrend = c(title = "Trend in the global economic impact of violence, 2008–2023",
                         sheet = "TrendYOY", source = "IEP Calculations", xtext = "", ytext = "",
                         type = "Chart", position = "Normal")

# Change in the global economic impact of violence, billions of PPP 2023 US dollars, 2008–2023

TABLE_ImpactChangeTrend = c(title = "Change in the global economic impact of violence, billions of PPP 2023 US dollars, 2008–2023",
                            sheet = "ImpactChangeTrend", source = "IEP Calculations", xtext = "", ytext = "",
                            type = "Table", position = "Normal")

# Indexed trend in the economic impact by domain
CHART_DomainTrend = c(title = "Trend in Domains (Indexed)",
                      sheet = "Domain", source = "IEP Calculations", xtext = "", ytext = "INDEXED CHANGE (2008=1)",
                      type = "Chart", position = "Normal")

# Breakdown of the global economic impact of the Armed Conflict domain, 2023
CHART_ArmedViolence = c(title = "Breakdown of the global economic impact of the Armed Conflict domain, 2023",
                        sheet = "ArmedViolence", source = "IEP Calculations", xtext = "", ytext = "",
                        type = "Chart", position = "Normal")

# Composition of the economic impact of Interpersonal Violence and Self-inflicted Violence domain, 2023
CHART_InterpersonalViolence = c(title = "Composition of the economic impact of Interpersonal Violence and Self-inflicted Violence domain, 2023",
                                sheet = "Interpersonal", source = "IEP Calculations", xtext = "", ytext = "",
                                type = "Chart", position = "Normal")

# Composition of the Violence Containment domain, 2023
CHART_ViolenceContainment = c(title = "Composition of the Violence Containment domain, 2023",
                              sheet = "ViolenceContainment", source = "IEP Calculations", xtext = "", ytext = "",
                              type = "Chart", position = "Normal")

# Per capita containment spending (military and internal security) by region, 2023 
CHART_PerCap = c(title = "Per capita containment spending (military and internal security) by region, 2023",
                 sheet = "ViolenceContainment", source = "IEP Calculations", xtext = "CONSTANT 2023 PPP, PER PERSON", ytext = "",
                 type = "Chart", position = "Normal")

# Combined Table of Military expenditure , 2023

TABLE_combined = c(title = "Combined Table",
                   sheet = "MEx", source = "IEP Calculations", xtext = "", ytext = "",
                   type = "Table", position = "Normal")


# Percentage Change in Economic Impact by Region (2021 to 2022)
CHART_EconImpactChange = c(title = "Percentage Change in Economic Impact by Region (2021 to 2023)",
                           sheet = "EconImpactChange", source = "IEP Calculations", xtext = "PERCENTAGE CHANGE", ytext = "",
                           type = "Chart", position = "Normal")


# The ten countries with the highest economic cost of violence, percentage of GDP
TABLE_TenCountries = c(title = "The Ten Countries with the Highest Economic Cost of Violence, Percentage of GDP",
                       sheet = "TenCountries", source = "IEP Calculations", xtext = "", ytext = "",
                       type = "Table", position = "Normal")

# Composition of the regional economic cost of violence, 2022

CHART_Composition = c(title = "Composition of the regional economic cost of violence, 2023",
                      sheet = "Composition", source = "IEP Calculations", xtext = "", ytext = "PROPORTION OF REGIONAL ECONOMIC IMPACT OF VIOLENCE",
                      type = "Chart", position = "Normal")

# Appendix table D.1

TABLE_Appendix = c(title = "Economic Cost of Violence",
                   sheet = "Appendix D.1", source = "IEP Calculations", xtext = "", ytext = "",
                   type = "Table", position = "Normal")



### --- Loading Data

econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")



## -- CHART_CompPie -----------------------------------------------------------
# A pie chart showing the composition of the global economic impact of violence

CHART_CompPie.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(indicator2 = if_else(row_number() > 6, "Other", indicator2)) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5),
    hjust = if_else(prop > 0.2, 0.5, 1)
  )


pCHART_CompPie <- ggplot(CHART_CompPie.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.5, y = labelPosition, label = label),
    hjust = 1, size = 3,
    direction = "both",  
    force = 0.5           
  ) +  
  geom_segment(data = CHART_CompPie.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none")




pCHART_CompPie <- f_ThemeTraining(plot = pCHART_CompPie, 
                                  chart_info = CHART_CompPie, 
                                  plottitle = "", 
                                  xaxis = "", 
                                  yaxis = "", 
                                  xgridline = "", 
                                  ygridline = "") +
  theme_void() +  
  theme(legend.position = "none")



pCHART_CompPie

## -- TABLE_ImpactChange -----------------------------------------------------------
# A table showing the change in the global 
# economic impact of violence, billions of PPP 2022 US dollars, 2021–2022

TABLE_ImpactChange.df <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="costppp") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  replace(is.na(.), 0) %>%
  group_by(indicator2, year) %>%
  summarise(direct = sum(direct)/1000000000, direct = round(direct, 0),
            indirect = sum(indirect)/1000000000, indirect = round(indirect, 0)) %>%
  mutate(multiplier = direct, total_impact = sum(direct, indirect, multiplier)) %>%
  select(-year)


TABLE_ImpactChange_2022 <- econ_impact.df %>%
  dplyr::filter(year==max(year-1), subtype=="costppp") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  replace(is.na(.), 0) %>%
  group_by(indicator2, year) %>%
  summarise(direct = sum(direct)/1000000000, direct = round(direct, 0),
            indirect = sum(indirect)/1000000000, indirect = round(indirect, 0)) %>%
  mutate(multiplier = direct, total_impact_2022 = sum(direct, indirect, multiplier)) %>%
  select(indicator2, total_impact_2022)

TABLE_ImpactChange.df <- TABLE_ImpactChange.df %>%
  left_join(TABLE_ImpactChange_2022, by = "indicator2") %>%
  mutate(total_change = total_impact-total_impact_2022, `PERCENTAGE CHANGE` = total_change/total_impact_2022*100, 
         `PERCENTAGE CHANGE` = round(`PERCENTAGE CHANGE`, 1)) %>%
  arrange(desc(total_impact)) %>%
  rename(Indicator = indicator2)

## -- CHART_Trend -----------------------------------------------------------
# A line chart showing the trend in the global economic impact of violence

CHART_Trend.df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

pCHART_Trend <- ggplot(data = CHART_Trend.df, aes(x = year, y = value/10^12)) +
  geom_line (size = 0.75, color = 'red') +
  # scale_x_continuous (breaks = c(2008:2023)) +
  scale_x_continuous(breaks = seq(min(CHART_Trend.df$year), max(CHART_Trend.df$year))) +
  labs(y = "Total Cost (Constant 2022 US$ PPP, trillions)")


pCHART_Trend <- f_ThemeTraining(plot = pCHART_Trend, 
                                chart_info = CHART_Trend_YOYTrend, 
                                plottitle = "", 
                                xaxis = "Include", 
                                yaxis = "Include", 
                                xgridline = "", 
                                ygridline = "Include",
                                include_source = FALSE)

## -- CHART_YOYTrend -----------------------------------------------------------
# A bar chart showing the YOY trend in the global economic impact of violence

CHART_Trend_YOYTrend.df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(perc_change = (value - dplyr::lag(value)) / dplyr::lag(value))

# Create chart

pCHART_YOYTrend <- CHART_Trend_YOYTrend.df %>%
  mutate(colour_group = case_when(
    perc_change == 0 ~ "Grey",
    perc_change > 0 ~ "Red",
    perc_change < 0 ~ "Green",
    TRUE ~ "Grey"
  )) %>%
  ggplot(aes(x = year, y = perc_change)) +
  geom_bar(stat = "identity", aes(fill = colour_group)) +
  scale_fill_manual(values = c("Red" = "red", "Green" = "#53C1AB", "Grey" = "darkgrey"))  +
  scale_y_continuous(labels = scales::percent, limits = c(-0.1, 0.1))+
  scale_x_continuous(breaks = seq(min(CHART_Trend.df$year), max(CHART_Trend.df$year)))

pCHART_Trend_YOYTrend <- pCHART_Trend / pCHART_YOYTrend

# Add theme

pCHART_Trend_YOYTrend <- f_ThemeTraining(
  pCHART_Trend_YOYTrend, 
  chart_info = CHART_Trend_YOYTrend,
  plottitle = "",
  xaxis = "",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "none")





## -- TABLE_ImpactChangeTrend -----------------------------------------------------------
# A table showing the change in the global 
# economic impact of violence, billions of PPP 2022 US dollars, 2008–2022

TABLE_ImpactChangeTrend.df <- econ_impact.df %>%
  dplyr::filter(year %in% c (min(year), max(year)), subtype=="impact") %>%
  group_by(indicator2, year) %>%
  summarise(value = sum(value)/1000000000, value = round(value, 1)) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(BILLIONS = `2023`-`2008`, `PERCENTAGE CHANGE` = BILLIONS/`2008`*100, 
         `PERCENTAGE CHANGE` = round(`PERCENTAGE CHANGE`, 0)) %>%
  arrange(desc(`PERCENTAGE CHANGE`)) %>%
  rename(Indicator = indicator2)

## -- CHART_DomainTrend -----------------------------------------------------------
# A line chart showing the indexed trend in the economic impact by domain

CHART_DomainTrend.df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  select(year, indicator2, value) %>%
  group_by(indicator2, year) %>%
  summarise(value = sum(value)) %>%
  group_by(year) %>%
  summarise(
    VC = sum(value[indicator2 %in% c("Military expenditure", "Internal security expenditure", "Peacebuilding", "Peacekeeping", "Private security")]),
    AC = sum(value[indicator2 %in% c("Conflict deaths", "GDP losses", "Refugees and IDPs", "Small arms", "Terrorism")]),
    ISV = sum(value[indicator2 %in% c("Fear", "Homicide", "Incarceration", "Suicide", "Violent crime")]),
  ) %>%
  mutate(VC1 = VC / first(VC),
         AC1 = AC / first(AC),
         ISV1 = ISV / first(ISV)) %>%
  select(-VC, -AC, -ISV)

pCHART_DomainTrend <- ggplot(CHART_DomainTrend.df, aes(x = year)) +
  geom_line(aes(y = VC1), color = "green", size = 1) +
  geom_line(aes(y = AC1), color = "red", size = 1) +
  geom_line(aes(y = ISV1), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0.80, 3), breaks = seq(0.80, 3, by = 0.20)) + 
  scale_x_continuous(breaks = c(2008:2023)) +
  theme_minimal() +
  annotate("text", x = max(CHART_DomainTrend.df$year), y = CHART_DomainTrend.df$VC1[nrow(CHART_DomainTrend.df)], label = "Violence Containment", vjust = -0.6, hjust = 1.5, color = "green") +
  annotate("text", x = max(CHART_DomainTrend.df$year), y = CHART_DomainTrend.df$AC1[nrow(CHART_DomainTrend.df)], label = "Armed Conflict", vjust = 10, hjust = 1, color = "red") +
  annotate("text", x = max(CHART_DomainTrend.df$year), y = CHART_DomainTrend.df$ISV1[nrow(CHART_DomainTrend.df)], label = "Interpersonal and Self-Inflicted Violence", vjust = 2, hjust = 1, color = "blue")

pCHART_DomainTrend <- f_ThemeTraining(plot = pCHART_DomainTrend, 
                                      chart_info = CHART_DomainTrend, 
                                      plottitle = "", 
                                      xaxis = "Include", 
                                      yaxis = "Include", 
                                      xgridline = "", 
                                      ygridline = "")


# Add peace labels
pCHART_DomainTrend <- f_PeaceLabels(pCHART_DomainTrend, 
                                    xaxis = "",
                                    yaxis = "Include",
                                    left_text = "",
                                    right_text = "",
                                    up_text = "Deterioration",
                                    down_text = "Improvement",
                                    yposition = 0.02) 

## -- CHART_ArmedViolence -----------------------------------------------------------
# A pie chart breakdown of the global economic impact of the Armed Conflict domain

CHART_ArmedViolence.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact",
                indicator2 %in% c("Terrorism", "Refugees and IDPs", "Small arms", "GDP losses", "Conflict deaths")) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5),  
    hjust = if_else(prop > 0.2, 0.5, 1)
  )


pCHART_ArmedViolence <- ggplot(CHART_ArmedViolence.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.5, y = labelPosition, label = label),
    hjust = 1, size = 3,
    direction = "both",  
    force = 0.5           
  ) +  
  geom_segment(data = CHART_ArmedViolence.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")




pCHART_ArmedViolence <- f_ThemeTraining(plot = pCHART_ArmedViolence, 
                                        chart_info = CHART_ArmedViolence, 
                                        plottitle = "", 
                                        xaxis = "", 
                                        yaxis = "", 
                                        xgridline = "", 
                                        ygridline = "") +
  theme_void() +  
  theme(legend.position = "none")

print(pCHART_ArmedViolence)

## -- CHART_InterpersonalViolence -----------------------------------------------------------
# A pie chart breakdown of the global economic impact of the Interpersonal and Self-Inflicted Violence domain

CHART_InterpersonalViolence.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact",
                indicator2 %in% c("Homicide",
                                  "Violent crime",
                                  "Incarceration",
                                  "Fear",                                  
                                  "Suicide")) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5), 
    hjust = if_else(prop > 0.2, 0.5, 1))

pCHART_InterpersonalViolence <- ggplot(CHART_InterpersonalViolence.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_InterpersonalViolence.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_InterpersonalViolence.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Interpersonal and Self-Inflicted Violence")

pCHART_InterpersonalViolence <- f_ThemeTraining(plot = pCHART_InterpersonalViolence, 
                                                chart_info = CHART_InterpersonalViolence, 
                                                plottitle = "", 
                                                xaxis = "", 
                                                yaxis = "", 
                                                xgridline = "", 
                                                ygridline = "") +
  theme_void() +  
  theme(legend.position = "none")

pCHART_InterpersonalViolence
## -- CHART_ViolenceContainment -----------------------------------------------------------
# A pie chart breakdown of the global economic impact of the Violence Containment domain
CHART_ViolenceContainment.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact",
                indicator2 %in% c("Military expenditure",
                                  "Internal security expenditure",
                                  "Private security",
                                  "Peacebuilding",                                  
                                  "Peacekeeping")) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    prop = (value / sum(value)),
    prop = round(prop, 2), 
    ymax = cumsum(prop),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(indicator2, "\n", prop*100, "%"),
    x_pos = if_else(prop > 0.2, 3.5, 4.5),  
    hjust = if_else(prop > 0.2, 0.5, 1)
  )

pCHART_ViolenceContainment <- ggplot(CHART_ViolenceContainment.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text_repel(
    aes(x = 4.5, y = labelPosition, label = label),
    hjust = 1, size = 3,
    direction = "both",  
    force = 0.5           
  ) +  
  geom_segment(data = CHART_ViolenceContainment.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 5.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Violence Containment")




pCHART_ViolenceContainment <- f_ThemeTraining(plot = pCHART_ViolenceContainment, 
                                              chart_info = CHART_ViolenceContainment, 
                                              plottitle = "", 
                                              xaxis = "", 
                                              yaxis = "", 
                                              xgridline = "", 
                                              ygridline = "") +
  theme_void() +  
  theme(legend.position = "none")

pCHART_ViolenceContainment

## -- CHART_PerCap -----------------------------------------------------------
# Bar chart showing the per capita containment spending by region

CHART_PerCap.df <- econ_impact.df %>%
  rename(ID_0 = iso3c) %>%
  add_region() %>%
  dplyr::filter(year == max(year), subtype == "costppp",
                indicator2 %in% c("Military expenditure", "Internal security expenditure",
                                  "Private security", "Peacekeeping", "Peacebuilding")) %>%
  select(ID_0, region, indicator2, value) %>%
  replace_na(list(iso3c = "KSV", region = "Europe")) %>%
  group_by(region) %>%
  summarise(value = sum(value))

tab_Pop <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "pop", 
                indicator=="milex") %>%
  select(iso3c, value) %>%
  rename(population = value, ID_0=iso3c) %>%
  add_region() %>%
  replace_na(list(iso3c = "KSV", region = "Europe")) %>%
  group_by(region) %>%
  summarise(population = sum(population))

CHART_PerCap.df <- CHART_PerCap.df %>%
  left_join(tab_Pop) %>%
  group_by(region) %>%
  mutate(per_cap = value/population) %>%
  arrange(desc(per_cap))

pCHART_PerCap <- ggplot(CHART_PerCap.df, aes(x = per_cap, y = reorder(region, -per_cap), fill = region)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rep("navy", 9), guide = FALSE) + 
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme(panel.grid.major.x = element_line(color = "grey", linetype = "solid"),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face = "bold"))                  

pCHART_PerCap <- f_ThemeTraining(plot = pCHART_PerCap, 
                                 chart_info = CHART_PerCap, 
                                 plottitle = "", 
                                 xaxis = "", 
                                 yaxis = "Include", 
                                 xgridline = "Include", 
                                 ygridline = "") +
  theme(legend.position = "none")

## -- TABLE_MEx -----------------------------------------------------------
# Three tables showing the ten countries with the highest total military expenditure,
# military expenditure per capita and military expenditure as a % of GDP

TABLE_MEx.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype=="costppp") %>%
  select(country, value) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  mutate(value=value/1000000000, value=round(value, 2))  %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (TOTAL, $US BILLIONS)" = value)

TABLE_PCapMEx.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype %in% c("costppp", "pop")) %>%
  pivot_wider(names_from = subtype, values_from = value) %>%
  mutate(value=costppp/pop, value=round(value, 2)) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  select(country, value)  %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (PER CAPITA, $US)" = value)

TABLE_GDPMEx.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), indicator=="milex", subtype %in% c("costppp", "gdpconsppp")) %>%
  pivot_wider(names_from = subtype, values_from = value) %>%
  mutate(value=costppp/gdpconsppp*100, value=round(value, 2)) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  select(country, value) %>%
  rename("COUNTRY" = country, "MILITARY EXPENDITURE (% OF GDP)" = value)

table1 <- kable(TABLE_MEx.df, "html") %>%
  kable_styling(full_width = FALSE)

table2 <- kable(TABLE_PCapMEx.df, "html") %>%
  kable_styling(full_width = FALSE)

table3 <- kable(TABLE_GDPMEx.df, "html") %>%
  kable_styling(full_width = FALSE)


writeLines(table1, "04_outputs/table1.html")
writeLines(table2, "04_outputs/table2.html")
writeLines(table3, "04_outputs/table3.html")


url1 <- "04_outputs/table1.html"
url2 <- "04_outputs/table2.html"
url3 <- "04_outputs/table3.html"

html1 <- read_html(url1)
html2 <- read_html(url2)
html3 <- read_html(url3)

tables1 <- html_table(html1, fill = TRUE)
tables2 <- html_table(html2, fill = TRUE)
tables3 <- html_table(html3, fill = TRUE)


str(tables1)
str(tables2)
str(tables3)

nrow(tables1)
ncol(tables1)


TABLE_combined.df <- bind_rows(tables1, tables2, tables3)
TABLE_combined.df <- bind_cols(tables1, tables2, tables3)



## -- CHART_EconImpact -----------------------------------------------------------
# A bar chart showing total economic impact by region

CHART_EconImpact.df <- econ_impact.df %>%
  rename(ID_0 = iso3c) %>%
  add_region() %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  replace_na(list(iso3c = "KSV", region = "Europe")) %>%
  group_by(region) %>%
  summarise(total_impact = sum(value)/1000000000) %>%
  arrange(desc(total_impact))

pCHART_EconImpact <- ggplot(CHART_EconImpact.df, aes(x = total_impact, y = reorder(region, total_impact), fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar(total_impact)), hjust = -0.2, size = 3) +
  scale_fill_manual(values = rep("maroon", 9), guide = FALSE) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ","))                   

pCHART_EconImpact <- f_ThemeTraining(plot = pCHART_EconImpact, 
                                     chart_info = CHART_EconImpactChange, 
                                     plottitle = "", 
                                     xaxis = "", 
                                     yaxis = "Include", 
                                     xgridline = "Include", 
                                     ygridline = "") +
  theme(legend.position = "none")

## -- CHART_EconImpactChange -----------------------------------------------------------
# A bar chart showing percentage change in total economic impact by region, 2021-2022

CHART_EconImpactChange.df <- econ_impact.df %>%
  rename(ID_0 = iso3c) %>%
  add_region() %>%
  dplyr::filter(year %in% c(max(year), max(year-1)), subtype == "impact") %>%
  replace_na(list(iso3c = "KSV", region = "Europe")) %>%
  group_by(region, year) %>%
  summarise(total_impact = sum(value)/1000000000) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = total_impact) %>%
  group_by(region) %>%
  mutate(perc_change = sum(`2023`-`2022`)/`2022`) %>%
  arrange(desc(`2023`))

pCHART_EconImpactChange <- ggplot(CHART_EconImpactChange.df, aes(x = perc_change, y = reorder(region, `2023`), fill = perc_change >= 0)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("green", "red"), guide = FALSE) +
  geom_text(aes(label = scales::percent(perc_change, accuracy=1)), hjust = -0.2, size = 3) +
  scale_x_continuous(labels = scales::percent_format(prefix = "", big.mark = ","))


pCHART_EconImpactChange <- pCHART_EconImpact + pCHART_EconImpactChange

pCHART_EconImpactChange <- f_ThemeTraining(plot = pCHART_EconImpactChange, 
                                           chart_info = CHART_EconImpactChange, 
                                           plottitle = "", 
                                           xaxis = "", 
                                           yaxis = "Include", 
                                           xgridline = "Include",
                                           include_source = FALSE,
                                           ygridline = "") +
  theme(legend.position = "none")

## -- TABLE_TenCountries -----------------------------------------------------------
# A table of the ten countries with the highest economic cost of violence as a % of GDP

# Transform Data
TABLE_TenCountries.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype=="costppp") %>%
  group_by(country, type) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  mutate(econ_cost = sum(indirect, direct))

tab_GDP <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="gdpconsppp") %>%
  select(country, year, value) %>%
  distinct(country, year, .keep_all = TRUE)

TABLE_TenCountries.df <- TABLE_TenCountries.df %>%
  left_join(tab_GDP) %>%
  group_by(country) %>%
  summarise(perc_gdp = econ_cost/value) %>%
  arrange(desc(perc_gdp)) %>%
  slice(1:10) %>% 
  add_row(country = 'Average', !!! colMeans(.[-1])) %>%
  mutate(perc_gdp = scales::percent(perc_gdp)) %>%
  rename("ECONOMIC COST OF VIOLENCE AS (% OF GDP)" = perc_gdp, "Country" = country)

## -- CHART_Composition  -----------------------------------------------------------
# A stacked bar chart showing the composition of the regional economic cost of violence

CHART_Composition.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  rename(ID_0=iso3c) %>%
  add_region() %>%
  replace_na(list(iso3c = "KSV", region = "Europe")) %>%
  mutate(domain = case_when(indicator2 == "Military expenditure" ~ "Military",
                            indicator2 %in% c("Internal security expenditure", "Private security") ~ "Internal and Private Security",
                            indicator2 %in% c("Violent crime", "Homicide", "Suicide") ~  "Violent Crime, Homicide and Suicide",
                            indicator2 %in% c("Small arms", "Terrorism", "Conflict deaths", "GDP losses", "Refugees and IDPs") ~  "Armed Conflict",
                            TRUE ~ "Other")) %>%
  group_by(region, domain) %>%
  summarise(value = sum(value))  %>%
  group_by(region) %>%
  mutate(total = sum(value)) %>%
  mutate(prop = value / total) %>%
  mutate(domain = fct_relevel(domain, "Other", "Armed Conflict", "Violent Crime, Homicide and Suicide", "Internal and Private Security", "Military"))

# Determine the sorting order by Military proportion
military_order <- CHART_Composition.df %>%
  dplyr::filter(domain == "Military") %>%
  arrange(desc(prop)) %>%
  pull(region)

# Reorder the region factor based on the Military proportion
CHART_Composition.df$region <- factor(CHART_Composition.df$region, levels = military_order)

# Plotting
pCHART_Composition <- ggplot(CHART_Composition.df, aes(x = region, y = prop, fill = domain)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = c("Military" = "#00847F",
                               "Internal and Private Security" = "#55C1AA",
                               "Violent Crime, Homicide and Suicide" = "#FAAB6C",
                               "Armed Conflict" = "red",
                               "Other" = "grey")) +
  scale_y_continuous(labels = scales::percent_format())

pCHART_Composition <- f_ThemeTraining(plot = pCHART_Composition, 
                                      chart_info = CHART_Composition, 
                                      plottitle = "", 
                                      xaxis = "", 
                                      yaxis = "Include", 
                                      xgridline = "Include", 
                                      ygridline = "")  +
  theme(legend.direction = "horizontal", legend.position = "top", axis.text.x = element_text(face = "bold")) +
  guides(fill = guide_legend(title = NULL))

## -- TABLE_Appendix D.1 -----------------------------------------------------------

TABLE_Appendix.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype=="costppp") %>%
  group_by(country) %>%
  summarise(value = sum(value)) %>% rename(costppp = value)


tab_GDP <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="gdpconsppp") %>%
  select(country, year, value) %>%
  distinct(country, year, .keep_all = TRUE)

TABLE_Appendix.df <- TABLE_Appendix.df %>%
  left_join(tab_GDP) %>%
  group_by(country) %>%
  summarise(perc_gdp = costppp/value, costppp) %>%
  arrange(desc(perc_gdp)) %>%
  mutate(perc_gdp = scales::percent(perc_gdp))

tab_imp <- econ_impact.df %>%
  dplyr::filter(year==max(year), subtype=="impact") %>%
  select(country, year, value) %>%
  group_by(country, year) %>%
  summarise(value = sum(value))


TABLE_Appendix.df <- TABLE_Appendix.df %>%
  left_join(tab_imp) %>% 
  rename(impact = value)


tab_per_cap <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype=="pop") %>%
  select(country, year, value) %>%
  group_by(country, year) %>%
  distinct(country, year, .keep_all = TRUE) %>%
  rename(pop = value) %>%
  left_join(tab_imp) %>%
  rename(impact = value) %>%
  mutate(per_capita_impact = (impact/pop)) %>%
  dplyr::select(c(`country`, `year`, `per_capita_impact`))

TABLE_Appendix.df <- TABLE_Appendix.df %>%
  left_join(tab_per_cap) %>%
  dplyr::select(-c(`year`))

TABLE_Appendix.df$costppp <- format(TABLE_Appendix.df$costppp, big.mark = ",")
TABLE_Appendix.df$impact <- format(TABLE_Appendix.df$impact , big.mark = ",")
TABLE_Appendix.df$per_capita_impact <- format(TABLE_Appendix.df$per_capita_impact , big.mark = ",")

TABLE_Appendix.df <- TABLE_Appendix.df  %>%
  mutate(Rank = row_number())

TABLE_Appendix.df <- TABLE_Appendix.df %>%
  rename(`Economic Cost of Violence as % of GDP, RANK` = Rank ) %>%
  rename(`Country` = country) %>%
  rename(`Economic Impact of Violence (US$ 2023 PPP)` = impact) %>%
  rename(`Per Capita Impact (2023 US$ PPP` = per_capita_impact) %>%
  rename(`Economic Cost of Violence as a percentage of GDP` = perc_gdp) %>%
  rename(`Economic Cost of Violence (US$ 2023 PPP` = costppp)

