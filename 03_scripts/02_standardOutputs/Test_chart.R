##### ----- GPI ECON COSTING STANDARD CHARTS AND TABLES
#' This script is for producing all the standard GPI Econ Costing Charts
##### -----


### --- Libraries, Variables, Functions
library(knitr)
library(openxlsx)
library(reshape2)

## -- Spreadsheet to save charts in
wb_SECTION3 <- createWorkbook()

### --- List of Standard Charts and Tables

# Trend in Economic Impact of Violence
CHART_EconImpact = c(title = "Trend in Economic Impact of Violence",
                     sheet = "EconImpact", source = "IEP Calculations", xtext = "year", ytext = "Economic Impact of Violence (Constant 2022 USD)",
                     type = "Chart", position = "Normal")

# Share in the Economic Impact of Violence
CHART_pie = c(title = "Share in the Economic Impact of Violence",
              sheet = "CompositionPie", source = "IEP Calculations", xtext = "", ytext = "",
              type = "Chart", position = "Large")




### --- Loading Data

econ_impact.df <- rio::import("04_outputs/Economic Impact of Violence.xlsx")


# 1. Trend Chart ====================================================================================================================
CHART_EconImpact.df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

p <- ggplot(data = CHART_EconImpact.df, aes(x = year, y = value/10^12)) +
  geom_line (size = 0.75, color = 'red') +
  scale_x_continuous (breaks = c(2008:2022)) +
  labs(y = "Total Cost (Constant 2022 US$ PPP, trillions)")


pCHART_EconImpact <- f_ThemeTraining(plot = p, 
                                       chart_info = CHART_EconImpact, 
                                       plottitle = "Include", 
                                       xaxis = "Include", 
                                       yaxis = "Include", 
                                       xgridline = "", 
                                       ygridline = "Include")

# 2. Composition Pie ======================================================================================================


CHART_pie.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup()

row <- CHART_pie.df %>%
  dplyr::filter(indicator2 %in% c("Terrorism",
                           # "Violent crime",
                           "Peacebuilding",
                           "Refugees and IDPs",
                           "Small arms",
                           "Incarceration",
                           "GDP losse",
                           "Fear",
                           "Peacekeeping"))


new_row <- data.frame(indicator2 = "Other", value = sum(row$value))


CHART_pie.df <- rbind(CHART_pie.df, new_row)


CHART_pie.df <- CHART_pie.df[!CHART_pie.df$indicator2 %in% c("Terrorism", 
                                  # "Violent crime", 
                                  "Peacebuilding",
                                  "Refugees and IDPs",
                                  "Small arms", 
                                  "Incarceration", 
                                  "GDP losses", 
                                  "Fear",
                                  "Peacekeeping"), ]

CHART_pie.df <- CHART_pie.df %>% 
  mutate(all = sum(value)) %>%
  mutate(Prop = value/all)


CHART_pie.df$Prop <- round(CHART_pie.df$Prop, 2)

CHART_pie.df$ymax = cumsum(CHART_pie.df$Prop)

CHART_pie.df$ymin = c(0, head(CHART_pie.df$ymax, n=-1))

CHART_pie.df$labelPosition <- (CHART_pie.df$ymax + CHART_pie.df$ymin) / 2

CHART_pie.df$labelPosition[2] <- 0.03

CHART_pie.df$Prop <- CHART_pie.df$Prop*100

CHART_pie.df$label <- paste0(CHART_pie.df$indicator2, "\n", CHART_pie.df$Prop, "%")


p1 <- ggplot(CHART_pie.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_pie.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_pie.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +  
  theme_void() +
  theme(legend.position = "none")

p1



pCHART_pie <- f_ThemeTraining(plot = p1, 
                                     chart_info = CHART_pie, 
                                     plottitle = "Include", 
                                     xaxis = "", 
                                     yaxis = "", 
                                     xgridline = "", 
                                     ygridline = "")


pCHART_pie 


# 3. Direct & Indirect Table ============================================================================================

table.df <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "costppp")


table.df <- table.df %>%
  dplyr::select(-c(`iso3c`, `year`, `indicator`,`subtype`, `country`, `domain`))


table1 <- table.df %>%
  dplyr::filter(type == "direct")


table2 <- table.df %>%
  dplyr::filter(type == "indirect")

table1 <- table1 %>%
  group_by(indicator2) %>%
  summarise(value = sum(value), type) %>%
  ungroup()

table1 <- table1 %>% distinct()

table2 <- table2 %>%
  group_by(indicator2) %>%
  summarise(value = sum(value), type) %>%
  ungroup()

table2 <- table2 %>% distinct()

new_table <- rbind(table1, table2)


new_table <- new_table %>%
  pivot_wider(names_from = type, values_from = value)

new_table$direct <- ifelse(is.na(new_table$direct), 0, new_table$direct)
new_table$indirect <- ifelse(is.na(new_table$indirect), 0, new_table$indirect)

new_table <- new_table %>%
  mutate(direct = direct/10^9) %>%
  mutate(indirect = indirect/10^9)

new_table$direct <- round(new_table$direct, 2)
new_table$indirect <- round(new_table$indirect, 2)

new_table <- new_table %>%
  mutate(multiplier = direct)

new_table <- new_table %>%
  mutate(impact = (direct + indirect + multiplier))


new_table <- rbind(new_table, c("total", sum(new_table$direct), sum(new_table$indirect), sum(new_table$multiplier), sum(new_table$impact)))

new_table$direct <- format(new_table$direct, big.mark = ",")

new_table$indirect <- format(new_table$indirect, big.mark = ",")

new_table$multiplier <- format(new_table$multiplier, big.mark = ",")

new_table$impact <- format(new_table$impact, big.mark = ",")



# 4. domain trends ======================================================================================================

domain <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  dplyr::select(c(`year`, `indicator2`, `value`))

tmp <- domain %>%
  group_by(indicator2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

tmp <- tmp %>%
  group_by(year) %>%
  summarise(VC = sum(value[indicator2 %in% c("Military expenditure", "Internal security expenditure", "Peacebuilding", "Peacekeeping", "Private security")]),
            AC = sum(value[indicator2 %in% c("Conflict deaths", "GDP losses", "Refugees and IDPs", "Small arms", "Terrorism")]),
            ISV = sum(value[indicator2 %in% c("Fear", "Homicide", "Incarceration", "Suicide", "Violent crime")]))

df <- tmp %>%
  mutate(VC1 = VC) %>%
  mutate(AC1 = AC)%>%
  mutate(ISV1 = ISV)


column_to_divide <- "VC1"
col_to_divide <- "AC1"
col_to_divide1 <- "ISV1"

df[[column_to_divide]] <- df[[column_to_divide]] / df[[column_to_divide]][1]
df[[col_to_divide]] <- df[[col_to_divide]] / df[[col_to_divide]][1]
df[[col_to_divide1]] <- df[[col_to_divide1]] / df[[col_to_divide1]][1]


df <- df %>%
dplyr::select(-c(`VC`, `AC`, `ISV`))

CHART_domain.df <- df


p2 <- ggplot(CHART_domain.df, aes(x = year)) +
  geom_line(aes(y = VC1), color = "green", size = 1) +
  geom_line(aes(y = AC1), color = "red", size = 1) +
  geom_line(aes(y = ISV1), color = "blue", size = 1) +
  scale_y_continuous(
    limits = c(0.70, 2.90),  
    breaks = seq(0.70, 2.90, by = 0.20)  
  ) + 
  scale_x_continuous(breaks = c(2008:2022)) +
  labs(title = "Domain Indexed Trend", x = "", y = "Improvement      CHANGE IN ECONOMIC IMPACT (2008 = 1)         Deterioration") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(
      face = "italic", 
      color = "black", 
      size = 12, 
      family = "Times"  
    )
  )


p2 <- p2 +
  annotate("text", x = max(df$year), y = df$VC1[nrow(df)], label = "Violence Containment", vjust = -0.6, hjust = 1.5, color = "green") +
  annotate("text", x = max(df$year), y = df$AC1[nrow(df)], label = "Armed Conflict", vjust = 10, hjust = 1, color = "red") +
  annotate("text", x = max(df$year), y = df$ISV1[nrow(df)], label = "Interpersonal and Self-Inflicted Violence", vjust = 2, hjust = 1, color = "blue")

p2




CHART_domain = c(title = "Trend in Domains (Indexed)",
                     sheet = "Domain", source = "IEP Calculations", xtext = "year", ytext = "Change in Economic Impact (2008 = 1)",
                     type = "Chart", position = "Normal")


pCHART_domain <- f_ThemeTraining(plot = p2, 
                              chart_info = CHART_domain, 
                              plottitle = "Include", 
                              xaxis = "Include", 
                              yaxis = "Include", 
                              xgridline = "", 
                              ygridline = "")


pCHART_domain 



# 5. Armed Conflict Composition Pie ====================================================================================

armed_pie <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup()

arm <- armed_pie %>%
  dplyr::filter(indicator2 %in% c("Terrorism",
                                  "Refugees and IDPs",
                                  "Small arms",
                                  "GDP losses",                                  
                                  "Conflict deaths"))
arm <- arm %>%
  mutate(all = sum(value)) %>%
  mutate(prop = value/all)


arm$prop <- round(arm$prop, 2)
arm$ymax = cumsum(arm$prop)
arm$ymin = c(0, head(arm$ymax, n=-1))
arm$labelPosition <- (arm$ymax + arm$ymin) / 2
arm$prop <- arm$prop*100
arm$label <- paste0(arm$indicator2, "\n", arm$prop, "%")
arm$labelPosition[4] <- 1


CHART_pie_arm.df <- arm


p3 <- ggplot(CHART_pie_arm.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_pie_arm.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_pie_arm.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")

p3



p4 = ggplot(CHART_pie_arm.df, aes(x = "", prop, fill = indicator2, label = label)) + geom_col() +
  coord_polar(theta = "y") + geom_text(position = position_stack(vjust = 0.5)) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "", caption = "Source: IEP") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")


p4


CHART_pie_arm = c(title = "Armed Conflict",
                 sheet = "Armed Conflict", source = "IEP Calculations", xtext = "", ytext = "",
                 type = "Chart", position = "Normal")


pCHART_pie_arm <- f_ThemeTraining(plot = p4, 
                                 chart_info = CHART_pie_arm, 
                                 plottitle = "Include", 
                                 xaxis = "", 
                                 yaxis = "", 
                                 xgridline = "", 
                                 ygridline = "")


pCHART_pie_arm 


# 6. Interpersonal and Self-Inflicted Violence ==========================================================================
int_violence <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup()

int <- int_violence %>%
  dplyr::filter(indicator2 %in% c("Homicide",
                                  "Violent crime",
                                  "incarceration",
                                  "Fear",                                  
                                  "Suicide"))
int <- int %>%
  mutate(all = sum(value)) %>%
  mutate(prop = value/all)


int$prop <- round(int$prop, 2)
int$ymax = cumsum(int$prop)
int$ymin = c(0, head(int$ymax, n=-1))
int$labelPosition <- (int$ymax + int$ymin) / 2
int$prop <- int$prop*100
int$label <- paste0(int$indicator2, "\n", int$prop, "%")
int$labelPosition[4] <- 1


CHART_pie_int.df <- int

p5 <- ggplot(CHART_pie_int.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_pie_int.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_pie_int.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")

p5

p6 = ggplot(CHART_pie_int.df, aes(x = "", prop, fill = indicator2, label = label)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "", caption = "Source: IEP") +
  annotate("text", x = 0, y = 0, 
           label = str_wrap("Interpersonal and Self-Inflicted Violence", width = 20))

p6


CHART_pie_int = c(title = "Interpersonal and Self-inflicted violence",
                  sheet = "Interpersonal", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")


pCHART_pie_int <- f_ThemeTraining(plot = p6, 
                                  chart_info = CHART_pie_int, 
                                  plottitle = "Include", 
                                  xaxis = "", 
                                  yaxis = "", 
                                  xgridline = "", 
                                  ygridline = "")


pCHART_pie_int



# 7. Violence Containment ===================================================================================

vio_contain <- econ_impact.df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup()

vio <- vio_contain %>%
  dplyr::filter(indicator2 %in% c("Military expenditure",
                                  "Internal security expenditure",
                                  "Private security",
                                  "Peacebuilding",                                  
                                  "Peacekeeping"))
vio <- vio %>%
  mutate(all = sum(value)) %>%
  mutate(prop = value/all)


vio$prop <- round(vio$prop, 2)
vio$ymax = cumsum(vio$prop)
vio$ymin = c(0, head(vio$ymax, n=-1))
vio$labelPosition <- (vio$ymax + vio$ymin) / 2
vio$prop <- vio$prop*100
vio$label <- paste0(vio$indicator2, "\n", vio$prop, "%")
vio$labelPosition[4] <- 1

CHART_pie_vio.df <- vio


p7 <- ggplot(CHART_pie_vio.df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = indicator2)) +
  geom_rect() +
  geom_text(data = CHART_pie_vio.df, aes(x = 4.5, y = labelPosition, label = label), hjust = 1, size = 3) +  
  geom_segment(data = CHART_pie_vio.df, aes(x = 4, y = labelPosition, xend = 4.4, yend = labelPosition), color = "black") +  
  scale_fill_brewer(palette = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 4.5)) +  
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", x = 0, y = 0, label = "Armed Conflict")

p7

p8 = ggplot(CHART_pie_vio.df, aes(x = "", prop, fill = indicator2, label = label)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "", caption = "Source: IEP") +
  annotate("text", x = 0, y = 0, 
           label = str_wrap("Violence Containment", width = 20))

p8


CHART_pie_vio = c(title = "Violence Containment",
                  sheet = "Violence", source = "IEP Calculations", xtext = "", ytext = "",
                  type = "Chart", position = "Normal")


pCHART_pie_vio <- f_ThemeTraining(plot = p8, 
                                  chart_info = CHART_pie_vio, 
                                  plottitle = "Include", 
                                  xaxis = "", 
                                  yaxis = "", 
                                  xgridline = "", 
                                  ygridline = "")


pCHART_pie_vio

# 8. Per Capita Violence Containment ======================================================================

region <- Peace_and_region %>%
  dplyr::select(-c(`peace_level`))


df <- left_join(region,econ_impact.df)

pop_data <- pop %>%
  dplyr::filter(year == max(year))

df <- df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  dplyr::select(c(`iso3c`, `region`, `indicator2`, `value`))

df <- df %>%
  dplyr::filter(indicator2 %in% c("Military expenditure", 
                                  "Internal security expenditure",
                                  "Private security",
                                  "Peacekeeping",
                                  "Peacebuilding"))
tmp <- df %>%
  group_by(region) %>%
  summarise(value = sum(value), iso3c)


tmp <- tmp %>% distinct()
tmp <- left_join(pop_data,tmp)
tmp <- tmp %>%
  group_by(region) %>%
  summarise(population = sum(population), value) %>%
  ungroup()

tmp <- tmp %>% distinct()
tmp <- tmp %>%
  mutate(per_cap_imp = value/population)
tmp <- tmp[order(-tmp$per_cap_imp), ]

CHART_per_cap_mil.df <- tmp

p9 <- ggplot(CHART_per_cap_mil.df, aes(x = per_cap_imp, y = reorder(region, -per_cap_imp), fill = region)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rep("navy", 9), guide = FALSE) + 
  labs(title = "Per Capita Economic Impact of Violence Containment by Region", x = "", y = "", caption = "Source: IEP") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme(panel.grid.major.x = element_line(color = "grey", linetype = "solid"),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(face = "bold"))  

p9

# Alt plot with byline

p10 <- ggplot(CHART_per_cap_mil.df, aes(x = per_cap_imp, y = reorder(region, -per_cap_imp), fill = region)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rep("navy", 9), guide = FALSE) + 
  labs(title = "Per Capita Containment spending (military and internal security) by Region, 2022", 
       x = "", y = "") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme(panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),  
        axis.text.y = element_text(face = "bold")) +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, 
           label = "Per Capita violence containment spending is more than 11 times higher in MENA than in sub-Saharan Africa", 
           color = "black", size = 3, fontface = "italic")

p10


CHART_per_cap_mil = c(title = "Per Capita Containment Spending (military and internal security) by region 2022",
                  sheet = "Military per cap", source = "IEP Calculations", xtext = "Constant US$ 2022", ytext = "",
                  type = "Chart", position = "Normal")


pCHART_per_cap_mil <- f_ThemeTraining(plot = p10, 
                                  chart_info = CHART_per_cap_mil, 
                                  plottitle = "Include", 
                                  xaxis = "", 
                                  yaxis = "", 
                                  xgridline = "", 
                                  ygridline = "")


pCHART_per_cap_mil

# 9. Total Economic Impact by region and change ============================================================

region <- Peace_and_region %>%
  dplyr::select(-c(`peace_level`))


df <- left_join(region,econ_impact.df)

pop_data <- pop %>%
  dplyr::filter(year == max(year))

df <- df %>%
  dplyr::filter(year == max(year), subtype == "impact") %>%
  dplyr::select(c(`iso3c`, `region`, `indicator2`, `value`))


tmp <- df %>%
  group_by(region) %>%
  summarise(value = sum(value), iso3c) %>%
  ungroup()

tmp <- left_join(pop_data,tmp)

tmp <- tmp %>% distinct()

tmp <- tmp %>%
  group_by(region) %>%
  summarise(population = sum(population), value) %>%
  ungroup()

tmp <- tmp %>% distinct()

tmp <- tmp %>%
  mutate(impact_per_cap = value/population)

CHART_Total_per_cap.df <- tmp


p11 <- ggplot(CHART_Total_per_cap.df, aes(x = impact_per_cap, y = reorder(region, impact_per_cap), fill = region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar(impact_per_cap)), hjust = -0.2, size = 3) +
  scale_fill_manual(values = rep("maroon", 9), guide = FALSE) + 
  labs(title = "Total Economic Impact by region", 
       x = "CONSTANT 2022 PPP, BILLIONS", y = "") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  theme(panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),  
        axis.text.y = element_text(face = "bold")) 

p11


CHART_Total_per_cap = c(title = "Total per capita Economic Impact by region",
                      sheet = "Total per cap", source = "IEP Calculations", xtext = "Constant US$ 2022", ytext = "",
                      type = "Chart", position = "Normal")


pCHART_Total_per_cap <- f_ThemeTraining(plot = p11, 
                                      chart_info = CHART_Total_per_cap, 
                                      plottitle = "Include", 
                                      xaxis = "", 
                                      yaxis = "", 
                                      xgridline = "", 
                                      ygridline = "")


pCHART_Total_per_cap

# Regional change 

df <- econ_impact.df %>%
  dplyr::filter(subtype == "impact") %>%
  dplyr::filter(year > 2020) %>%
  dplyr::select(-c(`type`, `domain`, `indicator`, `country`))

region <- Peace_and_region %>%
  dplyr::select(-c(`peace_level`))

df <- left_join(region,df)

df <- df %>%
  group_by(year, region) %>%
  summarise(value = sum(value))


tmp <- df %>%
  pivot_wider(names_from = year, values_from = value)

tmp <- tmp %>%
  mutate(change = (`2022` - `2021`))

tmp <- tmp %>%
  mutate(per_change = (`2022` - `2021`)/`2021`)


CHART_percentage_change.df <- tmp

p12 = ggplot(CHART_percentage_change.df, aes(x = per_change, y = region, fill = per_change >= 0)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = c("green", "red"), guide = FALSE) +
  labs(title = "PERCENT CHANGE IN ECONOMIC IMPACT (2021 TO 2022)", x = "PERCENTAGE CHANGE", y = "",
       caption = "Source: IEP", size = 3) +
  geom_text(aes(label = scales::percent(per_change)), hjust = -0.2, size = 3) +
  scale_x_continuous(labels = scales::percent_format(prefix = "", big.mark = ",")) +
  theme(axis.text.y = element_text(face = "bold"), axis.text.x = element_text(face = "bold"))


p12



CHART_percentage_change = c(title = "Impact Change by Region",
                        sheet = "Impact change", source = "IEP Calculations", xtext = "", ytext = "",
                        type = "Chart", position = "Normal")


pCHART_percentage_change <- f_ThemeTraining(plot = p12, 
                                        chart_info = CHART_percentage_change, 
                                        plottitle = "Include", 
                                        xaxis = "", 
                                        yaxis = "", 
                                        xgridline = "", 
                                        ygridline = "")


pCHART_percentage_change


# 10. Composition of the regional economic cost of violence 2022 ========================================

df <- econ_impact.df %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::filter(subtype == "impact")

df <- df %>%
  dplyr::select(c(`iso3c`, `indicator2`, `value`))


df <- df %>%
  group_by(indicator2) %>%
  summarise(value = sum(value), iso3c)

region <- Peace_and_region %>%
  dplyr::select(-c(`peace_level`))

df <- left_join(region,df) 

df <- df %>%
  group_by(indicator2, region) %>%
  summarise(value = sum(value))

tmp <- df %>%
  pivot_wider(names_from = region, values_from = value)


new <- tmp %>%
  dplyr::filter(indicator2 %in% c("Fear",
                                  "GDP losses",
                                  "Incarceration",
                                  "Peacekeeping",
                                  "Peacebuilding",
                                  "Refugees and IDPs",
                                  "Small arms",
                                  "Terrorism"))



new <- pivot_longer(data = new, cols = -indicator2,
                    names_to = "region",
                    values_to = "value")

new <- new %>%
  group_by(region) %>%
  summarise(value = sum(value))

new <- new %>%
  rename(other = 'value')

new <- mutate(new, indicator2 = if_else(region == "other", "other", NA_character_))
new <- pivot_wider(data = new, names_from = region, values_from = other)
new$indicator2 <- ifelse(is.na(new$indicator2), "Other", new$indicator2)

tmp <- tmp[tmp$indicator2 != "Fear", ]
tmp <- tmp[tmp$indicator2 != "GDP losses", ]
tmp <- tmp[tmp$indicator2 != "Incarceration", ]
tmp <- tmp[tmp$indicator2 != "Peacekeeping", ]
tmp <- tmp[tmp$indicator2 != "Peacebuilding", ]
tmp <- tmp[tmp$indicator2 != "Refugees and IDPs", ]
tmp <- tmp[tmp$indicator2 != "Small arms", ]
tmp <- tmp[tmp$indicator2 != "Terrorism", ]

tmp <- rbind(new,tmp)

new <- tmp %>%
  dplyr::filter(indicator2 %in% c("Violent crime",
                                  "Homicide",
                                  "Suicide"))


new <- pivot_longer(data = new, cols = -indicator2,
                    names_to = "region",
                    values_to = "value")

new <- new %>%
  group_by(region) %>%
  summarise(value = sum(value))

new <- new %>%
  rename(`Violent crime, Homicide, Suicide` = 'value')

new <- mutate(new, indicator2 = if_else(region == "other", "other", NA_character_))
new <- pivot_wider(data = new, names_from = region, values_from = `Violent crime, Homicide, Suicide`)
new$indicator2 <- ifelse(is.na(new$indicator2), "Violent crime, Homicide, Suicide", new$indicator2)

tmp <- tmp[tmp$indicator2 != "Violent crime", ]
tmp <- tmp[tmp$indicator2 != "Homicide", ]
tmp <- tmp[tmp$indicator2 != "Suicide", ]

tmp <- rbind(new,tmp)


new <- tmp %>%
  dplyr::filter(indicator2 %in% c("Internal security expenditure",
                                  "Private security"))

new <- pivot_longer(data = new, cols = -indicator2,
                    names_to = "region",
                    values_to = "value")

new <- new %>%
  group_by(region) %>%
  summarise(value = sum(value))

new <- new %>%
  rename(`Internal and private security` = 'value')

new <- mutate(new, indicator2 = if_else(region == "other", "other", NA_character_))
new <- pivot_wider(data = new, names_from = region, values_from = `Internal and private security`)
new$indicator2 <- ifelse(is.na(new$indicator2), "Internal and private security", new$indicator2)

tmp <- tmp[tmp$indicator2 != "Internal security expenditure", ]
tmp <- tmp[tmp$indicator2 != "Private security", ]

tmp <- rbind(new,tmp)

tmp$Total <- sum(tmp$`Asia-Pacific`)
tmp$ASP <- tmp$`Asia-Pacific` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`Central America and the Caribbean`)
tmp$CCA <- tmp$`Central America and the Caribbean` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$Europe)
tmp$EUR <- tmp$Europe / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`Middle East and North Africa`)
tmp$MENA <- tmp$`Middle East and North Africa` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`North America`)
tmp$NAM <- tmp$`North America` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`Russia and Eurasia`)
tmp$RE <- tmp$`Russia and Eurasia` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`South America`)
tmp$SAM <- tmp$`South America` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`South Asia`)
tmp$SA <- tmp$`South Asia` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))
tmp$Total <- sum(tmp$`sub-Saharan Africa`)
tmp$SSA <- tmp$`sub-Saharan Africa` / tmp$Total
tmp <- tmp %>% dplyr::select(-c(`Total`))


tmp <- tmp %>% 
  dplyr::select(-c(`Asia-Pacific`,
                   `Central America and the Caribbean`,
                   `Europe`,
                   `Middle East and North Africa`,
                   `North America`,
                   `Russia and Eurasia`,
                   `South America`,
                   `South Asia`,
                   `sub-Saharan Africa`))


tmp <- tmp %>%
  rename(`Asia-Pacific` = "ASP") %>%
  rename(`Central America and the Caribbean` = "CCA") %>%
  rename(`Europe` = "EUR") %>%
  rename(`North America` = "NAM") %>%
  rename(`Russia and Eurasia` = "RE") %>%
  rename(`South America` = "SAM") %>%
  rename(`South Asia` = "SA") %>%
  rename(`sub-Saharan Africa` = "SSA") 
  


new <- pivot_longer(data = tmp, cols = -`indicator2`,
                    names_to = "region",
                    values_to = "value")



new$region <- str_wrap(new$region, width = 15)

CHART_composition.df <- new

  
p13 <- ggplot(CHART_composition.df, aes(x = region, y = value, fill = indicator2)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::percent((value))), 
    position = position_stack(vjust = 0.5), 
    size = 3,
    color = "white",  
    show.legend = FALSE  
  ) +
  labs(title = "Composition of regional economic cost of violence",
       x = "",  
       y = "") +  
  scale_fill_manual(values = c(
    "Military expenditure" = "#00847F",
    "Internal and private security" = "#55C1AA",
    "Conflict deaths" = "red",
    "Violent crime, Homicide, Suicide" = "#FAAB6C",  
    "Other" = "grey"
  )) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    legend.direction = "horizontal",
    legend.position = "top",
    axis.text.x = element_text(face = "bold")
  ) + guides(fill = guide_legend(title = NULL))


p13


CHART_composition = c(title = "Regional Composition",
                            sheet = "Composition", source = "IEP Calculations", xtext = "", ytext = "",
                            type = "Chart", position = "Normal")


pCHART_composition <- f_ThemeTraining(plot = p13, 
                                            chart_info = CHART_composition, 
                                            plottitle = "Include", 
                                            xaxis = "", 
                                            yaxis = "", 
                                            xgridline = "", 
                                            ygridline = "")


pCHART_composition


# Creating workbook ================================================================================================


SECTION3_EXPORT = c("CHART_EconImpact", "CHART_pie", "CHART_domain", "CHART_pie_arm", "CHART_pie_int", "CHART_pie_vio", "CHART_per_cap_mil", "CHART_Total_per_cap", "CHART_percentage_change", "CHART_composition")


figure_count = 0
table_count = 0



f_ProjectExport("1", wb_SECTION3, CHARTBOOK_1, SECTION3_EXPORT)

