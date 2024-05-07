
# GPI data ----------------------------------------------------------------
gpidata <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, 
                                 `battle_deaths`,
                                 `homicides`,
                                 `incarceration`,
                                 `milex`, `fear`, 
                                 `terrorism_deaths`, 
                                 `displaced`, 
                                 `assessments`),
                        names_to = "element",
                        values_to = "value")



gpidata <- gpidata %>% 
  rename(iso3c=geocode, indicator = element) %>% 
  mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                 indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                 indicator == "fear" ~ "perceptions of criminality",
                                 indicator == "Homicides per 100,000" ~ "homicide rate",
                                 indicator == "displaced" ~ "refugees and idps",
                                 indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                 indicator == "assessments" ~ "un peacekeeping funding",
                                 TRUE ~ indicator))

pos <- unique(gpidata$iso3c)


# GPI grid ----------------------------------------------------------------

gpi.country <- read_csv("02_data/processed/gpi.country.csv")

gpi.grid <- gpi.country[["iso3c"]]

gpi.grid <- expand.grid(year=c(FIRST_GPI_YEAR:LATEST_YEAR), iso3c=unique(gpi.country$iso3c))


tab <- function(data){
  t <- with(data, table(iso3c, is.na(value)))
  View(t)
}


Peace_and_region <- read_excel("02_data/processed/Peace and region.xlsx",                               
                               col_types = c("text", "text", "numeric"))

Peace_and_region <- Peace_and_region %>%  rename(`GPI overall score`=`2020 Peace level`)

Peace_and_region$peace_level = ifelse(Peace_and_region$`GPI overall score` <= 1.45, "Very High Peace", 
                                      ifelse(Peace_and_region$`GPI overall score` >= 1.45 & Peace_and_region$`GPI overall score` <= 1.9, "High Peace", 
                                             ifelse(Peace_and_region$`GPI overall score` >= 1.9 & Peace_and_region$`GPI overall score` <= 2.4, "Medium Peace",
                                                    ifelse(Peace_and_region$`GPI overall score` >= 2.4 & Peace_and_region$`GPI overall score` <= 2.9, "Low Peace",
                                                           ifelse(Peace_and_region$`GPI overall score` >= 2.9, "Very Low Peace", NA)))))
Peace_and_region <- Peace_and_region %>% select(-`GPI overall score`)



######################################################## POPULATION  ############################################################

pop <- gpidata %>% dplyr::filter (indicator == "population") %>%
  rename (population = value) %>% 
  select (-indicator)

pop <- pop %>%
  mutate (variablename = "population") %>% 
  rename(value = population, geocode = iso3c)

pop <- f_index_data_pad(pop)

pop %<>% select (c(`geocode`, `year`, `imputed`)) %>%
  rename (iso3c = geocode, population = imputed)
