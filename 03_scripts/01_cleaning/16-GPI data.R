gpidata <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, `battle_deaths`, `homicides`, `incarceration`, `milex`, `fear`, `terrorism_deaths`, `displaced`, `assessments`),
                        names_to = "element", 
                        values_to = "value")


gpidata <- gpidata %>% 
  rename(iso3c=geocode, indicator = element)



gpidata <- gpidata %>% mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "homicides" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "incarceration" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))


homicide <- gpidata %>% 
  subset(indicator=="homicide rate") %>%
  dplyr::select(c(`iso3c`, `year`, `indicator`, `value`))


homicide <- gpi.grid %>% 
  left_join(homicide)

homicide <- homicide %>% 
  rename (geocode = iso3c, 
          variablename = indicator) %>% 
  mutate (variablename = "homicide rate") 

homicide <- f_index_data_pad(homicide)

homicide <- homicide %>%
  select (`geocode`, `year`, `variablename`, `imputed`) %>% 
  rename (iso3c = geocode, value = imputed, indicator = variablename)

homicide <- gpi.grid %>%
  left_join(homicide)

# incarceration rate ------------------------------------------------------

incar <- gpidata%>% 
  subset(indicator=="incarceration rate") %>%
  dplyr::select(c(`iso3c`, `year`, `indicator`, `value`))

incar <- gpi.grid %>% 
  left_join(incar, by = c("iso3c", "year"))

incar <- incar %>% 
  rename (geocode = iso3c, variablename = indicator) %>% 
  mutate (variablename = "incarceration rate") 


incar <- f_index_data_pad(incar)

incar <- incar %>% 
  select (`geocode`, `year`, `variablename`, `imputed`) %>% 
  rename (iso3c = geocode, value = imputed, indicator = variablename)

incar <- gpi.grid %>% 
  left_join(incar, by = c("iso3c", "year"))


# conflcit deaths -------------------------------------------------------------

conflict <- gpidata %>% 
  subset(indicator== "battle_deaths")

conflict <- gpi.grid %>% 
  left_join(conflict)

conflict <- conflict %>% 
  dplyr::select(c(`year`, `iso3c`, `indicator`, `value`))

conflict <- conflict  %>% 
  rename (geocode = iso3c, variablename = indicator) 

conflict$variablename[is.na(conflict$variablename)] <- "battle_deaths"

conflict <- f_index_data_pad(conflict)

conflict <- conflict %>% 
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, battle_deaths = imputed)

conflict <- gpi.grid %>% 
  left_join(conflict)


# refugees and IDPs -------------------------------------------------------
pop3 <- pop %>% rename(pop=population)


refugidp <- gpidata %>% 
  subset(indicator=="displaced" ) %>% 
  merge(pop3[,c("iso3c","year","pop")], by=c("iso3c", "year")) %>%  
  subset(select=c(iso3c,year,indicator,value)) %>% 
  rename(refug=value)

refugidp <- merge(refugidp, gdp.pc.constant[,c("iso3c","year","gdp.pc.cons")], by=c("iso3c","year"), all=TRUE)


# I multiply this my 0.6 as 40% of the refugee population are children and therefore not really contributing to GDP pc
# Note we have opted to a 10% resettlement rate.

refugidp$refugeidp <- refugidp[, "refug"]*refugidp[,"gdp.pc.cons"]*(1-0.1)

refugidp <- refugidp %>%   
  subset(select=c(iso3c,year,refugeidp)) 


# Imputation

refugidp <- refugidp %>% rename (geocode = iso3c, value = refugeidp) %>% 
  mutate (variablename = "refugee")  


refugidp <- f_index_data_pad(refugidp)

refugidp <- refugidp %>% 
  select (`geocode`, `year`,`imputed`) %>% 
  rename (iso3c = geocode, refugeidp = imputed)

refugidp <- gpi.grid %>% 
  left_join(refugidp)


# GDP losses for countries with >1000 deaths ------------------------------


gdplosses <- conflict %>% 
  mutate(conflict=ifelse(battle_deaths>999,1,0)) %>%
  merge(gdp.wdi[,c("iso3c","year","gdpcons")], by=c("iso3c","year"), all=TRUE) %>%
  mutate(gdplosses=ifelse(conflict==1,gdpcons*0.022,0)) %>% 
  subset(select=c(iso3c,year,gdplosses))

gdplosses <- subset(gdplosses,!(year<2007))
gdplosses <- subset(gdplosses,!(year>LATEST_YEAR))


# imputation
gdplosses <- gdplosses %>% 
  rename (geocode = iso3c, value = gdplosses) %>%
  mutate (variablename = "gdp losses")

gdplosses <- f_index_data_pad(gdplosses)

gdplosses <- gdplosses %>%
  select (c(`geocode`, `year`,`imputed`)) %>%
  rename (iso3c = geocode, gdplosses = imputed)


gdplosses <- gpi.grid %>%
  left_join(gdplosses)
