gpidata <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, `battle_deaths`, `homicides`, `incarceration`, `milex`, `fear`, `terrorism_deaths`, `displaced`, `assessments`),
                        names_to = "element",
                        values_to = "value")


gpidata <- gpidata %>% 
  rename(iso3c=geocode, indicator = element)


gpidata <- gpidata %>% mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "milex" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "Homicides per 100,000" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))


milex <- gpidata %>% 
  subset(indicator=="military expenditure (% gdp)") %>% 
  mutate(value = value /100) %>%
  dplyr::filter (year < 2022) %>%
  dplyr::select(-c(`country`, `peace_level`, `region`))



# Data for 2022 from SIPRI

milex2023 <- SIPRI_MILEX %>%
  select(-Notes) %>%
  gather(year, value, -Country) %>% subset(year>2005) %>%
  mutate (year = as.numeric(year))%>%
  rename(country=Country) %>%
  mutate(country=ifelse(country=="eSwatini","Swaziland",country)) %>%
  mutate(country=ifelse(country=="Norh Macedonia","Macedonia",country)) %>%
  mutate( iso3c=  countrycode(country, "country.name","iso3c")) %>%
  mutate(iso3c=ifelse(country=="Kosovo","KSV",iso3c)) %>%
  dplyr::filter (!country == "USSR")%>%
  select (iso3c, year, value) %>%
  dplyr::filter (year > 2021) %>%
  right_join(gpi.grid) %>%
  dplyr::filter(year > 2021) %>%
  mutate (value = as.numeric(value)) %>%
  mutate (indicator = "military expenditure (% gdp)")

# ===================================================================================

milex <- milex %>%
  rbind (milex2023)

rm(milex2023)


milex <- gpi.grid %>% 
  left_join(milex)

milex <- milex %>% 
  rename (geocode = iso3c, variablename = indicator)

milex <- milex %>% 
  group_by(geocode) %>%
  fill(value, .direction = "downup")

# milex <- f_index_data_pad(milex)

milex <- milex %>% 
  select (c(`year`, `geocode`, `value`)) %>% 
  rename (iso3c = geocode, milex = value)

milex <- gpi.grid %>% 
  left_join(milex)


milex <- milex %>% 
  left_join(gdp.wdi)

milex <- milex %>% 
  mutate (milex = milex * gdp) %>%
  select (iso3c, year, milex)

milex <- milex[-938, ]
milex <- milex %>% 
  group_by(iso3c) %>%
  fill(milex, .direction = "downup")
