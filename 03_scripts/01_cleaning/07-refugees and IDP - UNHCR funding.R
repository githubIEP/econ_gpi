########### UNHCR funding #########
unhcr <- UNHCR %>%
  gather(year,unhcr,-iso3c) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year))))

unhcr<- unhcr %>% 
  mutate(unhcr=as.numeric(as.character(unhcr)))
############ ADD IN 2021 ############
UNHCR_2021$iso3c = countrycode::countrycode(UNHCR_2021$country, "country.name", "iso3c")
UNHCR_2021$iso3c[UNHCR_2021$country=="Kosovo"] <- "KSV"
UNHCR_2021 <- na.omit(UNHCR_2021)
UNHCR_2021 <- UNHCR_2021[UNHCR_2021$iso3c %in% pos,]
UNHCR_2021 <- UNHCR_2021 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2021$year = 2021
UNHCR_2021 <- rename(UNHCR_2021, unhcr=total)
UNHCR_2021 <- UNHCR_2021 %>% select(iso3c,year,unhcr)

############ ADD IN 2022 ############

unhcr <- unhcr %>% rbind(UNHCR_2021) 
unhcr <- unhcr[unhcr$iso3c %in% pos,]
unhcr <- unhcr %>% na.omit() 
unhcr <- gpi.grid %>% left_join(unhcr) 
unhcr <- unhcr %>% 
  rename (geocode = iso3c, value = unhcr) %>% 
  mutate (variablename = "unhcr")

unhcr <-  f_index_data_pad(unhcr)

unhcr <- unhcr %>%
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, unhcr = imputed)

unhcr <- gpi.grid %>% left_join(unhcr)

# finished

