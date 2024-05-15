
secu.agnecy <- SECU_AGENCY %>%
  mutate(X2007=X2008, X2019=X2018,X2020=X2018,X2021=X2018, X2022=X2018, X2023=X2018)%>%
  mutate(iso3c=countrycode(country,"country.name","iso3c")) %>%
  gather(year, secu.agnecy, -c(country, iso3c))%>% 
  mutate(year=as.numeric(as.character(gsub("X","",year))))

secu.agnecy$iso3c[secu.agnecy$country=="Kosovo"] <- "KSV"
secu.agnecy <- secu.agnecy[secu.agnecy$iso3c %in% pos, ]    
secu.agnecy <- subset(secu.agnecy,select=c(iso3c,year,secu.agnecy))

secu.agnecy <- gpi.grid %>% 
  left_join(secu.agnecy)

# using regional average for South Sudan

secu.agnecy.region.average <- secu.agnecy %>%  
  left_join(Peace_and_region) %>% 
  select (-c (`peace_level`))

secu.agnecy.region.average <- secu.agnecy.region.average %>% 
  group_by(region,year) %>%
  summarise(average=mean(secu.agnecy, na.rm=T))


secu.agnecy <- secu.agnecy %>% 
  left_join(Peace_and_region, by = "iso3c")

secu.agnecy <- secu.agnecy %>% 
  left_join(secu.agnecy.region.average, by = c("region", "year"))

secu.agnecy <- secu.agnecy %>% 
  mutate (secu.agnecy = coalesce(secu.agnecy, average)) %>% 
  select (c(`year`, `iso3c`, `secu.agnecy`))

