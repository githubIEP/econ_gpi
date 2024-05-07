#######################      fear    ####################### 
fear <- gpidata %>%
  subset(indicator=="perceptions of criminality") %>%   rename_all(tolower) %>%
  select("iso3c", "year", "value")  %>%
  subset(!year==LATEST_YEAR+1) %>%
  mutate(variablename="fear")

fear <- gpi.grid %>% left_join(fear) %>% rename (geocode = iso3c)

fear <- fear %>% group_by(geocode) %>%
  fill(value, .direction = "downup")


fear <- fear %>% rename(iso3c = geocode) %>% rename(fear = value)

