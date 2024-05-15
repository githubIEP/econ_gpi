f_LibraryLoader(tidyverse,rio)
priv.secu <- PRIVATE_SECURITY %>% 
  select(-`...1`) %>%
  gather(year,value, -c("country","iso3c")) %>% select(-country)

priv.secu$year <- as.numeric(as.character(priv.secu$year))

latest_year = max(priv.secu$year)

data_new <- priv.secu %>% 
  filter(year == latest_year) %>%
  mutate(year = latest_year+1)

# Combine the original dataset with the new 2023 data
priv.secu <- bind_rows(priv.secu, data_new)

priv.secu <- priv.secu %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>%
  mutate(year=as.numeric(as.character(gsub(",","",year)))) 
#Add in population
tmp <- pop %>% 
  select(iso3c, year, population)

priv.secu <- left_join(priv.secu, tmp) %>%
  mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value) 

tmp <- priv.secu %>%  
  merge(unitcost.scaled[,c("iso3c","year", "privtsecurity.direct")], by=c("iso3c","year")) %>%
  mutate(privsecu.cost=privtsecurity.direct*value) %>%
  subset(select=c(iso3c,year, privsecu.cost)) %>%
  full_join(gpi.grid, by=c("iso3c","year")) %>%
  mutate(privsecu.cost=ifelse(is.na(privsecu.cost),0,privsecu.cost))

priv.secu = tmp

