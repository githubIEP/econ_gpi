#_________________________________________________________SUICIDE RATE_________________________________________________________________________________________________

suicide <- f_get.wdi("all","SH.STA.SUIC.P5",2006,LATEST_YEAR) %>% 
  mutate (year = year + 1) %>%
  rename(rate=SH.STA.SUIC.P5) %>% rename(value=rate) %>% mutate(variablename="suicide_rate") %>%
  subset(!iso3c=="PSE") %>% subset(!iso3c=="KSV") %>% subset(!iso3c=="TWN")


suicide <- left_join(gpi.grid, suicide)

# Checking what countries missing all country-year values 

suicide %>% group_by(iso3c) %>% summarize(count = sum(is.na(value))) %>% arrange(desc(count))

# Palestine Kosovo and Taiwan missing all country-year values

suicide <- suicide %>% rename(geocode=iso3c) %>% mutate(variablename = "suicide rate") %>% 
  subset(!geocode %in% c("KSV", "PSE", "TWN"))


suicide <- f_index_data_pad(suicide)

suicide <- suicide %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)


suicide <- gpi.grid %>% left_join(suicide) %>% mutate (variablename = "suicide rate")



# Using regional average for Kosovo Taiwan & Palestine

suicide.region.average <- suicide %>%  left_join(Peace_and_region) %>% select (-c (6))
suicide.region.average <- suicide.region.average %>% group_by(region,year) %>%
  summarise(average=mean(value, na.rm=T))



suicide <- suicide %>%  left_join(Peace_and_region, by = "iso3c")

suicide <- suicide   %>% left_join(suicide.region.average, by = c("region", "year"))

suicide <- suicide %>% mutate (value = coalesce(value, average)) %>% select (c(`year`:`value`))

rm(suicide.region.average, suicide_GBD)

suicide <- suicide %>% left_join(pop) %>%  distinct() %>% mutate(value=(population/100000*value)) %>% select(-population) 
suicide <- suicide %>% mutate(variablename="suicide_count") %>% select(iso3c,value, year) %>%  rename(suicidevalue=value)





# finished
