
temp = tempfile(fileext = ".xlsx")
dataURL <- WHITE_HOUSE
download.file(dataURL, destfile=temp, mode='wb')

test <- readxl::read_excel(temp, skip =2)
test <-test %>%  
  subset(`Function and Subfunction`=="Total, Veterans Benefits and Services") %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`, `2026`=`2026 estimate`, `2027`=`2027 estimate`) %>% 
  gather(year, value, -c(`Function and Subfunction`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6)

#Update the link to Table 6.1—Composition of Outlays: 1940–2027
temp = tempfile(fileext = ".xlsx")
dataURL <- WHITE_HOUSE_INTEREST
download.file(dataURL, destfile=temp, mode='wb')


test_interest <- readxl::read_excel(temp, skip =1)
test_row <- which(test_interest$Category == 'Net interest (2)')
test_interest <- test_interest[test_row, ]
test_interest <- test_interest[1, ]


test_interest <- test_interest %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`,`2026`=`2026 estimate`, `2027`=`2027 estimate`, ) %>% 
  gather(year, value, -c(`Category`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6) %>%
  mutate(interest=value*0.2) %>% select(year, interest)


vet_tmp <- left_join(test,test_interest)
vet_tmp <- vet_tmp %>%
  subset(year>2006) %>% 
  mutate(value=value+interest) %>% 
  select(year, value) %>%
  subset(year = LATEST_YEAR)
vet_tmp$iso3c = "USA"

vet_tmp <- vet_tmp %>% 
  full_join(gpi.grid, by=c("iso3c","year")) %>% 
  mutate(value=ifelse(is.na(value),0,value))

vet_tmp <- vet_tmp %>% 
  rename(vet.int=value)

vet = vet_tmp

vet <- gpi.grid %>% 
  left_join(vet, by = c("year", "iso3c"))

vet <- vet %>% 
  rename (geocode = iso3c, value = vet.int) %>% 
  mutate (variablename = "Veteran Costs")

vet <- vet %>% 
  rename(iso3c = geocode) %>% 
  rename(vet.int = value)
