# Unit Costs
# This is the unit cost data frame.
# The unit costs are then scaled for all countries and for all years by using the PPP scale

UNIT_COST.df <- UNIT_COST

UNIT_COST.df <- UNIT_COST.df %>%
  mutate(indicator = paste(Indicator, type, sep = '.'))

UNIT_COST.df <- UNIT_COST.df %>%
  select(indicator, unitcost)

UNIT_COST.df <- UNIT_COST.df %>%
  pivot_wider(names_from = indicator, values_from = unitcost)

UNITCOST_NEW.df <- cbind(ppp, UNIT_COST.df)

scale_fun <- function(x, scale) {
  x * scale
}

# Columns to apply scaling to

cols_to_scale <- c("homicide.direct",
                   "homicide.indirect",
                   "violentassault.direct",
                   "violentassault.indirect",
                   "sexualassault.direct", 
                   "sexualassault.indirect", 
                   "fear.indirect", 
                   "suicide.direct",
                   "suicide.indirect",
                   "minwage.direct",
                   "police.direct",
                   "privtsecurity.direct")  

# Apply scaling to selected columns

UNITCOST_SCALED.df <- UNITCOST_NEW.df %>%
  mutate_at(vars(cols_to_scale), 
            list(~ scale_fun(., scale)))

UNITCOST_SCALED.df <- UNITCOST_SCALED.df[,c(2,1,4:15)]



#VET AFFAIRS ===========================================================================================================

# This code downloads the vet affairs data frame from the white house website
# The code then filters for Total veterans benefits and services.
# The code then gives the total amount and estimates fro 1976 to 2027
# The code also adds interest to the veteran costs. 
# Finally we combine this vet data frame with the GPI grid data frame to include all the GPI countries
# All GPI countries except for USA are set to 0. 


dataURL <- WHITE_HOUSE  

TEST.df <- rio::import(dataURL, skip = 2)  

TEST.df <- TEST.df %>%  
  subset(`Function and Subfunction`=="Total, Veterans Benefits and Services") %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`, `2026`=`2026 estimate`, `2027`=`2027 estimate`) %>% 
  gather(year, value, -c(`Function and Subfunction`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6)

dataURL <- WHITE_HOUSE_INTEREST

Test_Interest.df <- rio::import(dataURL, skip = 1)

Test_Row <- Test_Interest.df %>%
  filter(Category == 'Net interest (2)') %>%
  pull(row_number())

Test_Interest.df <- Test_Interest.df %>%
  slice(Test_Row)

Test_Interest.df <- Test_Interest.df[1, ]

Test_Interest.df <- Test_Interest.df %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`,`2026`=`2026 estimate`, `2027`=`2027 estimate`, ) %>% 
  gather(year, value, -c(`Category`)) %>% 
  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% 
  mutate(value=value*10^6) %>%
  mutate(interest=value*0.2) %>% 
  select(year, interest)

Vet_Tmp.df <- left_join(Test.df,Test_Interest.df)

Vet_Tmp.df <- Vet_Tmp.df %>%
  subset(year>2006) %>% 
  mutate(value= value+interest) %>% 
  select(year, value) %>%
  subset(year = LATEST_YEAR)


Vet_Tmp.df <- Vet_Tmp.df %>%
  mutate(iso3c = "USA")

Vet_Tmp.df <- Vet_Tmp.df %>% 
  full_join(gpi.grid, by=c("iso3c","year")) %>% 
  mutate(value=ifelse(is.na(value),0,value))

Vet_Tmp.df <- Vet_Tmp.df %>% 
  rename(vet.int=value)

Vet.df = Vet_Tmp.df

Vet.df <- gpi.grid %>% 
  left_join(Vet.df, by = c("year", "iso3c"))

Vet.df <- Vet.df %>% 
  rename (geocode = iso3c, value = vet.int) %>% 
  mutate (variablename = "Veteran Costs")

Vet.df <- Vet.df %>% 
  rename(iso3c = geocode) %>% 
  rename(vet = value) %>%
  dplyr::select(-c(`variablename`))



# Suicide ===============================================================================================================

# This code downloads suicide data from the World Bank
# This is then combined with the GPI grid data frame to include all GPI countries to complete the data frame.
# The code then counts the number NAs in the data frame
# The code then imputes the missing values with last available data, as well as using regional peace averages


SUICIDE.df <- f_get.wdi("all","SH.STA.SUIC.P5",2006,LATEST_YEAR) %>% 
  mutate (year = year + 1) %>%
  rename(rate=SH.STA.SUIC.P5) %>% 
  rename(value=rate) %>% 
  mutate(variablename="suicide_rate") %>%
  subset(!iso3c=="PSE") %>%
  subset(!iso3c=="KSV") %>%
  subset(!iso3c=="TWN")

SUICIDE.df <- gpi.grid %>% 
  left_join(SUICIDE.df)

SUICIDE.df %>%
  group_by(iso3c) %>%
  summarize(count = sum(is.na(value))) %>%
  arrange(desc(count))

SUICIDE.df <- SUICIDE.df %>%
  rename(geocode=iso3c) %>%
  mutate(variablename = "suicide rate") %>% 
  subset(!geocode %in% c("KSV", "PSE", "TWN"))

SUICIDE.df <- f_index_data_pad(SUICIDE.df)

SUICIDE.df <- SUICIDE.df %>%
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

SUICIDE.df <- gpi.grid %>% 
  left_join(SUICIDE.df) %>%
  mutate (variablename = "suicide rate")

SUICIDE_REGION_AVERAGE.df <- SUICIDE.df %>%
  left_join(Peace_and_region) %>%
  select (-c (`peace_level`))

SUICIDE_REGION_AVERAGE.df <- SUICIDE_REGION_AVERAGE.df %>%
  group_by(region,year) %>%
  summarise(average=mean(value, na.rm=T))

SUICIDE.df <- SUICIDE.df %>% 
  left_join(Peace_and_region, by = "iso3c")

SUICIDE.df <- SUICIDE.df   %>% 
  left_join(SUICIDE_REGION_AVERAGE.df, by = c("region", "year"))

SUICIDE.df <- SUICIDE.df %>% 
  mutate (value = coalesce(value, average)) %>%
  select (c(`year`:`value`))

SUICIDE.df <- SUICIDE.df %>% 
  left_join(pop) %>%  
  distinct() %>% 
  mutate(value=(population/100000*value)) %>%
  select(-population) 

SUICIDE.df <- SUICIDE.df %>% 
  mutate(variablename="suicide_count") %>% 
  select(iso3c,value, year) %>% 
  rename(suicidevalue=value)

rm(SUICIDE_REGION_AVERAGE.df, suicide_GBD)


# Fear (Perception of Criminality) ======================================================================================

# This code pull the data from the GPI data set, which is the perceptions of criminality
# This code then fills the data set with all the GPI countries and years completing the data frame
# The dataset is complete but has NAs
# These NA's are then filled in through imputation using the fill() function


FEAR.df <- gpidata %>%
  subset(indicator=="perceptions of criminality") %>%   rename_all(tolower) %>%
  select("iso3c", "year", "value")  %>%
  subset(!year==LATEST_YEAR+1) %>%
  mutate(variablename="fear")

FEAR.df <- gpi.grid %>%
  left_join(FEAR.df) %>% 
  rename (geocode = iso3c)

FEAR.df <- FEAR.df %>%
  group_by(geocode) %>%
  fill(value, .direction = "downup")

FEAR.df <- FEAR.df %>% 
  rename(iso3c = geocode) %>% 
  rename(fear = value)


# UNHCR funding ============================================================================================================

# This gathers UNHCR funding from the UNHCR data set. 
# The UNHCR data set is combined with UNHCR data from 2021. 
# Once these are combined then the GPI grid data frame is added to it
# This completes data set, and fills in the NA values through imputation. 

UNHCR.df <- UNHCR %>%
  gather(year,unhcr,-iso3c) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year))))

UNHCR.df <- UNHCR.df %>% 
  mutate(unhcr=as.numeric(as.character(unhcr)))

############ ADD IN 2021 ############
UNHCR_2021.df <- UNHCR_2021 %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"))

UNHCR_2021.df <- UNHCR_2021.df %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"),
    iso3c = case_when(
      iso3c == "Kosovo" ~ "KSV",
      TRUE ~ iso3c
    )
  )

UNHCR_2021.df <- UNHCR_2021.df %>% 
  na.omit(UNHCR_2021.df)

UNHCR_2021.df <- UNHCR_2021.df %>%
  filter(iso3c %in% pos)

UNHCR_2021.df <- UNHCR_2021.df %>% 
  mutate(country=countrycode(iso3c,"iso3c","country.name")) 

UNHCR_2021.df <- UNHCR_2021.df %>%
  mutate(year = 2021)

UNHCR_2021.df <- UNHCR_2021.df %>%
  rename(unhcr=total)

UNHCR_2021.df <- UNHCR_2021.df %>% 
  select(iso3c,year,unhcr)


############ ADD IN 2022 ############

UNHCR.df <- UNHCR.df %>%
  rbind(UNHCR_2021.df) 

UNHCR.df <- UNHCR.df %>%
  filter(iso3c %in% pos)

UNHCR.df <- UNHCR.df %>%
  na.omit() 

UNHCR.df <- gpi.grid %>% 
  left_join(UNHCR.df) 

UNHCR.df <- UNHCR.df %>% 
  rename (geocode = iso3c, value = unhcr) %>% 
  mutate (variablename = "unhcr")

UNHCR.df <-  f_index_data_pad(UNHCR.df)

UNHCR.df <- UNHCR.df %>%
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, unhcr = imputed)

UNHCR.df <- gpi.grid %>% 
  left_join(UNHCR.df)

#################################### Violent assault ###################################
# This code pulls violent assault data
# The raw data frame has 3 column (country, year and value).
# We use the country code package to add the the three letter iso country code column
# However we have to manually rename country code, for example, Kosovo as the raw data has Kosovo listed as "Kosovo under UNSCR 1244"
# We rename the Kosovo in the country column as well. 
# We now add a new column called 'variablename' to the data frame
# This data frame is now  combined with the GPI country grid to fill all countries for all years. 
# It runs a loop to count missing values
# Once that is completed all the missing values are filled in with regional and peace averages

dataURL <- TOTAL_ASSAULT_TEST

VIOLENT_ASSAULT.df <- rio::import(dataURL, skip = 2) %>%
  filter(Category == "Serious assault") %>%
  filter(`Unit of measurement` == "Rate per 100,000 population") %>%
  dplyr::select(c(`Country`, `Year`, `VALUE`)) %>%
  rename(value = VALUE) %>%
  rename(country = Country) %>%
  rename(year = Year) %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)")



VIOLENT_ASSAULT.df = VIOLENT_ASSAULT.df %>% 
  mutate(value = round(value,2))


VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"))


VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"),
    iso3c = case_when(
      country == "Kosovo under UNSCR 1244" ~ "KSV",
      TRUE ~ iso3c
    )
  )


VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>%
  mutate(variablename = "violent assault")


VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>%
  filter(iso3c %in% pos)

VIOLENT_ASSAULT.df <- gpi.grid %>% 
  left_join(VIOLENT_ASSAULT.df)

VIOLENT_ASSAULT_tmp.df <- f_missing(VIOLENT_ASSAULT.df)

COUNT_OF_MISSING.df <- VIOLENT_ASSAULT_tmp.df %>%
  count(iso3c) %>%
  as.data.frame()

COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>%  
  mutate(Freq=as.numeric(n)) %>%
  rename(Var1 = iso3c) %>%
  select(-c(`n`)) 

for(c in 1:nrow(COUNT_OF_MISSING.df)){
  if(COUNT_OF_MISSING.df[c,"Var1"]=="PSE" & COUNT_OF_MISSING.df[c,"Freq"]==8) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Var1"]=="SSD" & COUNT_OF_MISSING.df[c,"Freq"]==PANEL_SSD) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Freq"]==PANEL_FREQ) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else {COUNT_OF_MISSING.df[c,"new"] <- "keep"}
}

COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>% 
  rename(iso3c=Var1)

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  left_join(COUNT_OF_MISSING.df)

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  subset(!new=="delete")

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% 
  mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% 
  mutate(variablename="violent assault")

VIOLENT_ASSAULT.df <- f_index_data_pad(VIOLENT_ASSAULT.df)

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)


## Average by peace and region

VIOLENT_ASSAULT_AVERAGE.df <- VIOLENT_ASSAULT.df %>%  
  left_join(Peace_and_region)

VIOLENT_ASSAULT_AVERAGE.df <- VIOLENT_ASSAULT_AVERAGE.df %>% 
  group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))

# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries

VIOLENT_ASSAULT_PEACE_LEVEL.df <- VIOLENT_ASSAULT.df %>%  
  left_join(Peace_and_region)

VIOLENT_ASSAULT_PEACE_LEVEL.df <- VIOLENT_ASSAULT_PEACE_LEVEL.df %>% 
  select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

VIOLENT_ASSAULT.df <- gpi.grid %>% 
  left_join(VIOLENT_ASSAULT.df)


##### From who is missing allocate the average rate by peace level and region

MISSING.df <- f_missing(VIOLENT_ASSAULT.df)

MISSING.df <- MISSING.df %>% 
  left_join(Peace_and_region)

MISSING.df <- MISSING.df %>%
  select(-variablename,-value)

MISSING.df <- MISSING.df %>% 
  left_join(VIOLENT_ASSAULT_AVERAGE.df) %>%
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>%
  mutate(variablename="violent assault") %>% 
  na.omit() 


#ADD to main DF

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  na.omit() %>% 
  rbind(MISSING.df) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
# 
VIOLENT_ASSAULT.df <- gpi.grid %>%
  left_join(VIOLENT_ASSAULT.df)

MISSING.df <- f_missing(VIOLENT_ASSAULT.df)


#If data still does not have a match so I will take the average of the peace level #

MISSING.df <- MISSING.df %>%  
  left_join(Peace_and_region)

MISSING.df <- MISSING.df %>% 
  select(-variablename,-value)

MISSING.df <- MISSING.df %>% 
  left_join(VIOLENT_ASSAULT_PEACE_LEVEL.df) %>% 
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>% 
  mutate(variablename="violent assault")

# The 'additional' missing data needs to be added in 

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>% 
  na.omit() %>% 
  rbind(MISSING.df)

VIOLENT_ASSAULT.df <- VIOLENT_ASSAULT.df %>%
  filter(iso3c %in% pos)

rm(VIOLENT_ASSAULT_AVERAGE.df)

rm(VIOLENT_ASSAULT_PEACE_LEVEL.df)



#################### Sexual violence #####################################################################################

# Sexual assault data is pulled from the Sexual data frame
# And the same process and transformations to the data frame are done to this data set as the Violent assault data set


# Not in GPI data 

SEXUAL_ASSAULT.df <- rio::import(dataURL, skip = 2) %>%
  filter(Category == "Sexual violence") %>%
  filter(`Unit of measurement` == "Rate per 100,000 population") %>%
  dplyr::select(c(`Country`, `Year`, `VALUE`)) %>%
  rename(value = VALUE) %>%
  rename(country = Country) %>%
  rename(year = Year) %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)")


SEXUAL_ASSAULT.df = SEXUAL_ASSAULT.df %>%
  mutate(value = round(value,2))

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c"))

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  mutate(
    iso3c = countrycode(country, "country.name", "iso3c"),
    iso3c = case_when(
      country == "Kosovo under UNSCR 1244" ~ "KSV",
      TRUE ~ iso3c
    )
  )


SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  mutate(variablename = "sexual assault")


SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  filter(iso3c %in% pos)


### WORK OUT WHO IS MISSING


SEXUAL_ASSAULT.df <- gpi.grid %>% 
  left_join(SEXUAL_ASSAULT.df)

MISSING.df <- f_missing(SEXUAL_ASSAULT.df)

COUNT_OF_MISSING.df <- MISSING.df %>%
  count(iso3c) %>%
  as.data.frame()

COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>%
  rename(Freq = n, Var1 = iso3c) %>%
  mutate(Freq=as.numeric(Freq))


# This will tell us who to delete to impute (you will need to update the numbers here)


for(c in 1:nrow(COUNT_OF_MISSING.df)){
  if(COUNT_OF_MISSING.df[c,"Var1"]=="PSE" & COUNT_OF_MISSING.df[c,"Freq"]==10) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Var1"]=="SSD" & COUNT_OF_MISSING.df[c,"Freq"]==PANEL_SSD) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Freq"]==PANEL_FREQ) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else {COUNT_OF_MISSING.df[c,"new"] <- "keep"}
}


COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>% 
  rename(iso3c=Var1)

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  left_join(COUNT_OF_MISSING.df)

#delete out all that dont have at least one variable!  


SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  subset(!new=="delete")
# We can now impute given what remains has at least one value

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% 
  mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>%
  mutate(variablename="sexual assault")

SEXUAL_ASSAULT.df <- f_index_data_pad(SEXUAL_ASSAULT.df)

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

## Average by peace and region


SEXUAL_ASSAULT_AVERAGE.df <- SEXUAL_ASSAULT.df %>%  
  left_join(Peace_and_region)

SEXUAL_ASSAULT_AVERAGE.df <- SEXUAL_ASSAULT_AVERAGE.df %>% 
  group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))

# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries


SEXUAL_ASSAULT_PEACE_LEVEL.df <- SEXUAL_ASSAULT.df %>%  
  left_join(Peace_and_region)

SEXUAL_ASSAULT_PEACE_LEVEL.df <- SEXUAL_ASSAULT_PEACE_LEVEL.df %>%
  select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

SEXUAL_ASSAULT.df <- gpi.grid %>% 
  left_join(SEXUAL_ASSAULT.df)

##### From who is missing allocate the average rate by peace level and region


MISSING.df <- f_missing(SEXUAL_ASSAULT.df)

MISSING.df <- MISSING.df %>%  
  left_join(Peace_and_region)

MISSING.df <- MISSING.df %>% 
  select(-variablename,-value)

MISSING.df <- MISSING.df %>% 
  left_join(SEXUAL_ASSAULT_AVERAGE.df) %>% 
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>% 
  mutate(variablename="sexual assault") %>%
  na.omit() 

#ADD to main DF

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  na.omit() %>% 
  rbind(MISSING.df) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.

SEXUAL_ASSAULT.df <- gpi.grid %>% 
  left_join(SEXUAL_ASSAULT.df)

MISSING.df <- f_missing(SEXUAL_ASSAULT.df)

#If data still does not have a match so I will take the average of the peace level #

MISSING.df <- MISSING.df %>%
  left_join(Peace_and_region)

MISSING.df <- MISSING.df %>% 
  select(-variablename,-value)

MISSING.df <- MISSING.df %>% 
  left_join(SEXUAL_ASSAULT_PEACE_LEVEL.df) %>%
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>% 
  mutate(variablename="sexual assault")
# The 'additional' missing data needs to be added in 

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>% 
  na.omit() %>%
  rbind(MISSING.df)

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  filter(iso3c %in% pos)

rm(SEXUAL_ASSAULT_AVERAGE.df)

rm(SEXUAL_ASSAULT_PEACE_LEVEL.df)

SEXUAL_ASSAULT.df <- SEXUAL_ASSAULT.df %>%
  filter(iso3c %in% pos)


##### TURN RATE TO COUNT FOR BOTH SEXUAL AND VIOLENT ASSAULT

CRIME.df <- bind_rows(VIOLENT_ASSAULT.df, SEXUAL_ASSAULT.df) %>%
  as.data.frame()

CRIME.df <- CRIME.df %>% 
  left_join(pop)

CRIME.df <- CRIME.df %>% 
  mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value, variablename)

CRIME.df <- CRIME.df %>% 
  mutate(value = round(value,3))

CRIME_CLEANED.df <- CRIME.df %>% 
  group_by(iso3c, year,variablename) %>% 
  summarise(value=sum(value)) %>% ungroup()

rm(CRIME.df, violent.assault.tmp, violent.assault_missing, violent.assault_missing_tmp, violent.assault_regional_average, sexual.assault.tmp, sexual.assault_missing,
   sexual.assault_missing_tmp, sexual.assault_regional_average, tmp, fear.region.average)

CRIME_CLEANED.df <- CRIME_CLEANED.df %>%
  spread(variablename,value)


# Small Arms ==================================================================================================================
# Small arms 
# This pulls data from the Small arms data frame 
# The raw data does not have the three letter iso code, so the code adds the iso3c column
# The code then combines it with the gpi grid data frame and completes the data frame with all  the GPI countries



Small_Arms.df <- SMALL_ARMS %>%  
  mutate(value = value * 1000000)

Small_Arms.df <- Small_Arms.df %>% 
  mutate(year=as.numeric(as.character(gsub("X","", year)))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) %>%
  mutate (year = year + SMALL_ARMS_YEAR)

Small_Arms.df <- gpi.grid %>% 
  left_join(Small_Arms.df) %>% 
  select (-Country) 

Small_Arms.df <- Small_Arms.df %>%
  rename (geocode = iso3c) %>% 
  dplyr::filter (complete.cases(value)) 


country_year <- Small_Arms.df %>%
  distinct(geocode) %>%
  expand(geocode, year = c(2008, 2009)) %>%
  mutate(value = NA)


Small_Arms.df <- Small_Arms.df %>% 
  rbind (country_year) %>% 
  mutate (variablename = "small arms")

rm (country_year)

Small_Arms.df <- f_index_data_pad(Small_Arms.df)

Small_Arms.df <- Small_Arms.df %>% 
  select (geocode, year, imputed) %>% 
  rename (iso3c = geocode, sarms = imputed) %>%
  right_join(gpi.grid) %>%
  mutate (sarms = case_when(is.na(sarms) ~ 0,
                            TRUE ~ sarms ))



# Security Agency Costs ================================================================================================== 


# Security agency data is drawn from the security agency data frame
# The raw data set has 3 columns (country, year, secu. agency)
# the code then adds the iso country code column
# We then combine it with the gpi grid data frame
# the code then assigns all the missing values with corresponding regional peace averages to complete the data frame

Secu_Agency.df <- SECU_AGENCY %>%
  mutate(X2007=X2008, X2019=X2018,X2020=X2018,X2021=X2018, X2022=X2018, X2023=X2018)%>%
  mutate(iso3c=countrycode(country,"country.name","iso3c")) %>%
  gather(year, secu.agency, -c(country, iso3c))%>% 
  mutate(year=as.numeric(as.character(gsub("X","",year))))

Secu_Agency.df <- Secu_Agency.df %>%
  mutate(iso3c = if_else(country == "Kosovo", "KSV", iso3c))

Secu_Agency.df <- Secu_Agency.df %>%
  filter(iso3c %in% pos)

Secu_Agency.df <- Secu_Agency.df %>%
  select(iso3c, year, secu.agency)

Secu_Agency.df <- gpi.grid %>% 
  left_join(Secu_Agency.df)

# using regional average for South Sudan


SECU_AGENCY_REGION_AVERAGE.df <- Secu_Agency.df %>%  
  left_join(Peace_and_region) %>% 
  select (-c (`peace_level`))

SECU_AGENCY_REGION_AVERAGE.df <- SECU_AGENCY_REGION_AVERAGE.df %>% 
  group_by(region,year) %>%
  summarise(average=mean(secu.agency, na.rm=T))

Secu_Agency.df <- Secu_Agency.df %>% 
  left_join(Peace_and_region, by = "iso3c")

Secu_Agency.df <- Secu_Agency.df %>% 
  left_join(SECU_AGENCY_REGION_AVERAGE.df, by = c("region", "year"))

Secu_Agency.df <- Secu_Agency.df %>% 
  mutate (secu.agency = coalesce(secu.agency, average)) %>% 
  select (c(`year`, `iso3c`, `secu.agency`))



# Peacebuilding ================================================================================================================

# This code filters the raw data set to only 'Official development assistance'.
# It is then additionally filtered for 'Gross Disbursements'
# The code then aggregates the peace building funding for each country
# The code then completes the data frame with all the GPI countries by left joining with the gpi grid data frame
# This creates NA's for some countries and those are set to zero


Peacebuilding.df <- PEACEBUILDING %>%   
  mutate(iso3c=countrycode(Recipient,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Recipient=="Kosovo","KSV",iso3c))

Peacebuilding.df <- Peacebuilding.df %>%
  filter(iso3c %in% pos)

# Check that that coutnries are the ones we actually want

Country.List.df <- Peacebuilding.df %>%
  count(Recipient) %>%
  as.data.frame()

Peacebuilding_countries.df <- PEACEBUILDING_FUNDING

Peacebuilding_countries.df <- Peacebuilding_countries.df %>%  
  mutate(iso3c=countrycode(Countries,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Countries=="Kosovo","KSV",iso3c)) %>%
  distinct(iso3c) %>%
  mutate(country=countrycode(iso3c,"iso3c", "country.name")) %>% 
  mutate(country=ifelse(iso3c=="KSV","Kosovo",country))

Peacebuilding.df <- Peacebuilding_countries.df %>%
  left_join(Peacebuilding.df, by = "iso3c")

Peacebuilding.df <- Peacebuilding.df %>% 
  subset(Donor=="Official Donors, Total") %>% 
  subset(Channel=="All Channels") %>% 
  subset(Flow=="Official Development Assistance") %>% 
  subset(Type.of.aid=="All Types, Total") %>% 
  subset(Value>0) %>% 
  subset(Amount.type=="Current Prices") %>% 
  subset(SECTOR=c("15210","15240","15250","15261","15230","15220","15130","15152","15113","15150","15153","15160","15170","15110","15111","15112","15151")) %>% 
  subset(Flow.type=="Gross Disbursements") 

Peacebuilding.df <- Peacebuilding.df %>%
  mutate(Year = Year+PEACEBUILDING_YEAR) %>% 
  group_by(Year, iso3c, Recipient) %>% 
  summarise(value=sum(Value, na.rm = TRUE)) %>% 
  mutate(variablename = "peacebuilding") %>%
  rename(country=Recipient) %>%
  rename(year = "Year") 

Peacebuilding.df <- Peacebuilding.df %>%
  mutate(value = value * 10^6)

Peacebuilding_Cleaned.df <- Peacebuilding.df %>% 
  select(iso3c,year,value) %>% 
  rename(peacebuilding=value) %>% 
  subset(year > 2006)


Peacebuilding_Cleaned.df <- gpi.grid %>% 
  left_join(Peacebuilding_Cleaned.df) 

Peacebuilding_Cleaned.df <- Peacebuilding_Cleaned.df %>%
  mutate (peacebuilding = case_when(is.na(peacebuilding) ~ 0,
                                    TRUE ~ peacebuilding ))

rm(Country.List.df)

rm(Peacebuilding_countries.df)


# Private Security ================================================================================================================

# This code pulls private security data from 2007 and 2022
# The code then has adds the column for the latest year
# The code then corrects the names for country codes
# The code then completes the data frame by combining it with the population and multiply it the rate
# This is then combined with the gpi grid data frame.


f_LibraryLoader(tidyverse,rio)

Private_Secu.df <- PRIVATE_SECURITY %>% 
  select(-`...1`) %>%
  gather(year,value, -c("country","iso3c")) %>% 
  select(-country)

Private_Secu.df <- Private_Secu.df %>%
  mutate(year = as.numeric(as.character(year)))

Latest_Year <- Private_Secu.df %>%
  summarise(latest_year = max(year)) %>%
  pull(latest_year)


data_new <- Private_Secu.df %>% 
  filter(year == Latest_Year) %>%
  mutate(year = Latest_Year+1)

# Combine the original dataset with the new 2023 data

Private_Secu.df <- bind_rows(Private_Secu.df, data_new)

Private_Secu.df <- Private_Secu.df %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>%
  mutate(year=as.numeric(as.character(gsub(",","",year)))) 

Private_Secu.df <- Private_Secu.df %>%
  mutate(iso3c = if_else(iso3c == "Kosovo", "KSV", iso3c))

#Add in population

Population.df <- pop %>% 
  select(iso3c, year, population)

Private_Secu.df <- left_join(Private_Secu.df, Population.df) %>%
  mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value) 

Private_Secu.df <- Private_Secu.df %>%
  full_join(gpi.grid, by=c("iso3c", "year")) %>%
  rename(priv.secu = value)



# Internal security  =================================================================================================

# Internal security data is pulled and is filtered for expenditures for public order and safety
# public expenditures are rounded to one decimal place 
# The data set for public expenditures and safety is then combined with the gpi grid to complete the data frame
# The code then creates a loops through which counts the number of missing countries and determine which ones to keep and which ones to delete
# It then creates a data frame of missing countries called "count_of_missing". 
# This data frame is then left-joined to the main data frame
# The code then imputes the missing countries which have more than 1 value
# The next step is to multiply the value (% of GDP spent on public order and safety) with the GDP current. (This will be converted to constant later)
# This gives us the dollar amount spent on public order and safety.
# This is however only for 67 countries, therefore there are 96 countries missing.
# To fill this in we use the police rate from the GPI data 
# The police rate data is then multiplied by the unit cost for police direct costs
# This is then combined with the original data frame to complete the data frame for 163 countries. 

Internal_Exp.df <- INTERNAL_SECURITY %>%
  rename(variable.name="COFOG Function", country="Country")

Internal_Exp.df <- Internal_Exp.df %>% 
  gather(year, value, -c(country, variable.name)) %>% 
  mutate(value=as.numeric(as.character(value))) %>% 
  mutate(year=as.numeric(as.character(year))) %>%  
  mutate(year=year+1) 

Internal_Exp.df <- Internal_Exp.df %>%  
  mutate(iso3c=countrycode(country,"country.name","iso3c"))

Internal_Exp.df <- Internal_Exp.df %>%
  mutate(iso3c = if_else(country == "Kosovo, Republic of", "KSV", iso3c))

Internal_Exp.df <- Internal_Exp.df %>%
  mutate(iso3c = if_else(country == "Eswatini, Kingdom of", "SWZ", iso3c))

Internal_Exp.df <- Internal_Exp.df %>%
  filter(iso3c %in% pos)

Internal_Exp.df <- Internal_Exp.df %>% 
  select(iso3c, year, value, country) %>%
  mutate(variablename="public order and safety") %>%
  subset(!(year<2005))

Internal_Exp.df <- Internal_Exp.df %>% 
  mutate(value=round(value,1))

GPI_GRID_UPDATE.df <- gpi.grid %>% 
  mutate(count=1) %>% 
  spread(year,count) %>% 
  gather(year, count, -iso3c) %>% 
  select(year,iso3c) %>% 
  mutate(year=as.numeric(as.character(year)))

#work out who is missing data

Internal_Exp.df <- GPI_GRID_UPDATE.df %>%
  left_join(Internal_Exp.df)

missing2 <- function(df){
  left_join(GPI_GRID_UPDATE.df,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}

### MISSING IS USING WRONG GRID


MISSING.df <- missing2(Internal_Exp.df)

COUNT_OF_MISSING.df <- as.data.frame(table(MISSING.df$iso3c))

COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>%  mutate(Freq=as.numeric(Freq))

# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 


for(c in 1:nrow(COUNT_OF_MISSING.df)){
  if(COUNT_OF_MISSING.df[c,"Var1"]=="PSE" & COUNT_OF_MISSING.df[c,"Freq"]==PANEL_PSE) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Var1"]=="SSD" & COUNT_OF_MISSING.df[c,"Freq"]==PANEL_SSD) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else if(COUNT_OF_MISSING.df[c,"Freq"]==PANEL_FREQ) {
    COUNT_OF_MISSING.df[c,"new"]<-"delete"}
  else {COUNT_OF_MISSING.df[c,"new"] <- "keep"}
}

COUNT_OF_MISSING.df <- COUNT_OF_MISSING.df %>% 
  rename(iso3c=Var1)

Internal_Exp.df <- Internal_Exp.df %>% 
  left_join(COUNT_OF_MISSING.df)

#delete out all that dont have at least one variable!  

Internal_Exp.df <- Internal_Exp.df %>% 
  subset(!new=="delete")

# We can now impute given what remains has at least one value


Internal_Exp.df <- Internal_Exp.df %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% 
  mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% 
  mutate(variablename="public order and safety")

Internal_Exp.df <- f_index_data_pad(Internal_Exp.df)

Internal_Exp.df <- Internal_Exp.df %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

Internal_Exp.df <- Internal_Exp.df %>%
  filter(year > 2006)

Internal_Exp.df <- Internal_Exp.df %>%
  filter(iso3c %in% pos)

Internal_Exp.df <- Internal_Exp.df %>%
  filter(!(year < 2006))

#use gdp current not the constant calc, you'll apply constant later 


Internal_Exp.df <- Internal_Exp.df %>% 
  merge(gdp.wdi[,c("iso3c","year","gdp")],by=c("iso3c","year")) %>% 
  mutate(value=value/100) %>%
  mutate(intsecu=value*gdp) %>%
  distinct()

Internal_Exp.df <- Internal_Exp.df %>%
  select(iso3c, year, intsecu)

Internal_Exp_new.df <- gpi.grid %>% 
  left_join(Internal_Exp.df)

Internal_Exp_new.df <- Internal_Exp_new.df %>%
  subset(is.na(intsecu))

INTERNAL_SECU <- Internal_Exp_new.df %>%
  distinct(iso3c) %>%
  pull(iso3c)


# FOR MISSING Data ======================================================


GPI_DATA.df <- GPI_DASHBOARD %>%
  select (c(`country`, `geocode`, `region`, `government`, `year`, `indicator`, `type`, `value`)) %>%
  mutate (year = year - 1) %>%
  subset(type=="raw") %>%
  rename(iso3c=geocode, govt = government) %>%
  subset(select=-type) %>%
  select(-region) %>%select(-govt) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  subset(!year==LATEST_YEAR)


# GPI countries 

POLICE.df <- GPI_DATA.df %>%
  filter(indicator == "police rate")

POLICE.df <- POLICE.df %>%
  mutate(value = as.numeric(as.character(value)))

POLICE.df <- POLICE.df %>%  
  select(country, iso3c, year, value)

POPULATION.df <- pop %>% 
  select(year,iso3c,population)

POLICE.df <- POLICE.df %>%
  full_join(POPULATION.df, by=c("iso3c", "year")) %>%
  rename(pop=population) %>% 
  mutate(pop100k=pop/10^5) %>% 
  mutate(value=value*pop100k) %>% 
  merge(UnitCost.Scaled.df[,c("iso3c", "year", "police.direct")], by=c("iso3c", "year"))%>%
  mutate(police.cost=value*police.direct) %>%
  select(iso3c, country, year, police.cost)

POLICE.df <- POLICE.df %>%
  filter(iso3c %in% INTERNAL_SECU.df)

POLICE.df <- POLICE.df %>%
  select(iso3c, year, police.cost)

POLICE.df <- POLICE.df %>%
  rename(intsecu = police.cost)

Internal_Exp_new.df <- Internal_Exp_new.df  %>%  
  select(year, iso3c) %>% 
  left_join(POLICE.df) %>% 
  select(year, iso3c, intsecu)

Internal_Exp.df <- Internal_Exp.df %>% 
  rbind(Internal_Exp_new.df)

Internal_Exp.df <- gpi.grid %>%
  left_join(Internal_Exp.df)

Internal_Exp_Count.df <- Internal_Exp.df %>%
  rename(value = intsecu)

Internal_Exp_Count.df <- Internal_Exp_Count.df %>%
  distinct()


count <- Internal_Exp_Count.df %>% 
  group_by(iso3c) %>% 
  tally()

# imputation

Internal_Exp.df <- Internal_Exp.df %>% 
  rename (geocode = iso3c, value = intsecu) %>% 
  mutate (variablename = "internal security")

Internal_Exp.df <- f_index_data_pad(Internal_Exp.df)

Internal_Exp.df <- Internal_Exp.df %>% 
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, intsecu = imputed)

Internal_Exp.df <- gpi.grid %>%
  left_join(Internal_Exp.df)


# Terrorism Deaths ==========================================================================================

# This code pulls terrorism deaths from the main GPI data frame
# It filters for only those who were killed
# it combines this data frame with the gpi grid to complete the data frame
# All countries that have na are now filled in with 0


GTI_SUM.df <- GPI_DATA %>%
  dplyr::select(c(`geocode`, `year`, `terrorism_deaths`)) %>%
  rename(iso3c = geocode) %>%
  rename(killed = terrorism_deaths)

GTI_SUM.df <- gpi.grid %>% 
  left_join(GTI_SUM.df)

GTI_SUM.df <- GTI_SUM.df %>%
  mutate (killed = case_when (is.na(killed) ~ 0,
                              TRUE ~ killed))


# Peacekeeping Data =================================================================================================

# Peacekeeping data is drawn from the main GPI data set
# It is then combined with gpi grid to include all gpi countries
# The missing country data is then filled in through imputation


PEACEKEEPING.df <- GPI_DATA %>% 
  dplyr::select (`geocode`, `year`, `assessments`) %>%
  rename (peacekeep = assessments, iso3c = geocode)

PEACEKEEPING.df <- gpi.grid %>% 
  left_join(PEACEKEEPING.df)

PEACEKEEPING.df <- PEACEKEEPING.df  %>% 
  mutate (variablename = "peacekeeping") %>%
  rename (geocode = iso3c, value = peacekeep) 

PEACEKEEPING.df <- f_index_data_pad(PEACEKEEPING.df)

PEACEKEEPING.df <- PEACEKEEPING.df %>% 
  select (c(`geocode`,`year`,`imputed`)) %>% 
  rename (iso3c = geocode, peacekeep = imputed)

PEACEKEEPING.df <- gpi.grid %>% 
  left_join(PEACEKEEPING.df)



# GPI data ==================================================================================================================

# The code is to create a gpi data frame which includes only homicide, terrorism deaths, military expenditure, in peacekeeping funding, refugees and idps, perception of criminality
# This is then later used for creating individual data frames


GPI_DATA.df <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, `battle_deaths`, `homicides`, `incarceration`, `milex`, `fear`, `terrorism_deaths`, `displaced`, `assessments`),
                        names_to = "element", 
                        values_to = "value")
GPI_DATA.df <- GPI_DATA.df %>% 
  rename(iso3c=geocode, indicator = element)

GPI_DATA.df <- GPI_DATA.df %>% 
  mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                 indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                 indicator == "Fear % population" ~ "perceptions of criminality",
                                 indicator == "homicides" ~ "homicide rate",
                                 indicator == "Refugees and IDPs" ~ "refugees and idps",
                                 indicator == "incarceration" ~ "incarceration rate",
                                 indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                 TRUE ~ indicator))


# Homicide ================================================================================================================

# This code now looks at homicide data from the above gpi data
# This data is completed using gpi grid data and missing or na values are imputed


HOMICIDE.df <- GPI_DATA.df %>% 
  subset(indicator=="homicide rate") %>%
  dplyr::select(c(`iso3c`, `year`, `indicator`, `value`))

HOMICIDE.df <- gpi.grid %>% 
  left_join(HOMICIDE.df)

HOMICIDE.df <- HOMICIDE.df %>% 
  rename (geocode = iso3c, 
          variablename = indicator) %>% 
  mutate (variablename = "homicide rate") 

HOMICIDE.df <- f_index_data_pad(HOMICIDE.df)

HOMICIDE.df <- HOMICIDE.df %>%
  select (`geocode`, `year`, `variablename`, `imputed`) %>% 
  rename (iso3c = geocode, value = imputed, indicator = variablename)

HOMICIDE.df <- gpi.grid %>%
  left_join(HOMICIDE.df) %>%
  rename(homicide = value)


# incarceration rate =========================================================================================================

# Similiar to the homicide data frame, incarceration data is drawn from the gpi data set
# Na values are imputed after the gpi grid data is combined. 


INCARCERATION.df <- GPI_DATA.df %>% 
  subset(indicator=="incarceration rate") %>%
  dplyr::select(c(`iso3c`, `year`, `indicator`, `value`))

INCARCERATION.df <- gpi.grid %>% 
  left_join(INCARCERATION.df, by = c("iso3c", "year"))

INCARCERATION.df <- INCARCERATION.df %>% 
  rename (geocode = iso3c, variablename = indicator) %>% 
  mutate (variablename = "incarceration rate") 

INCARCERATION.df <- f_index_data_pad(INCARCERATION.df)

INCARCERATION.df <- INCARCERATION.df %>% 
  select (`geocode`, `year`, `variablename`, `imputed`) %>% 
  rename (iso3c = geocode, value = imputed, indicator = variablename)

INCARCERATION.df <- gpi.grid %>% 
  left_join(INCARCERATION.df, by = c("iso3c", "year")) %>%
  dplyr::select(-c(`indicator`)) %>%
  rename(incar = value)


# conflcit deaths ===================================================================================================

# This conflict data set is drawn from the gpi data set
# It completes the data frame for all 163 countries by combining it with gpi grid data 
# Missing na values are imputed to fill the entire data frame


CONFLICT.df <- GPI_DATA.df %>% 
  subset(indicator== "battle_deaths")

CONFLICT.df <- gpi.grid %>% 
  left_join(CONFLICT.df)

CONFLICT.df <- CONFLICT.df %>% 
  dplyr::select(c(`year`, `iso3c`, `indicator`, `value`))

CONFLICT.df <- CONFLICT.df  %>% 
  rename (geocode = iso3c, variablename = indicator) 

CONFLICT.df <- CONFLICT.df %>%
  mutate(variablename = if_else(is.na(variablename), "battle_deaths", variablename))

CONFLICT.df <- f_index_data_pad(CONFLICT.df)

CONFLICT.df <- CONFLICT.df %>% 
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, battle_deaths = imputed)

CONFLICT.df <- gpi.grid %>% 
  left_join(CONFLICT.df)


# refugees and IDPs ==============================================================================================================

# We start by creating a population data frame that we will use later
# The refugee data is pulled from the gpi data set
# We combine the refugee numbers with gdp constant per capita to get the cost of refugees and idps 
# We make a few assumptions here (40% of refugees are children) and (10% resettlement rate)
# Once we get the refugee costs, we include all GPI countries
# All NA values are then imputed to complete the data frame


POPULATION.df <- pop %>% 
  rename(pop=population)

REFUGEE.df <- GPI_DATA.df %>% 
  subset(indicator=="displaced" ) %>% 
  merge(POPULATION.df[,c("iso3c","year","pop")], by=c("iso3c", "year")) %>%  
  subset(select=c(iso3c,year,indicator,value)) %>% 
  rename(refug=value)

REFUGEE.df <- REFUGEE.df %>%
  left_join(gdp.pc.constant %>% 
              select(iso3c, year, gdp.pc.cons), by = c("iso3c", "year"))

# I multiply this my 0.6 as 40% of the refugee population are children and therefore not really contributing to GDP pc
# Note we have opted to a 10% resettlement rate.

REFUGEE.df <- REFUGEE.df %>%
  mutate(refugeidp = refug * gdp.pc.cons * (1 - 0.1))

REFUGEE.df <- REFUGEE.df %>%   
  subset(select=c(iso3c,year,refugeidp)) 
# Imputation

REFUGEE.df <- REFUGEE.df %>% 
  rename (geocode = iso3c, value = refugeidp) %>% 
  mutate (variablename = "refugee")  

REFUGEE.df <- f_index_data_pad(REFUGEE.df)

REFUGEE.df <- REFUGEE.df %>% 
  select (`geocode`, `year`,`imputed`) %>% 
  rename (iso3c = geocode, refugeidp = imputed)

REFUGEE.df <- gpi.grid %>% 
  left_join(REFUGEE.df)


# GDP losses for countries with >1000 deaths ------------------------------

# This code starts with creating a new data frame from the conflict data frame
# The first chunk of code initializes a binary column, where it assigns a value of 1 for battle deaths over 999 and 0 for battle deaths under 999
# For all deaths the are over 999, the deaths are multiplied by constant gdp
# finally the code filters for year after 2007 to the latest year.
# Lastly the data frame is completed with the gpi grid data and na values are imputed.


GDP_LOSSES.df <- CONFLICT.df %>% 
  mutate(conflict=ifelse(battle_deaths>999,1,0)) %>%
  merge(gdp.wdi[,c("iso3c","year","gdpcons")], by=c("iso3c","year"), all=TRUE) %>%
  mutate(gdplosses=ifelse(conflict==1,gdpcons*0.022,0)) %>% 
  subset(select=c(iso3c,year,gdplosses))

GDP_LOSSES.df <- GDP_LOSSES.df %>%
  filter(!(year < 2007))

GDP_LOSSES.df <- GDP_LOSSES.df %>%
  filter(!(year > LATEST_YEAR))

# imputation


GDP_LOSSES.df <- GDP_LOSSES.df %>% 
  rename (geocode = iso3c, value = gdplosses) %>%
  mutate (variablename = "gdp losses")

GDP_LOSSES.df <- f_index_data_pad(GDP_LOSSES.df)

GDP_LOSSES.df <- GDP_LOSSES.df %>%
  select (c(`geocode`, `year`,`imputed`)) %>%
  rename (iso3c = geocode, gdplosses = imputed)

GDP_LOSSES.df <- gpi.grid %>%
  left_join(GDP_LOSSES.df)



# GPI Data =========================================================================================================


# We go back to re creating the gpi data to what it looked like before by filtering to include homicide, terrorism deaths, military expenditure, in peacekeeping funding, refugees and idps, perception of criminality



GPI_DATA.df <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, `battle_deaths`, `homicides`, `incarceration`, `milex`, `fear`, `terrorism_deaths`, `displaced`, `assessments`),
                        names_to = "element",
                        values_to = "value")

GPI_DATA.df <- GPI_DATA.df %>% 
  rename(iso3c=geocode, indicator = element)

GPI_DATA.df <- GPI_DATA.df %>% 
  mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "milex" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "Homicides per 100,000" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))


# Military expenditure ===========================================================================================

# We use two different data sources to complete the military data frame
# We use the GPI data to get military exp as a % of gdp for years prior to 2022
# We use Sipri data to get military exp as a % of gdp after 2022
# These two data frames are combined along with the gpi grid data frame to get a complete data frame for all countries and all years
# All na values are then imputed


MILITARY_EXP.df <- GPI_DATA.df %>% 
  subset(indicator=="military expenditure (% gdp)") %>% 
  mutate(value = value /100) %>%
  dplyr::filter (year < 2022) %>%
  dplyr::select(-c(`country`, `peace_level`, `region`))


# Data for 2022 from SIPRI

MILITARY_EXP_23.df <- SIPRI_MILEX %>%
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

MILITARY_EXP.df <- MILITARY_EXP.df %>%
  rbind (MILITARY_EXP_23.df)


MILITARY_EXP.df <- gpi.grid %>% 
  left_join(MILITARY_EXP.df)

MILITARY_EXP.df <- MILITARY_EXP.df %>% 
  rename (geocode = iso3c, variablename = indicator)

MILITARY_EXP.df <- MILITARY_EXP.df %>% 
  group_by(geocode) %>%
  fill(value, .direction = "downup")

MILITARY_EXP.df <- MILITARY_EXP.df %>% 
  select (c(`year`, `geocode`, `value`)) %>% 
  rename (iso3c = geocode, milex = value)

MILITARY_EXP.df <- gpi.grid %>% 
  left_join(MILITARY_EXP.df)

rm(MILITARY_EXP_23.df)
