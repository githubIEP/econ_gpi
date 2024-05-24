# Unit Costs
# This is the unit cost data frame.
# The unit costs are then scaled for all countries and for all years by using the PPP scale

unitcost <- UNIT_COST
unitcost <- within(unitcost, indicator <- paste(Indicator, type,sep='.'))
unitcost <- unitcost[,c("indicator", "unitcost")]
unitcost <- spread(unitcost, indicator, unitcost)
unitcost2 <- cbind(ppp, unitcost)
unitcost.scaled <- mutate_at(unitcost2,vars(`fear.indirect`:`violentassault.indirect`), funs(.*scale) )
unitcost.scaled <- unitcost.scaled[,c(2,1,4:15)]

#VET AFFAIRS ===========================================================================================================

# This code downloads the vet affairs data frame from the white house website
# The code then filters for Total veterans benefits and services.
# The code then gives the total amount and estimates fro 1976 to 2027
# The code also adds interest to the veteran costs. 
# Finally we combine this vet data frame with the GPI grid data frame to include all the GPI countries
# All GPI countries except for USA are set to 0. 


temp = tempfile(fileext = ".xlsx")
dataURL <- WHITE_HOUSE
download.file(dataURL, destfile=temp, mode='wb')
test <- readxl::read_excel(temp, skip =2)
test <-test %>%  
  subset(`Function and Subfunction`=="Total, Veterans Benefits and Services") %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`, `2026`=`2026 estimate`, `2027`=`2027 estimate`) %>% 
  gather(year, value, -c(`Function and Subfunction`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6)
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
  rename(vet = value) %>%
  dplyr::select(-c(`variablename`))



# Suicide ===============================================================================================================

# This code downloads suicide data from the World Bank
# This is then combined with the GPI grid data frame to include all GPI countries to complete the data frame.
# The code then counts the number NAs in the data frame
# The code then imputes the missing values with last available data, as well as using regional peace averages


suicide <- f_get.wdi("all","SH.STA.SUIC.P5",2006,LATEST_YEAR) %>% 
  mutate (year = year + 1) %>%
  rename(rate=SH.STA.SUIC.P5) %>% 
  rename(value=rate) %>% 
  mutate(variablename="suicide_rate") %>%
  subset(!iso3c=="PSE") %>%
  subset(!iso3c=="KSV") %>%
  subset(!iso3c=="TWN")
suicide <- left_join(gpi.grid, suicide)
suicide %>%
  group_by(iso3c) %>%
  summarize(count = sum(is.na(value))) %>%
  arrange(desc(count))
suicide <- suicide %>%
  rename(geocode=iso3c) %>%
  mutate(variablename = "suicide rate") %>% 
  subset(!geocode %in% c("KSV", "PSE", "TWN"))
suicide <- f_index_data_pad(suicide)
suicide <- suicide %>%
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)
suicide <- gpi.grid %>% 
  left_join(suicide) %>%
  mutate (variablename = "suicide rate")
suicide.region.average <- suicide %>%
  left_join(Peace_and_region) %>%
  select (-c (`peace_level`))
suicide.region.average <- suicide.region.average %>%
  group_by(region,year) %>%
  summarise(average=mean(value, na.rm=T))
suicide <- suicide %>% 
  left_join(Peace_and_region, by = "iso3c")
suicide <- suicide   %>% 
  left_join(suicide.region.average, by = c("region", "year"))
suicide <- suicide %>% 
  mutate (value = coalesce(value, average)) %>%
  select (c(`year`:`value`))
rm(suicide.region.average, suicide_GBD)
suicide <- suicide %>% 
  left_join(pop) %>%  
  distinct() %>% 
  mutate(value=(population/100000*value)) %>%
  select(-population) 
suicide <- suicide %>% 
  mutate(variablename="suicide_count") %>% 
  select(iso3c,value, year) %>% 
  rename(suicidevalue=value)



# This code pull the data from the GPI data set, which is the perceptions of criminality
# This code then fills the data set with all the GPI countries and years completing the data frame
# The dataset is complete but has NAs
# These NA's are then filled in through imputation using the fill() function
 

fear <- gpidata %>%
  subset(indicator=="perceptions of criminality") %>%   rename_all(tolower) %>%
  select("iso3c", "year", "value")  %>%
  subset(!year==LATEST_YEAR+1) %>%
  mutate(variablename="fear")
fear <- gpi.grid %>%
  left_join(fear) %>% 
  rename (geocode = iso3c)
fear <- fear %>%
  group_by(geocode) %>%
  fill(value, .direction = "downup")
fear <- fear %>% 
  rename(iso3c = geocode) %>% rename(fear = value)


# This gathers UNHCR funding from the UNHCR data set. 
# The UNHCR data set is combined with UNHCR data from 2021. 
# Once these are combined then the GPI grid data frame is added to it
# This completes data set, and fills in the NA values through imputation. 

########### UNHCR funding #########
unhcr <- UNHCR %>%
  gather(year,unhcr,-iso3c) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year))))
unhcr <- unhcr %>% 
  mutate(unhcr=as.numeric(as.character(unhcr)))
############ ADD IN 2021 ############
UNHCR_2021$iso3c = countrycode::countrycode(UNHCR_2021$country, "country.name", "iso3c")
UNHCR_2021$iso3c[UNHCR_2021$iso3c=="Kosovo"] <- "KSV"
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

violent.assault <- VIOLENT_ASSAULT %>% 
  rename(country=Country, year=Year, value=Rate) %>%
  select(country, year, value) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% 
  subset(year>2006)  %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)")
violent.assault = violent.assault %>% 
  mutate(value = round(value,2))
violent.assault$iso3c <- countrycode::countrycode(violent.assault$country, "country.name","iso3c")
violent.assault$iso3c[violent.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
violent.assault$country[violent.assault$iso3c=="KSV"] <- "Kosovo"
violent.assault$variablename <- "violent assault"
violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]
violent.assault <- gpi.grid %>% left_join(violent.assault)
tmp <- f_missing(violent.assault)
count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))
for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==8) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==PANEL_SSD) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==PANEL_FREQ) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}
count_of_missing <- count_of_missing %>% 
  rename(iso3c=Var1)
violent.assault <- violent.assault %>% 
  left_join(count_of_missing)
violent.assault <- violent.assault %>% 
  subset(!new=="delete")
violent.assault <- violent.assault %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="violent assault")
violent.assault <- f_index_data_pad(violent.assault)
violent.assault <- violent.assault %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)
## Average by peace and region
violent.assault.average <- violent.assault %>%  
  left_join(Peace_and_region)
violent.assault.average <- violent.assault.average %>% 
  group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))
# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
violent.assault.peace.level <- violent.assault %>%  
  left_join(Peace_and_region)
violent.assault.peace.level <- violent.assault.peace.level %>% 
  select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))
##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN
violent.assault <- gpi.grid %>% 
  left_join(violent.assault)
##### From who is missing allocate the average rate by peace level and region
tmp <- f_missing(violent.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% left_join(violent.assault.average) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="violent assault") %>% na.omit() 
#ADD to main DF
violent.assault <- violent.assault %>% 
  na.omit() %>% 
  rbind(tmp) 
##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
violent.assault <- gpi.grid %>% 
  left_join(violent.assault)
tmp <- f_missing(violent.assault)
#If data still does not have a match so I will take the average of the peace level #
tmp <- tmp %>%  
  left_join(Peace_and_region)
tmp <- tmp %>% 
  select(-variablename,-value)
tmp <- tmp %>% 
  left_join(violent.assault.peace.level) %>% 
  distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% 
  mutate(variablename="violent assault")
# The 'additional' missing data needs to be added in 
violent.assault <- violent.assault %>% 
  na.omit() %>% 
  rbind(tmp)
violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]
rm(violent.assault.average)
rm(violent.assault.peace.level)

#################### Sexual violence #####################################################################################

# Sexual assault data is pulled from the Sexual data frame
# And the same process and transformations to the data frame are done to this data set as the Violent assault data set


# Not in GPI data 
sexual.assault <- SEXUAL_ASSAULT %>% 
  rename(country=Country, year=Year, value="Measure Values", rate="Measure Names") %>% 
  select(country, year, value, rate) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% 
  subset(year>2006)  %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)") %>%
  subset(!rate=="Count")
sexual.assault = sexual.assault %>%
  mutate(value = round(value,2))
sexual.assault$iso3c <- countrycode::countrycode(sexual.assault$country, "country.name","iso3c")
sexual.assault$iso3c[sexual.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
sexual.assault$country[sexual.assault$iso3c=="KSV"] <- "Kosovo"
sexual.assault$variablename <- "sexual assault"
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]
### WORK OUT WHO IS MISSING
sexual.assault <- gpi.grid %>% 
  left_join(sexual.assault)
tmp <- f_missing(sexual.assault)
count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))
# This will tell us who to delete to impute (you will need to update the numbers here)
for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==10) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==PANEL_SSD) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==PANEL_FREQ) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}
count_of_missing <- count_of_missing %>% 
  rename(iso3c=Var1)
sexual.assault <- sexual.assault %>% 
  left_join(count_of_missing)
#delete out all that dont have at least one variable!  
sexual.assault <- sexual.assault %>% 
  subset(!new=="delete")
# We can now impute given what remains has at least one value
sexual.assault <- sexual.assault %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% 
  mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="sexual assault")
sexual.assault <- f_index_data_pad(sexual.assault)
sexual.assault <- sexual.assault %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)
## Average by peace and region
sexual.assault.average <- sexual.assault %>%  
  left_join(Peace_and_region)
sexual.assault.average <- sexual.assault.average %>% 
  group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))
# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
sexual.assault.peace.level <- sexual.assault %>%  
  left_join(Peace_and_region)
sexual.assault.peace.level <- sexual.assault.peace.level %>%
  select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))
##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN
sexual.assault <- gpi.grid %>% 
  left_join(sexual.assault)
##### From who is missing allocate the average rate by peace level and region
tmp <- f_missing(sexual.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% 
  left_join(sexual.assault.average) %>% 
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>% 
  mutate(variablename="sexual assault") %>%
  na.omit() 
#ADD to main DF
sexual.assault <- sexual.assault %>% 
  na.omit() %>% 
  rbind(tmp) 
##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
sexual.assault <- gpi.grid %>% 
  left_join(sexual.assault)
tmp <- f_missing(sexual.assault)
#If data still does not have a match so I will take the average of the peace level #
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% 
  left_join(sexual.assault.peace.level) %>%
  distinct() %>% 
  rename(value=average) %>%
  select(-peace_level)  %>% 
  select(-region) %>% 
  mutate(variablename="sexual assault")
# The 'additional' missing data needs to be added in 
sexual.assault <- sexual.assault %>% 
  na.omit() %>%
  rbind(tmp)
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]
rm(sexual.assault.average)
rm(sexual.assault.peace.level)
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]
##### TURN RATE TO COUNT FOR BOTH SEXUAL AND VIOLENT ASSAULT
crime <- as.data.frame(rbind(violent.assault, sexual.assault)) 
crime <- crime %>% 
  left_join(pop)
crime <- crime %>% 
  mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value, variablename)
crime = crime %>% 
  mutate(value = round(value,3))
crime2 <- crime %>% 
  group_by(iso3c, year,variablename) %>% 
  summarise(value=sum(value)) %>% ungroup()
rm(crime, violent.assault.tmp, violent.assault_missing, violent.assault_missing_tmp, violent.assault_regional_average, sexual.assault.tmp, sexual.assault_missing,
   sexual.assault_missing_tmp, sexual.assault_regional_average, tmp, fear.region.average)
crime2 <- crime2 %>%
  spread(variablename,value)


# Small Arms ==================================================================================================================
# Small arms 
# This pulls data from the Small arms data frame 
# The raw data does not have the three letter iso code, so the code adds the iso3c column
# The code then combines it with the gpi grid data frame and completes the data frame with all  the GPI countries
# 


small.arms <- SMALL_ARMS %>%  
  mutate(value = value * 1000000)
small.arms <- small.arms %>% mutate(year=as.numeric(as.character(gsub("X","", year)))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) %>%
  mutate (year = year + SMALL_ARMS_YEAR)
small.arms <- gpi.grid %>% 
  left_join(small.arms) %>% 
  select (-Country) 
small.arms <- small.arms %>%
  rename (geocode = iso3c) %>% 
  dplyr::filter (complete.cases(value)) 
country_year <- expand.grid(geocode = unique(small.arms$geocode), year = c(2008, 2009), value = NA) # adding back year 2008 and 2009
small.arms <- small.arms %>% 
  rbind (country_year) %>% 
  mutate (variablename = "small arms")
rm (country_year)
small.arms <- f_index_data_pad(small.arms)
small.arms <- small.arms %>% 
  select (geocode, year, imputed) %>% 
  rename (iso3c = geocode, sarms = imputed) %>%
  right_join(gpi.grid) %>%
  mutate (sarms = case_when(is.na(sarms) ~ 0,
                            TRUE ~ sarms ))

# Security agency data is drawn from the security agency data frame
# The raw data set has 3 columns (country, year, secu. agency)
# the code then adds the iso country code column
# We then combine it with the gpi grid data frame
# the code then assigns all the missing values with corresponding regional peace averages to complete the data frame

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

# Peacebuilding ================================================================================================================
# This code filters the raw data set to only 'Official development assistance'.
# It is then additionally filtered for 'Gross Disbursements'
# The code then aggregates the peace building funding for each country
# The code then completes the data frame with all the GPI countries by left joining with the gpi grid data frame
# This creates NA's for some countries and those are set to zero

pb <- PEACEBUILDING %>%   
  mutate(iso3c=countrycode(Recipient,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Recipient=="Kosovo","KSV",iso3c))
pb <- pb[pb$iso3c %in% pos,]
# Check that that coutnries are the ones we actually want
country.list <- as.data.frame(table(pb$Recipient))
rm(country.list)
pb.countries <- PEACEBUILDING_FUNDING
pb.countries <- pb.countries %>%  
  mutate(iso3c=countrycode(Countries,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Countries=="Kosovo","KSV",iso3c)) %>%
  distinct(iso3c) %>%
  mutate(country=countrycode(iso3c,"iso3c", "country.name")) %>% mutate(country=ifelse(iso3c=="KSV","Kosovo",country))
pb <- left_join(pb.countries,pb, by="iso3c" )
pb <- pb %>% subset(Donor=="Official Donors, Total") %>% 
  subset(Channel=="All Channels") %>% 
  subset(Flow=="Official Development Assistance") %>% 
  subset(Type.of.aid=="All Types, Total") %>% 
  subset(Value>0) %>% 
  subset(Amount.type=="Current Prices") %>% 
  subset(SECTOR=c("15210","15240","15250","15261","15230","15220","15130","15152","15113","15150","15153","15160","15170","15110","15111","15112","15151")) %>% 
  subset(Flow.type=="Gross Disbursements") 
pb <- pb %>% mutate(Year = Year+PEACEBUILDING_YEAR) %>% 
  group_by(Year, iso3c, Recipient) %>% 
  summarise(value=sum(Value, na.rm = TRUE)) %>% mutate(variablename = "peacebuilding") %>%
  rename(country=Recipient) %>%
  rename(year = "Year") 
pb$value <- pb$value*10^6
peacebuilding <- pb %>% 
  select(iso3c,year,value) %>% rename(peacebuilding=value) %>% 
  subset(year > 2006)
rm(pb.countries)
peacebuilding <- gpi.grid %>% 
  left_join(peacebuilding) 
peacebuilding <- peacebuilding %>%
  mutate (peacebuilding = case_when(is.na(peacebuilding) ~ 0,
                                    TRUE ~ peacebuilding ))


# Private Security ================================================================================================================

# This code pulls private security data from 2007 and 2022
# The code then has adds the column for the latest year
# The code then corrects the names for country codes
# The code then completes the data frame by combining it with the population and multiply it the rate
# This is then combined with the gpi grid data frame.


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
priv.secu$iso3c[priv.secu$iso3c=="Kosovo"] <- "KSV"
#Add in population
tmp <- pop %>% 
  select(iso3c, year, population)
priv.secu <- left_join(priv.secu, tmp) %>%
  mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value) 

priv.secu <- priv.secu %>%
  full_join(gpi.grid, by=c("iso3c", "year")) %>%
  rename(priv.secu = value)

# Internal security  ------------------------------------------------------

pos.exp <- INTERNAL_SECURITY %>%
  rename(variable.name="COFOG Function", country="Country")
pos.exp <- pos.exp %>% gather(year, value, -c(country, variable.name)) %>% 
  mutate(value=as.numeric(as.character(value))) %>% 
  mutate(year=as.numeric(as.character(year))) %>%  
  mutate(year=year+1) 
pos.exp <- pos.exp %>%  
  mutate(iso3c=countrycode(country,"country.name","iso3c"))
pos.exp$iso3c[pos.exp$country=="Kosovo, Republic of"] <- "KSV"
pos.exp$iso3c[pos.exp$country=="Eswatini, Kingdom of"] <- "SWZ"
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]
pos.exp <- pos.exp %>% 
  select(iso3c, year, value, country) %>%
  mutate(variablename="public order and safety") %>%
  subset(!(year<2005))
pos.exp <- pos.exp %>% 
  mutate(value=round(value,1))
gpi.grid.tmp <- gpi.grid %>% 
  mutate(count=1) %>% 
  spread(year,count) %>% 
  gather(year, count, -iso3c) %>% 
  select(year,iso3c) %>% 
  mutate(year=as.numeric(as.character(year)))
#work out who is missing data
pos.exp <- gpi.grid.tmp %>%
  left_join(pos.exp)
missing2 <- function(df){
  left_join(gpi.grid.tmp,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}
### MISSING IS USING WRONG GRID
tmp <- missing2(pos.exp)
count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))
# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 
for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==PANEL_PSE) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==PANEL_SSD) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==PANEL_FREQ) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}
count_of_missing <- count_of_missing %>% 
  rename(iso3c=Var1)
pos.exp <- pos.exp %>% 
  left_join(count_of_missing)
#delete out all that dont have at least one variable!  
pos.exp <- pos.exp %>% 
  subset(!new=="delete")
# We can now impute given what remains has at least one value
pos.exp <- pos.exp %>% 
  select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% 
  mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% 
  mutate(variablename="public order and safety")
pos.exp <- f_index_data_pad(pos.exp)
pos.exp <- pos.exp %>% 
  select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)
pos.exp <- pos.exp[pos.exp$year>2006,]
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]
pos.exp <- subset(pos.exp,!(year<2006))
#use gdp current not the constant calc, you'll apply constant later 
 pos.exp <- pos.exp %>% 
   merge(gdp.wdi[,c("iso3c","year","gdp")],by=c("iso3c","year")) %>% 
   mutate(value=value/100) %>%
   mutate(intsecu=value*gdp) %>%
   distinct()
pos.exp <- subset(pos.exp, select = c(iso3c,year,intsecu))
pos.exp1 <- gpi.grid %>% 
  left_join(pos.exp)
pos.exp1 <- pos.exp1 %>%
  subset(is.na(intsecu))
pos.intsec <- unique(pos.exp1$iso3c)
#########################################################   FOR MISSING Data    ################################################
gpidata <- GPI_DASHBOARD %>%
  select (c(`country`, `geocode`, `region`, `government`, `year`, `indicator`, `type`, `value`)) %>%
  mutate (year = year - 1) %>%
  subset(type=="raw") %>%
  rename(iso3c=geocode, govt = government) %>%
  subset(select=-type) %>%
  select(-region) %>%select(-govt) %>%
  mutate(value=as.numeric(as.character(value))) %>%
  subset(!year==LATEST_YEAR)
# GPI countries 
police <- subset(gpidata, indicator=="police rate" ) 
police$value <- as.numeric(as.character(police$value))
police <- police %>%  
  select(country, iso3c, year, value)
tmp <- pop %>% 
  select(year,iso3c,population)
police <- police %>%
  full_join(tmp, by=c("iso3c", "year")) %>%
  rename(pop=population) %>% 
  mutate(pop100k=pop/10^5) %>% 
  mutate(value=value*pop100k) %>% 
  merge(unitcost.scaled[,c("iso3c", "year", "police.direct")], by=c("iso3c", "year"))%>%
  mutate(police.cost=value*police.direct)%>% select(iso3c, country, year, police.cost)
police <- police[police$iso3c %in%pos.intsec, ]
police <- subset(police, select = c(iso3c,year,police.cost))
police <- rename(police,intsecu =police.cost)
pos.exp1 <- pos.exp1  %>%  
  select(year, iso3c) %>% 
  left_join(police) %>% 
  select(year, iso3c, intsecu)
pos.exp <- pos.exp %>% 
  rbind(pos.exp1)
pos.exp <- gpi.grid %>%
  left_join(pos.exp)
pos.exp3 <- rename(pos.exp,value=intsecu)
pos.exp3 <- distinct(pos.exp3)
count <- pos.exp3 %>% 
  group_by(iso3c) %>% 
  tally()
# imputation
pos.exp <- pos.exp %>% 
  rename (geocode = iso3c, value = intsecu) %>% 
  mutate (variablename = "internal security")
pos.exp <- f_index_data_pad(pos.exp)
pos.exp <- pos.exp %>% 
  select (c(`geocode`, `year`, `imputed`)) %>% 
  rename (iso3c = geocode, intsecu = imputed)
pos.exp <- gpi.grid %>% left_join(pos.exp)




gti.sum <- GPI_DATA %>%
  dplyr::select(c(`geocode`, `year`, `terrorism_deaths`)) %>%
  rename(iso3c = geocode) %>%
  rename(killed = terrorism_deaths)
gti.sum <- gpi.grid %>% 
  left_join(gti.sum)
gti.sum <- gti.sum %>%
  mutate (killed = case_when (is.na(killed) ~ 0,
                                                   TRUE ~ killed))


peacekeeping <- GPI_DATA %>% 
  dplyr::select (`geocode`, `year`, `assessments`) %>%
  rename (peacekeep = assessments, iso3c = geocode)
peacekeeping <- gpi.grid %>% 
  left_join(peacekeeping)
peacekeeping <- peacekeeping  %>% 
  mutate (variablename = "peacekeeping") %>%
  rename (geocode = iso3c, value = peacekeep) 
peacekeeping <- f_index_data_pad(peacekeeping)
peacekeeping <- peacekeeping %>% 
  select (c(`geocode`,`year`,`imputed`)) %>% 
  rename (iso3c = geocode, peacekeep = imputed)
peacekeeping <- gpi.grid %>% 
  left_join(peacekeeping)





gpidata <- pivot_longer(data = GPI_DATA,
                        cols = c(`population`, `battle_deaths`, `homicides`, `incarceration`, `milex`, `fear`, `terrorism_deaths`, `displaced`, `assessments`),
                        names_to = "element", 
                        values_to = "value")
gpidata <- gpidata %>% 
  rename(iso3c=geocode, indicator = element)
gpidata <- gpidata %>% 
  mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
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
  left_join(homicide) %>%
  rename(homicide = value)


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
  left_join(incar, by = c("iso3c", "year")) %>%
  dplyr::select(-c(`indicator`)) %>%
  rename(incar = value)


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
milex <- milex %>% 
  select (c(`year`, `geocode`, `value`)) %>% 
  rename (iso3c = geocode, milex = value)
milex <- gpi.grid %>% 
  left_join(milex)

