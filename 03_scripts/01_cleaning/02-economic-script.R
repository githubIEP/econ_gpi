
# Getting GDP per cap PPP Constant, GDP per capita current, , GDP per capita PPP Constant, GDP Deflator from WDI

wdi.all <- f_get.wdi("all",c("NY.GDP.PCAP.PP.KD", "PA.NUS.PPPC.RF", "NY.GDP.DEFL.ZS",
                             "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD"),
                     FIRST_GPI_YEAR-1,LATEST_YEAR) %>% mutate(year = year+1)

wdi.data <- wdi.all %>% 
  rename('GDP per cap PPP Constant 2017' = NY.GDP.PCAP.PP.KD,  
         'PPP Conversion Factor' = PA.NUS.PPPC.RF, 'GDP Deflator' = NY.GDP.DEFL.ZS, 
         'GDP per cap Current' = NY.GDP.PCAP.CD,'GDP per cap Constant' = NY.GDP.PCAP.KD)

# This data frame is then combined with the gpi grid data frame and then converted the data frame from wide to long
# This code converts the data frame from wide to long

wdi.data <- gpi.grid %>%
  left_join(wdi.data, by = c("iso3c", "year")) %>%
  mutate(country = case_when(
    iso3c == "KSV" ~ "Kosovo",
    iso3c == "TWN" ~ "Taiwan",
    TRUE ~ country
  )) %>%
  pivot_longer(cols = c(`GDP per cap PPP Constant 2017`:`GDP per cap Constant`),
               names_to = "variablename", 
               values_to = "value") %>%
  rename(geocode = iso3c) %>%
  ungroup()

# Imputation
# This creates a imputed data set. It would filter out only the complete cases
# What the next set of codes would group the data frame by the variable name, region and year, to get a regional average.
# This would complete the data frame and all na values would filled in with the regional average

wdi.data.impute <- wdi.data %>%
  select (geocode, year, variablename, value) %>%
  dplyr::filter(complete.cases(value))

wdi.grid <- wdi.data %>% 
  select (geocode, year, variablename)

wdi.data.impute <- wdi.data.impute %>%
  f_index_data_pad() %>% 
  right_join(wdi.grid, by = c("geocode", "year", "variablename")) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    variablename,
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  dplyr::filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    variablename, 
    geocode) %>%
  # # mutate(imputed2 = imputeTS::na_interpolation(imputed)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(`imputation_type`:`imputed`))

# This is now the main data frame which will form all the other data frames for GDP, PPP and GDP deflator
wdi.data <- wdi.data.impute
# Thsi get rid of all the unnecessary data frames
rm(wdi.data.impute, wdi.grid)

# =======================================================================================================================================

## GDP Values
# These set of codes is to capture GDP current, GDP per capita constant, GDP per capiata PPP constant and GDP deflator.
# we add the population data set and multiply it with per cap values to get total gdp in current, constant and constant ppp terms
# We convert this from long to wide and rename columns from per capita to GDP.
# Each of them is now a column
# convert the three new columns as numeric as they are now classified as characters

gdp.wdi <- wdi.data %>%
  filter(variablename %in% c("GDP per cap Current", "GDP per cap Constant", "GDP per cap PPP Constant 2017")) %>%
  rename(iso3c = geocode) %>%
  left_join(pop, by = c("iso3c", "year")) %>%
  mutate(value = value * population) %>%
  select(-population) %>%
  pivot_wider(names_from = variablename, values_from = value) %>%
  rename(gdp = 'GDP per cap Current', gdpcons = 'GDP per cap Constant', gdpconsppp = 'GDP per cap PPP Constant 2017') %>%
  mutate(across(c(gdp, gdpcons, gdpconsppp), ~ as.numeric(as.character(.))))

# We now create a function to be used to convert gdp from constant 2017 to constant in the latest year
# It starts by pulling the US GDP deflator from the wdi data and treats it like a separate object
# The next set of codes creates a function which create the latest year constant coefficient
# The function starts by pulling the latest year and last value in the data frame and by pulling the row with where the gdp deflator = 100
# the coefficient is called ratio as it takes the value = 100 and divides it by latest value in the data frame
# This coefficient will now by used to multiply it with gdp to convert it to constant terms in the latest year

CONSTANT_PARA <- wdi.data %>%
  filter(variablename == "GDP Deflator", geocode == "USA") 

calculate_ratio <- function(CONSTANT_PARA) {
  
  latest_value <- CONSTANT_PARA %>%
    filter(year == max(year), value == max(value)) %>%
    pull(value) 
  
  value_100 <- CONSTANT_PARA %>%
    filter(value == 100) %>%
    pull(value)
  
  ratio <- latest_value / value_100
  
  return(ratio)
}

ratio_result <- calculate_ratio(CONSTANT_PARA)

gdp.wdi <- gdp.wdi %>% 
  mutate (gdpcons = ratio_result * gdpcons) # turning WDI constant 2015 to latest year constant


# These set of codes fill in or impute missing values for the remaining columns 
gdp.wdi <- gdp.wdi %>%
  group_by(iso3c) %>%
  fill(gdp, gdpcons, gdpconsppp, .direction = "downup")

# ==================================================================================================================

## GDP per cap Constant
# We now use the ratio coefficient to convert the gdp per cap constant to gdp per cap constant in the latest year

gdp.pc.constant <- wdi.data %>%
  dplyr::filter(variablename == "GDP per cap Constant") %>%
  select(geocode, year, value) %>%
  rename(iso3c = geocode, gdp.pc.cons = value) %>%
  mutate(gdp.pc.cons = ratio_result * gdp.pc.cons) # turning constant 2015 to constant 2023

# ==================================================================================================================================
## GDP Deflator

# We are now creating a data frame with gdp deflator
# We filter for the latest year and create a new data frame with the latest year
# The code then divides the latest year deflator with current
# This is to create a delfator in constant terms for the latest year for all countries
# The next code then groups the data set by years and takes the median value for all countries.

deflator <- wdi.data %>%
  dplyr::filter(variablename == "GDP Deflator") %>%
  select(geocode, year, value) %>%
  rename(iso3c = geocode) %>%
  left_join(dplyr::filter(., year == LATEST_YEAR), by = "iso3c") %>%
  mutate(value = value.x / value.y) %>%
  select(iso3c, year = year.x, value) %>%
  rename(deflator = value) %>%
  group_by(year) %>%
  summarize(deflator = median(deflator))


# ==============================================================================================================================

## PPP Conversion Factor
ppp.conv <- wdi.data %>% 
  dplyr::filter (variablename == "PPP Conversion Factor")

ppp.conv  <- ppp.conv  %>%
  select (geocode, year, value) %>%
  rename (iso3c = geocode)

# ========================================================================================================================

## PPP US Ratio
# These set of codes creates the PPP scale which will be used to multiply it with the unit costs in the next script
# We start by creating the the GDP cap for all countries
# we then filter out the the per capita for USA and treat it like a seperate data frame
# we then left join both data frames and divide the US per cap ppp with the rest of the countries 
# We get the PPP scale which later be applied to the unit costs in the next script

ppp <- wdi.data %>%
  filter(variablename == "GDP per cap PPP Constant 2017") %>%
  select(geocode, year, value) %>%
  rename(iso3c = geocode, gdpc.ppp.con = value) %>%
  left_join(filter(wdi.data, variablename == "GDP per cap PPP Constant 2017" & geocode == "USA") %>%
              select(year, value) %>%
              rename(gdpc.ppp.con_us = value), by = "year") %>%
  mutate(scale = gdpc.ppp.con / gdpc.ppp.con_us) %>%
  select(iso3c, year, scale)

rm(wdi.gdpc.ppp, ppp_us, wdi.data, wdi.all)    

# =========================================================================================================
