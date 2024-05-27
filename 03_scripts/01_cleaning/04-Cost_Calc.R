# List of data frames to join
data_frames <- list(
  homicide, conflict, gti.sum, fear, suicide, priv.secu, secu.agnecy,
  small.arms, peacebuilding, peacekeeping, unhcr, vet, deflator,
  unitcost.scaled, milex, gdp.wdi, incar, refugidp, gdplosses, ppp, pop, ppp.conv
)

# Perform left joins in a loop
COST.df <- crime2
for(df in data_frames) {
  COST.df <- COST.df %>% left_join(df)
}

# Rename and join the final data frame
COST.df <- COST.df %>%
  rename(con.median = value) %>%
  left_join(pos.exp)

# fill in the NA's 

COST.df <- COST.df %>%
  dplyr::select(-c(`indicator`, `variablename`))
COST.df <- COST.df %>%
  mutate(priv.secu = ifelse(is.na(priv.secu),0,priv.secu))


# Check to see if there are any NAs
na <- sum(is.na(COST.df)) 
na

COST.df <- COST.df %>%
  mutate(assault.dir.cost = `violent assault`*violentassault.direct, assault.indir.cost=`violent assault`*violentassault.indirect) %>%
  mutate(sexassault.dir.cost=`sexual assault`*sexualassault.direct, sexassault.indir.cost=`sexual assault`*sexualassault.indirect)%>%
  mutate(suicide.dir.cost=`suicidevalue`*suicide.direct, suicide.indir.cost=`suicidevalue`*suicide.indirect) %>% 
  mutate(homi.dir.cost=homicide*homicide.direct, homi.indir.cost=homicide*homicide.indirect)%>%
  mutate(incar.dir.cost=incar*minwage.direct)%>%
  mutate(fear.indir.cost=fear*fear.indirect)%>%
  mutate(battle_deaths.dir.cost=battle_deaths * homicide.direct)%>%
  mutate(terrdeath.dir.cost=killed*homicide.direct, terrdeath.indir.cost=killed*homicide.indirect) %>%
  mutate(privsecu.cost = privtsecurity.direct*priv.secu) %>%
  mutate(milex = milex * gdp)

COST.df <- COST.df %>%
  distinct()


# Dividing these cost estimators with GDP deflator
COST.df <- mutate_at(COST.df, vars(`milex`,
                                   `intsecu`, 
                                   `privsecu.cost`,
                                   `secu.agnecy`,
                                   `sarms`,
                                   `peacebuilding`, 
                                   `peacekeep`, 
                                   `unhcr`, 
                                   `vet`),
                     funs(./deflator))

# Renaming these variables
COST.df <- COST.df %>%
  rename(milex.dir.cost = milex, 
         intsecu.dir.cost = intsecu, 
         privsecu.dir.cost = privsecu.cost, 
         sarms.dir.cost = sarms, 
         peacekeep.dir.cost = peacekeep,
         peacebuilding.dir.cost = peacebuilding,
         vet.dir.cost = vet,
         unhcr.dir.cost = unhcr)


# This next chunk of code converts this data frame from wide to long
# This coverts all the cost indicators from wide to long except for country, year, population, GDP and PPP factor.
# This will create 10 columns.

COST.df <- COST.df %>% 
  subset(select=c(`iso3c`, 
                  `year`, 
                  `assault.dir.cost`,
                  `assault.indir.cost`,
                  `sexassault.dir.cost`,
                  `sexassault.indir.cost`, 
                  `suicide.dir.cost`, 
                  `suicide.indir.cost`, 
                  `homi.dir.cost`, 
                  `homi.indir.cost`, 
                  `incar.dir.cost`,
                  `fear.indir.cost`,
                  `battle_deaths.dir.cost`,
                  `terrdeath.dir.cost`,
                  `terrdeath.indir.cost`,
                  `refugeidp`,
                  `gdplosses`,
                  `gdp`,
                  `gdpcons`,
                  `gdpconsppp`,
                  `population`,
                  `con.median`,
                  `milex.dir.cost`, 
                  `intsecu.dir.cost`, 
                  `privsecu.dir.cost`,  
                  `sarms.dir.cost`, 
                  `peacekeep.dir.cost`, 
                  `peacebuilding.dir.cost`,
                  `vet.dir.cost`, 
                  `unhcr.dir.cost`
  )) %>% 
  gather(indicator,value,-c(`iso3c`,`year`,`gdp`,`gdpcons`,`gdpconsppp`,`con.median`,`population`)) %>%
  separate(indicator,c("indicator","type","cost")) %>% 
  subset(select=-cost) %>% mutate(type=ifelse(indicator=="refugeidp","indir",type)) %>% 
  mutate(type1=ifelse(type=="indir","indirect","direct")) %>% 
  subset(select=-type) %>% rename(type=type1)

COST.df <- COST.df %>%
  rename(costusd = value) %>%
  rename(pop = population)
cost <- COST.df

# So these are the cost indicators that are not in constant USD PPP
pos.ppp <- c("gdplosses", "intsecu", "milex", "peacebuilding", "peacekeep", "privsecu",
             "refugeidp", "sarms", "secu.agnecy", "unhcr", "vet")

# This code initializes a binary column, that selects indicators in the pos.ppp list would given a 1 and the others a 0.
# The reason for this is create a new Cost PPP column, the columns where PPP = 1, are the converted to cost PPP by dividing it to ppp convertor
cost$ppp <- as.integer(cost$indicator %in% pos.ppp)
cost$costppp <- ifelse(cost$ppp == "1", cost$costusd/cost$con.median, cost$costusd )
cost$type[cost$indicator=="gdplosses"]='indirect'


# This is creating the cost impact column
# The econ impact of violence is direct costs + indirect costs + multiplier (direct costs)
# This code looks at the `type` column and checks if the the cost type is direct. If it is then it multiplies it by 2 to account for the multiplier
# if the cost type in indirect then the cost type is just the cost PPP. 

cost <- cost %>%
  mutate(impact = case_when(
    type == "direct" ~ costppp * 2,
    type == "indirect" ~ costppp,
    TRUE ~ costppp
  ))

domain_map <- c("assault" = "Violent crime", "battle" = "Conflict", "fear" = "Other",
                "gdplosses" = "Conflict", "homi" = "Homicide", "incar" = "Internal security expenditure",
                "intsecu" = "Internal security expenditure", "milex" = "Military expenditure",
                "peacebuilding" = "Conflict", "peacekeep" = "Conflict", "privsecu" = "Private security expenditure",
                "refugeidp" = "Conflict", "sarms" = "Other", "secu.agnecy" = "Internal security expenditure",
                "sexassault" = "Violent crime", "suicide" = "Suicide", "terrdeath" = "Conflict",
                "unhcr" = "Conflict", "vet" = "Military expenditure", "wounded" = "Conflict")
cost$domain <- domain_map[cost$indicator]

indicator_map <- c("assault" = "Violent crime", "battle" = "Conflict deaths", "fear" = "Fear",
                   "gdplosses" = "GDP losses", "homi" = "Homicide", "incar" = "Incarceration",
                   "intsecu" = "Internal security expenditure", "milex" = "Military expenditure",
                   "peacebuilding" = "Peacebuilding", "peacekeep" = "Peacekeeping", "privsecu" = "Private security",
                   "refugeidp" = "Refugees and IDPs", "sarms" = "Small arms", "secu.agnecy" = "Internal security expenditure",
                   "sexassault" = "Violent crime", "suicide" = "Suicide", "terrdeath" = "Terrorism",
                   "unhcr" = "Refugees and IDPs", "vet" = "Military expenditure", "wounded" = "Terrorism")
cost$indicator2 <- indicator_map[cost$indicator]


# Sanity check
# This checks if there are any NA in the data frame
# If there are any the code then prints which columns have the NA values

na <- sum(is.na(cost))
na
na_counts <- colSums(is.na(cost))
columns_with_na <- which(na_counts > 0)
names_with_na <- names(cost)[columns_with_na]
names_with_na

# Selects the columns necessary for the final cost data frame
econcost <- cost %>%
  select(iso3c, year, indicator, costusd, type, gdp, gdpcons, gdpconsppp, pop, costppp, impact, domain, indicator2) %>%
  gather(subtype, value, -c(iso3c, year, indicator, type, domain, indicator2)) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"))

econcost$value[econcost$indicator == "gdplosses" & econcost$iso3c %in% c("IND", "CHN")] <- 0
econcost <- econcost %>% distinct()
econcost$country[econcost$iso3c == "KSV"] <- "Kosovo"

# this tmp data frame looks at the impact data trend over the years
tmp <- econcost %>%
  subset(subtype=="impact") %>% 
  group_by(year) %>% 
  summarise(total=sum(value)) %>%
  ungroup()

# save the final data frame as an excel file

rio::export(econcost, "04_outputs/Economic Impact of Violence1.xlsx")



