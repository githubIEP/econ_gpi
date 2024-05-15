
COST.df <- crime2 %>%
  left_join(homicide) %>%
  left_join(conflict) %>%
  left_join(gti.sum) %>%
  left_join(fear) %>%
  left_join(suicide) %>%
  left_join(priv.secu) %>%
  left_join(secu.agnecy) %>%
  left_join(small.arms) %>%
  left_join(peacebuilding) %>%
  left_join(peacekeeping) %>%
  left_join(unhcr) %>%
  left_join(vet) %>%
  left_join(deflator) %>%
  left_join(unitcost.scaled) %>%
  left_join(milex) %>%
  left_join(gdp.wdi) %>%
  left_join(incar) %>%
  left_join(refugidp) %>%
  left_join(gdplosses) %>%
  left_join(ppp) %>%
  left_join(pop) %>%
  left_join(ppp.conv) %>%
  rename(con.median = value) %>%
  left_join(pos.exp)


COST.df <- COST.df %>%
  dplyr::select(-c(`indicator`, `variablename`))
COST.df <- COST.df %>%
  mutate(priv.secu = ifelse(is.na(priv.secu),0,priv.secu))

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

COST.df <- COST.df %>%
  rename(milex.dir.cost = milex, 
         intsecu.dir.cost = intsecu, 
         privsecu.dir.cost = privsecu.cost, 
         sarms.dir.cost = sarms, 
         peacekeep.dir.cost = peacekeep,
         peacebuilding.dir.cost = peacebuilding,
         vet.dir.cost = vet,
         unhcr.dir.cost = unhcr)

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
pos.ppp <- c("gdplosses", "intsecu", "milex", "peacebuilding", "peacekeep", "privsecu",
             "refugeidp", "sarms", "secu.agnecy", "unhcr", "vet")

cost$ppp <- as.integer(cost$indicator %in% pos.ppp)
cost$costppp <- ifelse(cost$ppp == "1", cost$costusd/cost$con.median, cost$costusd )
cost$type[cost$indicator=="gdplosses"]='indirect'

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

na <- sum(is.na(cost))
na
na_counts <- colSums(is.na(cost))
columns_with_na <- which(na_counts > 0)
names_with_na <- names(cost)[columns_with_na]
names_with_na

econcost <- cost %>%
  select(iso3c, year, indicator, costusd, type, gdp, gdpcons, gdpconsppp, pop, costppp, impact, domain, indicator2) %>%
  gather(subtype, value, -c(iso3c, year, indicator, type, domain, indicator2)) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"))

econcost$value[econcost$indicator == "gdplosses" & econcost$iso3c %in% c("IND", "CHN")] <- 0
econcost <- econcost %>% distinct()
econcost$country[econcost$iso3c == "KSV"] <- "Kosovo"

tmp <- econcost %>%
  subset(subtype=="impact") %>% 
  group_by(year) %>% 
  summarise(total=sum(value)) %>%
  ungroup()

rio::export(econcost, "04_outputs/Economic Impact of Violence1.xlsx")
