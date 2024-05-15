gti.sum <- GPI_DATA %>%
  dplyr::select(c(`geocode`, `year`, `terrorism_deaths`)) %>%
  rename(iso3c = geocode) %>%
  rename(killed = terrorism_deaths)


gti.sum <- gpi.grid %>% left_join(gti.sum)

gti.sum <- gti.sum %>% mutate (killed = case_when (is.na(killed) ~ 0,
                                                   TRUE ~ killed))
