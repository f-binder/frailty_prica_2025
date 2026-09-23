analysis_wide %>% count(cart_line) %>% add_pcump()

glimpse(analysis_wide)

analysis_survival <- read_rds("data/processed/analysis_survival.rds")

glimpse(analysis_survival)


analysis_survival %>%
  count(os_event, pfs_event, death_disease, death_reason) %>% cw()


source("scripts/00_packages.R")
str_subset(names(analysis_survival), "death")
