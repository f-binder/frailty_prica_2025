analysis_wide %>% count(cart_line) %>% add_pcump()

glimpse(analysis_wide)

analysis_survival <- read_rds("data/processed/analysis_survival.rds")
survival_raw <- readRDS("data/processed/survival_updated_raw.rds")

glimpse(analysis_survival)
glimpse(survival_raw)


analysis_survival %>%
  count(os_event, pfs_event, death_disease, death_reason) #%>% cw()

# identify patients that need to be reviewed - unknown cause of death (relapse vs non-relapse mortality)
analysis_survival %>%
  filter((os_event == 1 & (is.na(death_disease) | death_disease == 2)) |
         str_detect(tolower(death_reason), fixed("t cell"))) %>%
  select(study_id, date_infusion, os_event, death_disease, death_reason) %>%
  left_join(
    survival_raw %>% select(study_id, death_date)
  ) %>%
  arrange(is.na(death_reason), is.na(death_disease)) %>%
  mutate(across(where(is.POSIXct), as.Date)) %>% cw()


source("scripts/00_packages.R")
str_subset(names(analysis_survival), "death")
