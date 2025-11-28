# Explore cutoffs

# grip strength ---------------------
td_gs

# age always an integer
all(base$age %%1 == 0)

nrow(td_gs)

all_integers_td_gs <- tibble(age_integer = 1:100) %>%
  cross_join(td_gs) %>%
  filter(age_integer <= age_range_max &
           age_integer >= age_range_min)


#short validation
all_integers_td_gs %>%
  count(age_integer) %>%
  summarise(setequal(unique(age_integer), min(age_integer):max(age_integer)))

all_integers_td_gs %>%
  count(age_integer) %>% count(n)

# sex
# 0 = Male
# 1 = Female


long %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  filter(!is.na(study_id) & study_id %in% base$study_id) %>%
  left_join(
    base %>%
      select(study_id, age, sex)
  ) %>%
select(study_id, timepoint_n, grip_average_kg, age, sex) %>%
  filter(timepoint_n == 0) %>%
  mutate(across(sex, ~case_match(., 0 ~ "Male", 1 ~ "Female", .default = "Other"))) %>%
  left_join(
    all_integers_td_gs,
    by = c("age" = "age_integer",
           "sex")
  ) %>%
  mutate(low_limit_1sd = mean_kg - sd_kg,
         low_limit_2sd = mean_kg - 2*sd_kg) %>%
  count(below_limit = grip_average_kg < low_limit_2sd) %>%
  add_pcump() %>%
  select(1, n, p) %>% cw()


# gait speed ------------
glimpse(long)
long %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  filter(!is.na(study_id) & study_id %in% base$study_id) %>%
  filter(timepoint_n == 0) %>%
  count(walk_mpers < 0.8) %>%
  add_pcump()
  skimr::skim(walk_mpers)
  

# mini cog -------------------
  long %>%
    mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                   TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
    filter(!is.na(study_id) & study_id %in% base$study_id) %>%
    filter(timepoint_n == 0) %>%
    count(minicog_score < 3) %>% add_pcump()
    skimr::skim(minicog_score)
  
    
# PHQ-2 ----------------
    long %>%
      mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                     TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
      filter(!is.na(study_id) & study_id %in% base$study_id) %>%
      filter(timepoint_n == 0) %>%
      count(phq_10)
      # count(minicog_score < 3) %>% add_pcump()
    skimr::skim(phq_total_score)

