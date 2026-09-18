# Date validation

# check survival data
glimpse(base)
glimpse(surv)

base %>% janitor::get_dupes()
surv %>% janitor::get_dupes()

 # same set of ids - OK
n_distinct(base$study_id)
n_distinct(surv$study_id)

sum(is.na(base$study_id))
sum(is.na(surv$study_id))

all(surv$study_id %in% base$study_id)
setequal(unique(surv$study_id), unique(base$study_id)) # same set of ids

# duplictes
map(list(base, surv), function(x) x %>% select(study_id) %>% janitor::get_dupes())

# how many have dates of end of study/progression/death and what is their relationship
# proportion empty
names(base)
names(surv)

surv %>%
  select(study_id, eos_dov, eos_reason, prog_date, death_date, death_disease, death_reason, last_followup) %>%
  glimpse()

surv %>%
  select(study_id, eos_dov, eos_reason, prog_date, death_date, death_disease, death_reason, last_followup) %>%
  naniar::vis_miss() + theme(plot.margin = unit(c(4,3,2,3), "lines"))


# check dates per id - at least one full of progression, death or last follow-up?
surv %>%
  select(study_id, death_date, last_followup) %>%
  pivot_longer(cols = c(death_date, last_followup), names_to = "date_type", values_to = "date") %>%
  group_by(study_id) %>%
  summarise(n_filas = n(),
            n_full_dates = sum(!is.na(date))) %>%
  count(n_filas, n_full_dates) # they all have one full date (death of last follow-up)

# death always after end of study date?
surv %>%
  select(study_id, eos_dov, death_date, last_followup) %>%
  mutate(last_date = coalesce(death_date, last_followup)) %>%
  mutate(last_date_position = case_when(is.na(eos_dov) | is.na(last_date) ~ "one is empty",
                                        last_date > eos_dov ~ "after study end",
                                        last_date == eos_dov ~ "same date",
                                        last_date < eos_dov ~ "last date earlier - error?",
                                        TRUE ~ "error?")) %>%
  count(last_date_position) # ok, survival dates


# Are all follow-up visits in order? - OK for previous data
# long %>% filter(!is.na(study_id) & study_id %in% base$study_id) %>%
repeated_measures_clean %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  relocate(study_id, timepoint_n) %>%
  arrange(study_id, timepoint_n) %>%
  group_by(study_id) %>%
  summarise(ordered_dates = !is.unsorted(dov)) %>% count(ordered_dates) # Ok, assessment dates are ordered


td <- 
  # long %>% # for previous data
  repeated_measures_clean %>% # for new data
  select(study_id, timepoint, dov, cfs_score, grip_average_kg) %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  relocate(study_id, timepoint_n) %>%
  left_join(
    # base %>% # for old data
    survival_raw %>% # new data
      select(study_id, eos_dov, eos_reason, prog_date, death_date)
  )

# all "end" dates happen after follow up visits
td
vct_end_dates <- c("eos_dov", "prog_date", "death_date") 
vct_end_dates

td %>%
  mutate(across(all_of(vct_end_dates), list(later = ~ . >= dov))) %>%
  count(pick(ends_with("later")), sort = T)

td %>%
  select(-cfs_score, -grip_average_kg) %>%
  mutate(across(all_of(vct_end_dates), list(later = ~ . >= dov))) %>%
  filter(if_any(ends_with("later"), ~!.)) %>%
  select(-timepoint_n) #%>% cw()

# obtain the minimal end date
td_first_end_date <- base %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates)) %>%
      pivot_longer(cols = -study_id) %>%
      filter(!is.na(value)) %>%
      group_by(study_id) %>%
      summarise(first_end_date = min(value))
  )

td_first_end_date
naniar::vis_miss(td_first_end_date)

# practical correction: re-assign the day before the first end-date
long %>%
  # select(study_id, timepoint, dov, cfs_score, grip_average_kg) %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  relocate(study_id, timepoint_n) %>%
  left_join(td_first_end_date) %>%
  summarise(sum(!is.na(first_end_date) & first_end_date < dov)) # only 3 cases, OK

ti_check <- long %>%
  # select(study_id, timepoint, dov, cfs_score, grip_average_kg) %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  relocate(study_id, timepoint_n) %>%
  left_join(td_first_end_date) %>%
  mutate(across(dov, ~as_datetime(ifelse(!is.na(first_end_date) & first_end_date < dov,
                                         first_end_date - days(1), .)))) %>%
  select(study_id, timepoint, dov)


comparison <- waldo::compare(long %>% select(study_id, timepoint, dov),
                             ti_check)

length(comparison)

all_equal(long %>% select(study_id, timepoint, dov),
          ti_check)

ti_check[c(9, 106, 111),]

td_long <- long %>%
  # select(study_id, timepoint, dov, cfs_score, grip_average_kg) %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  relocate(study_id, timepoint_n) %>%
  left_join(td_first_end_date) %>%
  mutate(across(dov, ~as_datetime(ifelse(!is.na(first_end_date) & first_end_date < dov,
                                         first_end_date - days(1), .)))) 

# TODO now td_long offers an operative solution to our date problem before.
# consider reviewind data to correct primary source


# re-validate dates
# all dates in order
td_long %>%
  filter(!is.na(study_id) & study_id %in% base$study_id) %>%
  arrange(study_id, timepoint_n) %>%
  group_by(study_id) %>%
  summarise(ordered_dates = !is.unsorted(dov)) %>% count(ordered_dates) # Ok, ordered


# revalidate all assessment dates before minimal end date
td_long %>%
  filter(!is.na(first_end_date)) %>%
  summarise(all(dov <= first_end_date)) # Ok, either before or on the first end date




# long table for points
tg_dates <-
  td_long %>%
  select(study_id, date = dov, date_type = timepoint) %>%
  bind_rows(
    base %>%
      select(
        study_id, eos_dov, prog_date, death_date) %>%
      pivot_longer(cols = -study_id, names_to = "date_type", values_to = "date")
  ) %>%
  relocate(study_id, date_type) %>%
  mutate(across(date_type, ~factor(., levels = c("baseline", "1 month", "3 months", "6 months", "12 months",
                                                 "eos_dov", "prog_date", "death_date"))))
# mutate(across(date_type, ~ifelse(str_detect(., "month"), "follow-up", .)))

tg_dates %>%
  ggplot(aes(y = study_id, x = date)) +
  geom_point(shape = 21, aes(fill = date_type), size = 3, alpha = 0.8) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "left") +
  guides(fill = guide_legend(direction = "vertical", title = NULL))

# Validate date order

tg_dates
tg_dates %>% naniar::vis_miss() + theme(plot.margin = unit(c(10,10,0,0), "lines"))

tg_dates <- tg_dates %>%
  filter(!is.na(date)) %>%
  arrange(study_id, date_type)



#distance between dovs and baseline are reasonable given timepoint?
td_long %>%
  select(study_id, timepoint_n, dov) %>%
  left_join(
    td_long %>%
      filter(timepoint_n == 0) %>%
      select(study_id, baseline_date = dov)
  ) %>%
  mutate(day_difference = as.numeric(difftime(dov, baseline_date, units = "days"))) %>%
  mutate(month_difference = day_difference / (365.25/12)) %>%
  group_by(timepoint_n) %>%
  skimr::skim(month_difference) %>%
  as_tibble() %>%
  select(var = skim_variable, timepoint_n, matches("p\\d+$")) %>%
  mutate(range = paste0(round(numeric.p0, 1), " - ", round(numeric.p100, 1))) # %>%
#cw() # Excellent differences
