# create a state_time table

glimpse(td_long)

td_state_time <- expand_grid(
  study_id = unique(base$study_id),
  timepoint_n = sort(unique(td_long$timepoint_n))
)


# eos_reason:
# 0 Completed Participation 
# 1 Patient Withdrawal 
# 2 Disease Progression 
# 3 Death 
# 4 Ineligible 
# 5 PI discretion


# subset that dies: eos == death? ues
base %>%
  select(study_id, all_of(vct_end_dates), eos_reason) %>%
  filter(eos_reason == 2) %>%
  summarise(n = n(),
            all_eos_date = all(!is.na(eos_dov)),
            all_progression_date = all(!is.na(prog_date)),
            n_empty_prog = sum(is.na(prog_date)),
            avg_death_date= mean(!is.na(death_date)),
            eos_equal_prog = mean(eos_dov == prog_date, na.rm = T))

base %>%
  select(study_id, all_of(vct_end_dates), eos_reason) %>%
  filter(eos_reason == 2) %>%
  filter(is.na(prog_date) | prog_date != eos_dov) # safe to use eos date


# subset that progresses: eos == prog? yes
base %>%
  select(study_id, all_of(vct_end_dates), eos_reason) %>%
  filter(eos_reason == 3) %>%
  summarise(n = n(),
            all_empty_progression = all(is.na(prog_date)),
            all_death_date = all(!is.na(death_date)),
            eos_equal_death = all(eos_dov == death_date))
  

# 5 timepoints for each patient
td_temp <- td_state_time %>%
  left_join(
    td_long %>%
      select(study_id, timepoint_n, dov) %>%
      add_column(status = "assessed")
  ) %>%
  left_join(
    td_long %>%
      filter(timepoint_n == 0) %>%
      select(study_id, baseline_date = dov)
  )

td_temp

# subset of patients who needs data completed 
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>% count(!is.na(eos_dov), eos_reason)

# eos_reason:
# 0 Completed Participation 
# 1 Patient Withdrawal 
# 2 Disease Progression 
# 3 Death 
# 4 Ineligible 
# 5 PI discretion


# one reason at a time?
# reason 0 - completed participation. month 12 is present
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(eos_reason == 0) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp))) %>% View()

# reason 1 - withdrawal
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(eos_reason == 1) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp))) %>% View()

# reason 2 - progression
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(eos_reason == 2) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp))) %>% View()

# reason 3 - death
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(eos_reason == 3) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp))) %>% View()

# reason 4 - ineligible - no one

# reason 5 - PI discretion
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(eos_reason == 5) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp))) %>% View()

# empty reason
td_temp %>%
  filter(is.na(dov) | is.na(status)) %>%
  distinct(study_id) %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason)
  ) %>%
  filter(is.na(eos_reason)) %>%
  left_join(td_temp) %>%
  relocate(all_of(names(td_temp)))  %>% select(1:5) %>%
  slice(1:25) %>% print(n = 25)

# the main reason is to distinguish patients who need data filled vs those censored/progressed/dead

td_temp <- td_temp  %>%
  arrange(study_id, timepoint_n) %>%
  group_by(study_id) %>%
  mutate(
    # 1) Is there any observed visit strictly before / after this row?
    has_prev_obs = lag(cumany(!is.na(dov)), default = FALSE),
    has_next_obs = {
      has_after_or_at <- rev(cumany(rev(!is.na(dov))))
      lead(has_after_or_at, default = FALSE)
    },
    
    # This is a "true" internal missing visit (to be eligible for filling)
    internal_missing = is.na(dov) & has_prev_obs & has_next_obs,
    
    # 2) Immediate neighbours in this patient's series
    prev_dov = lag(dov),
    next_dov = lead(dov),
    
    # 3) Decide which neighbour to use as filler
    filler_side = case_when(
      internal_missing & !is.na(prev_dov) ~ "previous",
      internal_missing &  is.na(prev_dov) & !is.na(next_dov) ~ "next",
      TRUE ~ NA_character_
    ),
    
    filler_timepoint_n = case_when(
      filler_side == "previous" ~ lag(timepoint_n),
      filler_side == "next"     ~ lead(timepoint_n),
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-has_prev_obs, -has_next_obs, -prev_dov, -next_dov)

glimpse(td_temp)


td_temp %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason),
  ) %>%
  left_join(
    td_temp %>%
      filter(!is.na(dov)) %>%
      arrange(study_id, dov) %>%
      group_by(study_id) %>%
      summarise(last_fu_date = max(dov))
  )


# for patients with terminally missing data, we will add the reason of attrition, then check if eos_dov is too far away
td_temp %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason),
  ) %>%
  left_join(
    td_temp %>%
      filter(!is.na(dov)) %>%
      arrange(study_id, dov) %>%
      group_by(study_id) %>%
      summarise(last_fu_date = max(dov))
  )

# try status
td_temp %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason),
  ) %>%
  left_join(
    td_temp %>%
      filter(!is.na(dov)) %>%
      arrange(study_id, dov) %>%
      group_by(study_id) %>%
      summarise(last_fu_date = max(dov))
  ) %>%
  mutate(
    status_recat = case_when(
      !is.na(dov) & status == "assessed" ~ "follow-up",
      !is.na(dov) ~ "error_review",
      is.na(dov) & internal_missing ~ "follow-up to fill",
      is.na(dov) & !internal_missing & eos_reason %in% c(2,3) ~ "progression/death",
      is.na(dov) & !internal_missing & eos_reason %in% c(1,5) ~ "censored",
      is.na(dov) & !internal_missing & is.na(eos_reason) ~ "censored2",
      TRUE ~ "next step"
      )
    ) %>% count(status, status_recat)
  


# 
td_fu <- td_temp %>%
  left_join(
    base %>%
      select(study_id, all_of(vct_end_dates), eos_reason),
  ) %>%
  left_join(
    td_temp %>%
      filter(!is.na(dov)) %>%
      arrange(study_id, dov) %>%
      group_by(study_id) %>%
      summarise(last_fu_date = max(dov))
  ) %>%
  mutate(
    status_recat = case_when(
      !is.na(dov) & status == "assessed" ~ "follow-up",
      !is.na(dov) ~ "error_review",
      is.na(dov) & internal_missing ~ "follow-up to fill",
      is.na(dov) & !internal_missing & eos_reason %in% c(2,3) ~ "progression/death",
      is.na(dov) & !internal_missing &
        (eos_reason %in% c(1,5) | is.na(eos_reason)) ~ "censored/withdrawal",
      TRUE ~ "review"
    )
  )

# eos_reason:
# 0 Completed Participation 
# 1 Patient Withdrawal 
# 2 Disease Progression 
# 3 Death 
# 4 Ineligible 
# 5 PI discretion

td_fu
glimpse(td_fu)

# add cfs
glimpse(td_long)

td_fu %>%
  mutate(timepoint_for_scales = 
           case_when(!is.na(dov) ~ timepoint_n,
                     is.na(dov) & internal_missing & !is.na(filler_timepoint_n) ~ filler_timepoint_n,
                     is.na(dov) & internal_missing & is.na(filler_timepoint_n) ~ 99,
                     is.na(dov) & !internal_missing ~ NA_integer_,
                     TRUE ~ 98
                     
           )) %>%
  count(!is.na(dov), internal_missing, timepoint_for_scales)

td_fu <- td_fu %>%
  mutate(timepoint_for_scales = 
           case_when(!is.na(dov) ~ timepoint_n,
                     is.na(dov) & internal_missing & !is.na(filler_timepoint_n) ~ filler_timepoint_n,
                     is.na(dov) & internal_missing & is.na(filler_timepoint_n) ~ 99,
                     is.na(dov) & !internal_missing ~ NA_integer_,
                     TRUE ~ 98
                     
           )) %>%
  left_join(
    td_long %>%
      select(study_id, timepoint_n, cfs_score, grip_average_kg),
    by = c("study_id", "timepoint_for_scales" = "timepoint_n")
  ) %>%
  # count(!is.na(dov), internal_missing, !is.na(cfs_score), !is.na(grip_average_kg))
  mutate(cfs_cat = case_when(
    is.na(cfs_score) ~ NA_character_,
    cfs_score >= 4 ~ "frail",
    cfs_score < 4 ~ "non-frail"
  ))


td_fu %>%
  count(status_recat, cfs_cat)

td_fu %>%
  mutate(status_fu = case_when(
    status_recat %in% c("follow-up", "follow-up to fill") & is.na(cfs_cat) ~ "follow-up: no CFS",
    status_recat %in% c("follow-up", "follow-up to fill") & !is.na(cfs_cat) ~ 
      paste0("follow-up: ", cfs_cat),
    TRUE ~ status_recat
    )) %>%
  count(status_recat, cfs_cat, status_fu)

td_fu <- td_fu %>%
  mutate(status_fu = case_when(
    status_recat %in% c("follow-up", "follow-up to fill") & is.na(cfs_cat) ~ "follow-up: no CFS",
    status_recat %in% c("follow-up", "follow-up to fill") & !is.na(cfs_cat) ~ 
      paste0("follow-up: ", cfs_cat),
    TRUE ~ status_recat
  )) %>%
  mutate(across(status_fu, ~factor(., levels = c("censored/withdrawal",  
                                                 "follow-up: non-frail",
                                                 "follow-up: no CFS",
                                                 "follow-up: frail",
                                                 "progression/death"))))


glimpse(td_fu)








# Alluvial plot of state_days ======================================================
library(ggalluvial)

# ti_fu_cats
# # First, count frequencies
# ta_alluvial %>%
#   filter(timepoint == "status_00") %>%
#   select(patient_id, status_base = fu_status_sankey) %>%
#   left_join(ta_alluvial) %>%
#   filter(timepoint == "status_06") %>%
#   group_by(status_base) %>%
#   count(fu_status_sankey) %>%
#   add_pcump()
# 
# # all lost/declined on month 6 are in the same situation on month 12? - Yes
# ta_alluvial %>%
#   filter(timepoint == "status_06" & str_detect(fu_status_sankey, "declined")) %>%
#   select(patient_id, status_06 = fu_status_sankey) %>%
#   left_join(ta_alluvial) %>%
#   filter(timepoint == "status_12") %>%
#   # group_by(status_base) %>%
#   count(status_06, fu_status_sankey) %>%
#   add_pcump() # yes
# 
# # 6-month incidence of death by different baseline FS
# ta_alluvial %>%
#   filter(timepoint == "status_00") %>%
#   select(patient_id, status_base = fu_status_sankey) %>%
#   left_join(ta_alluvial) %>%
#   filter(timepoint == "status_06" & str_detect(fu_status_sankey, "dead|^FS|incomplete")) %>%
#   group_by(status_base) %>%
#   count(fu_status_sankey) %>%
#   add_pcump()
# 
# # 12-month mortality by 6 month FS
# ta_alluvial %>%
#   filter(timepoint == "status_06" & str_detect(fu_status_sankey, "^FS")) %>%
#   select(patient_id) %>%
#   left_join(ta_alluvial) %>%
#   filter(timepoint == "status_12" & str_detect(fu_status_sankey, "incomplete|declined", negate = T)) %>%
#   select(patient_id) %>%
#   left_join(ta_alluvial %>%
#               filter(timepoint == "status_06") %>%
#               select(patient_id, status_06 = fu_status_sankey)) %>%
#   left_join(ta_alluvial %>% filter(timepoint == "status_12") %>%
#               select(patient_id, fu_status_sankey)) %>%
#   group_by(status_06) %>%
#   count(fu_status_sankey) %>%
#   add_pcump()
# 
# ta_alluvial %>% count(fu_status_sankey)
# 
# # change label of categories
# ta_alluvial %>%
#   mutate(across(fu_status_sankey,
#                 ~case_when(str_detect(., "[1-3]|^No") ~ paste0("FS: ", .),
#                            TRUE ~ .))) %>%
#   count(fu_status_sankey)
# 
# 
# if(!any(str_detect(ta_alluvial$fu_status_sankey, "^FS"))){
#   ta_alluvial <- ta_alluvial %>%
#     mutate(across(fu_status_sankey,
#                   ~case_when(str_detect(., "[1-3]|^No") ~ paste0("FS: ", .),
#                              TRUE ~ .)))
# }
# 
# ta_alluvial %>%
#   count(fu_status_sankey)
# # possibly add ventilation status to the state-day table
# 
# 
# unique(ta_alluvial$fu_status_sankey) %>% dput()
# the_levels <- c("FS: No Limit", "FS: 1-3", "FS: >3",
#                 "missing/incomplete appt",
#                 "lost/declined",
#                 "dead")
# the_levels
# 
# ta_alluvial %>% count(timepoint)

library(ggalluvial)

txt_sz <- 10

td_fu %>%
  # mutate(across(status, ~ ifelse(day_n == 0, "In ICU", .))) %>%
  # mutate(site = fct_relevel(fu_status_sankey, the_levels)) %>%
  ggplot(aes(x = timepoint_n, stratum = status_fu, alluvium = study_id,
             # fill = as.factor(sitio != "Hospitalario")
             fill = status_fu
             #alpha = as.numeric(as.factor(ID_EPISODIO))
  )) +
  # geom_vline(xintercept = string_marcar_dia,
  #            color = "gold", size = 20, alpha = 0.5) +
  geom_flow(stat = "alluvium", alpha = 0.8) +
  geom_stratum(alpha = 0.9, width = 0.45) +
  # geom_text(stat = 'stratum',
  #           aes(label = str_remove_all(str_remove_all(toupper(after_stat(stratum)),"(?<=\\b\\S)[\\S]*"), " ")))
  # geom_text(stat = "stratum",
  #           aes(label = paste0(str_remove_all(str_remove_all(toupper(after_stat(stratum)),
  #                                                            "(?<=\\b\\S)[\\S]*"), " "), "\n",
  #                              percent(after_stat(prop), accuracy = 1)))) +
  geom_label(stat = "stratum",
             aes(label = paste0(after_stat(count), "\n(", scales::percent(after_stat(prop), accuracy = 1), ")"),
             # aes(label = paste0(str_replace_all(stratum, c("missing/incomplete appt" = "incomplete",
             #                                               "No Limit" = "0",
             #                                               "follow-up: " = "")),
             #                    # "\n", scales::percent(after_stat(prop), accuracy = 1)
             #                    "\n(n = ", after_stat(count), ")"
             # ),
             fill = status_fu, alpha = as.factor(str_detect(status_fu, "incomplete"))),
             label.size = NA, show.legend = F
  ) +
  # scale_alpha_manual(values = c(0,0)) +
  # scale_fill_manual(
  #   values = four_colors[1:3]
  #   values = c(pals::ocean.curl(7)[c(2,4)], "darkslategray")) +
  # scale_fill_brewer() +
  scale_fill_manual(values = c(
    "grey80"
    , "#2a9d8f"
    , "#e9c46a"
    , "#e07a5f"
    , "grey50"
    # , "grey50"
    ),
                    name = NULL, labels = function(x) str_replace(str_to_title(str_remove(x, "follow-up: ")), "Cfs", "CFS")) +
  scale_alpha_manual(guide = "none", values = c(0, 0.7)) +
  scale_x_continuous(breaks = c(0, 1, 3, 6, 12),
                     labels = function(x) ifelse(x == 0, "Baseline", paste0("Month ", as.character(x)))) +
  labs(x = NULL) +
  theme_minimal(base_size = txt_sz*1.1) +
  theme(legend.position = "top",
        # axis.title.x = element_blank(),
        axis.text = element_text(size = txt_sz*1.5)) #+
  # guides(fill = guide_legend(override.aes = list(alpha = 0.7), ncol = 2))
  

# count proportios
td_fu %>%
  filter(timepoint_n == 0) %>%
  mutate(baseline_cfs = str_to_title(str_remove(status_fu, "follow-up: "))) %>%
  select(study_id, baseline_cfs) %>%
  left_join(td_fu %>% filter(timepoint_n == 6) %>% select(study_id, status_fu)) %>%
  count(baseline_cfs, status_fu) %>%
  group_by(baseline_cfs) %>%
  mutate(n_baseline = paste0("n = ",sum(n)),
         p = n/sum(n)) %>%
  relocate(n_baseline) %>%
  group_by(baseline_cfs) %>%
  mutate(group_label = ifelse(row_number() == 1, baseline_cfs, ""),
         across(n_baseline, ~ifelse(row_number() == 1, ., ""))) %>%
  ungroup() %>%
  relocate(group_label, n_baseline) %>% select(-baseline_cfs) %>%
  gt::gt() %>%
  gt::fmt_percent(columns = p, decimals = 1) %>%
  gt::cols_align(align = "left") %>%
  gt::cols_label(status_fu = "6-month status", p = "Perc.")



  
  # CFS scores of subset on remission by month 6
  td_fu %>%
    filter(timepoint_n == 6 & str_detect(status_fu, "follow")) %>%
    distinct(study_id) %>%
    left_join(td_fu) %>%
    filter(timepoint_n <= 6) %>%
    left_join(
      td_fu %>%
        filter(timepoint_n == 0) %>%
        select(study_id, baseline_cat = status_fu) %>%
        mutate(across(baseline_cat, ~str_replace(., fixed("follow-up: "), "baseline: ")))
    ) %>%
    ggplot(aes(x = timepoint_n, y = cfs_score)) +
    geom_smooth(aes(group = study_id),
                color = "grey20", lty = "dotted",
                show.legend = F, size = 0.7, alpha = 0.8) +
    geom_smooth(aes(color = baseline_cat)) +
    scale_y_continuous(limits = c(0.5, 9.5), breaks = scales::breaks_width(1)) +
    scale_x_continuous(breaks = c(0,1,3,6)) +
    theme_minimal()
  


