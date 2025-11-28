# Exploration
library(tidyverse)

thepath <- "data/raw/Frailty and CAR T - Frailty Data - 09Jul2025.xlsx"

hojas <- readxl::excel_sheets(thepath)

lit <- map(hojas, ~readxl::read_xlsx(thepath, sheet = .))

names(lit) <- janitor::make_clean_names(hojas)
names(lit)
names(lit) <- c("base", "long")

base <- lit$base
glimpse(base)

# count IDs and rows
base %>%
  summarise(n_rows = nrow(.),
            ids_all_full = all(!is.na(study_id)),
            ids_all_different = n_distinct(study_id) == nrow(.))


# describe baseline scores to compare with output:
glimpse(base)

base %>%
  skimr::skim(cirs_total,
              ves13_score)


# Long data
long <- lit$long
glimpse(long)

  # all long ids in baseline data as well? one is not
n_distinct(long$study_id, na.rm = T)
long %>% anti_join(base %>% select(study_id))

# long data, summarise measures to compare with printout
long %>% count(timepoint) %>%
  mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                                 TRUE ~ as.numeric(str_extract(timepoint, "\\d+"))))

long %>%
mutate(timepoint_n = case_when(timepoint == "baseline" ~ 0,
                               TRUE ~ as.numeric(str_extract(timepoint, "\\d+")))) %>%
  group_by(timepoint_n) %>%
  skimr::skim(cfs_score, grip_average_kg, walk_time, walk_mpers, minicog_score, phq_total_score, phq_10) %>%
  as_tibble() %>%
  select(timepoint_n, variable = skim_variable, n_missing, matches("mean|sd\\b|p0|p25|p50|p75|p100")) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(timepoint_n, variable) %>% View() # OK, matches reported summary measures



# CFS score at baseline
str_subset(tolower(names(base)), "cfs")

other_studies <- tribble(
  ~study, ~author_string, ~median_cfs, ~p25, ~p75, ~mean_cfs, ~sd_cfs, ~prop_below_4,
  "COHO - ICU Heme", "Munshi & Mehta 2024", 4,  3, 6, NA, NA, 0.39,
  "Older Pts. Lymphoma", "Dhir & Prica 2022", NA, NA, NA,  2.77, 1.29, .77
)

other_studies
other_studies <- other_studies %>%
  mutate(center = coalesce(median_cfs, mean_cfs),
         left = ifelse(!is.na(p25), p25, mean_cfs - sd_cfs),
         right = ifelse(!is.na(p75), p75, mean_cfs + sd_cfs)) %>%
  mutate(i = 1:nrow(other_studies)) %>%
  mutate(position = 20 + i*1.5)


# proportion with frailty >= 4 at baseline
long %>%
  filter(!is.na(study_id) & timepoint == "baseline") %>%
  count(cfs_score >= 4) %>%
  mutate(p = n/sum(n))

prop_csf_4more_baseline <- mean(long$cfs_score[!is.na(long$study_id) & long$timepoint == "baseline"] >= 4, na.rm = T)
prop_csf_4more_baseline

long %>%
  filter(!is.na(study_id) & timepoint == "baseline") %>%
  # skimr::skim(cfs_score) %>%
  ggplot(aes(x = cfs_score)) +
  geom_histogram(fill = "darkcyan") +
  geom_density(stat = "count") +
  geom_vline(xintercept = mean(long$cfs_score[long$timepoint == "baseline"]),
             lty = "dashed") +
  geom_errorbar(data = other_studies,
                aes(xmin = left, xmax = right, y = position), inherit.aes = F
  ) +
  geom_point(data = other_studies, inherit.aes = F,
             aes(x = center, y = position)) +
  geom_text(data = other_studies, inherit.aes = F,
            aes(y = position, x = center + 0.3, vjust = ifelse(i == 1, 1.3, -0.3),
                label = author_string)) +
  annotate("label", label = paste0("CFS ≥ 4: ", scales::percent(prop_csf_4more_baseline)),
           x = 6.5, y = 12, label.padding = unit(0.75, "lines")) +
  labs(x = "Baseline Clinical Frailty Score (possible scores 1-9)", y = "n") +
  scale_x_continuous(limits = c(0.5, 9.5), breaks = scales::breaks_width(1)) +
  scale_y_continuous(expand = expansion(mult = c(NA, 0.15))) +
  theme_minimal()

# grip strength
long %>%
  filter(!is.na(study_id) & timepoint == "baseline") %>%
  left_join(base %>% select(study_id, sex)) %>%
  mutate(across(sex, ~case_match(., 0 ~ "Male", 1 ~ "Female", .default = as.character(sex)))) %>%
  filter(!is.na(sex)) %>%
  # group_by(sex) %>%
  # skimr::skim(grip_average_kg)
  ggplot(aes(x = grip_average_kg)) +
  geom_density(aes(y = after_stat(count), fill = sex), show.legend = F,
               # fill = "darkcyan",
               bw = 1.5) +
  scale_fill_manual(values = scales::alpha(c("#009878", "#324a60"), alpha = 0.4)) +
  facet_wrap(~sex, ncol = 1)

long %>%
  filter(!is.na(study_id) & timepoint == "baseline") %>%
  left_join(base %>% select(study_id, sex)) %>%
  mutate(across(sex, ~fct_rev(case_match(., 0 ~ "Male", 1 ~ "Female", .default = as.character(sex))))) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = grip_average_kg, y = sex)) +
  geom_boxplot(aes(fill = sex), show.legend = F) +
  scale_fill_manual(values = scales::alpha(c("#324a60", "#009878"), alpha = 0.4)) +
  labs(x = "Grip Strength (kg)") +
  theme_minimal(base_size = 14)



# states at different time points
# eos_reason:
# 0 Completed Participation 
# 1 Patient Withdrawal 
# 2 Disease Progression 
# 3 Death 
# 4 Ineligible 
# 5 PI discretion


glimpse(base)
base %>%
  select(study_id, eos_dov, eos_reason, prog_date, death_date) %>%
  count(eos_reason)

glimpse(long)

 