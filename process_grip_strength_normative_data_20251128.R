grip_strength_path <- "utils/Normative_grip_strength_data.xlsx"

vector_colnames_gs <- 
  readxl::read_xlsx(grip_strength_path, col_names = F, n_max = 2) %>%
  summarise_all(
    ~paste0(tolower(.[1]), "_", .[2])
  ) %>%
  mutate(across(everything(), ~str_remove(., "^NA_"))) %>%
  janitor::make_clean_names()

vector_colnames_gs

tidy_gs_table <- readxl::read_xlsx(grip_strength_path, skip = 2, col_types = "text",
                  col_names = vector_colnames_gs) %>%
  mutate(age2 = age) %>%
  tidyr::fill(age, .direction = "down") %>%
  select(age, hand, ends_with("kg")) %>%
  mutate(age_range_min = as.numeric(str_extract(age, "^\\d+")),
         age_range_max = as.numeric(ifelse(!is.na(str_extract(age, "\\d+$")),
                                str_extract(age, "\\d+$"),
                                "130"))) %>%
  relocate(age, age_range_min, age_range_max)

tidy_gs_table

# all columns that should be numbers are numbers?
tidy_gs_table %>%
  summarise(across(ends_with("_kg"),
                   ~all(str_detect(., "^\\d+\\.?\\d?"))))

tidy_gs_table %>%
  summarise(across(ends_with("_kg"),
                   ~all(!is.na(as.numeric(.)))))
    # ok numbers



td_gs <- tidy_gs_table %>%
  group_by(age, age_range_min, age_range_max) %>%
  summarise(
    all_lr = all(hand %in% c("L", "R")) & any(hand == "R") & any(hand == "R"),
    all_2_lines = n() == 2,
    across(ends_with("_kg"), ~mean(as.numeric(.)))
  ) %>%
  ungroup() %>%
  arrange(age_range_min) %>%
  select(-starts_with("all_")) %>%
  group_by(pick(starts_with("age"))) %>%
  pivot_longer(cols = ends_with("_kg")) %>%
  mutate(sex = str_to_title(str_extract(name, "male|female")),
         measure = str_extract(name, "sd|mean")) %>%
  ungroup() %>%
  select(age, age_range_min, age_range_max, sex, measure, value) %>%
  pivot_wider(id_cols = c(starts_with("age"), sex), names_from = "measure", values_from = "value") %>%
  rename(mean_kg = mean, sd_kg = sd)

td_gs
td_gs %>%
  cw()
td_gs %>%
  writexl::write_xlsx("utils/processed_grip_strength_normative_data.xlsx")

td_gs_for_plot <- td_gs %>%
  rowwise() %>%
  mutate(mid_age_point = mean(c_across(c(age_range_min, age_range_max)))) %>%
  ungroup() %>%
  mutate(across(mid_age_point, ~ifelse(. > 90, 80, .)))

td_gs_for_plot %>%
  ggplot(aes(x = mid_age_point, y = mean_kg)) +
  # geom_rect(aes(xmin = 0, xmax = 100, ymin = -Inf, ymax = 10), fill = "white") +
  geom_point(aes(color = sex), position = position_dodge(width = 1), size = 2.5) +
  geom_errorbar(aes(ymin = mean_kg - 2*sd_kg,
                    ymax = mean_kg + 2*sd_kg,
                    color = sex), position = position_dodge(width = 1),
                linewidth = 0.75) +
  # geom_text(aes(y =8, label = age), hjust = 1, angle = 90) +
  # scale_y_continuous(limits = c(0, NA), breaks = c(20, 40, 60)) +
  scale_x_continuous(breaks = td_gs_for_plot$mid_age_point,
                     labels = td_gs_for_plot$age) +
  # coord_cartesian(xlim = c(4, 85)) +
  theme_minimal(base_size = 14) +
  labs(y = "Mean Grip Strength (+/- 2*SD)", x = "Age Group (y)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

c(unique(td_gs_for_plot$mid_age_point[!is.na(td_gs_for_plot$mid_age_point)]), 80)
