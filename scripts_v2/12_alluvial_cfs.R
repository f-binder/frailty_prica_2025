# ==============================================================================
# 12_alluvial_cfs.R
#
# Purpose : Visual intuition for "repeated CFS predicts outcome": an alluvial
#           (Sankey-type) plot of each patient's state at baseline, 1, 3, 6
#           and 12 months, showing that most progressions / deaths flow out of
#           the frail (CFS >= 4) stratum rather than the non-frail one.
#           Based on fu_alluvial_20251125.R (project folder), with these changes:
#   - Events are placed by their DATE (event on or before the visit's target
#     date = infusion + visit month), not by the end-of-study reason, and the
#     event state is absorbing (once progressed/dead, always progressed/dead).
#     Progression uses prog_date_final (with the proxy for relapse deaths
#     without a progression date, see 02_clean_derive.R).
#   - Event-free patients at a visit:
#       Non-frail (CFS 1-3) / Frail (CFS >= 4) if CFS was assessed;
#       "No CFS" if still followed in the study but CFS not available at that
#         visit (missed visit or CFS not done);
#       "Censored / off study" if no further study assessment (left the
#         frailty study, or the visit falls after the data cut / last contact).
#     Main version: no imputation. Alternative version (_locf): a missed visit
#     between two attended ones carries the previous CFS forward, as in the
#     original script.
#   - Two endpoints: progression or death (PFS-type, main) and death only
#     (OS-type; the time-varying Cox model is significant after adjustment for
#     OS but not for PFS, see 08_survival_baseline.R).
#   - Visits drawn evenly spaced.
#   - Table of transitions: among event-free patients who are frail vs
#     non-frail at a visit, how many have the event by the next visit (Fisher
#     exact test, descriptive; patients contribute to several intervals).
# Inputs  : data/processed/v2/analysis_wide.rds, analysis_long.rds
# Outputs : output/v2/figures/fig5_alluvial_cfs_{pfs,os}[_locf].*
#           output/v2/tables/alluvial_states_{pfs,os}.csv,
#           alluvial_transitions_{pfs,os}.csv
# ==============================================================================

source(here::here("scripts_v2", "00_setup.R"))
SCRIPT_NAME <- "12_alluvial_cfs.R"
section("12 - Alluvial plot: CFS state and progression / death over the first year")

if (!requireNamespace("ggalluvial", quietly = TRUE)) stop("Please install ggalluvial: install.packages('ggalluvial')")
library(ggalluvial)

w  <- read_proc("analysis_wide")
al <- read_proc("analysis_long")

FRAIL_CUT <- 4   # CFS >= 4 = frail (same as cfs_cat ">3")
STATE_LEVELS <- c("Censored / off study", "Non-frail (CFS 1-3)", "No CFS", "Frail (CFS 4+)", "EVENT")
STATE_COLS <- c("#d0d0d0", "#2a9d8f", "#e9c46a", "#e07a5f", "#6b6b6b")

## ---- Patient x visit states -------------------------------------------------------------

grid <- expand_grid(study_id = w$study_id, visit_month = unname(VISITS)) %>%
  left_join(w %>% select(study_id, date_infusion, eos_dov, last_contact, death_date, prog_date_final), by = "study_id") %>%
  left_join(al %>% select(study_id, visit_month, dov, cfs_score), by = c("study_id", "visit_month")) %>%
  mutate(target_date = date_infusion + round(visit_month * DAYS_PER_MONTH),
         attended = !is.na(dov)) %>%
  group_by(study_id) %>%
  arrange(visit_month, .by_group = TRUE) %>%
  mutate(later_attended = rev(cumsum(rev(lead(attended, default = FALSE)))) > 0,
         earlier_cfs    = lag(cfs_score),
         # last observed CFS before this visit (for the LOCF version)
         locf_cfs       = { x <- cfs_score; for (i in seq_along(x)[-1]) if (is.na(x[i])) x[i] <- x[i - 1]; lag(x) }) %>%
  ungroup()

derive_states <- function(endpoint = c("pfs", "os"), locf = FALSE) {
  endpoint <- match.arg(endpoint)
  ev_label <- if (endpoint == "pfs") "Progression / death" else "Death"
  grid %>%
    mutate(
      event_date = if (endpoint == "pfs") pmin(prog_date_final, death_date, na.rm = TRUE) else death_date,
      event_by_visit = visit_month > 0 & !is.na(event_date) & event_date <= target_date,
      cfs_used = case_when(!is.na(cfs_score) ~ cfs_score,
                           locf & !attended & later_attended ~ locf_cfs,
                           TRUE ~ NA_real_),
      # in the frailty study at this visit: attended it or a later one, or not yet
      # off study (end-of-study date, data cut and last contact all after the visit)
      still_followed = attended | later_attended |
        target_date <= pmin(coalesce(eos_dov, DATA_CUT), DATA_CUT, last_contact),
      state = case_when(
        event_by_visit                 ~ ev_label,
        !is.na(cfs_used) & cfs_used >= FRAIL_CUT ~ STATE_LEVELS[4],
        !is.na(cfs_used)               ~ STATE_LEVELS[2],
        still_followed                 ~ STATE_LEVELS[3],
        TRUE                           ~ STATE_LEVELS[1]),
      state = factor(state, levels = replace(STATE_LEVELS, 5, ev_label)),
      visit = factor(names(VISITS)[match(visit_month, VISITS)], levels = names(VISITS)),
      pos = as.integer(visit))
}

## ---- Plot ---------------------------------------------------------------------------------

plot_alluvial <- function(st, title, subtitle) {
  cols <- setNames(STATE_COLS, levels(st$state))
  x_lab <- c("Baseline", paste("Month", VISITS[-1]))
  ggplot(st, aes(x = pos, stratum = state, alluvium = study_id, fill = state)) +
    geom_flow(alpha = 0.6, width = 0.32) +
    geom_stratum(width = 0.32, alpha = 0.95, colour = "white", linewidth = 0.4) +
    geom_text(stat = "stratum", size = 2.9, lineheight = 0.9,
              aes(label = ifelse(after_stat(count) >= 3,
                                 paste0(after_stat(count), "\n(", percent(after_stat(prop), accuracy = 1), ")"), ""),
                  colour = after_stat(stratum) %in% levels(st$state)[c(4, 5)])) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = COL$ink), guide = "none") +
    scale_fill_manual(values = cols, name = NULL, drop = FALSE) +
    scale_x_continuous(breaks = seq_along(VISITS), labels = x_lab, expand = expansion(add = 0.25)) +
    scale_y_continuous(breaks = seq(0, 80, 20), expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Patients") +
    theme_pub(10) +
    theme(legend.position = "top", panel.grid.major.y = element_blank(), axis.line.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_text(size = 10, colour = COL$ink)) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(alpha = 1, colour = NA)))
}

## ---- Transitions table --------------------------------------------------------------------

transitions <- function(st) {
  ev <- levels(st$state)[5]
  pairs <- st %>% select(study_id, pos, visit, state) %>%
    left_join(st %>% transmute(study_id, pos = pos - 1L, next_visit = visit, next_state = state), by = c("study_id", "pos")) %>%
    filter(!is.na(next_state), state %in% levels(st$state)[c(2, 4)]) %>%
    mutate(frail = state == levels(st$state)[4], event_next = next_state == ev)
  per_interval <- pairs %>%
    group_by(interval = paste(visit, "→", next_visit), pos) %>%
    group_modify(function(d, k) {
      tab <- table(factor(d$frail, c(FALSE, TRUE)), factor(d$event_next, c(FALSE, TRUE)))
      tibble(nonfrail_n = sum(!d$frail), nonfrail_events = sum(d$event_next[!d$frail]),
             frail_n = sum(d$frail), frail_events = sum(d$event_next[d$frail]),
             p = if (all(rowSums(tab) > 0)) fisher.test(tab)$p.value else NA_real_)
    }) %>% ungroup() %>% arrange(pos) %>% select(-pos)
  pooled <- pairs %>% summarise(nonfrail_n = sum(!frail), nonfrail_events = sum(event_next[!frail]),
                                frail_n = sum(frail), frail_events = sum(event_next[frail])) %>%
    mutate(interval = "All intervals (patient-intervals)", p = NA_real_)
  bind_rows(per_interval, pooled) %>%
    mutate(nonfrail_pct = pct(nonfrail_events / nonfrail_n, 0), frail_pct = pct(frail_events / frail_n, 0),
           rr = (frail_events / frail_n) / (nonfrail_events / nonfrail_n))
}

print_transitions <- function(tr, ev_label) {
  message(sprintf("\nEvent (%s) by the next visit, among event-free patients frail vs non-frail at a visit %s:",
                  str_to_lower(ev_label), SIG_LEGEND))
  print(tr %>% transmute(interval,
                         `non-frail` = sprintf("%d/%d (%s%%)", nonfrail_events, nonfrail_n, nonfrail_pct),
                         frail = sprintf("%d/%d (%s%%)", frail_events, frail_n, frail_pct),
                         ratio = ifelse(is.finite(rr), sprintf("%.1f", rr), "-"), p = fmt_p_star(p)), n = Inf)
}

## ---- Run: both endpoints, with and without LOCF ---------------------------------------------

for (ep in c("pfs", "os")) {
  ev_label <- if (ep == "pfs") "Progression / death" else "Death"
  for (locf in c(FALSE, TRUE)) {
    st <- derive_states(ep, locf)
    suffix <- paste0(ep, if (locf) "_locf" else "")
    sub <- paste0(
      "Event-free patients by Clinical Frailty Scale at each visit; ",
      str_to_lower(ev_label), " is absorbing (dated on or before the visit)",
      if (locf) "\nMissed visits between two attended visits carry the previous CFS forward" else
        "\n\"No CFS\": still in the study but CFS not available at that visit (no imputation)")
    p <- plot_alluvial(st, sprintf("CFS state and %s over the first year after CAR T (n=%d)",
                                   str_to_lower(ev_label), n_distinct(st$study_id)), sub)
    save_figure(p, paste0("fig5_alluvial_cfs_", suffix), width = 10, height = 6)

    if (!locf) {
      counts <- st %>% count(visit, state) %>% group_by(visit) %>% mutate(pct = pct(n / sum(n), 0)) %>% ungroup()
      save_table(counts, paste0("alluvial_states_", ep))
      message(sprintf("\n%s - states by visit:", ev_label))
      print(counts %>% transmute(visit, state, cell = sprintf("%d (%s%%)", n, pct)) %>%
              pivot_wider(names_from = visit, values_from = cell, values_fill = "0"), width = Inf)

      tr <- transitions(st)
      save_table(tr %>% mutate(rr = round(rr, 2)), paste0("alluvial_transitions_", ep))
      print_transitions(tr, ev_label)
      pooled <- tr %>% filter(str_detect(interval, "^All"))
      record(paste0("alluvial_", ep, "_next_event_pct_frail"), pooled$frail_pct,
             note = sprintf("%d/%d patient-intervals", pooled$frail_events, pooled$frail_n))
      record(paste0("alluvial_", ep, "_next_event_pct_nonfrail"), pooled$nonfrail_pct,
             note = sprintf("%d/%d patient-intervals", pooled$nonfrail_events, pooled$nonfrail_n))

      # where do the first-year events come from? state at the last visit before the event
      src <- st %>% arrange(study_id, pos) %>% group_by(study_id) %>%
        mutate(prev_state = lag(state)) %>%
        filter(state == ev_label, lag(state) != ev_label) %>% ungroup() %>%
        count(prev_state) %>% mutate(pct = pct(n / sum(n), 0))
      message(sprintf("%s within 12 months (n=%d): state at the preceding visit", ev_label, sum(src$n)))
      print(src)
      save_table(src, paste0("alluvial_event_source_", ep))
    }
  }
}

message("12_alluvial_cfs.R complete.")
