# ---------------------------------------------------------------------------
# 10_hss_stem_benefit_counts.R
#
# Purpose:
#   HSS-vs-STEM contrast in benefit endorsement, reported in Results alongside
#   Figure 4B: respondent-level count of benefits endorsed (0-5) compared
#   between HSS and STEM editors with a Mann-Whitney (Wilcoxon rank-sum) test.
#   Counting per respondent avoids within-respondent pseudoreplication from
#   testing the 5 benefit items separately. Also reports per-discipline mean
#   endorsement rates (the "68% and 56% versus 23-38%" sentence) and an
#   any-benefit 2x2 Fisher test as an alternative simple framing.
#
# Inputs:
#   data/blinding_survey_dat.csv (read-only)
#
# Outputs:
#   tables/table_hss_stem_benefits.csv   group summaries + test results
#   Console: per-discipline rates, group means, U statistic, p-values
#
# Key parameters / sample construction:
#   sample == "analytical"; respondents with any missing benefit item excluded.
#   HSS  = Arts & Humanities, Social Sciences
#   STEM = Life Sciences, Physical Sciences, Engineering & Technology,
#          Clinical Pre-Clinical & Health
#   Multidisciplinary and "Not indicated" belong to neither group and are
#   excluded from the two-group test (they still appear in the per-discipline
#   table). No stochastic steps; results are deterministic.
#
# Run from repo root:  Rscript scripts/10_hss_stem_benefit_counts.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
})

out_dir <- "tables"
data_path <- "data/blinding_survey_dat.csv"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ben_items <- paste0("benefit_", letters[1:5])
hss_fields <- c("Arts & Humanities", "Social Sciences")
stem_fields <- c("Life Sciences", "Physical Sciences",
                 "Engineering & Technology", "Clinical Pre-Clinical & Health")

dat <- read_csv(data_path, show_col_types = FALSE) %>%
  filter(sample == "analytical") %>%
  mutate(
    # rowSums without na.rm: NA when any benefit item is missing
    n_ben = rowSums(across(all_of(ben_items))),
    group = case_when(
      journal_super_category %in% hss_fields ~ "HSS",
      journal_super_category %in% stem_fields ~ "STEM",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(n_ben))

# --- Per-discipline mean benefit-endorsement rate ---------------------------
disc_rates <- dat %>%
  filter(!is.na(journal_super_category)) %>%
  group_by(journal_super_category) %>%
  summarise(n = n(), mean_rate = mean(n_ben) / 5, .groups = "drop") %>%
  arrange(desc(mean_rate))

cat("=== Per-discipline mean benefit-endorsement rate (item-complete respondents) ===\n")
disc_rates %>%
  mutate(line = sprintf("  %-35s n=%3d  mean rate=%.1f%%",
                        journal_super_category, n, 100 * mean_rate)) %>%
  pull(line) %>% walk(~ cat(.x, "\n"))

# --- HSS vs STEM: per-respondent benefit count ------------------------------
hss  <- dat %>% filter(group == "HSS")  %>% pull(n_ben)
stem <- dat %>% filter(group == "STEM") %>% pull(n_ben)

cat("\n=== HSS vs STEM: number of benefits endorsed (0-5), per respondent ===\n")
cat(sprintf("  HSS : n=%3d  mean=%.2f  median=%.0f  rate=%.1f%%\n",
            length(hss), mean(hss), median(hss), 20 * mean(hss)))
cat(sprintf("  STEM: n=%3d  mean=%.2f  median=%.0f  rate=%.1f%%\n",
            length(stem), mean(stem), median(stem), 20 * mean(stem)))

# Normal approximation with continuity and tie correction (ties preclude the
# exact test), matching scipy.stats.mannwhitneyu(alternative = "two.sided")
mw <- wilcox.test(hss, stem, alternative = "two.sided",
                  exact = FALSE, correct = TRUE)
cat(sprintf("  Mann-Whitney U=%.0f, p=%.2e\n", mw$statistic, mw$p.value))

# --- Any-benefit (>= 1) 2x2 Fisher test -------------------------------------
tbl <- rbind(
  HSS  = c(endorsed = sum(hss  >= 1), none = sum(hss  == 0)),
  STEM = c(endorsed = sum(stem >= 1), none = sum(stem == 0))
)
ft <- fisher.test(tbl)
cat("\n=== Endorsed >=1 benefit (HSS vs STEM) ===\n")
cat(sprintf("  HSS : %d/%d = %.1f%%\n", tbl["HSS", 1], sum(tbl["HSS", ]),
            100 * tbl["HSS", 1] / sum(tbl["HSS", ])))
cat(sprintf("  STEM: %d/%d = %.1f%%\n", tbl["STEM", 1], sum(tbl["STEM", ]),
            100 * tbl["STEM", 1] / sum(tbl["STEM", ])))
cat(sprintf("  Fisher exact OR=%.2f, p=%.2e\n", ft$estimate, ft$p.value))

# --- Table ------------------------------------------------------------------
out <- tibble(
  group = c("HSS", "STEM"),
  n = c(length(hss), length(stem)),
  mean_benefits = c(mean(hss), mean(stem)),
  median_benefits = c(median(hss), median(stem)),
  mean_rate = c(mean(hss), mean(stem)) / 5,
  mw_U = mw$statistic,
  mw_p = mw$p.value,
  fisher_any_or = unname(ft$estimate),
  fisher_any_p = ft$p.value
)
write_csv(out, file.path(out_dir, "table_hss_stem_benefits.csv"))

cat("\nDone. Table written to", out_dir, "\n")
