# ---------------------------------------------------------------------------
# 04_si_figures_tables.R
#
# Supplementary figures and tables for the blinded peer review survey.
# Reproduces the four supplementary figures (endorsement by impact factor
# quartile, business model, and publisher HQ region; and the considered vs.
# not-considered forest plot for single-blind journals) and the three
# supplementary tables (sample characteristics, discipline by publishing
# model, and the four-strategy adjustment sensitivity analysis).
#
# Inputs:
#   data/blinding_survey_dat.csv
#   data/meta_stats.csv
#
# Outputs:
#   figures/FigureS2_if-quartile.png
#   figures/FigureS3_business-model.png
#   figures/FigureS4_hq-region.png
#   figures/FigureS5_considered.png
#   tables/TableS2_sample_characteristics.csv
#   tables/table_discipline_by_model.csv
#   tables/TableS3_adjustment_sensitivity.csv
#
# Run from the repository root:
#   Rscript scripts/04_si_figures_tables.R
# ---------------------------------------------------------------------------

library(tidyverse)
library(scales)
library(patchwork)
library(logistf)

# Fixed seed (20260611) so the Monte Carlo Fisher test (simulate.p.value) used
# for the discipline row of Table S2 gives reproducible p-values across runs
set.seed(20260611)

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)


# --- Theme -----------------------------------------------------------------
theme_blinding <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption = element_text(size = 11, color = "grey30", hjust = 0)
  )
theme_set(theme_blinding)


# --- Colors ----------------------------------------------------------------
colors_type <- c(
  "Challenge" = "#e6550d",
  "Benefit" = "#3182bd"
)

heatmap_benefits_low <- "#deebf7"
heatmap_benefits_high <- "#3182bd"
heatmap_challenges_low <- "#fee8c8"
heatmap_challenges_high <- "#e34a33"


# --- Short labels ----------------------------------------------------------
challenge_labels <- c(
  challenge_a = "Author burden",
  challenge_b = "Editorial office burden",
  challenge_c = "Anonymization not fully successful",
  challenge_d = "Hard to identify conflict of interest",
  challenge_e = "May reduce reviewer participation",
  challenge_f = "Limits contextualizing work"
)

benefit_labels <- c(
  benefit_a = "More fair/objective review",
  benefit_b = "Reduces prestige bias",
  benefit_c = "Reduces gender bias",
  benefit_d = "Reduces country/location bias",
  benefit_e = "Increased review quality"
)

item_labels <- c(challenge_labels, benefit_labels)


# --- Helper: Wilson score CI -----------------------------------------------
wilson_ci <- function(x, n, conf.level = 0.95) {
  if (n == 0) return(tibble(prop = NA_real_, lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - conf.level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- (z / denom) * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  tibble(prop = p_hat, lower = max(0, center - margin), upper = min(1, center + margin))
}


# --- Helper: compute endorsement rates with Wilson CIs ---------------------
calc_endorsement_rates <- function(data, group_vars = NULL) {
  items <- names(item_labels)

  data_long <- data %>%
    select(all_of(c(group_vars, items))) %>%
    pivot_longer(
      cols = all_of(items),
      names_to = "item",
      values_to = "endorsed"
    ) %>%
    mutate(
      type = if_else(str_starts(item, "challenge"), "Challenge", "Benefit"),
      item_label = item_labels[item]
    )

  rates <- data_long %>%
    group_by(across(all_of(c(group_vars, "item", "type", "item_label")))) %>%
    summarise(
      n_endorsed = sum(endorsed, na.rm = TRUE),
      # NA-safe denominator: count only respondents with an observed value,
      # so missing items are never silently treated as non-endorsements
      n_total = sum(!is.na(endorsed)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      ci = list(wilson_ci(n_endorsed, n_total)),
      prop = ci$prop,
      lower = ci$lower,
      upper = ci$upper
    ) %>%
    ungroup() %>%
    select(-ci)

  rates
}


# --- Data import -----------------------------------------------------------
all_data <- read_csv("data/blinding_survey_dat.csv", show_col_types = FALSE)

analytical_dat <- all_data %>% filter(sample == "analytical")
endorsement_dat <- analytical_dat

# Preprocessing provenance (filter cascade and impact-factor augmentation
# counts). Reported in the manuscript text; none of the outputs below depend
# on it, but it is read here so the script's declared inputs match the
# archived data directory.
meta_stats <- read_csv("data/meta_stats.csv", show_col_types = FALSE)


# ===========================================================================
# Table S2: Sample characteristics by publishing model
# ===========================================================================

# Helper: build a cross-tab row block for one variable
crosstab_block <- function(data, var, var_label) {
  data %>%
    filter(!is.na(.data[[var]]) & .data[[var]] != "") %>%
    count(pub_model, .data[[var]]) %>%
    rename(category = 2) %>%
    group_by(pub_model) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(cell = paste0(n, " (", round(pct, 1), "%)")) %>%
    select(pub_model, category, cell) %>%
    pivot_wider(names_from = pub_model, values_from = cell, values_fill = "0 (0.0%)") %>%
    mutate(Variable = var_label, .before = category)
}

# Helper: Fisher exact test p-value for a variable. The exact test is
# deterministic and feasible for all Table S2 variables except the 8x2
# discipline table, where it exhausts the network-algorithm workspace;
# only that case falls back to seeded Monte Carlo simulation.
fisher_p <- function(data, var) {
  tbl <- data %>%
    filter(!is.na(.data[[var]]) & .data[[var]] != "") %>%
    count(pub_model, .data[[var]]) %>%
    rename(category = 2) %>%
    pivot_wider(names_from = pub_model, values_from = n, values_fill = 0L)
  mat <- as.matrix(tbl[, -1])
  exact <- tryCatch(fisher.test(mat, workspace = 2e8)$p.value,
                    error = function(e) NA_real_)
  if (!is.na(exact)) return(exact)
  fisher.test(mat, simulate.p.value = TRUE, B = 10000)$p.value
}

# Helper: attach the p-value to the first row of each variable block
add_p <- function(block, p_val) {
  block$p <- ""
  block$p[1] <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
  block
}

tab1_disc <- crosstab_block(analytical_dat, "journal_super_category", "Discipline")
tab1_biz <- crosstab_block(analytical_dat, "business_model", "Business model")
tab1_lang <- crosstab_block(analytical_dat, "language", "Language")
tab1_hq <- crosstab_block(analytical_dat, "hq_region", "Publisher HQ region")
tab1_if <- crosstab_block(analytical_dat, "if_quartile", "IF quartile")

pval_disc <- fisher_p(analytical_dat, "journal_super_category")
p_biz <- fisher_p(analytical_dat, "business_model")
p_lang <- fisher_p(analytical_dat, "language")
p_hq <- fisher_p(analytical_dat, "hq_region")
p_if <- fisher_p(analytical_dat, "if_quartile")

table1 <- bind_rows(
  add_p(tab1_disc, pval_disc),
  add_p(tab1_biz, p_biz),
  add_p(tab1_lang, p_lang),
  add_p(tab1_hq, p_hq),
  add_p(tab1_if, p_if)
) %>%
  rename(Category = category, `Fisher p` = p)

# Suppress repeated Variable labels
table1 <- table1 %>%
  mutate(Variable = if_else(duplicated(Variable), "", Variable))

write_csv(table1, "tables/TableS2_sample_characteristics.csv")


# ===========================================================================
# Discipline by publishing model (feeds the rendered sample-characteristics table)
# ===========================================================================

tab_s2 <- analytical_dat %>%
  count(journal_super_category, pub_model) %>%
  pivot_wider(names_from = pub_model, values_from = n, values_fill = 0L) %>%
  mutate(Total = `Single blind` + `Double/triple blind`,
         `% double/triple-blind` = round(100 * `Double/triple blind` / Total, 1)) %>%
  arrange(desc(`% double/triple-blind`)) %>%
  rename(Discipline = journal_super_category) %>%
  select(Discipline, `Single blind`, `Double/triple blind`, Total,
         `% double/triple-blind`)

write_csv(tab_s2, "tables/table_discipline_by_model.csv")


# ===========================================================================
# Table S3: Adjustment strategy sensitivity (Firth odds ratios)
#
# Each adjustment strategy uses its own maximal sample (respondents with
# complete data for that model's covariates), rather than restricting all
# models to the smallest common denominator.
# ===========================================================================

base_dat <- endorsement_dat %>%
  filter(
    pub_model == "Double/triple blind" |
      (pub_model == "Single blind" & !is.na(consid_status))
  ) %>%
  mutate(
    db = if_else(pub_model == "Double/triple blind", 1L, 0L),
    log_if = log(impact_factor)
  )

dat_unadj <- base_dat
dat_if    <- base_dat %>% filter(!is.na(impact_factor))
dat_disc  <- base_dat %>%
  filter(journal_super_category != "Not indicated", !is.na(journal_super_category))
dat_full  <- base_dat %>%
  filter(!is.na(impact_factor),
         journal_super_category != "Not indicated", !is.na(journal_super_category))

n_unadj <- nrow(dat_unadj)
n_if    <- nrow(dat_if)
n_disc  <- nrow(dat_disc)
n_full  <- nrow(dat_full)

items <- names(item_labels)

# Helper: fit one Firth model, extract the publishing-model coefficient
fit_one <- function(item_name, data, formula) {
  d <- data %>% mutate(y = .data[[item_name]])
  fit <- logistf(formula, data = d)
  ci <- confint(fit)
  tibble(
    item = item_name,
    item_label = item_labels[item_name],
    type = if_else(str_starts(item_name, "challenge"), "Challenge", "Benefit"),
    or = exp(coef(fit)["db"]),
    lower = exp(ci["db", 1]),
    upper = exp(ci["db", 2]),
    p = fit$prob["db"]
  )
}

# Fit each model on its own proper sample
comp_unadj <- map_dfr(items, ~ fit_one(.x, dat_unadj, y ~ db)) %>%
  mutate(model = "Unadjusted", n_model = n_unadj)

comp_if <- map_dfr(items, ~ fit_one(.x, dat_if, y ~ db + log_if)) %>%
  mutate(model = "+ IF", n_model = n_if)

comp_disc <- map_dfr(items, ~ fit_one(.x, dat_disc, y ~ db + journal_super_category)) %>%
  mutate(model = "+ Discipline", n_model = n_disc)

comp_full <- map_dfr(items, ~ fit_one(.x, dat_full, y ~ db + journal_super_category + log_if)) %>%
  mutate(model = "+ Both", n_model = n_full)

comp_all <- bind_rows(comp_unadj, comp_if, comp_disc, comp_full) %>%
  mutate(
    model_label = paste0(model, " (N = ", n_model, ")"),
    model_label = factor(model_label, levels = c(
      paste0("Unadjusted (N = ", n_unadj, ")"),
      paste0("+ IF (N = ", n_if, ")"),
      paste0("+ Discipline (N = ", n_disc, ")"),
      paste0("+ Both (N = ", n_full, ")")
    ))
  )

# FDR on the discipline-only model (the primary adjustment)
comp_disc_fdr <- comp_disc %>%
  group_by(type) %>%
  mutate(q_disc = p.adjust(p, method = "BH")) %>%
  ungroup()

fmt_or <- function(or, lo, hi) {
  paste0(round(or, 2), " [", round(lo, 2), ", ", round(hi, 2), "]")
}

table_wide <- comp_all %>%
  mutate(or_ci = fmt_or(or, lower, upper)) %>%
  select(type, item, item_label, model_label, or_ci) %>%
  pivot_wider(names_from = model_label, values_from = or_ci) %>%
  left_join(comp_disc_fdr %>% select(item, q_disc), by = "item") %>%
  # Report very small q-values as "<0.001" rather than letting them round to 0
  mutate(`FDR q (discipline)` = if_else(q_disc < 0.001, "<0.001",
                                        sprintf("%.3f", q_disc))) %>%
  select(-item, -q_disc) %>%
  rename(Type = type, Item = item_label)

write_csv(table_wide, "tables/TableS3_adjustment_sensitivity.csv")


# ===========================================================================
# Figure S2: Perceived endorsement by impact factor quartile
# ===========================================================================

if_dat <- endorsement_dat %>%
  filter(
    !is.na(if_quartile),
    pub_model == "Double/triple blind" | !is.na(consid_status)
  )

if_rates <- calc_endorsement_rates(if_dat, group_vars = "if_quartile")

# Add N to quartile labels
if_n <- if_dat %>%
  count(if_quartile) %>%
  mutate(if_label = paste0(if_quartile, "\n(N=", n, ")"))

if_rates <- if_rates %>%
  left_join(if_n, by = "if_quartile") %>%
  mutate(if_label = factor(if_label, levels = if_n$if_label))

# Challenge heatmap
p_chal_if <- ggplot(
    if_rates %>% filter(type == "Challenge"),
    aes(x = item_label, y = if_label, fill = prop)
  ) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(prop * 100), "%")), size = 3) +
  scale_fill_gradient(low = heatmap_challenges_low, high = heatmap_challenges_high,
                      labels = percent, limits = c(0, NA)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  labs(x = NULL, y = NULL, fill = "Rate",
       subtitle = "Challenges") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "right")

# Benefit heatmap
p_ben_if <- ggplot(
    if_rates %>% filter(type == "Benefit"),
    aes(x = item_label, y = if_label, fill = prop)
  ) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(prop * 100), "%")), size = 3) +
  scale_fill_gradient(low = heatmap_benefits_low, high = heatmap_benefits_high,
                      labels = percent, limits = c(0, NA)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  labs(x = NULL, y = NULL, fill = "Rate",
       subtitle = "Benefits") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "right")

fig_s1 <- p_chal_if | p_ben_if

ggplot2::ggsave(
  filename = "figures/FigureS2_if-quartile.png",
  plot = fig_s1, width = 14, height = 5,
  dpi = 300, bg = "white"
)


# ===========================================================================
# Figure S3: Perceived endorsement by business model
# ===========================================================================

# Use correct denominators: SB branch respondents + all DB
biz_dat <- endorsement_dat %>%
  filter(
    !is.na(business_model),
    pub_model == "Double/triple blind" | !is.na(consid_status)
  )

biz_rates <- calc_endorsement_rates(biz_dat,
                                    group_vars = c("pub_model", "business_model"))

# Add n to business_model legend labels. Legend n's are pooled across
# publishing models (the color scale is shared across facets); per-panel n's
# are reported in the caption.
biz_n <- biz_dat %>%
  count(business_model) %>%
  mutate(biz_label = paste0(business_model, " (n = ", n, ")"))

biz_rates <- biz_rates %>%
  left_join(biz_n %>% select(business_model, biz_label), by = "business_model")

colors_biz <- c("for-profit" = "#7570b3", "non-profit" = "#66a61e")
colors_biz_n <- setNames(
  colors_biz[biz_n$business_model],
  biz_n$biz_label
)

biz_rates <- biz_rates %>%
  mutate(item_label = fct_reorder(item_label, prop))

fig_s2 <- ggplot(biz_rates,
                 aes(x = prop, y = item_label, color = biz_label)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.5, linewidth = 0.5,
                  position = position_dodge(width = 0.4)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = colors_biz_n) +
  facet_grid(type ~ pub_model, scales = "free_y", space = "free_y",
             labeller = labeller(pub_model = c("Single blind" = "Single blind",
                                               "Double/triple blind" = "Double-blind"))) +
  labs(x = "Perceived endorsement rate", y = NULL, color = NULL)

ggplot2::ggsave(
  filename = "figures/FigureS3_business-model.png",
  plot = fig_s2, width = 14, height = 8,
  dpi = 300, bg = "white"
)


# ===========================================================================
# Figure S4: Perceived endorsement by publisher HQ region
# ===========================================================================

hq_dat <- endorsement_dat %>%
  filter(
    !is.na(hq_region),
    pub_model == "Double/triple blind" | !is.na(consid_status)
  )

hq_rates <- calc_endorsement_rates(hq_dat, group_vars = "hq_region")

# Add n to legend labels
hq_n <- hq_dat %>%
  count(hq_region) %>%
  mutate(hq_label = paste0(hq_region, " (n = ", n, ")"))
hq_rates <- hq_rates %>%
  left_join(hq_n %>% select(hq_region, hq_label), by = "hq_region")

colors_hq <- c(
  "UK"             = "#1b9e77",
  "USA"            = "#d95f02",
  "Europe (other)" = "#7570b3",
  "Asia-Pacific"   = "#e7298a",
  "Other"          = "#66a61e"
)
colors_hq_n <- setNames(
  colors_hq[hq_n$hq_region],
  hq_n$hq_label
)

hq_rates <- hq_rates %>%
  mutate(item_label = fct_reorder(item_label, prop))

fig_s3 <- ggplot(hq_rates,
                 aes(x = prop, y = item_label, color = hq_label)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.5, linewidth = 0.5,
                  position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = colors_hq_n) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(x = "Perceived endorsement rate", y = NULL, color = NULL)

ggplot2::ggsave(
  filename = "figures/FigureS4_hq-region.png",
  plot = fig_s3, width = 14, height = 8,
  dpi = 300, bg = "white"
)


# ===========================================================================
# Figure S5: Considered vs. not considered (single-blind journals only)
# ===========================================================================

# Only SB journals with a consideration status and discipline data
sb_consid_dat <- endorsement_dat %>%
  filter(pub_model == "Single blind", !is.na(consid_status),
         journal_super_category != "Not indicated", !is.na(journal_super_category)) %>%
  mutate(considered = if_else(consid_status == "Considered", 1L, 0L))

# Fit Firth logistic for each item: endorsed ~ considered + discipline
or_results_6 <- map_dfr(names(item_labels), function(item_name) {
  d <- sb_consid_dat %>% mutate(y = .data[[item_name]])
  fit <- logistf(y ~ considered + journal_super_category, data = d)
  ci <- confint(fit)
  tibble(
    item = item_name,
    item_label = item_labels[item_name],
    type = if_else(str_starts(item_name, "challenge"), "Challenge", "Benefit"),
    or = exp(coef(fit)["considered"]),
    lower = exp(ci["considered", 1]),
    upper = exp(ci["considered", 2]),
    p = fit$prob["considered"]
  )
})

or_results_6 <- or_results_6 %>%
  mutate(item_label = fct_reorder(item_label, or))

fig_s4 <- ggplot(or_results_6, aes(x = or, y = item_label, color = type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.6, linewidth = 0.6) +
  scale_x_log10() +
  scale_color_manual(values = colors_type) +
  labs(x = "Odds ratio (log scale)", y = NULL, color = NULL)

ggplot2::ggsave(
  filename = "figures/FigureS5_considered.png",
  plot = fig_s4, width = 10, height = 7,
  dpi = 300, bg = "white"
)
