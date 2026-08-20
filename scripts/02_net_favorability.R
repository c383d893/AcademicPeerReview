# ---------------------------------------------------------------------------
# 02_net_favorability.R
#
# Purpose:
#   Respondent-level net favorability: do editors experience more benefits than
#   challenges? Restates the headline cross-model gap at the level of the
#   individual editor (net = benefits endorsed - challenges endorsed).
#
# Inputs:
#   data/blinding_survey_dat.csv (read-only)
#
# Outputs:
#   tables/table_descriptives_n299.csv           group descriptives with Wilson CIs
#   tables/table_adjusted_burden_n285.csv        adjusted mean counts (proportional + equal)
#   tables/table_burden_db_coefs.csv             quasibinomial db coefficients
#   tables/table_net_collapsed_cells.csv         collapsed net-score cell counts
#   tables/table_bootstrap_contrasts_n299.csv    bootstrap contrasts
#
# Key parameters / analytic frame (matches the GLMM spec / Fig 4A):
#   sample == "analytical"; DB = all double/triple-blind respondents;
#   SB = single-blind respondents routed to the branching question
#   (consid_status in {Considered, Not considered}). n = 299 (158 SB / 141 DB),
#   zero item missingness. Adjusted models drop journal_super_category ==
#   "Not indicated"/NA, matching the published Firth models (n = 285:
#   150 SB / 135 DB).
#   Per respondent: n_chal = sum(challenge_a..f) (0-6);
#   n_ben = sum(benefit_a..e) (0-5); net = n_ben - n_chal (-6..+5).
#   Models: (1) quasibinomial burden GLMs + emmeans; (2) ordinal CLM on
#   collapsed net + OLS/HC3 sensitivity; (3) HEADLINE Firth logistic for
#   I(net > 0) ~ db + discipline (n = 285); stratified bootstrap B = 10,000.
#   seed = 20260610.
#
# Run from repo root:  Rscript scripts/02_net_favorability.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(logistf)
  library(ordinal)
  library(sandwich)
  library(lmtest)
  library(emmeans)
})

out_dir <- "tables"
data_path <- "data/blinding_survey_dat.csv"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(20260610)

# --- Data prep --------------------------------------------------------------

all_data <- read_csv(data_path, show_col_types = FALSE)

chal_items <- paste0("challenge_", letters[1:6])
ben_items  <- paste0("benefit_",  letters[1:5])

dat <- all_data %>%
  filter(sample == "analytical") %>%
  filter(
    pub_model == "Double/triple blind" |
      (pub_model == "Single blind" &
         consid_status %in% c("Considered", "Not considered"))
  ) %>%
  mutate(
    db = if_else(pub_model == "Double/triple blind", 1L, 0L),
    n_chal = rowSums(across(all_of(chal_items))),
    n_ben  = rowSums(across(all_of(ben_items))),
    net    = n_ben - n_chal,
    net_std = n_ben / 5 - n_chal / 6,
    group  = factor(if_else(db == 1L, "Double/triple blind (experienced)",
                            "Single blind (anticipated)"),
                    levels = c("Double/triple blind (experienced)",
                               "Single blind (anticipated)"))
  )

# Frame checks: n = 299 (158 SB / 141 DB), zero item missingness
stopifnot(nrow(dat) == 299,
          sum(dat$db == 0) == 158,
          sum(dat$db == 1) == 141,
          !anyNA(dat[, c(chal_items, ben_items)]))

# Discipline-complete frame for adjusted models (matches published Firth models)
dat_adj <- dat %>%
  filter(!is.na(journal_super_category),
         journal_super_category != "Not indicated") %>%
  mutate(journal_super_category = factor(journal_super_category))
stopifnot(nrow(dat_adj) == 285,
          sum(dat_adj$db == 0) == 150,
          sum(dat_adj$db == 1) == 135)

# --- Descriptives (n = 299, unadjusted) -------------------------------------

wilson_ci <- function(x, n, conf = 0.95) {
  ci <- prop.test(x, n, correct = FALSE)$conf.int
  c(lower = ci[1], upper = ci[2])
}

desc <- dat %>%
  group_by(group) %>%
  summarize(
    n = n(),
    mean_chal = mean(n_chal),
    mean_ben = mean(n_ben),
    mean_net = mean(net),
    median_net = median(net),
    n_net_pos = sum(net > 0),
    n_net_zero = sum(net == 0),
    n_net_neg = sum(net < 0),
    p_net_pos = mean(net > 0),
    p_net_neg = mean(net < 0),
    p_zero_chal = mean(n_chal == 0),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    pos_lo = wilson_ci(n_net_pos, n)[1], pos_hi = wilson_ci(n_net_pos, n)[2],
    neg_lo = wilson_ci(n_net_neg, n)[1], neg_hi = wilson_ci(n_net_neg, n)[2]
  ) %>%
  ungroup()

write_csv(desc, file.path(out_dir, "table_descriptives_n299.csv"))

# --- Model 1: bounded-binomial burden models (n = 285, adjusted) -------------

fit_chal <- glm(cbind(n_chal, 6 - n_chal) ~ db + journal_super_category,
                family = quasibinomial, data = dat_adj)
fit_ben  <- glm(cbind(n_ben, 5 - n_ben) ~ db + journal_super_category,
                family = quasibinomial, data = dat_adj)

# Primary: population-averaged adjusted means (weights = "proportional",
# averaging over the observed discipline distribution). Sensitivity: emmeans
# default equal weighting across discipline levels (balanced-population means).
emm_grid <- function(fit, k, label, wts) {
  emmeans(fit, ~ db, type = "response", weights = wts) %>%
    as_tibble() %>%
    mutate(outcome = label, weighting = wts,
           adj_mean_count = prob * k,
           count_lo = asymp.LCL * k, count_hi = asymp.UCL * k)
}

burden_tab <- bind_rows(
  emm_grid(fit_chal, 6, "challenges (of 6)", "proportional"),
  emm_grid(fit_ben,  5, "benefits (of 5)",   "proportional"),
  emm_grid(fit_chal, 6, "challenges (of 6)", "equal"),
  emm_grid(fit_ben,  5, "benefits (of 5)",   "equal")
) %>%
  mutate(group = if_else(db == 1, "DB (experienced)", "SB (anticipated)")) %>%
  select(outcome, weighting, group, prob, adj_mean_count, count_lo, count_hi)
write_csv(burden_tab, file.path(out_dir, "table_adjusted_burden_n285.csv"))

# db coefficient tests for the burden models (quasibinomial t-tests)
burden_coef <- bind_rows(
  broom::tidy(fit_chal) %>% mutate(outcome = "challenges"),
  broom::tidy(fit_ben)  %>% mutate(outcome = "benefits")
) %>% filter(term == "db")
write_csv(burden_coef, file.path(out_dir, "table_burden_db_coefs.csv"))

# --- Model 2: ordinal CLM on collapsed net + OLS/HC3 sensitivity -------------

dat_adj <- dat_adj %>%
  mutate(
    net_collapsed = case_when(
      net <= -4 ~ "<=-4",
      net >=  4 ~ ">=+4",
      TRUE      ~ as.character(net)
    ),
    net_collapsed = factor(net_collapsed,
                           levels = c("<=-4", "-3", "-2", "-1", "0",
                                      "1", "2", "3", ">=+4"),
                           ordered = TRUE)
  )

cell_counts <- dat_adj %>% count(group, net_collapsed)
write_csv(cell_counts, file.path(out_dir, "table_net_collapsed_cells.csv"))

fit_clm <- clm(net_collapsed ~ db + journal_super_category, data = dat_adj)
clm_ci <- confint(fit_clm)   # profile-likelihood CI
clm_or <- exp(c(coef(fit_clm)["db"], clm_ci["db", ]))
clm_p <- summary(fit_clm)$coefficients["db", "Pr(>|z|)"]

# OLS sensitivity with HC3 sandwich SEs
fit_ols <- lm(net ~ db + journal_super_category, data = dat_adj)
ols_hc3 <- coeftest(fit_ols, vcov = vcovHC(fit_ols, type = "HC3"))
ols_ci  <- coefci(fit_ols, vcov = vcovHC(fit_ols, type = "HC3"))["db", ]

# --- Model 3 (HEADLINE): Firth logistic for net > 0 (n = 285) ----------------

fit_firth <- logistf(I(net > 0) ~ db + journal_super_category, data = dat_adj)
firth_ci <- confint(fit_firth)
firth_or <- exp(c(coef(fit_firth)["db"], firth_ci["db", ]))
firth_p  <- fit_firth$prob["db"]

# Sensitivity: standardized net score (equalizes the 6-vs-5 item asymmetry)
fit_firth_std <- logistf(I(net_std > 0) ~ db + journal_super_category,
                         data = dat_adj)
firth_std_ci <- confint(fit_firth_std)
firth_std_or <- exp(c(coef(fit_firth_std)["db"], firth_std_ci["db", ]))
firth_std_p  <- fit_firth_std$prob["db"]

# Sensitivity: ordinal CLM on standardized score direction is the same model;
# instead rerun the CLM on the raw-net collapse using net_std sign categories
# is redundant -- the binary Firth + the OLS on net_std cover it.
fit_ols_std <- lm(net_std ~ db + journal_super_category, data = dat_adj)
ols_std_hc3 <- coeftest(fit_ols_std, vcov = vcovHC(fit_ols_std, type = "HC3"))
ols_std_ci  <- coefci(fit_ols_std, vcov = vcovHC(fit_ols_std, type = "HC3"))["db", ]

# --- Stratified nonparametric bootstrap (n = 299, unadjusted) ----------------

B <- 10000
sb_net <- dat$net[dat$db == 0]
db_net <- dat$net[dat$db == 1]

boot_one <- function() {
  sb <- sample(sb_net, length(sb_net), replace = TRUE)
  db <- sample(db_net, length(db_net), replace = TRUE)
  c(d_pos = mean(db > 0) - mean(sb > 0),
    d_neg = mean(db < 0) - mean(sb < 0),
    d_mean = mean(db) - mean(sb))
}
boot_res <- t(replicate(B, boot_one()))

boot_tab <- tibble(
  contrast = c("P(net>0): DB - SB", "P(net<0): DB - SB", "mean net: DB - SB"),
  observed = c(mean(db_net > 0) - mean(sb_net > 0),
               mean(db_net < 0) - mean(sb_net < 0),
               mean(db_net) - mean(sb_net)),
  boot_lo = apply(boot_res, 2, quantile, 0.025),
  boot_hi = apply(boot_res, 2, quantile, 0.975)
)
write_csv(boot_tab, file.path(out_dir, "table_bootstrap_contrasts_n299.csv"))


# --- Console report -----------------------------------------------------------

cat("=============================================================\n")
cat("NET FAVORABILITY: benefits endorsed minus challenges endorsed\n")
cat("=============================================================\n\n")

cat("Frame: n =", nrow(dat), "(SB", sum(dat$db == 0), "/ DB", sum(dat$db == 1),
    "); adjusted models n =", nrow(dat_adj), "(SB", sum(dat_adj$db == 0),
    "/ DB", sum(dat_adj$db == 1), ")\n\n")

cat("--- Descriptives (n = 299, unadjusted) ---\n")
print(as.data.frame(desc %>% select(group, n, mean_chal, mean_ben, mean_net,
                                    median_net, p_net_pos, p_net_neg,
                                    p_zero_chal)), digits = 3)

cat("\nPer-group Wilson 95% CIs:\n")
print(as.data.frame(desc %>%
        transmute(group,
                  p_net_pos = sprintf("%.1f%% [%.1f, %.1f]",
                                      100*p_net_pos, 100*pos_lo, 100*pos_hi),
                  p_net_neg = sprintf("%.1f%% [%.1f, %.1f]",
                                      100*p_net_neg, 100*neg_lo, 100*neg_hi))))

cat("\n--- Model 1: adjusted mean counts (quasibinomial + emmeans, n = 285) ---\n")
print(as.data.frame(burden_tab), digits = 3)
cat("\ndb coefficients (log-odds scale, quasibinomial t-tests):\n")
print(as.data.frame(burden_coef %>% select(outcome, estimate, std.error,
                                           statistic, p.value)), digits = 3)

cat("\n--- Model 2: ordinal CLM on collapsed net (n = 285) ---\n")
cat(sprintf("Cumulative OR for db (toward higher net): %.2f [%.2f, %.2f], p = %.3g\n",
            clm_or[1], clm_or[2], clm_or[3], clm_p))
cat(sprintf("OLS sensitivity (HC3): db = %.2f net-score units [%.2f, %.2f], p = %.3g\n",
            ols_hc3["db", "Estimate"], ols_ci[1], ols_ci[2],
            ols_hc3["db", "Pr(>|t|)"]))

cat("\n--- Model 3 HEADLINE: Firth logistic, I(net > 0) ~ db + discipline (n = 285) ---\n")
cat(sprintf("OR (DB vs SB) = %.2f [%.2f, %.2f], p = %.3g\n",
            firth_or[1], firth_or[2], firth_or[3], firth_p))

cat("\n--- Sensitivity: standardized net (n_ben/5 - n_chal/6) ---\n")
cat(sprintf("Firth OR for I(net_std > 0): %.2f [%.2f, %.2f], p = %.3g\n",
            firth_std_or[1], firth_std_or[2], firth_std_or[3], firth_std_p))
cat(sprintf("OLS (HC3) on net_std: db = %.3f [%.3f, %.3f], p = %.3g\n",
            ols_std_hc3["db", "Estimate"], ols_std_ci[1], ols_std_ci[2],
            ols_std_hc3["db", "Pr(>|t|)"]))

cat("\n--- Stratified bootstrap (B = 10,000, percentile, n = 299) ---\n")
print(as.data.frame(boot_tab), digits = 3)

cat("\nDone. Tables written to", out_dir, "\n")
