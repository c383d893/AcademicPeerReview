# ---------------------------------------------------------------------------
# 01_robustness_family_glmm.R
#
# Purpose:
#   Pooled family-level perception gap and item-by-model heterogeneity (GLMM).
#   Replaces 11 anticonservative per-item tests (and the audit-flagged,
#   mislabeled single-OR GLMM robustness check) with:
#     (1) one pooled DB-vs-SB odds ratio per item family (challenges, benefits)
#         from a respondent-level random-intercept GLMM, and
#     (2) a formal item x model heterogeneity test (LRT of m1 vs m0),
#     (3) per-item follow-up contrasts via emmeans, BH-corrected within family,
#     (4) a cluster-robust stacked-GLM companion estimate of the pooled OR.
#
# Inputs:
#   data/blinding_survey_dat.csv (311 rows; read-only)
#
# Outputs:
#   tables/table_pooled_or_lrt.csv        pooled ORs + LRTs, primary and sensitivity
#   tables/table_glm_cluster_robust.csv   cluster-robust marginal companion
#   tables/table_peritem_emmeans_bh.csv   per-item OR, CI, BH q
#
# Key parameters / sample construction:
#   Analytical sample: sample == "analytical" (pub_model non-NA; n = 301)
#   Branch-denominator rule: single-blind respondents enter ONLY if
#     consid_status is "Considered" or "Not considered" (drops 2 SB; n = 299)
#   PRIMARY discipline handling: drop journal_super_category == "Not indicated"
#     (matches the published Firth analytic sample; n = 285)
#   SENSITIVITY: retain "Not indicated" as an explicit factor level (n = 299)
#
# Models (per family, fitted separately because the two families have
# opposite-signed gaps that cancel when pooled):
#   m0: y ~ item + db + journal_super_category + (1 | respondent_id)
#   m1: y ~ item * db + journal_super_category + (1 | respondent_id)
#   binomial, default Laplace approximation (nAGQ = 1), bobyqa optimizer.
#   seed = 42.
#
# Run from repo root:  Rscript scripts/01_robustness_family_glmm.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(emmeans)
  library(sandwich)
})

out_dir <- "tables"
data_path <- "data/blinding_survey_dat.csv"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(42)

# --- Item labels ------------------------------------------------------------
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

# --- Data preparation -------------------------------------------------------
raw <- read_csv(data_path, show_col_types = FALSE)

base_dat <- raw %>%
  filter(sample == "analytical") %>%                       # n = 301
  # Branch-denominator rule: DB all; SB only if routed to the branching Q
  filter(
    pub_model == "Double/triple blind" |
      (pub_model == "Single blind" &
         consid_status %in% c("Considered", "Not considered"))
  ) %>%                                                    # n = 299
  mutate(
    db = factor(if_else(pub_model == "Double/triple blind", "DB", "SB"),
                levels = c("SB", "DB")),
    respondent_id = factor(row_number())
  )

stopifnot(nrow(base_dat) == 299,
          sum(base_dat$db == "SB") == 158,
          sum(base_dat$db == "DB") == 141)

make_long <- function(dat) {
  dat %>%
    select(respondent_id, db, journal_super_category,
           all_of(names(item_labels))) %>%
    pivot_longer(all_of(names(item_labels)),
                 names_to = "item", values_to = "y") %>%
    mutate(
      family = if_else(str_starts(item, "challenge"), "Challenge", "Benefit"),
      item = factor(item, levels = names(item_labels))
    )
}

# PRIMARY sample: drop "Not indicated" discipline (matches Firth models, n = 285)
primary_dat <- base_dat %>%
  filter(journal_super_category != "Not indicated",
         !is.na(journal_super_category)) %>%
  mutate(journal_super_category = factor(journal_super_category))

# SENSITIVITY sample: keep "Not indicated" as explicit level (n = 299)
sens_dat <- base_dat %>%
  mutate(journal_super_category = factor(journal_super_category))

cat(sprintf("Primary sample n = %d (SB %d, DB %d); sensitivity n = %d\n",
            nrow(primary_dat),
            sum(primary_dat$db == "SB"), sum(primary_dat$db == "DB"),
            nrow(sens_dat)))

long_primary <- make_long(primary_dat)
long_sens    <- make_long(sens_dat)

# --- Model fitting per family -----------------------------------------------
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

fit_family <- function(long_all, fam, label, profile_ci = TRUE) {
  d <- long_all %>% filter(family == fam) %>% droplevels()

  m0 <- glmer(y ~ item + db + journal_super_category + (1 | respondent_id),
              data = d, family = binomial, control = ctrl)
  m1 <- glmer(y ~ item * db + journal_super_category + (1 | respondent_id),
              data = d, family = binomial, control = ctrl)

  # Pooled DB effect: Wald CI always; profile CI if it converges
  est <- fixef(m0)["dbDB"]
  wald <- confint(m0, parm = "dbDB", method = "Wald")
  prof <- if (profile_ci) {
    tryCatch(
      suppressMessages(confint(m0, parm = "dbDB", method = "profile")),
      error = function(e) matrix(NA_real_, 1, 2)
    )
  } else matrix(NA_real_, 1, 2)

  lrt <- anova(m0, m1)

  # Cluster-robust stacked-GLM companion (no random intercept; clustered SEs)
  g <- glm(y ~ item + db + journal_super_category, data = d, family = binomial)
  V <- vcovCL(g, cluster = d$respondent_id)
  b <- coef(g)["dbDB"]
  se <- sqrt(V["dbDB", "dbDB"])
  z <- b / se
  glm_row <- tibble(
    family = fam,
    or = exp(b),
    lo = exp(b - 1.96 * se),
    hi = exp(b + 1.96 * se),
    p = 2 * pnorm(-abs(z))
  )

  # Per-item follow-up from the interaction model
  emm <- emmeans(m1, ~ db | item, type = "response")
  ctr <- contrast(emm, method = "revpairwise", by = "item")  # DB / SB odds
  ctr_sum <- summary(ctr, by = NULL, adjust = "BH", infer = TRUE) %>%
    as_tibble() %>%
    mutate(family = fam)

  list(
    family = fam, label = label, n_resp = n_distinct(d$respondent_id),
    m0 = m0, m1 = m1,
    pooled = tibble(
      family = fam,
      or = exp(est),
      wald_lo = exp(wald[1]), wald_hi = exp(wald[2]),
      prof_lo = exp(prof[1]), prof_hi = exp(prof[2]),
      p_wald = 2 * pnorm(-abs(est / sqrt(vcov(m0)["dbDB", "dbDB"]))),
      resp_sd = attr(VarCorr(m0)$respondent_id, "stddev")
    ),
    lrt = tibble(
      family = fam,
      chisq = lrt$Chisq[2], df = lrt$Df[2], p = lrt$`Pr(>Chisq)`[2]
    ),
    glm_cl = glm_row,
    items = ctr_sum
  )
}

cat("\n=== PRIMARY (n = 285, discipline-adjusted, Laplace) ===\n")
res_ch <- fit_family(long_primary, "Challenge", "Challenges")
res_be <- fit_family(long_primary, "Benefit",   "Benefits")

report_family <- function(res) {
  p <- res$pooled; l <- res$lrt; g <- res$glm_cl
  cat(sprintf(
    paste0("%s (n = %d respondents)\n",
           "  pooled OR(DB vs SB) = %.2f, Wald 95%% CI [%.2f, %.2f], p = %.2g\n",
           "  profile 95%% CI [%.2f, %.2f]\n",
           "  respondent-intercept SD = %.2f\n",
           "  item x model LRT: chisq = %.2f, df = %d, p = %.2g\n",
           "  cluster-robust stacked GLM: OR = %.2f [%.2f, %.2f], p = %.2g\n"),
    res$label, res$n_resp,
    p$or, p$wald_lo, p$wald_hi, p$p_wald,
    p$prof_lo, p$prof_hi,
    p$resp_sd,
    l$chisq, l$df, l$p,
    g$or, g$lo, g$hi, g$p))
}
report_family(res_ch)
report_family(res_be)

# Convergence / singularity diagnostics
for (r in list(res_ch, res_be)) {
  for (mn in c("m0", "m1")) {
    m <- r[[mn]]
    msgs <- m@optinfo$conv$lme4$messages
    cat(sprintf("%s %s: singular = %s; messages = %s\n",
                r$label, mn, isSingular(m),
                if (is.null(msgs)) "none" else paste(msgs, collapse = " | ")))
  }
}

# --- Sensitivity: retain "Not indicated" (n = 299) --------------------------
cat("\n=== SENSITIVITY (n = 299, 'Not indicated' retained as factor level) ===\n")
res_ch_s <- fit_family(long_sens, "Challenge", "Challenges", profile_ci = FALSE)
res_be_s <- fit_family(long_sens, "Benefit",   "Benefits",   profile_ci = FALSE)
report_family(res_ch_s)
report_family(res_be_s)

# --- Tables ------------------------------------------------------------------
pooled_tab <- bind_rows(
  res_ch$pooled %>% mutate(sample = "primary (n=285)"),
  res_be$pooled %>% mutate(sample = "primary (n=285)"),
  res_ch_s$pooled %>% mutate(sample = "sensitivity (n=299)"),
  res_be_s$pooled %>% mutate(sample = "sensitivity (n=299)")
) %>%
  left_join(
    bind_rows(
      res_ch$lrt %>% mutate(sample = "primary (n=285)"),
      res_be$lrt %>% mutate(sample = "primary (n=285)"),
      res_ch_s$lrt %>% mutate(sample = "sensitivity (n=299)"),
      res_be_s$lrt %>% mutate(sample = "sensitivity (n=299)")
    ) %>% rename(lrt_chisq = chisq, lrt_df = df, lrt_p = p),
    by = c("family", "sample")
  )
write_csv(pooled_tab, file.path(out_dir, "table_pooled_or_lrt.csv"))

glm_tab <- bind_rows(
  res_ch$glm_cl %>% mutate(sample = "primary (n=285)"),
  res_be$glm_cl %>% mutate(sample = "primary (n=285)")
)
write_csv(glm_tab, file.path(out_dir, "table_glm_cluster_robust.csv"))

item_tab <- bind_rows(res_ch$items, res_be$items) %>%
  mutate(
    item = str_remove(as.character(item), "^item"),
    item_label = item_labels[item]
  ) %>%
  select(family, item, item_label, odds.ratio, asymp.LCL, asymp.UCL,
         z.ratio, p.value) %>%
  rename(or_db_vs_sb = odds.ratio, ci_lo = asymp.LCL, ci_hi = asymp.UCL,
         z = z.ratio, q_bh = p.value)
write_csv(item_tab, file.path(out_dir, "table_peritem_emmeans_bh.csv"))

cat("\nPer-item emmeans contrasts (DB vs SB odds ratios, BH-adjusted within family):\n")
print(as.data.frame(item_tab), digits = 3)


cat("\nDone. Tables written to", out_dir, "\n")
