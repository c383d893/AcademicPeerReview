# ---------------------------------------------------------------------------
# 03_anticipation_ratios.R
#
# Purpose:
#   Anticipation-inflation ratios (anticipated vs experienced),
#   discipline-standardized. Restates the headline anticipated-vs-experienced
#   gap on the probability / risk-ratio scale. Descriptive / estimation framing
#   only; the
#   inferential weight stays on the GLMM and Firth models in the main analysis.
#
# Inputs:
#   data/blinding_survey_dat.csv (read-only)
#
# Outputs:
#   tables/table_S_item_ratios.csv       all 11 items: naive + standardized rates/ratios
#   tables/family_summary_ratios.csv     family geometric/arithmetic means x naive/std
#
# Key parameters / denominator rule:
#   SB "anticipated" rates: only the 158 single-blind respondents routed to the
#   branching question (consid_status in {Considered, Not considered}).
#   DB "experienced" rates: all 141 double/triple-blind adopters. n = 299.
#   Per item: challenges anticipation-inflation index AII = p_SB / p_DB;
#   benefits realization ratio RR = p_DB / p_SB. Two versions: naive (Wilson
#   CIs) and discipline-standardized (marginal standardization of Firth fits,
#   sparse strata collapsed Humanities+Social / STEM / Multidisciplinary;
#   "Not indicated" excluded, n = 285).
#   Uncertainty: nonparametric bootstrap B = 10,000, stratified by pub_model,
#   percentile CIs. seed = 20260610.
#
# Run from repo root:  Rscript scripts/03_anticipation_ratios.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(logistf)
})

set.seed(20260610)
B <- 10000L

data_path <- "data/blinding_survey_dat.csv"
outdir <- "tables"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- Labels -----------------------------------------------------------------
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
items    <- names(item_labels)
ch_items <- names(challenge_labels)
be_items <- names(benefit_labels)

# --- Wilson score CI ---------------------------------------------------------
wilson_ci <- function(x, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- (z / denom) * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  c(prop = p_hat, lower = max(0, center - margin), upper = min(1, center + margin))
}

# --- Data: analytical sample + strict branch filter --------------------------
all_data <- read_csv(data_path, show_col_types = FALSE)

analysis_dat <- all_data %>%
  filter(sample == "analytical") %>%
  filter(
    pub_model == "Double/triple blind" |
      (pub_model == "Single blind" &
         consid_status %in% c("Considered", "Not considered"))
  ) %>%
  mutate(
    db = if_else(pub_model == "Double/triple blind", 1L, 0L),
    # collapsed discipline strata for standardization only
    super3 = case_when(
      journal_super_category %in% c("Arts & Humanities", "Social Sciences") ~ "HumanitiesSocial",
      journal_super_category == "Multidisciplinary" ~ "Multidisciplinary",
      journal_super_category == "Not indicated" ~ NA_character_,
      is.na(journal_super_category) ~ NA_character_,
      TRUE ~ "STEM"
    )
  )

n_sb <- sum(analysis_dat$db == 0)
n_db <- sum(analysis_dat$db == 1)
stopifnot(n_sb == 158, n_db == 141, nrow(analysis_dat) == 299)

cat(sprintf("Analytical sample after branch filter: n = %d (SB anticipated = %d, DB experienced = %d)\n",
            nrow(analysis_dat), n_sb, n_db))
cat(sprintf("Discipline-standardization subset (super3 non-missing): n = %d (SB %d, DB %d)\n\n",
            sum(!is.na(analysis_dat$super3)),
            sum(!is.na(analysis_dat$super3) & analysis_dat$db == 0),
            sum(!is.na(analysis_dat$super3) & analysis_dat$db == 1)))

# Matrices for fast bootstrap
Y       <- as.matrix(analysis_dat[, items])
db_vec  <- analysis_dat$db
sup_vec <- analysis_dat$super3
sb_idx  <- which(db_vec == 0)
db_idx  <- which(db_vec == 1)

# --- Fast Firth logistic regression (Firth 1993 scoring; coefficients only) --
# Validated below against logistf() point estimates on the observed data.
firth_fit <- function(X, y, maxit = 60, tol = 1e-9) {
  beta <- rep(0, ncol(X))
  for (it in seq_len(maxit)) {
    eta <- as.vector(X %*% beta)
    p <- plogis(eta)
    w <- p * (1 - p)
    XtW <- t(X * w)
    info <- XtW %*% X
    cholI <- tryCatch(chol(info), error = function(e) NULL)
    if (is.null(cholI)) return(NULL)
    # hat diagonal of W^(1/2) X (X'WX)^-1 X' W^(1/2)
    XV <- X * sqrt(w)
    h <- rowSums((XV %*% chol2inv(cholI)) * XV)
    U <- as.vector(t(X) %*% (y - p + h * (0.5 - p)))
    step <- backsolve(cholI, forwardsolve(t(cholI), U))
    # mild damping for stability on resamples
    if (max(abs(step)) > 5) step <- step * 5 / max(abs(step))
    beta <- beta + step
    if (max(abs(U)) < tol) break
  }
  beta
}

# Marginal standardization for one bootstrap (or observed) sample:
# fit Firth model per item on the discipline subset; predict every respondent
# under db = 0 and db = 1; average -> standardized p_SB, p_DB per item.
standardized_rates <- function(dat_idx) {
  keep <- dat_idx[!is.na(sup_vec[dat_idx])]
  dd <- data.frame(db = db_vec[keep], super3 = sup_vec[keep])
  X  <- model.matrix(~ db + super3, data = dd)
  db_col <- which(colnames(X) == "db")
  X0 <- X; X0[, db_col] <- 0
  X1 <- X; X1[, db_col] <- 1
  out <- matrix(NA_real_, nrow = length(items), ncol = 2,
                dimnames = list(items, c("p_sb", "p_db")))
  for (j in seq_along(items)) {
    y <- Y[keep, items[j]]
    b <- firth_fit(X, y)
    if (is.null(b)) return(NULL)
    out[j, 1] <- mean(plogis(as.vector(X0 %*% b)))
    out[j, 2] <- mean(plogis(as.vector(X1 %*% b)))
  }
  out
}

# Ratios + family summaries from a (n_items x 2) rate matrix
# challenges: AII = p_SB / p_DB ; benefits: realization = p_DB / p_SB
gmean <- function(x) exp(mean(log(x)))
make_ratios <- function(p_sb, p_db) {
  r <- ifelse(grepl("^challenge", items), p_sb / p_db, p_db / p_sb)
  names(r) <- items
  c(r,
    gm_ch = gmean(r[ch_items]), gm_be = gmean(r[be_items]),
    am_ch = mean(r[ch_items]),  am_be = mean(r[be_items]))
}

# --- Validate firth_fit against logistf() on observed data -------------------
dd_obs <- analysis_dat %>% filter(!is.na(super3))
X_obs  <- model.matrix(~ db + super3, data = dd_obs)
max_diff <- max(sapply(items, function(it) {
  y <- dd_obs[[it]]
  b_ref <- coef(logistf(y ~ db + super3, data = dd_obs %>% mutate(y = y), pl = FALSE))
  b_new <- firth_fit(X_obs, y)
  max(abs(b_ref - b_new))
}))
cat(sprintf("Validation: max |coef difference| firth_fit vs logistf across 11 items = %.2e\n\n", max_diff))
stopifnot(max_diff < 1e-4)

# --- Point estimates ----------------------------------------------------------
# (a) naive rates with Wilson CIs
naive_obs <- map_dfr(items, function(it) {
  x_sb <- sum(Y[sb_idx, it]); x_db <- sum(Y[db_idx, it])
  w_sb <- wilson_ci(x_sb, n_sb); w_db <- wilson_ci(x_db, n_db)
  tibble(
    item = it, item_label = item_labels[it],
    type = if_else(str_starts(it, "challenge"), "Challenge", "Benefit"),
    p_sb_naive = w_sb["prop"], p_sb_naive_lo = w_sb["lower"], p_sb_naive_hi = w_sb["upper"],
    p_db_naive = w_db["prop"], p_db_naive_lo = w_db["lower"], p_db_naive_hi = w_db["upper"]
  )
})

# (b) discipline-standardized rates
std_obs_mat <- standardized_rates(seq_len(nrow(analysis_dat)))
std_obs <- tibble(item = items,
                  p_sb_std = std_obs_mat[, "p_sb"],
                  p_db_std = std_obs_mat[, "p_db"])

ratios_naive_obs <- make_ratios(naive_obs$p_sb_naive, naive_obs$p_db_naive)
ratios_std_obs   <- make_ratios(std_obs$p_sb_std, std_obs$p_db_std)

# --- Bootstrap (B = 10,000, stratified by pub_model) -------------------------
cat(sprintf("Bootstrapping (B = %d, stratified by pub_model) ...\n", B))
n_stats <- length(items) * 2 + 8 + length(items) * 2  # ratios x2, summaries x8, std rates x2
boot_mat <- matrix(NA_real_, nrow = B, ncol = n_stats)
colnames(boot_mat) <- c(paste0("naive_", items), paste0("std_", items),
                        "naive_gm_ch", "naive_gm_be", "naive_am_ch", "naive_am_be",
                        "std_gm_ch", "std_gm_be", "std_am_ch", "std_am_be",
                        paste0("psb_std_", items), paste0("pdb_std_", items))
n_failed <- 0L
t0 <- proc.time()[3]
for (b in seq_len(B)) {
  idx <- c(sample(sb_idx, n_sb, replace = TRUE),
           sample(db_idx, n_db, replace = TRUE))
  sb_b <- idx[seq_len(n_sb)]; db_b <- idx[n_sb + seq_len(n_db)]
  p_sb_n <- colMeans(Y[sb_b, , drop = FALSE])
  p_db_n <- colMeans(Y[db_b, , drop = FALSE])
  rn <- make_ratios(p_sb_n, p_db_n)
  sm <- standardized_rates(idx)
  if (is.null(sm)) { n_failed <- n_failed + 1L; next }
  rs <- make_ratios(sm[, "p_sb"], sm[, "p_db"])
  boot_mat[b, ] <- c(rn[items], rs[items],
                     rn[c("gm_ch", "gm_be", "am_ch", "am_be")],
                     rs[c("gm_ch", "gm_be", "am_ch", "am_be")],
                     sm[, "p_sb"], sm[, "p_db"])
  if (b %% 2000 == 0) cat(sprintf("  %d / %d replicates (%.0f s elapsed)\n", b, B, proc.time()[3] - t0))
}
cat(sprintf("Bootstrap done in %.0f s; failed replicates: %d\n\n", proc.time()[3] - t0, n_failed))

pct_ci <- function(col) quantile(boot_mat[, col], c(0.025, 0.975), na.rm = TRUE)

# --- Assemble supplementary table --------------------------------------------
item_table <- naive_obs %>%
  left_join(std_obs, by = "item") %>%
  rowwise() %>%
  mutate(
    ratio_naive    = unname(ratios_naive_obs[item]),
    ratio_naive_lo = pct_ci(paste0("naive_", item))[1],
    ratio_naive_hi = pct_ci(paste0("naive_", item))[2],
    ratio_std      = unname(ratios_std_obs[item]),
    ratio_std_lo   = pct_ci(paste0("std_", item))[1],
    ratio_std_hi   = pct_ci(paste0("std_", item))[2],
    p_sb_std_lo    = pct_ci(paste0("psb_std_", item))[1],
    p_sb_std_hi    = pct_ci(paste0("psb_std_", item))[2],
    p_db_std_lo    = pct_ci(paste0("pdb_std_", item))[1],
    p_db_std_hi    = pct_ci(paste0("pdb_std_", item))[2],
    ratio_definition = if_else(type == "Challenge",
                               "anticipated / experienced (AII)",
                               "experienced / anticipated (realization)")
  ) %>%
  ungroup()

write_csv(item_table %>%
            select(item, item_label, type, ratio_definition,
                   p_sb_naive, p_sb_naive_lo, p_sb_naive_hi,
                   p_db_naive, p_db_naive_lo, p_db_naive_hi,
                   ratio_naive, ratio_naive_lo, ratio_naive_hi,
                   p_sb_std, p_sb_std_lo, p_sb_std_hi,
                   p_db_std, p_db_std_lo, p_db_std_hi,
                   ratio_std, ratio_std_lo, ratio_std_hi),
          file.path(outdir, "table_S_item_ratios.csv"))

# --- Family summaries (abstract-ready) ---------------------------------------
fam <- tribble(
  ~family, ~summary, ~version, ~estimate, ~lo, ~hi,
  "Challenges (anticipated / experienced)", "geometric mean", "naive",
    ratios_naive_obs["gm_ch"], pct_ci("naive_gm_ch")[1], pct_ci("naive_gm_ch")[2],
  "Challenges (anticipated / experienced)", "geometric mean", "discipline-standardized",
    ratios_std_obs["gm_ch"], pct_ci("std_gm_ch")[1], pct_ci("std_gm_ch")[2],
  "Challenges (anticipated / experienced)", "arithmetic mean", "naive",
    ratios_naive_obs["am_ch"], pct_ci("naive_am_ch")[1], pct_ci("naive_am_ch")[2],
  "Challenges (anticipated / experienced)", "arithmetic mean", "discipline-standardized",
    ratios_std_obs["am_ch"], pct_ci("std_am_ch")[1], pct_ci("std_am_ch")[2],
  "Benefits (experienced / anticipated)", "geometric mean", "naive",
    ratios_naive_obs["gm_be"], pct_ci("naive_gm_be")[1], pct_ci("naive_gm_be")[2],
  "Benefits (experienced / anticipated)", "geometric mean", "discipline-standardized",
    ratios_std_obs["gm_be"], pct_ci("std_gm_be")[1], pct_ci("std_gm_be")[2],
  "Benefits (experienced / anticipated)", "arithmetic mean", "naive",
    ratios_naive_obs["am_be"], pct_ci("naive_am_be")[1], pct_ci("naive_am_be")[2],
  "Benefits (experienced / anticipated)", "arithmetic mean", "discipline-standardized",
    ratios_std_obs["am_be"], pct_ci("std_am_be")[1], pct_ci("std_am_be")[2]
)
write_csv(fam, file.path(outdir, "family_summary_ratios.csv"))

cat("=== Family summaries (bootstrap percentile 95% CIs) ===\n")
print(as.data.frame(fam %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
cat("\n=== Item-level ratios ===\n")
print(item_table %>%
        transmute(item_label, type,
                  naive = sprintf("%.2f (%.2f-%.2f)", ratio_naive, ratio_naive_lo, ratio_naive_hi),
                  standardized = sprintf("%.2f (%.2f-%.2f)", ratio_std, ratio_std_lo, ratio_std_hi)) %>%
        as.data.frame(), row.names = FALSE)

# --- Rank concordance (descriptive; no tests on 6 and 5 points) --------------
rho_ch_naive <- cor(naive_obs$p_sb_naive[1:6],  naive_obs$p_db_naive[1:6],  method = "spearman")
rho_be_naive <- cor(naive_obs$p_sb_naive[7:11], naive_obs$p_db_naive[7:11], method = "spearman")
rho_ch_std   <- cor(std_obs$p_sb_std[1:6],  std_obs$p_db_std[1:6],  method = "spearman")
rho_be_std   <- cor(std_obs$p_sb_std[7:11], std_obs$p_db_std[7:11], method = "spearman")
lin_ccc <- function(x, y) 2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2)
ccc_ch <- lin_ccc(std_obs$p_sb_std[1:6],  std_obs$p_db_std[1:6])
ccc_be <- lin_ccc(std_obs$p_sb_std[7:11], std_obs$p_db_std[7:11])
cat(sprintf("\nSpearman rho (naive):        challenges %.2f | benefits %.2f\n", rho_ch_naive, rho_be_naive))
cat(sprintf("Spearman rho (standardized): challenges %.2f | benefits %.2f\n", rho_ch_std, rho_be_std))
cat(sprintf("Lin's CCC (standardized):    challenges %.2f | benefits %.2f\n\n", ccc_ch, ccc_be))

cat("Tables written to:\n  ", file.path(outdir, "table_S_item_ratios.csv"), "\n  ",
    file.path(outdir, "family_summary_ratios.csv"), "\n")
