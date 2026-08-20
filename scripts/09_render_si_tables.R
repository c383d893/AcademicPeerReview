# ---------------------------------------------------------------------------
# 09_render_si_tables.R
#
# Purpose:
#   Render the survey supplementary tables as editable Word tables with
#   captions (paste straight into the SI). SI numbering S2-S6 (Table S1 is the
#   bibliometric model-results table, produced by the Part 1 scripts):
#     S2 = sample characteristics, WITH per-discipline % double-blind folded in
#     S3 = cross-model ORs under four adjustment approaches
#     S4 = pooled type-level GLMM
#     S5 = anticipated vs experienced endorsement rates
#     S6 = duplicate-response sensitivity (all 11 challenges and benefits)
#
# Inputs (all read-only, produced by the upstream table scripts):
#   tables/TableS2_sample_characteristics.csv
#   tables/table_discipline_by_model.csv
#   tables/TableS3_adjustment_sensitivity.csv
#   tables/table_pooled_or_lrt.csv        (scripts/01_robustness_family_glmm.R)
#   tables/table_S_item_ratios.csv        (scripts/03_anticipation_ratios.R)
#   tables/duplicate_sensitivity.csv
#
# Outputs:
#   tables/SI_tables.docx
#
# Run from repo root:  Rscript scripts/09_render_si_tables.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(officer) })

tab <- "tables"
dir.create(tab, showWarnings = FALSE, recursive = TRUE)

fmtp <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
na0  <- function(df) mutate(df, across(everything(), ~replace_na(as.character(.x), "")))

cap <- list(
  S2 = "Table S2. Characteristics of the survey sample by peer-review model. Distribution of the 301 analytical respondents across discipline, business model, language, publisher headquarters region, and impact-factor quartile, split by required peer-review model. Cell values are counts (column %); the final column gives the percentage of each discipline that uses double-blind review. p-values are from Fisher's exact tests (the 8x2 discipline table uses 10,000 Monte Carlo replicates; all other tests are exact). Discipline (p < 0.0001) and impact-factor quartile (p < 0.001) differed significantly between models; language differed (p = 0.04); business model (p = 0.24) and publisher headquarters region (p = 0.057, N = 224 codable responses) did not differ.",
  S3 = "Table S3. Cross-model odds ratios under four adjustment approaches. Odds of perceiving each challenge or benefit (double/triple- vs. single-blind editors) under four adjustment approaches (unadjusted; + impact factor; + discipline; + both), each via Firth penalized logistic regression. Values are odds ratios with 95% confidence intervals; the final column gives the Benjamini-Hochberg false-discovery-rate q-value from the discipline-adjusted (primary) model.",
  S4 = "Table S4. Pooled type-level generalized linear mixed models of challenge or benefit endorsement. Binomial GLMM pooling all individual challenges and benefits per type (i.e., Challenge or Benefit), with each challenge or benefit, the peer-review-model contrast, and discipline as fixed effects and a random intercept for respondent. Pooled odds ratio with 95% profile-likelihood CI and a likelihood-ratio test for between-challenge or -benefit heterogeneity, for the primary (N = 285) and sensitivity (N = 299) samples.",
  S5 = "Table S5. Anticipated versus experienced endorsement rates, by challenge or benefit. Discipline-standardized proportion of single-blind editors anticipating each effect and of double-blind editors reporting it as experienced, with their ratio and 95% CI.",
  S6 = "Table S6. Sensitivity of survey results to duplicate responses. Per-challenge or -benefit selection rates (all 11 challenges and benefits, both models) under the full analytical sample (N = 301) and after removing one response from each of the 14 pairs of responses that named the same journal (N = 287). All discipline-adjusted odds ratios retained direction and significance; the headline review-quality gap strengthened slightly (OR 7.27 to 8.26)."
)

# ---- S2: sample characteristics + per-discipline % double-blind ----
samp_raw <- read_csv(file.path(tab, "TableS2_sample_characteristics.csv"), show_col_types = FALSE)
disc_raw <- read_csv(file.path(tab, "table_discipline_by_model.csv"), show_col_types = FALSE)

samp_j <- samp_raw %>%
  left_join(disc_raw %>% transmute(
    Category = Discipline,
    pct_num = `% double/triple-blind`,
    pct = sprintf("%.1f (%d/%d)", `% double/triple-blind`, `Double/triple blind`, Total)),
    by = "Category")

# the discipline block is the first 8 rows of the sample-characteristics CSV
disc <- samp_j[1:8, ]
other <- samp_j[-(1:8), ]
disc <- disc[order(-disc$pct_num), ]                 # order disciplines high -> low %
disc_lbl <- "Discipline"
disc_fp  <- disc$`Fisher p`[!is.na(disc$`Fisher p`) & disc$`Fisher p` != ""][1]
disc$Variable  <- ""; disc$Variable[1]  <- disc_lbl  # keep the block label + p on the first row
disc$`Fisher p` <- ""; disc$`Fisher p`[1] <- disc_fp

S2 <- bind_rows(disc, other) %>%
  transmute(Variable, Category,
            `Double-blind` = `Double/triple blind`, `Single blind`,
            `% double-blind` = ifelse(is.na(pct), "", pct),
            `Fisher p`) %>% na0()

# ---- S3: adjustment-approach sensitivity (presentation-formatted CSV) ----
S3 <- read_csv(file.path(tab, "TableS3_adjustment_sensitivity.csv"), show_col_types = FALSE) %>% na0()

# ---- S4: pooled GLMM ----
S4 <- read_csv(file.path(tab, "table_pooled_or_lrt.csv"), show_col_types = FALSE) %>%
  mutate(lo = coalesce(prof_lo, wald_lo), hi = coalesce(prof_hi, wald_hi)) %>%
  transmute(Family = family, Sample = sample,
            `Pooled OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", or, lo, hi),
            `Heterogeneity chi-sq (df)` = sprintf("%.1f (%d)", lrt_chisq, lrt_df),
            `p` = fmtp(lrt_p))

# ---- S5: anticipated vs experienced, standardized ----
S5 <- read_csv(file.path(tab, "table_S_item_ratios.csv"), show_col_types = FALSE) %>%
  transmute(Item = item_label, Type = type,
            `Anticipated (single-blind)` = sprintf("%.0f%%", p_sb_std * 100),
            `Experienced (double-blind)` = sprintf("%.0f%%", p_db_std * 100),
            `Ratio (95% CI)` = sprintf("%.2f (%.2f-%.2f)", ratio_std, ratio_std_lo, ratio_std_hi))

# ---- S6: duplicate-response sensitivity (all 11 items, both models) ----
S6 <- read_csv(file.path(tab, "duplicate_sensitivity.csv"), show_col_types = FALSE) %>%
  filter(section == "endorsement_rate_pct") %>%
  mutate(Item = item_label,
         Model = ifelse(str_starts(metric, "DB"), "Double-blind", "Single blind"),
         fam = ifelse(str_detect(metric, "benefit"), "1_benefit", "2_challenge")) %>%
  arrange(fam, Item, Model) %>%
  transmute(Item, Model,
            `Original % (N=301)` = sprintf("%.1f", original),
            `Deduplicated % (N=287)` = sprintf("%.1f", deduplicated))

tables <- list(S2 = S2, S3 = S3, S4 = S4, S5 = S5, S6 = S6)

doc <- read_docx()
for (nm in names(tables)) {
  doc <- doc %>%
    body_add_par(cap[[nm]], style = "Normal") %>%
    body_add_table(tables[[nm]], header = TRUE) %>%
    body_add_par("", style = "Normal")
}
print(doc, target = file.path(tab, "SI_tables.docx"))

cat("done:", file.path(tab, "SI_tables.docx"), "with Tables S2-S6\n")
