# Code and data to reproduce: Peer review models across disciplines: experienced benefits of blinding outweigh perceived risks

Code, data, and de-identified survey data to reproduce the analyses, figures, and supplementary
tables in the paper.

## Part 1: Bibliometric analysis — Figures 1 & 2

This section compiles peer review model dataset and runs associated analyses.

### Data

- `wos_full_202404_Rformat.txt` — web of science journals and associated metadata; see text for full description of column names
- `doaj_reduced_202404_Rformat.txt` — directory of open access journals journals and associated metadata; see text for full description of column names
- `Cat_JCR_JournalResults_04_2024_Clean_Rformat.txt` — web of science impact factors
- `merged_blinding.rds` — merged wos, doaj, and impact factor
- `merged_blinding.csv` — .csv. version of `merged_blinding.rds`
- `merged_blinding_topj256.rds` — top journals by journal impact factor for each of 256 categories; there are 260 due to ties in JIF per category
- `merged_blinding_topj256.csv` — .csv version of `merged_blinding_topj256.rds`
- `Blinding_cat_merge_Nov202024.txt` — 256 categories collapsed to 21 larger categories; some have up to 4 corresponding larger categories
- `journalcategories.rds` —  merged wos, doaj, and impact factor with 21 larger cateogries
- `merged_blinding_topj20&20perquartpercat.rds` — extracted top 20 and 20 per quartile per category from 21 large categories, without resampling from merged_blinding_topj256.rds
- `merged_blinding_topj20&20perquartpercat.csv` - .csv version of `merged_blinding_topj20&20perquartpercat.rds`
- `Transposedownload.csv` — downloaded Transpose dataset from website
- `Transposeclean.rds` —  cleaned Transpose dataset to merge with study dataset
- `Transposeclean2.csv` —  cleaned Transpose in excel.
- `Transposeclean_final.rds` —  cleaned Transpose dataset with collapsed peer review model to merge with study dataset
- `merged_blinding_wdjtrans.rds` —  final merged dataset
- `merged_blinding_wdjtrans.csv` —  .csv version of `merged_blinding_wdjtrans.rds`
- `merged_blinding_wdjtrans_final.rds` — final merged dataset with cleaned peer review model
- `merged_blinding_wdjtrans_final.csv` — .csv version of `merged_blinding_wdjtrans_final.rds`
- `Full_CitizenScience_Peer-review_Study_April232026.txt` — ISSN of merged_blinding_wdjtrans_final.RDS plus our hand collected additional peer review model data
- `merged_blinding_wdjtrans_final_peerreview.rds` — final merged dataset with cleaned peer review model plus hand collected additional peer review model data
- `merged_blinding_wdjtrans_final_peerreview.csv` — .csv version of `merged_blinding_wdjtrans_final_peerreview.rds`
- `Blinding_cat_merge_May132026.txt` — 256 categories collapsed to 21 larger categories; some corrections for final analysis from `Blinding_cat_merge_Nov202024.txt` 

### Reproduce

Install the packages (below), then run in sequence using the RProject "BlindingGitHub.Rproj".

### Scripts: *Found in root directory.

| # | Script | Produces |
|---|---|---|
| A | `A.Blinding_cleaning.R` | Merge peer review model sources and add impact factor |
| B | `B.Blinding_Impact.R` | Determine journals for peer-review model survey |
| C | `C.Transpose_PR_clean.R` | Incorporate transpose data into dataset |
| D | `D.Manual_PR_join.R` | Incorporate manually (web) searched peer-review model for survey journals into database |
| E | `E.Analyses.R` | All analyses of database. **Figs 1-3 & SI1** |

All scripts must be run in order.

### Requirements

R (>= 4.2). Packages:

```r
install.packages(c(
  "tidyverse",                                                  # data handling 
  "lme4", "lmerTest",                                           # models
  "ggplot2", "emmeans", "ggeffects", "multcomp", "ggalluvial"   # plotting + visualization
  ))
```

## Part 2: Survey analysis — Figures 4, 5, S2–S7 and Tables S2–S6

Everything in this section runs from the de-identified respondent data.

### Data

- `data/blinding_survey_dat.csv` — de-identified respondent data (311 rows):
  categorical journal/editor attributes and the 11 benefit/challenge items.
  No names, emails, journal titles, free text, timestamps, or IP addresses.
- `data/meta_stats.csv` — aggregate filter-cascade and impact-factor counts.
- `tables/duplicate_sensitivity.csv` — precomputed summary of the duplicate-title
  sensitivity check (its raw step needs the non-public survey export).

### Reproduce

Install the packages (below), then from the repository root:

```r
source("run_all.R")   # runs scripts 01-09 in order
```

or run individually with `Rscript scripts/NN_name.R`. Figures are written as PNG to
`figures/`; the supplementary tables document to `tables/SI_tables.docx`.

### Scripts

| # | Script | Produces |
|---|---|---|
| 01 | `01_robustness_family_glmm.R` | pooled OR / LRT robustness tables |
| 02 | `02_net_favorability.R` | net-favorability models |
| 03 | `03_anticipation_ratios.R` | anticipated-to-experienced rate ratios (Fig 5B, Table S5) |
| 04 | `04_si_figures_tables.R` | **Figs S2–S5**, Tables S2–S3 CSVs |
| 05 | `05_fig4_perceived.R` | **Figure 4** |
| 06 | `06_fig5_crossmodel.R` | **Figure 5** |
| 07 | `07_figS6_net_favorability.R` | **Figure S6** |
| 08 | `08_figS7_by_discipline.R` | **Figure S7** |
| 09 | `09_render_si_tables.R` | **`SI_tables.docx`** (Tables S2–S6) |
| 10 | `10_hss_stem_benefit_counts.R` | HSS vs STEM benefit-count test (Results p < 0.0001) |

Figure S1 and Table S1 are bibliometric (Part 1, script E); the survey outputs
therefore start at Figure S2 / Table S2, matching the manuscript SI numbering.

Scripts 03 and 04 must run before 06 and 09 (which consume their tables); this is
the order `run_all.R` uses.

### Requirements

R (>= 4.2). Packages:

```r
install.packages(c(
  "tidyverse", "scales", "patchwork",       # data handling + figures
  "lme4", "emmeans", "logistf", "ordinal",  # GLMM, Firth, ordinal models
  "sandwich", "lmtest", "broom",            # robust SEs / tidying
  "officer"                                 # SI_tables.docx
))
```

Each script sets an explicit random seed, so bootstrap/permutation results are
reproducible.
