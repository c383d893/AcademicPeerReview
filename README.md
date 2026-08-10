# Code and data to reproduce: Peer review models across disciplines: experienced benefits of blinding outweigh perceived risks

Code, data, and de-identified survey data to reproduce the analyses, figures, and supplementary
tables in the paper.

## Part 1: Bibliometric analysis — Figures 1 & 2

This section compiles peer review model dataset and runs assocaited analyses.

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

## Part 2: Survey analysis — Figures 3, 4, S1–S6 and Tables S1–S5

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
| 03 | `03_anticipation_ratios.R` | anticipated-to-experienced rate ratios (Fig 4, Table S5) |
| 04 | `04_si_figures_tables.R` | **Figs S1–S4**, **Tables S1–S3** |
| 05 | `05_fig3_perceived.R` | **Figure 3** |
| 06 | `06_fig4_crossmodel.R` | **Figure 4** |
| 07 | `07_figS5_net_favorability.R` | **Figure S5** |
| 08 | `08_figS6_by_discipline.R` | **Figure S6** |
| 09 | `09_render_si_tables.R` | **`SI_tables.docx`** (Tables S1–S5) |

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
