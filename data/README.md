# Data dictionary — survey data (Part 2)

## `blinding_survey_dat.csv`

De-identified editor-survey responses, one row per respondent (311 rows). Contains no
names, emails, journal titles, free text, timestamps, or IP addresses. Missing values
are `NA`.

| Column | Values | Description |
|---|---|---|
| `sample` | `analytical`, `optional` | `analytical` = the 301 respondents in the analytical sample (screened for consent, eligibility, and a reported required peer-review model); `optional` = the 10 optional-blind respondents excluded from modeling |
| `pub_model` | `Single blind`, `Double/triple blind` | Journal's required peer-review model |
| `consid_status` | `Considered`, `Not considered`, `Yes`, `No` | Whether the journal has considered adopting double-blind review. Single-blind respondents routed to the branching question have `Considered`/`Not considered` (`NA` for the 2 who were not routed); `Yes`/`No` occur only for optional-blind respondents' version of the item |
| `journal_super_category` | 7 disciplines, `Not indicated` | Discipline, collapsed from the 21 Clarivate ESI research areas: Arts & Humanities, Social Sciences, Multidisciplinary, Engineering & Technology, Clinical Pre-Clinical & Health, Life Sciences, Physical Sciences |
| `journal_category` | 21 ESI research areas | Respondent-selected Clarivate ESI research area (before collapsing) |
| `impact_factor` | numeric | Self-reported journal impact factor (optional item) |
| `if_quartile` | `Q1 (lowest)`–`Q4 (highest)` | Impact-factor quartile within the survey sample |
| `business_model` | `for-profit`, `non-profit` | Journal's business model |
| `model_reported` | `Yes`, `No` | Whether the peer-review model is reported in the journal's author guidelines |
| `language` | `English only`, `Multilingual/Other` | Publication language |
| `hq_region` | `USA`, `UK`, `Europe (other)`, `Asia-Pacific`, `Other` | Publisher headquarters region (optional item) |
| `challenge_a`–`challenge_f` | `0`/`1` | Check-all-that-apply challenge items: (a) author burden to anonymize, (b) editorial-office burden, (c) anonymization not fully successful, (d) hard to identify conflicts of interest, (e) may reduce reviewer participation, (f) limits contextualizing the work |
| `benefit_a`–`benefit_e` | `0`/`1` | Check-all-that-apply benefit items: (a) more fair/objective review, (b) reduces prestige bias, (c) reduces gender bias, (d) reduces country/location bias, (e) increased review quality |

Single-blind respondents report *anticipated* challenges/benefits; double-blind
respondents report *experienced* ones (see the survey instrument, Appendix S1 of the
paper).

## `meta_stats.csv`

Aggregate counts only: the response filter cascade (invited → responded → consented →
eligible → analytical) and impact-factor availability, used for reporting sample
construction. No respondent-level information.

## `tables/duplicate_sensitivity.csv` (in `tables/`)

Precomputed summary of the duplicate-response sensitivity check (Table S6): per-item
endorsement rates and discipline-adjusted odds ratios for the full analytical sample
(N = 301) and after removing one response from each of the 14 pairs of responses that
named the same journal (N = 287). Precomputed because the flagging step requires the
non-public raw survey export.
