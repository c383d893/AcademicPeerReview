# run_all.R
# Reproduce every survey figure and table from the de-identified data.
# Run from the repository root:  Rscript run_all.R
#
# Scripts are ordered so that each one's inputs already exist: the analysis and
# supplementary scripts (01-04) write the tables that the figure and table-render
# scripts (06, 09) consume.

scripts <- c(
  "scripts/01_robustness_family_glmm.R",   # -> tables/ (pooled OR / LRT robustness)
  "scripts/02_net_favorability.R",         # -> tables/ (net-favorability models)
  "scripts/03_anticipation_ratios.R",      # -> tables/ (anticipated-to-experienced ratios)
  "scripts/04_si_figures_tables.R",        # -> figures/ S2-S5, tables/ S2-S3 CSVs
  "scripts/05_fig4_perceived.R",           # -> figures/Figure4
  "scripts/06_fig5_crossmodel.R",          # -> figures/Figure5
  "scripts/07_figS6_net_favorability.R",   # -> figures/FigureS6
  "scripts/08_figS7_by_discipline.R",      # -> figures/FigureS7
  "scripts/09_render_si_tables.R",         # -> tables/SI_tables.docx (Tables S2-S6)
  "scripts/10_hss_stem_benefit_counts.R"   # -> tables/ (HSS vs STEM benefit counts)
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  system2("Rscript", s)
}
message("\nAll scripts complete. See figures/ and tables/.")
