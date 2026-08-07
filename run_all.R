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
  "scripts/04_si_figures_tables.R",        # -> figures/ S1-S4, tables/ S1-S3
  "scripts/05_fig3_perceived.R",           # -> figures/Figure3
  "scripts/06_fig4_crossmodel.R",          # -> figures/Figure4
  "scripts/07_figS5_net_favorability.R",   # -> figures/FigureS5
  "scripts/08_figS6_by_discipline.R",      # -> figures/FigureS6
  "scripts/09_render_si_tables.R"          # -> tables/SI_tables.docx
)

for (s in scripts) {
  message("\n==== ", s, " ====")
  system2("Rscript", s)
}
message("\nAll scripts complete. See figures/ and tables/.")
