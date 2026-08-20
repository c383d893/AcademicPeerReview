# 06_fig5_crossmodel.R
#
# Manuscript Figure 5 (two-panel):
#   Panel A — discipline-adjusted Firth odds ratios (double-blind vs
#             single-blind) for each challenge and benefit item
#   Panel B — the same contrasts as discipline-standardized endorsement rates,
#             anticipated (open circles) to experienced (filled circles), with
#             fold-change labels
#   Items are ordered identically in both panels (by OR within family).
#
# Inputs:
#   tables/TableS3_adjustment_sensitivity.csv
#     ("plus Discipline" column = the published discipline-adjusted Firth ORs)
#   tables/table_S_item_ratios.csv
#     (discipline-standardized rates and fold-changes from scripts/03)
#
# Output: figures/Figure5.pdf, figures/Figure5.png (300 dpi)
# Run from the repo root: Rscript scripts/06_fig5_crossmodel.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

col_benefit <- "#2171B5"
col_challenge <- "#E6550D"

ratios <- read_csv("tables/table_S_item_ratios.csv",
                   show_col_types = FALSE)

# Parse the discipline-adjusted Firth ORs ("0.25 [0.11, 0.51]") from Table S3
s3 <- read_csv("tables/TableS3_adjustment_sensitivity.csv",
               show_col_types = FALSE) %>%
  rename(item_label = Item, type = Type) %>%
  mutate(
    or = as.numeric(str_extract(`+ Discipline (N = 285)`, "^[0-9.]+")),
    lower = as.numeric(str_extract(`+ Discipline (N = 285)`, "(?<=\\[)[0-9.]+")),
    upper = as.numeric(str_extract(`+ Discipline (N = 285)`, "(?<=, )[0-9.]+"))
  )

# One canonical item order for both panels: by OR within family
ord <- s3 %>%
  mutate(type = recode(type, "Benefit" = "Benefits", "Challenge" = "Challenges")) %>%
  arrange(type, or) %>%
  pull(item_label)

dat_rates <- ratios %>%
  mutate(
    type = recode(type, "Benefit" = "Benefits", "Challenge" = "Challenges"),
    item_label = factor(item_label, levels = ord),
    ratio_lab = sprintf("×%.1f", ratio_std)
  )

dat_or <- s3 %>%
  mutate(
    type = recode(type, "Benefit" = "Benefits", "Challenge" = "Challenges"),
    item_label = factor(item_label, levels = ord)
  )

wrap24 <- function(x) str_wrap(x, width = 24)

# --- Panel A (left): OR forest, carries the item labels ---
p_forest <- ggplot(dat_or, aes(x = or, y = item_label, color = type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.55, linewidth = 0.8) +
  scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10)) +
  scale_y_discrete(labels = wrap24) +
  scale_color_manual(values = c("Benefits" = col_benefit, "Challenges" = col_challenge)) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(x = "Odds ratio, double-blind vs single-blind (log scale)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12))

# --- Panel B (right): dumbbells, anticipated (open) -> experienced (filled) ---
p_dumbbell <- ggplot(dat_rates, aes(y = item_label, color = type)) +
  geom_segment(aes(x = p_sb_std, xend = p_db_std, yend = item_label),
               linewidth = 0.9,
               arrow = arrow(length = unit(0.13, "cm"), type = "closed")) +
  geom_point(aes(x = p_sb_std), shape = 21, fill = "white", size = 3, stroke = 1.1) +
  geom_point(aes(x = p_db_std), size = 3) +
  geom_text(aes(x = pmax(p_sb_std, p_db_std) + 0.045, label = ratio_lab),
            size = 3.1, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.92),
                     breaks = seq(0, 0.75, 0.25)) +
  scale_y_discrete(labels = wrap24) +
  scale_color_manual(values = c("Benefits" = col_benefit, "Challenges" = col_challenge)) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(x = "Endorsement rate (discipline-standardized)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12),
        axis.text.y = element_blank())

fig <- (p_forest | p_dumbbell) +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(tag_levels = "A", tag_suffix = ".")

dir.create("figures", showWarnings = FALSE)
ggsave("figures/Figure5.png", fig, width = 11, height = 7, dpi = 300, bg = "white")
cat("done: figures/Figure5.png\n")
