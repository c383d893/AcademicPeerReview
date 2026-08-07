# 05_fig3_perceived.R
# Manuscript Figure 3: perceived benefits & challenges of fully blinded review.
#   Panel A — overall pooled endorsement rates with Wilson CIs (items on y).
#   Panel B — the same items by discipline, as benefit/challenge heatmaps.
# Panel B is transposed relative to earlier drafts: items on y (readable,
# aligned with A), disciplines on x; one clean percent per cell.
#
# Input:  data/blinding_survey_dat.csv
# Output: figures/Figure3.pdf, figures/Figure3.png
# Run from the repo root: Rscript scripts/05_fig3_perceived.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
})

# ---- constants ----
colors_type <- c("Challenge" = "#e6550d", "Benefit" = "#3182bd")
heatmap_benefits_low   <- "#deebf7"; heatmap_benefits_high   <- "#3182bd"
heatmap_challenges_low <- "#fee8c8"; heatmap_challenges_high <- "#e34a33"

challenge_labels <- c(
  challenge_a = "Author burden",
  challenge_b = "Editorial office burden",
  challenge_c = "Anonymization not fully successful",
  challenge_d = "Hard to identify conflict of interest",
  challenge_e = "May reduce reviewer participation",
  challenge_f = "Limits contextualizing work")
benefit_labels <- c(
  benefit_a = "More fair/objective review",
  benefit_b = "Reduces prestige bias",
  benefit_c = "Reduces gender bias",
  benefit_d = "Reduces country/location bias",
  benefit_e = "Increased review quality")
item_labels <- c(challenge_labels, benefit_labels)

# ---- helpers ----
wilson_ci <- function(x, n, conf.level = 0.95) {
  if (n == 0) return(tibble(prop = NA_real_, lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - conf.level) / 2); p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- (z / denom) * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  tibble(prop = p_hat, lower = max(0, center - margin), upper = min(1, center + margin))
}
calc_endorsement_rates <- function(data, group_vars = NULL) {
  items <- names(item_labels)
  data %>%
    select(all_of(c(group_vars, items))) %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "endorsed") %>%
    mutate(type = if_else(str_starts(item, "challenge"), "Challenge", "Benefit"),
           item_label = item_labels[item]) %>%
    group_by(across(all_of(c(group_vars, "item", "type", "item_label")))) %>%
    summarise(n_endorsed = sum(endorsed, na.rm = TRUE),
              n_total = sum(!is.na(endorsed)), .groups = "drop") %>%
    rowwise() %>%
    mutate(ci = list(wilson_ci(n_endorsed, n_total)),
           prop = ci$prop, lower = ci$lower, upper = ci$upper) %>%
    ungroup() %>% select(-ci)
}

# ---- data ----
all_data <- read_csv("data/blinding_survey_dat.csv", show_col_types = FALSE)
endorsement_dat <- all_data %>% filter(sample == "analytical")

pooled_dat <- endorsement_dat %>%
  filter(pub_model == "Double/triple blind" | !is.na(consid_status))

overall_rates <- calc_endorsement_rates(pooled_dat) %>%
  group_by(type) %>% mutate(item_label = fct_reorder(item_label, prop)) %>% ungroup()
item_levels <- levels(overall_rates$item_label)   # shared item order for A and B

disc_dat <- endorsement_dat %>%
  filter(!is.na(journal_super_category),
         pub_model == "Double/triple blind" | !is.na(consid_status))
disc_n <- disc_dat %>% count(journal_super_category) %>%
  mutate(disc_label = paste0(journal_super_category, " (N=", n, ")"))
disc_rates <- calc_endorsement_rates(disc_dat, group_vars = "journal_super_category") %>%
  left_join(disc_n, by = "journal_super_category") %>%
  filter(journal_super_category != "Not indicated") %>%   # missing discipline, not a field (-> N=285, 7 disciplines)
  mutate(item_label = factor(as.character(item_label), levels = item_levels))

# order disciplines on x by mean benefit endorsement (high -> low gradient)
disc_order <- disc_rates %>% filter(type == "Benefit") %>%
  group_by(disc_label) %>% summarise(m = mean(prop), .groups = "drop") %>%
  arrange(desc(m)) %>% pull(disc_label)
disc_rates <- disc_rates %>% mutate(disc_label = factor(disc_label, levels = disc_order))

# ============================ Panel A ============================
theme_A <- theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13, face = "bold"),
        panel.spacing = unit(1.3, "lines"),
        axis.text.y = element_text(size = 11),
        plot.tag = element_text(size = 18, face = "bold"))

panelA <- overall_rates %>%
  mutate(type = recode(type, "Benefit" = "Benefits", "Challenge" = "Challenges")) %>%
  ggplot(aes(prop, item_label, color = type)) +
  geom_pointrange(aes(xmin = lower, xmax = upper), linewidth = 1, size = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_y_discrete(labels = \(x) str_wrap(x, 24)) +
  scale_color_manual(values = c("Benefits" = colors_type[["Benefit"]],
                                "Challenges" = colors_type[["Challenge"]])) +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Perceived endorsement rate", y = NULL) +
  theme_A

# ============================ Panel B ============================
heat_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = 10),
        axis.text.y = element_text(size = 11),
        plot.subtitle = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.margin = margin(4, 8, 4, 4))

heat_panel <- function(df, low, high, subtitle) {
  thr <- 0.62 * max(df$prop, na.rm = TRUE)   # white text on the darker cells, per-panel
  ggplot(df, aes(disc_label, item_label, fill = prop)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = paste0(round(prop * 100), "%"), color = prop > thr), size = 3.1) +
    scale_fill_gradient(low = low, high = high, limits = c(0, NA)) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"), guide = "none") +
    scale_y_discrete(labels = \(x) str_wrap(x, 22)) +
    labs(x = NULL, y = NULL, subtitle = subtitle) +
    heat_theme
}
panelB_ben <- heat_panel(filter(disc_rates, type == "Benefit") %>% droplevels(),
                         heatmap_benefits_low, heatmap_benefits_high, "Benefits")
panelB_chal <- heat_panel(filter(disc_rates, type == "Challenge") %>% droplevels(),
                          heatmap_challenges_low, heatmap_challenges_high, "Challenges")

# ============================ Combine ============================
# Tag manually (A. on the forest, B. on the first heatmap) and nest WITHOUT
# wrap_elements, so patchwork aligns panel B's heatmaps to full width under panel A.
panelA     <- panelA + labs(tag = "A.")
panelB_ben <- panelB_ben + labs(tag = "B.")

fig3 <- panelA / (panelB_ben | panelB_chal) +
  plot_layout(heights = c(1, 1.4)) &
  theme(plot.tag = element_text(face = "bold", size = 18),
        plot.tag.position = c(0, 1))

# ---- export ----
dir.create("figures", showWarnings = FALSE)
ggsave("figures/Figure3.png", fig3, width = 13, height = 11, dpi = 300, bg = "white")
cat("done: figures/Figure3.png\n")
