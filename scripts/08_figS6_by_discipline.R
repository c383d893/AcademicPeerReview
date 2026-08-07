# 08_figS6_by_discipline.R
# Figure S6: a by-field version of the perceived benefits/challenges. For each
# benefit/challenge, one colored point + Wilson CI per discipline, dodged on the
# same row. Same data as the Figure 3B heatmap, drawn as pointranges.
#
# Input:  data/blinding_survey_dat.csv
# Output: figures/FigureS6_perceived_by_discipline.png
# Run from the repo root: Rscript scripts/08_figS6_by_discipline.R

suppressPackageStartupMessages({
  library(tidyverse); library(scales); library(patchwork)
})

# ---- constants / helpers ----
colors_type <- c("Challenge" = "#e6550d", "Benefit" = "#3182bd")
challenge_labels <- c(
  challenge_a = "Author burden", challenge_b = "Editorial office burden",
  challenge_c = "Anonymization not fully successful",
  challenge_d = "Hard to identify conflict of interest",
  challenge_e = "May reduce reviewer participation",
  challenge_f = "Limits contextualizing work")
benefit_labels <- c(
  benefit_a = "More fair/objective review", benefit_b = "Reduces prestige bias",
  benefit_c = "Reduces gender bias", benefit_d = "Reduces country/location bias",
  benefit_e = "Increased review quality")
item_labels <- c(challenge_labels, benefit_labels)

wilson_ci <- function(x, n, conf.level = 0.95) {
  if (n == 0) return(tibble(prop = NA_real_, lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - conf.level) / 2); p_hat <- x / n
  denom <- 1 + z^2 / n
  center <- (p_hat + z^2 / (2 * n)) / denom
  margin <- (z / denom) * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  tibble(prop = p_hat, lower = max(0, center - margin), upper = min(1, center + margin))
}
calc_rates <- function(data, group_vars = NULL) {
  items <- names(item_labels)
  data %>% select(all_of(c(group_vars, items))) %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "endorsed") %>%
    mutate(type = if_else(str_starts(item, "challenge"), "Challenge", "Benefit"),
           item_label = item_labels[item]) %>%
    group_by(across(all_of(c(group_vars, "item", "type", "item_label")))) %>%
    summarise(n_endorsed = sum(endorsed, na.rm = TRUE),
              n_total = sum(!is.na(endorsed)), .groups = "drop") %>%
    rowwise() %>% mutate(ci = list(wilson_ci(n_endorsed, n_total)),
                         prop = ci$prop, lower = ci$lower, upper = ci$upper) %>%
    ungroup() %>% select(-ci)
}

# ---- data ----
all_data <- read_csv("data/blinding_survey_dat.csv", show_col_types = FALSE)
endorsement_dat <- all_data %>% filter(sample == "analytical")
pooled_dat <- endorsement_dat %>%
  filter(pub_model == "Double/triple blind" | !is.na(consid_status))
overall_rates <- calc_rates(pooled_dat) %>%
  group_by(type) %>% mutate(item_label = fct_reorder(item_label, prop)) %>% ungroup()
item_levels <- levels(overall_rates$item_label)

disc_dat <- endorsement_dat %>%
  filter(!is.na(journal_super_category),
         pub_model == "Double/triple blind" | !is.na(consid_status))
disc_n <- disc_dat %>% count(journal_super_category) %>%
  mutate(disc_label = paste0(journal_super_category, " (N=", n, ")"))
disc_rates <- calc_rates(disc_dat, group_vars = "journal_super_category") %>%
  left_join(disc_n, by = "journal_super_category") %>%
  filter(journal_super_category != "Not indicated") %>%   # missing discipline, not a field (matches Fig 3B N=285)
  mutate(item_label = factor(as.character(item_label), levels = item_levels))

# discipline order = by mean benefit endorsement (for legend + color mapping)
disc_order <- disc_rates %>% filter(type == "Benefit") %>%
  group_by(disc_label) %>% summarise(m = mean(prop), .groups = "drop") %>%
  arrange(desc(m)) %>% pull(disc_label)
disc_rates <- disc_rates %>% mutate(disc_label = factor(disc_label, levels = disc_order))

# Dark2 qualitative palette (7 disciplines), colorblind-reasonable
disc_cols <- setNames(
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"),
  disc_order)

# ---- by-field version ----
figS6 <- disc_rates %>%
  mutate(type = recode(type, "Benefit" = "Benefits", "Challenge" = "Challenges")) %>%
  ggplot(aes(prop, item_label, color = disc_label)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  position = position_dodge(width = 0.78),
                  size = 0.32, fatten = 1.7, linewidth = 0.55) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_y_discrete(labels = \(x) str_wrap(x, 26)) +
  scale_color_manual(values = disc_cols, name = "Discipline (N)") +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Perceived endorsement rate", y = NULL) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE,
                              override.aes = list(linewidth = 0.9, size = 0.5))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 13, face = "bold"),
        panel.spacing = unit(1.2, "lines"),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold"))

dir.create("figures", showWarnings = FALSE)
ggsave("figures/FigureS6_perceived_by_discipline.png", figS6,
       width = 13, height = 8.5, dpi = 300)
cat("done: figures/FigureS6_perceived_by_discipline.png\n")
